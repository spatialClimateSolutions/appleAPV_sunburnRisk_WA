import os
from pathlib import Path
import pandas as pd
import requests
import shutil

# ==== USER SETTINGS ====
# Set APPLE_APV_DATA_ROOT to the supplementary-data directory when data are
# stored outside this repository. Otherwise, the repository root is used.
REPO_ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = Path(os.environ.get("APPLE_APV_DATA_ROOT", REPO_ROOT)).expanduser().resolve()

api_key = os.environ.get("NREL_API_KEY")
if not api_key:
    raise RuntimeError("Set the NREL_API_KEY environment variable before running this script.")

loc56_path = DATA_ROOT / "site_locations" / "Loc56.csv"
output_root = DATA_ROOT / "weather" / "NSRDB_Cleaned_Data_56_Sites"

full_name = os.environ.get("NREL_FULL_NAME", "")
email = os.environ.get("NREL_EMAIL", "")
affiliation = os.environ.get("NREL_AFFILIATION", "")

# ==== CONSTANTS ====
YEARS = list(range(2013, 2023))  # 2013–2022
API_URL = "https://developer.nrel.gov/api/nsrdb/v2/solar/nsrdb-GOES-aggregated-v4-0-0-download.csv"

# Search offsets (approx 4 km per 0.04° lat/lon)
SEARCH_STEPS = [0.04, 0.08]  # ~4 km, ~8 km

# ==== LOAD SITES ====
loc_df = pd.read_csv(loc56_path)
print(f"Loaded {len(loc_df)} sites from Loc56.csv.")

# ==== HELPER FUNCTIONS ====
def request_year(lat, lon, year):
    """Try to download one year of data for a given lat/lon."""
    params = {
        "api_key": api_key,
        "wkt": f"POINT({lon} {lat})",
        "names": year,
        "leap_day": "false",
        "interval": "60",    # v4 aggregated supports hourly data
        "utc": "false",
        "full_name": full_name,
        "email": email,
        "affiliation": affiliation,
        "mailing_list": "false"
    }
    response = requests.get(API_URL, params=params)
    if response.status_code == 200 and "ERROR" not in response.text:
        return response.text
    return None


def try_coordinate(fid, lat, lon):
    """
    Try downloading all 10 years for a given coordinate.
    Return True if successful, False otherwise.
    """
    site_folder = os.path.join(output_root, str(fid))
    os.makedirs(site_folder, exist_ok=True)

    for year in YEARS:
        output_file = os.path.join(site_folder, f"solar_{year}_FID{fid}.csv")
        text = request_year(lat, lon, year)
        if text is None:
            # Failure → cleanup partial files
            for f in os.listdir(site_folder):
                os.remove(os.path.join(site_folder, f))
            return False
        with open(output_file, "w", encoding="utf-8") as f:
            f.write(text)
        print(f"  Saved {output_file}")

    return True


def generate_neighbors(lat, lon, step):
    """Generate neighbor coordinates in 8 directions for a given step size."""
    offsets = [
        (step, 0), (-step, 0), (0, step), (0, -step),
        (step, step), (step, -step), (-step, step), (-step, -step)
    ]
    for dlat, dlon in offsets:
        yield lat + dlat, lon + dlon


# ==== MAIN LOOP ====
results = []

for _, row in loc_df.iterrows():
    fid = row["FID"]
    lat, lon = row["Lat"], row["Long"]
    print(f"\n=== Processing FID {fid} at ({lat}, {lon}) ===")

    success = try_coordinate(fid, lat, lon)
    final_lat, final_lon = lat, lon

    if not success:
        # Try fallback search
        found = False
        for step in SEARCH_STEPS:
            print(f"  Attempting fallback search with step {step}°...")
            for nlat, nlon in generate_neighbors(lat, lon, step):
                print(f"   Trying neighbor ({nlat}, {nlon})...")
                if try_coordinate(fid, nlat, nlon):
                    final_lat, final_lon = nlat, nlon
                    found = True
                    break
            if found:
                break

        if not found:
            print(f"❌ Failed for FID {fid}")
            # Ensure no partial folder remains
            site_folder = os.path.join(output_root, str(fid))
            if os.path.exists(site_folder):
                shutil.rmtree(site_folder)
            results.append([fid, lat, lon, None, None, "failed"])
            continue

    results.append([fid, lat, lon, final_lat, final_lon, "success"])

# ==== SAVE SUMMARY ====
summary_file = os.path.join(output_root, "failed_sites.csv")
pd.DataFrame(results, columns=[
    "FID", "original_lat", "original_lon", "final_lat", "final_lon", "status"
]).to_csv(summary_file, index=False)

print(f"\nProcess complete. Summary written to {summary_file}")
