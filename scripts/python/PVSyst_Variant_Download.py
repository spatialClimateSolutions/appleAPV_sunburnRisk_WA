import subprocess
import shutil
import os
import time
import pandas as pd
import re
from pathlib import Path

# ==== USER SETTINGS ====
# Set APPLE_APV_DATA_ROOT to the supplementary-data directory when data are
# stored outside this repository. Otherwise, the repository root is used.
REPO_ROOT = Path(__file__).resolve().parents[2]
DATA_ROOT = Path(os.environ.get("APPLE_APV_DATA_ROOT", REPO_ROOT)).expanduser().resolve()
PVSYST_ROOT = DATA_ROOT / "pvsyst_inputs"

pvsyst_exe = "PVsystCLI"

# Path to the PVSyst data folder
base_path = str(PVSYST_ROOT)

# PVSyst Project file, make sure the variants you load are part of the project
project_file = "Tracking Apple Panels_Project.PRJ"

# This file defines the PVSyst outputs
model_file = os.path.join(base_path, "Models", "GlobEff_E_Grid.RVT")

# This is used to index the correct folder when files are outputted, don't change
user_hourly_dir = os.path.join(base_path, "UserHourly")

# Root to weather folder, note that the folder this references will still have site ID subfolders
weather_root_folder = str(DATA_ROOT / "weather" / "NSRDB_Cleaned_Data_56_Sites")

# Output folder, change name per variant
output_root_folder = str(DATA_ROOT / "pvsyst_results" / "HighDensityHats")

# Site files, usually shouldn't matter much but these can be finnicky. If a site is failing, choose a new location 
# closer to it (or check the time zones) and try both
site_file_backup = os.path.join(base_path, "Sites", "Washington State_MN82.SIT")
site_file_primary = os.path.join(base_path, "Sites", "SunriseTesting142.SIT")

# Weather conversion protocol, you will have to change this if you use different weather datasets. This can be done in PVSyst Weather "Custom Files" (See video)
mef_file_primary = os.path.join(base_path, "Meteo", "September_NSRDB2.MEF")
mef_file_secondary = os.path.join(base_path, "Meteo", "NSRDB_Grace_Conversion_Protocol.MEF")

# Control variant is the name of the variant without any PV array over top
# Variant pair refers to the PV site + control and the solo PV site, respectively
control_variant = "VCL"
variant_pair_1 = ("VC7", "VC8")

# DC capacity of each, pv_and_bifacial here is VC2, pv_only is VC3, bifacial_only (control) is VC0
pv_variables = {
    variant_pair_1: {'pv_and_bifacial': 129, 'pv_only': 105, 'bifacial_only': 24}
}



# Don't change this stuff! (or do, but you shouldn't have to) #

# This block loops through all the variants and runs CLI commands for each. At the end of the commands, you should have # outputted 3 files from PVSyst. If they're all from the same year/variant, the script will automatically merge them to # the [year]_processed.csv output we're familiar with


# All variants to run simulations for (control + both from the pair)
all_variants = [control_variant] + list(variant_pair_1)

# Utility function to clean up leftover files if something crashes
def cleanup_site_files(variant_paths):
    for path in variant_paths.values():
        if os.path.exists(path):
            try:
                os.remove(path)
            except Exception:
                pass

# Make sure the "UserHourly" directory exists (this is where PVSyst dumps results)
os.makedirs(user_hourly_dir, exist_ok=True)

# === Loop through each site folder in the weather data ===
for site_id in os.listdir(weather_root_folder):
    site_path = os.path.join(weather_root_folder, site_id)
    if not os.path.isdir(site_path):
        continue  # Skip if it's not a folder

    # === Loop through each weather file inside this site ===
    for file in os.listdir(site_path):
        # Only look at files with names like "solar_2020_FID56.csv"
        match = re.match(r"solar_(\d{4})_FID(\d+)\.csv", file)
        if not match:
            continue

        # Extract the year and site ID (FID) from the filename
        year = match.group(1)
        fid = match.group(2)
        input_csv_path = os.path.join(site_path, file)

        # Define where the processed output will be saved:
        #   output_root_folder/<fid>/<year>_processed.csv
        site_output_dir = os.path.join(output_root_folder, fid)
        os.makedirs(site_output_dir, exist_ok=True)
        output_csv = os.path.join(site_output_dir, f"{year}_processed.csv")

        # Skip if this site/year combo has already been processed
        if os.path.exists(output_csv):
            continue

        # Temporary .MET file that PVSyst needs to run simulations
        meteo_output_path = os.path.join(
            base_path, "Meteo", f"TempSite_{fid}_{year}.MET"
        )
        variant_paths = {}

        try:
            # === Convert raw weather file into PVSyst format ===
            # Try the primary MEF file first, then fall back to the secondary
            convert_success = False
            for mef_file in [mef_file_primary, mef_file_secondary]:
                convert_cmd = [
                    pvsyst_exe, "convert-meteo",
                    f'-icf:{input_csv_path}',  # Input CSV weather file
                    f'-imf:{mef_file}',       # Conversion protocol
                    f'-isf:{site_file_primary}',  # SIT file (site definition)
                    f'-omf:{meteo_output_path}'   # Output MET file
                ]
                try:
                    subprocess.run(convert_cmd, check=True)
                    convert_success = True
                    break
                except subprocess.CalledProcessError:
                    continue

            if not convert_success:
                raise RuntimeError("Conversion failed with both MEF files using primary SIT.")

            # === Run simulations for each variant (control + pair) ===
            for variant in all_variants:
                output_filename = f"WSU_Testing_{variant}_HourlyRes_0.CSV"
                output_path = os.path.join(user_hourly_dir, output_filename)

                # Inner function that actually calls PVSyst CLI
                def run_simulation():
                    cmd = [
                        pvsyst_exe, "run-simulation",
                        f'-w:{base_path}',       # Base PVsyst folder
                        f'-p:{project_file}',    # Project file
                        f'-v:{variant}',         # Variant name
                        f'-imf:{meteo_output_path}',  # Weather file (MET)
                        f'-irf:{model_file}',    # Results template
                        f'-ocf:{output_path}'    # Output CSV path
                    ]
                    subprocess.run(cmd, check=True)

                try:
                    # Run the simulation
                    run_simulation()

                    # Wait briefly for output file to appear (CLI is sometimes slow)
                    for _ in range(10):
                        if os.path.exists(output_path):
                            break
                        time.sleep(1)

                    # If still no output, retry using backup SIT file
                    if not os.path.exists(output_path):
                        convert_cmd_retry = [
                            pvsyst_exe, "convert-meteo",
                            f'-icf:{input_csv_path}',
                            f'-imf:{mef_file_primary}',
                            f'-isf:{site_file_backup}',  # backup site file
                            f'-omf:{meteo_output_path}'
                        ]
                        subprocess.run(convert_cmd_retry, check=True)
                        run_simulation()
                        for _ in range(10):
                            if os.path.exists(output_path):
                                break
                            time.sleep(1)

                    if not os.path.exists(output_path):
                        raise FileNotFoundError(f"Output file for {variant} not found after retry.")

                    # Rename and move the result file into the site folder
                    renamed_path = os.path.join(site_output_dir, f"{variant}_{year}.csv")
                    shutil.move(output_path, renamed_path)
                    variant_paths[variant] = renamed_path

                except Exception as sim_error:
                    raise RuntimeError(f"Simulation failed for {variant}: {sim_error}")

            # === Process the output files into a final clean CSV ===
            # - Reads PVSyst outputs
            # - Combines them
            # - Calculates bifacial flux
            # - Saves final processed file
            df_control = pd.read_csv(variant_paths[control_variant], skiprows=13, header=None, encoding='cp1252', usecols=[0, 1, 2])
            df_control.iloc[:, 1] = df_control.iloc[:, 1].clip(lower=0)
            control_flux = df_control.iloc[:, 1]
            date_time = df_control.iloc[:, 0]

            final_df = pd.DataFrame({
                "Date": date_time,
                "Control Flux (W/m^2)": control_flux
            })

            v1, v2 = variant_pair_1
            params = pv_variables[variant_pair_1]

            df_v1 = pd.read_csv(variant_paths[v1], skiprows=13, header=None, encoding='cp1252', usecols=[0, 1, 2])
            df_v2 = pd.read_csv(variant_paths[v2], skiprows=13, header=None, encoding='cp1252', usecols=[0, 1, 2])

            df_v1_flux = df_v1.iloc[:, 1].clip(lower=0)
            df_v2_flux = df_v2.iloc[:, 1].clip(lower=0)
            df_v2_output = df_v2.iloc[:, 2].clip(lower=0)

            # Equation: (PV+bifacial - PV only) / bifacial-only capacity
            processed_flux = (
                (df_v1_flux * params['pv_and_bifacial'] - df_v2_flux * params['pv_only']) / params['bifacial_only']
            ).clip(lower=0)

            final_df["Variant 1 Flux (W/m^2)"] = processed_flux
            final_df["Variant 1 Solar Output (kW)"] = df_v2_output

            # Clean up raw intermediate outputs
            for path in [variant_paths[v1], variant_paths[v2]]:
                if os.path.exists(path):
                    os.remove(path)
            if os.path.exists(variant_paths[control_variant]):
                os.remove(variant_paths[control_variant])

            # Save the final processed file
            final_df.to_csv(output_csv, index=False, float_format='%.10f')

        except Exception as e:
            print(f"FAILED for site {site_id} year {year}: {e}")
            cleanup_site_files(variant_paths)

        finally:
            # Always clean up the temporary MET file
            if os.path.exists(meteo_output_path):
                try:
                    os.remove(meteo_output_path)
                except Exception:
                    pass
