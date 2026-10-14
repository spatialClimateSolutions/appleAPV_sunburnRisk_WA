# appleAPV_sunburnRisk_WA

Code and PVSyst configuration assets for the Washington State apple agrivoltaics and sunburn-risk analysis associated with the journal publication.

The workflow combines NSRDB weather data, PVSyst simulations of agrivoltaic system variants, R-based irradiance and economic analyses, and comparisons of apple sunburn-risk results generated with AgWeatherNet and NSRDB weather data.

## Repository structure

```text
scripts/
├── python/
│   ├── NSRDB_Download.py
│   └── PVSyst_Variant_Download.py
└── R/
    ├── GraphingMaps.R
    ├── IrradiancePlots.R
    └── plot_appleSunburn_AWNvsNSRDBcompare.Rmd
pvsyst_inputs/
├── Models/
├── Meteo/
├── Projects/
├── Sites/
└── PVSyst_asset_manifest.tsv
```

Large weather, site-year, and sunburn-result datasets are not included in this repository. They will be included in a separate data repository as supplementary data accompanying the published paper.

## Workflow

1. Prepare `site_locations/Loc56.csv`.
2. Download hourly NSRDB data with `scripts/python/NSRDB_Download.py`.
3. Run PVSyst simulations with `scripts/python/PVSyst_Variant_Download.py`.
4. Analyze irradiance and capacity factor with `scripts/R/IrradiancePlots.R`.
5. Generate maps and energy/economic summaries with `scripts/R/GraphingMaps.R`.
6. Compare AWN and NSRDB sunburn-risk results with `scripts/R/plot_appleSunburn_AWNvsNSRDBcompare.Rmd`.

## Configuration

The publication-ready copies use the `APPLE_APV_DATA_ROOT` environment variable. If it is not set, scripts use the repository root as the data root.

The expected data layout is:

```text
data_root/
├── site_locations/Loc56.csv
├── weather/NSRDB_Cleaned_Data_56_Sites/<FID>/solar_<year>_FID<FID>.csv
├── pvsyst_inputs/
│   ├── Models/
│   ├── Meteo/
│   ├── Projects/
│   └── Sites/
├── pvsyst_results/
└── sunburn_results/
```

Example:

```bash
export APPLE_APV_DATA_ROOT="/path/to/supplementary-data"
```

The R scripts use `file.path()` and the Python scripts use `pathlib.Path`, so paths work on macOS, Windows, and Linux without editing user-specific Google Drive or Windows paths.

## NSRDB API credentials

`NSRDB_Download.py` requires an NREL API key, but no credential is stored in this repository. Set credentials locally:

```bash
export NREL_API_KEY="your_api_key_here"
export NREL_FULL_NAME="Your Name"
export NREL_EMAIL="your_email@example.org"
export NREL_AFFILIATION="Your Institution"
python scripts/python/NSRDB_Download.py
```

The previously embedded API key and personal request metadata were removed from the public copy. Any previously exposed key should be rotated or revoked.

## Script descriptions

### `NSRDB_Download.py`

Reads site IDs and coordinates from `Loc56.csv`, requests hourly NSRDB GOES aggregated data for 2013–2022, searches nearby coordinates when a site request fails, and writes one weather CSV per site-year plus `failed_sites.csv`.

### `PVSyst_Variant_Download.py`

Converts weather CSV files through PVSyst, runs the configured control and PV variants, extracts `GlobEff` and `E_Grid`, calculates processed bifacial flux, and writes `<year>_processed.csv` files. The current settings run `VCL`, `VC7`, and `VC8`; the repository also archives all 18 `Tracking Apple Panels_Project.VC*` files.

PVSyst 8.0.11 and a compatible PVSyst CLI installation are required. PVSyst is proprietary software and is not included here.

### `IrradiancePlots.R`

Joins processed PVSyst results with weather files, calculates flux/GHI, flux/control, and capacity-factor metrics, writes combined CSV files, produces hourly boxplots, and reports missing files.

### `GraphingMaps.R`

Combines site-year results, calculates irradiance, energy, capacity factor, energy density, and LCOE metrics, and generates Washington State maps and summary figures.

### `plot_appleSunburn_AWNvsNSRDBcompare.Rmd`

Reads AWN and NSRDB sunburn-risk tables, standardizes scenario and variant labels, applies the documented site exclusions, and creates annual, monthly, difference, and reduction figures. It renders to HTML and saves PDF figures.

## Supplementary data

The separate supplementary-data repository should include:

- `site_locations/Loc56.csv`;
- downloaded NSRDB weather files for 2013–2022;
- `failed_sites.csv`;
- the PVSyst inputs listed in `pvsyst_inputs/PVSyst_asset_manifest.tsv`;
- processed PVSyst site-year files;
- `sunburn_hours_day_56_AWN.csv`;
- `sunburn_hours_day_56_NSRDB.csv`; and
- documentation of variables, exclusions, software versions, and processing dates.

These files will be included as supplementary data accompanying the published paper. The final supplementary-data repository URL and publication citation will be added when available.

## Reproducibility notes

Results depend on the NSRDB data version/API endpoint, PVSyst 8.0.11, PVSyst project and variant files, weather-conversion protocols, timestamp/time-zone handling, site exclusions, economic assumptions, and R/Python package versions. For reproducing the publication, use the archived supplementary data rather than downloading replacement data from a changing API.

The PVSyst assets preserve their original contents. Some contain historical absolute paths from earlier computers; these paths are retained for PVSyst provenance and are not used by the publication scripts' path configuration.

## Citation

Please cite the associated journal publication when using this code or the accompanying data. The formal citation and DOI will be added when available.
