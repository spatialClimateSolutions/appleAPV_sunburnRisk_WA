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
    ├── FST/
    │   ├── AWN_QA_QC.R
    │   ├── FST_56_AWN_stations.R
    │   └── FST_56_NSRDB_stations.R
    ├── IrradiancePlots.R
    └── plot_appleSunburn_AWNvsNSRDBcompare.Rmd
pvsyst_inputs/
├── Models/
├── Meteo/
├── Projects/
├── Sites/
└── PVSyst_asset_manifest.tsv
```

Large weather, site-year, and sunburn-result datasets are not included in this repository. They are included in a separate data repository as supplementary data accompanying the published paper.

## Workflow

1. Prepare `site_locations/Loc56.csv`
2. Download hourly NSRDB data with `scripts/python/NSRDB_Download.py`
3. Run PVSyst simulations with `scripts/python/PVSyst_Variant_Download.py`
4. Analyze irradiance and capacity factor with `scripts/R/IrradiancePlots.R`
5. Generate maps and energy/economic summaries with `scripts/R/GraphingMaps.R`
6. Compare AWN and NSRDB sunburn-risk results with `scripts/R/plot_appleSunburn_AWNvsNSRDBcompare.Rmd`

## Configuration

The scripts use the `APPLE_APV_DATA_ROOT` environment variable. If it is not set, scripts use the repository root as the data root.

The data layout is:

```text
data_root/
├── site_locations/Loc56.csv
├── awn_raw/
├── awn_qaqc/
│   ├── Combined_data/Complete_data/
│   ├── Combined_data/Cleaned_data/
│   └── diagnostics/
├── weather/NSRDB_Cleaned_Data_56_Sites/<FID>/solar_<year>_FID<FID>.csv
├── pvsyst_inputs/
│   ├── Models/
│   ├── Meteo/
│   ├── Projects/
│   └── Sites/
├── pvsyst_results/
├── pvsyst_results_awn/
├── fst_results/
│   ├── AWN/
│   └── NSRDB/
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

## Script descriptions

### `NSRDB_Download.py`

Reads site IDs and coordinates from `Loc56.csv`, requests hourly NSRDB GOES aggregated data for 2013–2022, searches nearby coordinates when a site request fails, and writes one weather CSV per site-year plus `failed_sites.csv`.

### `PVSyst_Variant_Download.py`

Converts weather CSV files through PVSyst, runs the configured control and PV variants, extracts `GlobEff` and `E_Grid`, calculates processed bifacial flux, and writes `<year>_processed.csv` files. The current settings run `VCL`, `VC7`, and `VC8`; the repository also archives all 18 `Tracking Apple Panels_Project.VC*` files.

PVSyst 8.0.11 and a compatible PVSyst CLI installation are required. PVSyst is proprietary software and is not included here.

### `IrradiancePlots.R`

Joins processed PVSyst results with weather files, calculates flux/GHI, flux/control, and capacity-factor metrics, writes combined CSV files, produces hourly boxplots, and reports missing files.

### Fruit-surface-temperature and sunburn-risk workflow

The scripts in `scripts/R/FST/` are the collaborator’s archived FST workflow. `AWN_QA_QC.R` reads raw AgWeatherNet (AWN) station files, converts Fahrenheit/mph measurements to degrees Celsius and m/s, screens incomplete or implausible station-years, writes cleaned AWN files, and produces weather diagnostics. `FST_56_AWN_stations.R` and `FST_56_NSRDB_stations.R` then join cleaned AWN or NSRDB weather with the corresponding PVSyst Control and Variant 1 irradiance outputs.

Both FST scripts use `agclimtools::fruit_surface_temp()` and evaluate a 43°C fruit-surface-temperature threshold. They calculate a baseline plus Control and Variant 1 scenarios under wind-speed multipliers of 0.90, 0.85, 0.80, 0.75, 0.70, and 0.65 (the script labels these as WS10, WS15, WS20, WS25, WS30, and WS35). Monthly results report threshold-exceedance hours and days. The scripts also write diagnostic comparisons of weather-source radiation against PVSyst radiation.

The publication sunburn-risk tables `sunburn_hours_day_56_AWN.csv` and `sunburn_hours_day_56_NSRDB.csv` are downstream products of these AWN- and NSRDB-based FST simulations, respectively. They are supplied as supplementary results; the raw weather, cleaned weather, PVSyst outputs, and intermediate FST tables are kept outside this code repository because of size and provenance requirements. The exact output filenames produced by the archived scripts are retained in the scripts; publication tables may additionally reflect the project’s final collation and naming step.

The archived QA/QC script contains exploratory sections and unresolved references inherited from the collaborator’s research workflow. Review those sections before running the complete script; the FST scripts are the reproducibility entry points for the sunburn-risk calculations.

### `GraphingMaps.R`

Combines site-year results, calculates irradiance, energy, capacity factor, energy density, and LCOE metrics, and generates Washington State maps and summary figures.

### `plot_appleSunburn_AWNvsNSRDBcompare.Rmd`

Reads AWN and NSRDB sunburn-risk tables, standardizes scenario and variant labels, applies the documented site exclusions, and creates annual, monthly, difference, and reduction figures. It renders to HTML and saves PDF figures.

## Supplementary data

The separate supplementary-data repository includes:

- `site_locations/Loc56.csv`
- the PVSyst inputs listed in `pvsyst_inputs/PVSyst_asset_manifest.tsv`
- processed PVSyst site-year files
- `sunburn_hours_day_56_AWN.csv`
- `sunburn_hours_day_56_NSRDB.csv`

Downloaded NSRDB weather files for 2013–2022 are not made available as part of the journal publication given the size of the data and the fact they are freely and publicly available for download using the provided script.

These files are included as supplementary data accompanying the published paper

## Reproducibility notes

Results depend on the NSRDB data version/API endpoint, PVSyst 8.0.11, PVSyst project and variant files, weather-conversion protocols, timestamp/time-zone handling, site exclusions, economic assumptions, and R/Python package versions. For reproducing the publication, use the archived supplementary data rather than downloading replacement data from a changing API.

The PVSyst assets preserve their original contents. Some contain historical absolute paths from earlier computers; these paths are retained for PVSyst provenance and are not used by the publication scripts' path configuration.

## Citation

The formal citation and DOI will be added when available.
