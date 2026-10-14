library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)

# === Configuration for 6 Variants ===
working_root <- normalizePath(getwd(), mustWork = FALSE)
repo_root <- if (basename(working_root) == "R" && basename(dirname(working_root)) == "scripts") {
  normalizePath(file.path(working_root, "../.."), mustWork = FALSE)
} else {
  working_root
}
data_root <- Sys.getenv("APPLE_APV_DATA_ROOT", unset = repo_root)
data_root <- normalizePath(data_root, mustWork = FALSE)
weather_root <- file.path(data_root, "weather", "NSRDB_Cleaned_Data_56_Sites")
results_root <- file.path(data_root, "pvsyst_results")
output_root <- file.path(data_root, "outputs")

variants <- tibble(
  variant_ID = paste0("V", 2:7),
  resultsFolder = c(
    file.path(results_root, "Tracking - High Density"),
    file.path(results_root, "Tracking - Medium Density"),
    file.path(results_root, "Tracking - Low Density"),
    file.path(results_root, "Hats - High Density"),
    file.path(results_root, "Hats - Medium Density"),
    file.path(results_root, "Hats - Low Density")
  ),
  saveToFolder = c(
    file.path(output_root, "HighDensityTracking_Results"),
    file.path(output_root, "MediumDensityTracking_Results"),
    file.path(output_root, "LowDensityTracking_Results"),
    file.path(output_root, "HighDensityHats_Results"),
    file.path(output_root, "MediumDensityHats_Results"),
    file.path(output_root, "LowDensityHats_Results")
  )
)

capacity_df <- tibble(variant_ID = paste0("V", 2:7), capacity_kW = rep(73.44, 6), area_acres = rep(0.3274146, 6))
years <- 2013:2022
timeAdjustment <- -minutes(30)

# === Missing files collector ===
missing_files <- tibble(variant = character(), location = character(), year = integer(), file_type = character(), path = character())

# === Safe plot function ===
plot_pv_efficiency_by_hour <- function(data, column) {
  if (!column %in% names(data)) return(NULL)
  non_na_rows <- sum(!is.na(data[[column]]))
  if (non_na_rows == 0) return(NULL)
  
  p <- data %>%
    mutate(hour = hour(timestamp),
           value_pct = .data[[column]] * 100) %>%
    filter(!is.na(value_pct), value_pct >= 0, hour >= 4) %>%
    ggplot(aes(x = hour, y = value_pct, group = hour)) +
    geom_boxplot(outlier.size = 0.3, fill = "steelblue", alpha = 0.6) +
    scale_x_continuous(breaks = seq(4, 23, 2)) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "Hour", y = " ") +
    theme_minimal(base_size = 16) +
    theme(
      axis.title.x = element_text(size = 36, face = "bold"),
      axis.title.y = element_text(size = 36, face = "bold"),
      axis.text.x  = element_text(size = 32, face = "bold"),
      axis.text.y  = element_text(size = 32, face = "bold")
    )
  
  return(p)
}

safe_save_plot <- function(filename, plot_obj, width = 11, height = 8.5, units = "in", dpi = 300) {
  if (is.null(plot_obj)) return()
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename, plot = plot_obj, width = width, height = height, units = units, dpi = dpi)
}

# === Load and process a single location-year ===
process_location_year <- function(location_id, year, variant_id, resultsFolder) {
  pv_path <- file.path(resultsFolder, location_id, paste0(year, "_processed.csv"))
  weather_path <- file.path(weather_root, location_id, paste0("solar_", year, "_FID", location_id, ".csv"))
  
  # Collect missing files
  if (!file.exists(pv_path)) {
    missing_files <<- bind_rows(missing_files, tibble(variant = variant_id, location = location_id, year = year, file_type = "PV", path = pv_path))
    return(NULL)
  }
  if (!file.exists(weather_path)) {
    missing_files <<- bind_rows(missing_files, tibble(variant = variant_id, location = location_id, year = year, file_type = "Weather", path = weather_path))
    return(NULL)
  }
  
  # Read PV data
  pv_raw <- read_csv(pv_path, show_col_types = FALSE)
  
  flux_col <- "Variant 1 Flux (W/m^2)"
  kW_col   <- "Variant 1 Solar Output (kW)"
  
  
  if (!(flux_col %in% names(pv_raw)) || !(kW_col %in% names(pv_raw))) return(NULL)
  
  pv <- pv_raw %>%
    mutate(timestamp = parse_date_time(Date, orders = c("mdy HM","dmy HM","ymd HM")) + timeAdjustment,
           location = location_id,
           year = year) %>%
    select(timestamp,
           controlFlux = `Control Flux (W/m^2)`,
           flux = all_of(flux_col),
           kW   = all_of(kW_col))
  
  # Read weather data
  weather <- read_csv(weather_path, skip = 2, show_col_types = FALSE) %>%
    mutate(timestamp = make_datetime(Year, Month, Day, Hour, Minute),
           GHI = as.numeric(GHI),
           DNI = as.numeric(DNI)) %>%
    select(timestamp, GHI, DNI)
  
  # Join and compute percentages
  joined <- left_join(pv, weather, by = "timestamp") %>%
    mutate(
      controlFlux_perc_ghi = ifelse(GHI > 0, controlFlux / GHI, NA),
      perc_ghi = ifelse(GHI > 0, flux / GHI, NA),
      perc_control = ifelse(controlFlux > 0, flux / controlFlux, NA),
      variant_ID = variant_id,
      CF = kW / capacity_df$capacity_kW[capacity_df$variant_ID == variant_id]
    )
  
  return(joined)
}

# === Loop over Variants and save plots ===
for (i in seq_len(nrow(variants))) {
  v_id <- variants$variant_ID[i]
  resultsFolder <- variants$resultsFolder[i]
  saveToFolder <- variants$saveToFolder[i]
  
  locations <- list.dirs(resultsFolder, full.names = FALSE, recursive = FALSE)
  locations <- locations[locations != ""]
  
  all_data <- map_dfr(locations, function(loc) {
    map_dfr(years, ~process_location_year(loc, .x, v_id, resultsFolder))
  })
  
  if (nrow(all_data) == 0) next
  
  # Save combined CSV
  out_csv <- file.path(saveToFolder, paste0("combined_", v_id, ".csv"))
  dir.create(saveToFolder, showWarnings = FALSE, recursive = TRUE)
  write_csv(all_data, out_csv)
  
  # Save plots
  safe_save_plot(file.path(saveToFolder, paste0("percentGHI_boxplot_", v_id, ".png")),
                 plot_pv_efficiency_by_hour(all_data, "perc_ghi"))
  safe_save_plot(file.path(saveToFolder, paste0("percentControl_boxplot_", v_id, ".png")),
                 plot_pv_efficiency_by_hour(all_data, "perc_control"))
  safe_save_plot(file.path(saveToFolder, paste0("controlFluxGHI_boxplot_", v_id, ".png")),
                 plot_pv_efficiency_by_hour(all_data, "controlFlux_perc_ghi"))
}

# === Print missing files summary ===
if (nrow(missing_files) == 0) {
  cat("✅ All files found. No missing files.\n")
} else {
  cat("\n⚠️ Missing files summary:\n")
  missing_files %>%
    arrange(variant, location, year, file_type) %>%
    mutate(path = basename(path)) %>%
    print(n = Inf)
}
