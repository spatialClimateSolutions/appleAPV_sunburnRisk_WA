# === Libraries ===
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(sf)
library(leaflet)
library(scales)
library(tigris)
library(stringr)
library(cowplot)

options(tigris_use_cache = TRUE)
# Set APPLE_APV_DATA_ROOT to the supplementary-data directory when data are
# stored outside this repository. Otherwise, use the repository root.
working_root <- normalizePath(getwd(), mustWork = FALSE)
repo_root <- if (basename(working_root) == "R" && basename(dirname(working_root)) == "scripts") {
  normalizePath(file.path(working_root, "../.."), mustWork = FALSE)
} else {
  working_root
}
data_root <- Sys.getenv("APPLE_APV_DATA_ROOT", unset = repo_root)
data_root <- normalizePath(data_root, mustWork = FALSE)
root_directory <- file.path(data_root, "weather")

# ============================
# Configuration for 56 Sites (From Broken Script)
# ============================
saveToFolder <- file.path(data_root, "outputs", "Apples3.0")

resultsFolder <- file.path(data_root, "pvsyst_results")
weather_root  <- file.path(data_root, "weather", "NSRDB_Cleaned_Data_56_Sites")
all_data_fileName <- "combinedFluxLocYearWeather_56_allSites"
siteLocs <- file.path(data_root, "site_locations", "Loc56.csv")

years <- 2013:2022

# Variants: short ID for lookups, descriptive folder name (From Broken Script)
variantFolders <- tibble(
  variant_ID = c("V2","V3","V4","V5","V6","V7"),
  folder     = c(
    "Tracking - High Density",
    "Tracking - Medium Density", 
    "Tracking - Low Density",
    "Hats - High Density",
    "Hats - Medium Density",
    "Hats - Low Density"
  )
)

# Capacity lookup (From Broken Script)
capacity_df <- tibble(
  variant_ID  = c("V2","V3","V4","V5","V6","V7"),
  capacity_kW = c(119.68, 76.16, 47.3, 105, 80, 58.8),
  area_acres  = c(0.3274146, 0.3274146, 0.3274146, 0.2619317, 0.2619317, 0.2619317)
)

# Site lookup - 56 sites (From Broken Script)
site_lookup <- read_csv(siteLocs) %>%
  transmute(
    location_name = filename,
    SITE_ID       = as.character(FID),  # Keep as character for folder matching
    latitude      = Lat,
    longitude     = Long
  )

# --- CAPEX & OPEX (From Working Script, adjusted for V2-V7 only) ---
CAPEX_df <- tibble(
  variant_ID = c("V2","V3","V4","V5","V6","V7"),
  CAPEX      = c(1703.00, 1928.00, 1998.61, 1421.21, 1469.06, 1544.43),
  OPEX       = c(27.24,   30.06,   30.95,   23.77,   24.37,   25.32) # $/kW/yr
)

# --- Financial assumptions ---
i <- 0.04; n <- 30
CRF <- (i * (1+i)^n) / (((1+i)^n) - 1)

timeAdjustment <- -minutes(30)

# === Helper: safe save ===
safe_save_plot <- function(filename, plot_obj, w=11, h=8.5, dpi=300, units="in") {
  if (is.null(plot_obj)) return(invisible(FALSE))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggsave(filename, plot = plot_obj, width = w, height = h, units = units, dpi = dpi)
  message("Saved: ", filename)
  invisible(TRUE)
}

# === MODIFIED Helper function: process one site-year for a variant folder ===
# Adapted for 56-site naming conventions
process_location_year <- function(variant_folder_name, location_id, year) {
  # Construct paths using Broken Script's naming convention
  pv_path <- file.path(resultsFolder, variant_folder_name, location_id, paste0(year, "_processed.csv"))
  
  # KEY CHANGE: Weather files use "FID" prefix (from Broken Script)
  weather_path <- file.path(weather_root, location_id, paste0("solar_", year, "_FID", location_id, ".csv"))
  
  if (!file.exists(pv_path)) { 
    warning("Missing PV: ", pv_path)
    return(NULL) 
  }
  if (!file.exists(weather_path)) {
    warning("Missing Weather: ", weather_path)
    return(NULL)
  }
  
  message("Processing: ", variant_folder_name, " | Site: ", location_id, " | Year: ", year)
  
  # Read PV data (same format as Working Script)
  pv_raw <- read_csv(pv_path, show_col_types = FALSE)
  
  # Extract short variant id from mapping table
  variant_info <- variantFolders %>% filter(folder == variant_folder_name)
  if (nrow(variant_info) == 0) {
    warning("Variant folder not found in mapping: ", variant_folder_name)
    return(NULL)
  }
  short_id <- variant_info$variant_ID[1]
  
  pv <- pv_raw %>%
    mutate(
      timestamp   = parse_date_time(.[[1]], orders = c("mdy HM","dmy HM","ymd HM")) + timeAdjustment,
      location    = as.character(location_id),   # Keep as character to match site_lookup
      year        = year,
      controlFlux = .[[2]],
      flux        = .[[3]],
      kW          = .[[4]],
      variant_ID  = short_id
    ) %>%
    # 🔧 Apply scaling for V5–V7 if needed (from Working Script)
    mutate(
      kW = ifelse(variant_ID %in% c("V5"), kW * 0.001, kW)
    ) %>%
    select(timestamp, location, year, controlFlux, flux, kW, variant_ID)
  
  # Read weather data (same as Working Script)
  weather <- read_csv(weather_path, skip = 2, show_col_types = FALSE) %>%
    mutate(
      timestamp = make_datetime(Year, Month, Day, Hour, Minute),
      GHI = as.numeric(GHI),
      DNI = as.numeric(DNI)
    ) %>%
    select(timestamp, GHI, DNI)
  
  joined <- left_join(pv, weather, by = "timestamp") %>%
    mutate(
      controlFlux_perc_ghi = ifelse(GHI > 0, controlFlux / GHI, NA_real_),
      perc_ghi             = ifelse(GHI > 0, flux / GHI,        NA_real_),
      perc_control         = ifelse(controlFlux > 0, flux / controlFlux, NA_real_),
      CF                   = kW / capacity_df$capacity_kW[capacity_df$variant_ID == short_id]
    )
  
  # Light diagnostics
  na_ghi  <- sum(is.na(joined$GHI))
  total_n <- nrow(joined)
  if (na_ghi > 0) {
    message(sprintf("  Note: %s | %s | %d rows (%.1f%%) missing GHI after join.",
                    variant_folder_name, location_id, na_ghi, 100*na_ghi/total_n))
  }
  joined
}

# === Optional: hourly % boxes (From Working Script) ===
plot_pv_efficiency_by_hour <- function(data, column) {
  non_na_rows <- sum(!is.na(data[[column]]))
  if (non_na_rows == 0) return(NULL)
  data %>%
    mutate(hour = hour(timestamp),
           value_pct = .data[[column]] * 100) %>%
    filter(!is.na(value_pct), value_pct >= 0) %>%
    ggplot(aes(x = hour, y = value_pct, group = hour)) +
    geom_boxplot(outlier.size = 0.3, fill = "steelblue", alpha = 0.6) +
    scale_x_continuous(breaks = seq(0, 23, 2)) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal() +
    labs(x = "Hour", y = "%", title = paste("Hourly", column))
}

# ============================
# MAIN PROCESSING PIPELINE
# ============================

# Use variant folder names from mapping table (not folder detection)
variant_folder_names <- variantFolders$folder

for (variant_folder in variant_folder_names) {
  cat("\n=== Processing variant folder:", variant_folder, "===\n")
  
  # Get sites for this variant (check which ones exist)
  variant_path <- file.path(resultsFolder, variant_folder)
  if (!dir.exists(variant_path)) {
    warning("Variant folder does not exist: ", variant_path)
    next
  }
  
  sites <- list.dirs(variant_path, full.names = FALSE, recursive = FALSE)
  sites <- sites[sites != ""]
  
  # Filter to only sites that exist in our site_lookup
  valid_sites <- sites[sites %in% site_lookup$SITE_ID]
  if (length(valid_sites) == 0) {
    warning("No valid sites found for variant: ", variant_folder)
    next
  }
  
  cat("Processing", length(valid_sites), "sites for variant", variant_folder, "\n")
  
  variant_data <- map_dfr(valid_sites, function(loc) {
    map_dfr(years, ~process_location_year(variant_folder, loc, .x))
  })
  
  if (nrow(variant_data) == 0) { 
    warning("No data loaded for: ", variant_folder) 
    next 
  }
  
  # Save variant-specific data
  variant_out <- file.path(saveToFolder, variant_folder)
  dir.create(variant_out, showWarnings = FALSE, recursive = TRUE)
  write_csv(variant_data, file.path(variant_out, paste0(all_data_fileName, "_", variant_folder, ".csv")))
  
  safe_save_plot(file.path(variant_out, "percentGHI_boxplot.png"),
                 plot_pv_efficiency_by_hour(variant_data, "perc_ghi"))
}

# === Collect all data (From Working Script) ===
all_data <- map_dfr(variant_folder_names, function(variant_folder) {
  file_path <- file.path(saveToFolder, variant_folder, paste0(all_data_fileName, "_", variant_folder, ".csv"))
  if (file.exists(file_path)) {
    read_csv(file_path, show_col_types = FALSE) %>%
      left_join(capacity_df, by = "variant_ID")
  } else NULL
}) %>%
  mutate(variant_name = case_when(
    variant_ID == "V2" ~ "Tracking - High Density",
    variant_ID == "V3" ~ "Tracking - Medium Density",
    variant_ID == "V4" ~ "Tracking - Low Density",
    variant_ID == "V5" ~ "EW Dual-Tilt - High Density",
    variant_ID == "V6" ~ "EW Dual-Tilt - Medium Density",
    variant_ID == "V7" ~ "EW Dual-Tilt - Low Density",
    .default = variant_ID))

all_data <- mutate(all_data, variant_name = factor(variant_name, levels = c(
  "EW Dual-Tilt - Low Density",
  "EW Dual-Tilt - Medium Density",
  "EW Dual-Tilt - High Density",
  "Tracking - Low Density",
  "Tracking - Medium Density",
  "Tracking - High Density"),
  labels = c("EW Dual-Tilt\n Low Density",
             "EW Dual-Tilt\nMedium Density",
             "EW Dual-Tilt\nHigh Density",
             "Tracking\nLow Density",
             "Tracking\nMedium Density",
             "Tracking\nHigh Density")))

cat("\nall_data shape: "); print(dim(all_data))
cat("NA counts in all_data:\n"); print(colSums(is.na(all_data)))

# ============================
# REST OF WORKING SCRIPT ANALYSIS PIPELINE
# ============================

# Shading analysis
mean_shading_byLoc_df <- all_data %>%
  group_by(variant_ID, location) %>%
  summarize(avg_perc_control = mean(perc_control, na.rm = TRUE), .groups = "drop")

p_shading <- ggplot(mean_shading_byLoc_df, aes(x = variant_ID, y = avg_perc_control)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Shading Efficiency by Variant", y = "Average Flux / ControlFlux", x = "Variant")

safe_save_plot(file.path(saveToFolder, "shading_boxplot.png"), p_shading)

# Capacity Factor & Energy aggregation
all_data_perSiteperYear <- all_data %>%
  group_by(location, year, variant_ID,variant_name, ) %>%
  summarize(
    avg_annual_CF    = mean(CF,  na.rm = TRUE),
    total_annual_kWh = sum(kW,   na.rm = TRUE),
    avg_annual_GHI   = mean(GHI, na.rm = TRUE),
    area_acres       = mean(area_acres, na.rm = TRUE),
    capacity_kW      = mean(capacity_kW, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    annual_CF_check = total_annual_kWh / (8760 * capacity_kW)
  )

all_data_perSite <- all_data_perSiteperYear %>%
  group_by(location, variant_ID, variant_name, capacity_kW, area_acres) %>%
  summarize(
    avg_annual_CF   = mean(annual_CF_check, na.rm = TRUE),
    avg_annual_kWh  = mean(total_annual_kWh, na.rm = TRUE),
    avg_annual_GHI  = mean(avg_annual_GHI,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    energyDensity_kWhperAcre = avg_annual_kWh / area_acres,
    energyDensity_MWhperAcre = energyDensity_kWhperAcre / 1000,
    avg_annual_MWh_scaled    = avg_annual_kWh / 1000
  )

# Boxplots
p_cf_box <- ggplot(all_data_perSite, aes(x = variant_ID, y = avg_annual_CF, fill = variant_ID)) +
  geom_boxplot() + theme_minimal() + labs(title = "Capacity Factor per Variant", y = "Average CF", x = "Variant")
safe_save_plot(file.path(saveToFolder, "capacity_factor_boxplot.png"), p_cf_box)

p_edensity_box <- ggplot(all_data_perSite, aes(x = variant_ID, y = energyDensity_MWhperAcre, fill = variant_ID)) +
  geom_boxplot() + theme_minimal() + labs(title = "Energy Density per Acre", y = "MWh / acre", x = "Variant")
safe_save_plot(file.path(saveToFolder, "energy_density_boxplot.png"), p_edensity_box)

# GHI calculation
all_data_GHI_perDay <- all_data %>%
  mutate(day = day(timestamp), month = month(timestamp), year = year(timestamp)) %>%
  group_by(location, day, month, year, variant_ID, variant_name) %>%
  summarize(kWh_m2_day = sum(GHI, na.rm = TRUE)/1000, .groups = "drop") %>%
  group_by(location) %>%
  summarize(GHI_kWh_m2_day = mean(kWh_m2_day, na.rm = TRUE), .groups = "drop")

# Merge with site lookup + LCOE calculation

all_data_perSite_LCOE <- all_data_perSite %>%
  left_join(CAPEX_df, by = "variant_ID") %>%
  mutate(
    denom_kWh_perkWyr = 8760 * avg_annual_CF,
    LCOE_USDperkWh    = ifelse(denom_kWh_perkWyr > 0,
                               (CAPEX * CRF + OPEX) / denom_kWh_perkWyr,
                               NA_real_),
    LCOE_USDperMWh    = LCOE_USDperkWh * 1000
  ) %>%
  select(-denom_kWh_perkWyr) %>%
  # Convert location to character to match SITE_ID
  mutate(location = as.character(location)) %>%
  left_join(site_lookup, by = c("location" = "SITE_ID")) %>%
  # Also convert location in GHI data to character for the join
  left_join(all_data_GHI_perDay %>% mutate(location = as.character(location)), 
            by = "location")


# Save the table used for mapping
write_csv(all_data_perSite_LCOE, file.path(saveToFolder, "all_data_perSite_LCOE.csv"))

cat("\nall_data_perSite_LCOE shape: "); print(dim(all_data_perSite_LCOE))
cat("NA counts in all_data_perSite_LCOE:\n"); print(colSums(is.na(all_data_perSite_LCOE)))

# ============================
# MAPPING SECTION (From Working Script)
# ============================
wa_counties <- counties(state = "WA", cb = TRUE, class = "sf") %>% st_transform(4326)

# County labels
county_labels <- wa_counties %>%
  mutate(label = NAME, geometry = st_point_on_surface(geometry)) %>%
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2])

# Build sf for sites
representatives_sf <- all_data_perSite_LCOE %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

# Generic per-variant map function
make_map_by_variant <- function(column, variant_id, legend_title, scale_mult = 1) {
  dat <- representatives_sf %>% filter(variant_ID == variant_id)
  if (nrow(dat) == 0) return(NULL)
  
  dat <- dat %>% mutate(.map_value = .data[[column]] * scale_mult)
  
  ggplot() +
    geom_sf(data = wa_counties, fill = "#f1f1f1", color = "#bdbdbd", size = 0.3) +
    geom_sf(data = dat, aes(fill = .map_value), shape = 21, size = 4, color = "black", stroke = 0.6) +
    geom_text(data = county_labels, aes(x = lon, y = lat, label = label),
              size = 3, color = "#585858", check_overlap = TRUE) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1,
                         guide = guide_colorbar(barheight = grid::unit(5, "cm"),
                                                barwidth = grid::unit(0.5, "cm")),
                         name = legend_title) +
    coord_sf(xlim = c(-125, -116), ylim = c(45.5, 49.2), expand = FALSE) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "right",
      legend.text  = element_text(size = 8),
      legend.title = element_text(size = 9),
      axis.text    = element_blank(),
      axis.ticks   = element_blank(),
      panel.grid   = element_blank(),
      axis.title   = element_blank()
    )
}

# Make maps for each variant
variant_list <- sort(unique(representatives_sf$variant_ID))

for (v in variant_list) {
  p_cf_map <- make_map_by_variant("avg_annual_CF", v, "Average annual\nCapacity Factor", 1)
  safe_save_plot(file.path(saveToFolder, paste0("map_CF_", v, ".png")), p_cf_map, w=6, h=4)
  
  p_ed_map <- make_map_by_variant("energyDensity_MWhperAcre", v, "Energy density\n(MWh / acre / yr)", 1)
  safe_save_plot(file.path(saveToFolder, paste0("map_energyDensity_", v, ".png")), p_ed_map, w=6, h=4)
  
  p_lcoe_map <- make_map_by_variant("LCOE_USDperMWh", v, "LCOE ($/MWh)", 1)
  safe_save_plot(file.path(saveToFolder, paste0("map_LCOE_", v, ".png")), p_lcoe_map, w=6, h=4)
}

# GHI map
ghi_sf <- all_data_perSite_LCOE %>%
  distinct(location, GHI_kWh_m2_day, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

p_ghi <- ggplot() +
  geom_sf(data = wa_counties, fill = "#f1f1f1", color = "#bdbdbd", size = 0.3) +
  geom_sf(data = ghi_sf, aes(fill = GHI_kWh_m2_day), shape = 21, size = 4, color = "black", stroke = 0.6) +
  geom_text(data = county_labels, aes(x = lon, y = lat, label = label),
            size = 3, color = "#585858", check_overlap = TRUE) +  ### FOR COUNTY LABEL TEXT SIZE
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       guide = guide_colorbar(barheight = grid::unit(5, "cm"),
                                              barwidth  = grid::unit(0.5, "cm")),
                       name = "Avg GHI\n(kWh/m²/day)") +
  coord_sf(xlim = c(-125, -116), ylim = c(45.5, 49.2), expand = FALSE) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text    = element_blank(),
    axis.ticks   = element_blank(),
    panel.grid   = element_blank(),
    axis.title   = element_blank()
  )

safe_save_plot(file.path(saveToFolder, "map_GHI.png"), p_ghi, w=6, h=4)

# LCOE boxplot
p_lcoe_box <- all_data_perSite_LCOE %>%
  ggplot(aes(x = variant_name, y = LCOE_USDperkWh*1000)) +
  geom_boxplot(outlier.size = 0.3, fill = "steelblue", alpha = 0.7) +
  theme_bw(base_size = 9)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),strip.background = element_blank(),           # removes facet background
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey95", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey95", linetype = "dashed"),
        plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm")) + # Increase margins 
  geom_jitter(width = 0.15, alpha = 0.6) +
  #theme_minimal(base_size = 9) +
  labs(x = "Variant", y = "Levelized Cost of Electricity\n(USD/MWh)")

safe_save_plot(file.path(saveToFolder, "LCOE_boxplot_minimal.pdf"), p_lcoe_box, w=5, h=4)

# NA troubleshooting report
na_report <- all_data_perSite_LCOE %>%
  transmute(
    variant_ID, location,
    missing_CF          = is.na(avg_annual_CF),
    missing_CAPEX       = is.na(CAPEX),
    missing_OPEX        = is.na(OPEX),
    missing_coords      = is.na(latitude) | is.na(longitude),
    missing_LCOEperkWh  = is.na(LCOE_USDperkWh),
    missing_LCOEperMWh  = is.na(LCOE_USDperMWh)
  ) %>%
  group_by(variant_ID) %>%
  summarize(across(starts_with("missing_"), ~sum(.x, na.rm = TRUE)), .groups = "drop")

write_csv(na_report, file.path(saveToFolder, "NA_report_by_variant.csv"))
print(na_report)

# ============================
# COMPOSITE VISUALIZATIONS (From Working Script)
# ============================
# === CUSTOM COMPOSITE: V2–V7 LCOE maps + one LCOE boxplot ===
library(patchwork)

# --- Subset variants ---
rep_v2to7 <- representatives_sf %>%
  filter(variant_ID %in% paste0("V", 2:7))

# --- Custom titles ---
variant_titles <- c(
  "V2" = "Tracking - High Density",
  "V3" = "Tracking - Medium Density",
  "V4" = "Tracking - Low Density",
  "V5" = "EW Dual-Tilt - High Density",
  "V6" = "EW Dual-Tilt - Medium Density",
  "V7" = "EW Dual-Tilt - Low Density"
)

# --- Get global LCOE range for all maps ---
lcoe_range <- range(rep_v2to7$LCOE_USDperMWh, na.rm = TRUE)

# --- Map helper (no legend inside each map) ---
makeMap_single <- function(data, column, legend_title, v_label){
  dat <- data %>% filter(variant_ID == v_label)
  
  ggplot() +
    geom_sf(data = wa_counties, fill = "#f1f1f1",
            color = "#bdbdbd", size = 0.3) +
    geom_sf(data = dat,
            aes(fill = .data[[column]]),
            shape = 21, size = 3, color = "black", stroke = 0.3) +
    geom_text(data = county_labels,
              aes(x = lon, y = lat, label = label),
              size = 1.5, color = "#585858", check_overlap = FALSE) +
    geom_text(data = dat,
              aes(x = longitude, y = latitude,
                  label = round(.data[[column]])),
              size = 2, nudge_x = 0, color = "black", check_overlap = TRUE) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1,
                         limits = lcoe_range,
                         name = legend_title) +
    coord_sf(xlim = c(-125, -116), ylim = c(45.5, 49.2), expand = FALSE) +
    ggtitle(variant_titles[[v_label]]) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "none",  # suppress individual legends
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(1, 1, 1, 1)
    )
}

# --- Make maps (no legend individually) ---
map_v2 <- makeMap_single(rep_v2to7, "LCOE_USDperMWh", "LCOE (USD/MWh)", "V2")
map_v3 <- makeMap_single(rep_v2to7, "LCOE_USDperMWh", "LCOE (USD/MWh)", "V3")
map_v4 <- makeMap_single(rep_v2to7, "LCOE_USDperMWh", "LCOE (USD/MWh)", "V4")
map_v5 <- makeMap_single(rep_v2to7, "LCOE_USDperMWh", "LCOE (USD/MWh)", "V5")
map_v6 <- makeMap_single(rep_v2to7, "LCOE_USDperMWh", "LCOE (USD/MWh)", "V6")
map_v7 <- makeMap_single(rep_v2to7, "LCOE_USDperMWh", "LCOE (USD/MWh)", "V7")

# --- Shared legend (use dummy plot with global range) ---
legend_plot <- ggplot(rep_v2to7, aes(x = longitude, y = latitude,
                                     fill = LCOE_USDperMWh)) +
  geom_point(shape = 21, size = 3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       limits = lcoe_range,
                       name = "LCOE (USD/MWh)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.height = unit(0.6, "cm"),
        legend.key.width  = unit(0.3, "cm"),
        legend.text  = element_text(size = 7),
        legend.title = element_text(size = 8)) +
  guides(fill = guide_colorbar(barheight = 0.5, barwidth = 5, direction = "horizontal"))

shared_legend <- cowplot::get_legend(legend_plot)
shared_legend <- cowplot::get_plot_component(legend_plot, 'guide-box-bottom', return_all = TRUE)
legend_boxplot_as_plot <- as.ggplot(shared_legend)  # requires ggplotify


# --- LCOE boxplot (all V2–V7, single color) ---
lcoe_box <- ggplot(rep_v2to7,
                   aes(x = variant_ID, y = LCOE_USDperMWh)) +
  geom_boxplot(fill = "lightblue", outlier.size = 0.3, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  labs(y = "LCOE ($/MWh)", x = NULL,
       title = "Levelized Cost of Energy (V2–V7)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

# --- Arrange 6 maps in 3 rows x 2 cols ---
maps_grid <- (map_v5 | map_v2) /
  (map_v6 | map_v3) /
  (map_v7 | map_v4)

# --- Add shared legend to the right of the maps grid ---
maps_with_legend <- cowplot::plot_grid(maps_grid, shared_legend,
                                       nrow = 2,
                                       rel_heights = c(1, 0.1))

# --- Final composite: maps + boxplot ---
composite_custom <- wrap_plots(
  maps_with_legend,
  wrap_plots(
    plot_spacer(),    # adds padding to left of boxplot only
    p_lcoe_box,
    plot_spacer(), 
    widths = c(0.02, 0.96,0.02),
    ncol = 3
  ),
  ncol = 1,
  heights = c(5, 1)
) #+ plot_annotation(tag_levels = 'A', theme = theme(plot.tag = element_text(size = 12, face = "bold"), plot.tag.position = c(0, 1)))

# --- Save ---
safe_save_plot(file.path(saveToFolder, "Composite_V2toV7_maps_LCOEbox.png"),
               composite_custom, w=12, h=14)
safe_save_plot(file.path(saveToFolder, "Composite_V2toV7_maps_LCOEbox_theme_bw.pdf"),
               composite_custom, w=6.5, h=9)




# --- Variant titles for legends ---
variant_titles_CF <- list(
  V2 = "Tracking - \nHigh Density",
  V3 = "Tracking - \nMedium Density",
  V4 = "Tracking - \nLow Density",
  V5 = "EW Dual-Tilt - \nHigh Density",
  V6 = "EW Dual-Tilt - \nMedium Density",
  V7 = "EW DUal-Tilt - \nLow Density"
)

variant_titles_ED <- list(
  V2 = "Tracking - \nHigh Density",
  V3 = "Tracking - \nMedium Density",
  V4 = "Tracking - \nLow Density",
  V5 = "EW Dual-Tilt - \nHigh Density",
  V6 = "EW Dual-Tilt - \nMedium Density",
  V7 = "EW DUal-Tilt - \nLow Density"
)







# ============================
# Composite: WA map (V2 energy density) + all variant-specific legends (grid layout)
# ============================

# --- Base map: V2 Energy Density ---
map_v2_ed <- make_map_by_variant(
  column = "energyDensity_MWhperAcre",
  variant_id = "V2",
  legend_title = "Energy density\n(MWh / acre / yr)"
) +
  theme(legend.position = "none")  # remove legend from map

# --- Helper: extract legend for one variant ---
get_energy_legend <- function(v_id) {
  dat <- representatives_sf %>% filter(variant_ID == v_id)
  
  val_min <- min(dat$energyDensity_MWhperAcre, na.rm = TRUE)
  val_max <- max(dat$energyDensity_MWhperAcre, na.rm = TRUE)
  if (val_max == val_min) val_max <- val_min + 1e-3
  
  legend_breaks <- seq(val_min, val_max, length.out = 4)
  
  gg <- ggplot(dat, aes(x = longitude, y = latitude, fill = energyDensity_MWhperAcre)) +
    geom_point(shape = 21, size = 3, color = "black") +
    scale_fill_distiller(
      palette = "YlOrRd", direction = 1,
      name = variant_titles_ED[[v_id]],
      breaks = legend_breaks,
      labels = format(round(legend_breaks, 0), nsmall = 0),   # 0 decimals
      guide = guide_colorbar(
        label.position = "right",
        label.hjust = 0.5,  # center the text labels
        ticks = TRUE
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.key.height = unit(0.8, "cm"),  #LEGEND SIZE
      legend.key.width  = unit(0.55, "cm"),
      legend.text  = element_text(size = 9),
      legend.title = element_text(size = 10) ##LEGEND TEXT SIZE
    )
  
  cowplot::get_legend(gg)
}


# --- Extract legends for V2–V7 ---
energy_legends <- lapply(paste0("V", 2:7), get_energy_legend)

# --- Arrange legends in two rows of three ---
upper_legends_ed <- cowplot::plot_grid(energy_legends[[1]], energy_legends[[2]], energy_legends[[3]], ncol = 3)
lower_legends_ed <- cowplot::plot_grid(energy_legends[[4]], energy_legends[[5]], energy_legends[[6]], ncol = 3)

upper_legends_ed_shifted <- ggdraw() + draw_plot(upper_legends_ed, y = -0.02)
lower_legends_ed_shifted <- ggdraw() + draw_plot(lower_legends_ed, y = 0.5)

legends_grid_ed <- plot_grid(upper_legends_ed_shifted, lower_legends_ed_shifted,
                             ncol = 1, rel_heights = c(1, 0.6))

# --- Combine map and legends ---
energy_density_composite <- plot_grid(
  map_v2_ed, legends_grid_ed,
  ncol = 2, rel_widths = c(2, 1)
)

# --- Save ---
safe_save_plot(file.path(saveToFolder, "Composite_V2toV7_map_EnergyDensity_allLegends.png"),
               energy_density_composite, w = 12, h = 7)
safe_save_plot(file.path(saveToFolder, "Composite_V2toV7_map_EnergyDensity_allLegends.pdf"),
               energy_density_composite, w = 12, h = 7)

# ============================
# Composite: WA map (V2 Capacity Factor) + all variant-specific legends (grid layout)
# ============================

# --- Base map: V2 Capacity Factor ---
map_v2_cf <- make_map_by_variant(
  column = "avg_annual_CF",
  variant_id = "V2",
  legend_title = "Capacity Factor"
) +
  theme(legend.position = "none")  # remove legend from map

# --- Helper: extract legend for one variant ---
get_cf_legend <- function(v_id) {
  dat <- representatives_sf %>% filter(variant_ID == v_id)
  
  val_min <- min(dat$avg_annual_CF, na.rm = TRUE)
  val_max <- max(dat$avg_annual_CF, na.rm = TRUE)
  if (val_max == val_min) val_max <- val_min + 1e-3
  
  legend_breaks <- seq(val_min, val_max, length.out = 4)
  
  gg <- ggplot(dat, aes(x = longitude, y = latitude, fill = avg_annual_CF)) +
    geom_point(shape = 21, size = 3, color = "black") +
    scale_fill_distiller(
      palette = "YlOrRd", direction = 1,
      name = variant_titles_CF[[v_id]],
      breaks = legend_breaks,
      labels = format(round(legend_breaks, 3), nsmall = 3),  # 3 decimals
      guide = guide_colorbar(
        label.position = "right",
        label.hjust = 0.5,  # center the text labels
        ticks = TRUE
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.key.height = unit(0.8, "cm"), #Legend Size
      legend.key.width  = unit(0.55, "cm"),
      legend.text  = element_text(size = 9),
      legend.title = element_text(size = 10)
    )
  
  cowplot::get_legend(gg)
}

# --- Extract legends for V2–V7 ---
cf_legends <- lapply(paste0("V", 2:7), get_cf_legend)

# --- Arrange legends in two rows of three ---
upper_legends_cf <- cowplot::plot_grid(cf_legends[[1]], cf_legends[[2]], cf_legends[[3]], ncol = 3)
lower_legends_cf <- cowplot::plot_grid(cf_legends[[4]], cf_legends[[5]], cf_legends[[6]], ncol = 3)

upper_legends_cf_shifted <- ggdraw() + draw_plot(upper_legends_cf, y = -0.02)
lower_legends_cf_shifted <- ggdraw() + draw_plot(lower_legends_cf, y = 0.5)

legends_grid_cf <- plot_grid(upper_legends_cf_shifted, lower_legends_cf_shifted,
                             ncol = 1, rel_heights = c(1, 0.6))

# --- Combine map and legends ---
capacity_factor_composite <- plot_grid(
  map_v2_cf, legends_grid_cf,
  ncol = 2, rel_widths = c(2, 1)
)

# --- Save ---
safe_save_plot(file.path(saveToFolder, "Composite_V2toV7_map_CF_allLegends.png"),
               capacity_factor_composite, w = 12, h = 7)
safe_save_plot(file.path(saveToFolder, "Composite_V2toV7_map_CF_allLegends.pdf"),
               capacity_factor_composite, w = 12, h = 7)



##======================##
## LCOE NEW##
# === Map function, styled like your example ===
# === LCOE Map (only V2–V7, labels inside circles, bold) ===
makeMaps_LCOE <- function(data, column, varName){
  data_filtered <- data %>%
    filter(variant_ID %in% paste0("V", 2:7))   # 🔒 keep only V2–V7
  
  ggplot() +
    geom_sf(data = wa_counties, fill = "#f1f1f1", color = "#bdbdbd", size = 0.3) +
    
    geom_sf(data = data_filtered, aes(fill = .data[[column]]),
            shape = 21, size = 3.5, color = "black", stroke = 0.3) +  #changing size for circle size
    
    # REMOVED: Number labels inside circles
    
    
    # 🔒 Labels inside the circles, bolded
    geom_text(data = data_filtered %>% mutate(label_val = round(.data[[column]], 0)),
              aes(x = st_coordinates(geometry)[,1],
                  y = st_coordinates(geometry)[,2],
                  label = label_val),
              size = 2, #Changing this, text within circles
              color = "black", 
              fontface = "bold",
              check_overlap = TRUE) +
    
    geom_text(data = county_labels,
              aes(x = lon, y = lat, label = label),
              size = 1.5, color = "#585858", check_overlap = TRUE) + #Changing size for county labels
    
    scale_fill_distiller(palette = "YlOrRd", direction = -1, name = varName) +
    
    guides(fill = guide_colorbar(direction = "horizontal",
                                 barwidth = 10, barheight = 0.5,
                                 title.position = "top")) +
    
    coord_sf(xlim = c(-125, -116), ylim = c(45.5, 49.2), expand = FALSE) +
    
    facet_wrap(~variant_ID, ncol = 3, labeller = as_labeller(variant_titles)) +
    
    theme_minimal(base_size = 10) +  #base font size
    theme(
      legend.position = "bottom",
      legend.key.height = unit(0.5, "cm"),
      legend.key.width  = unit(0.3, "cm"),
      legend.text  = element_text(size = 6),
      legend.title = element_text(size = 6),
      axis.text    = element_blank(),
      axis.ticks   = element_blank(),
      panel.grid   = element_blank(),
      axis.title   = element_blank(),
      panel.spacing = unit(0.01, "lines"),
      plot.margin  = margin(0,0,0,0)
    )
}

# === Example call ===
LCOE_map <- makeMaps_LCOE(representatives_sf, "LCOE_USDperMWh", "LCOE ($/MWh)")

# Save as landscape PDF (11 x 8.5 inches)
ggsave(
  filename = file.path(saveToFolder, "LCOE_map_variants2to7.pdf"),
  plot     = LCOE_map,
  device   = cairo_pdf,   # ensures embedded fonts
  width    = 8.5,          # landscape width
  height   = 5,         # landscape height
  units    = "in"
)


cat("\n=== Script completed successfully! ===\n")
cat("Results saved to:", saveToFolder, "\n")