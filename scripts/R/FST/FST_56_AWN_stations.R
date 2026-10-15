

# ==== PORTABLE PATH CONFIGURATION =======================================
# Set APPLE_APV_DATA_ROOT to the supplementary-data directory when data are
# stored outside this repository. Otherwise, use the current working folder.
data_root <- Sys.getenv("APPLE_APV_DATA_ROOT", unset = getwd())
data_root <- normalizePath(data_root, mustWork = FALSE)

awn_clean_root <- file.path(data_root, "awn_qaqc", "Combined_data", "Complete_data")
pvsyst_awn_root <- file.path(data_root, "pvsyst_results_awn")
fst_awn_root <- file.path(data_root, "fst_results", "AWN")
dir.create(fst_awn_root, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(awn_clean_root)) {
  stop("AWN cleaned-data directory does not exist: ", awn_clean_root)
}
if (!dir.exists(pvsyst_awn_root)) {
  stop("AWN PVsyst-results directory does not exist: ", pvsyst_awn_root)
}

# ========================================================================
# FST SIMULATIONS: AWN WEATHER + PVSYST SHADING
# Purpose: merge hourly AWN weather with PVsyst irradiance outputs,
# simulate fruit surface temperature (FST) under shading/wind scenarios,
# summarize FST >= 43 deg C events, and compare radiation inputs.
# Inputs: QA/QC AWN station CSV files and PVsyst output CSV files.
# Dependencies: agclimtools::fruit_surface_temp() plus tidyverse packages.
# Execution environment: archived WSU Kamiak HPC paths; update for reuse.
# ========================================================================

## Load all packages
## This script is run in hpc kamiak cluster, to run this it requires both AWN weather and PVSyst shades modeling data
library(agclimtools,warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(viridisLite, warn.conflicts = FALSE)
library(viridis, warn.conflicts = FALSE) 
library(reshape2, warn.conflicts = FALSE)


# -- Helper reads one CSV and adds its filename to preserve provenance. --
## function to read the data
read_plus <- function(flnm) {
  read_csv(flnm) %>%
    mutate(filename = flnm)
}


# -- Import cleaned AWN weather files, retaining filename for station-ID --
## read weather data
tbl_with_sources <-
  list.files(path = awn_clean_root,
             pattern = "\\.csv$",
             full.names = T, recursive = T) %>%
  map_df(~read_plus(.))


# -- Retain timestamp keys and physical drivers of FST: air temperature --
# (deg C), dew point (deg C), wind (m/s), incoming solar flux (W/m2).
### modify for required variables
tbl_with_sources<- tbl_with_sources %>% 
  dplyr::select(c("Year","Month","Day","Hour","STATION_NAME","LATITUDE","LONGITUDE","Air_temp_C","Dew_point_c","Windspeed_ms",
                  "SOLAR_RAD_WM2", "RELATIVE_HUMIDITY_.","filename")) %>% 
  mutate(station_id = str_split_fixed(filename,"//",2)[,1],
         station_id = str_split_fixed(station_id,"/",9)[,9],
         station_id = str_split_fixed(station_id,"_",3)[,3]) %>% 
  dplyr::select(-filename)



# -- Import PVsyst shading runs and attach the source filename. --
# The directory path is used to derive the PVsyst design variant and
# station identifier; this parsing is sensitive to path depth.
pvsyst_AWN <-
  list.files(path = pvsyst_awn_root,
             pattern = "\\.csv$",
             full.names = T, recursive = T) %>%
  map_df(~read_plus(.))


# -- Split PVsyst date/time strings; extract year and design/station from --
# filename. Preserve Control and Variant 1 irradiance (W/m2).
pvsyst_AWN<- pvsyst_AWN %>% 
  separate(Date, into = c("date","time"), sep = " ") %>% 
  separate(date, into = c("Day","Month", "Year1"), sep = "/") %>% 
  separate(time, into = c("Hour", "Minutes")) %>% 
  mutate(filename_v1 = str_split_fixed(filename,"//",2)[,1],
         variants = str_split_fixed(filename_v1,"//",10)[,9],
         station_id = str_split_fixed(filename_v1,"/",10)[,10],
         Year = str_split_fixed(filename,"//",2)[,2],
         Year = str_split_fixed(Year,"_",2)[,1]) %>% 
  dplyr::select(c("Year","Month","Day","Hour","Control Flux (W/m^2)","Variant 1 Flux (W/m^2)","variants","station_id")) %>% 
  rename("Control_flux_Wm2"="Control Flux (W/m^2)","Variant_1_flux_Wm2"="Variant 1 Flux (W/m^2)")

# -- Convert date/time fields to numeric. Shift PVsyst hour back by one --
# na.omit() removes any rows with missing values.
## change the column type
pvsyst_AWN <- pvsyst_AWN %>% 
  mutate(Year = as.numeric(Year),
         Month = as.numeric(Month),
         Day = as.numeric(Day),
         Hour = as.numeric(Hour),
         Hour = Hour-1) %>% na.omit()
## read the databack
write.csv(pvsyst_AWN, file.path(fst_awn_root, "pvsyst_AWN.csv"), row.names = F)




# -- Inner-join AWN meteorology and PVsyst irradiance using all shared --
# field names. Compare input/output row counts and join uniqueness.
### Combine both into one
AWN_PVsyT <- pvsyst_AWN %>% inner_join(tbl_with_sources)
write.csv(AWN_PVsyT, file.path(fst_awn_root, "AWN_PVsyT.csv"), row.names = F)



# -- Apply the energy-balance FST routine to observed AWN weather and --
# PVsyst irradiance under the same scenario definitions as NSRDB:
# Baseline = original AWN radiation, unchanged wind;
# Control = PVsyst Control flux, unchanged wind;
# C_WS10..35 = Control flux + wind reduced 10..35%;
# V1_WS10..35 = Variant 1 flux + wind reduced 10..35%.
# Other meteorological forcing remains identical among scenarios.
## AWN fruit surface temperature
AWN_FST <- AWN_PVsyT %>% 
  mutate(wind_10 = Windspeed_ms*0.9,
         wind_15 = Windspeed_ms*0.85,
         wind_20 = Windspeed_ms*0.8, #20% reduction in windspeed is the baseline
         wind_25 = Windspeed_ms*0.75,
         wind_30 = Windspeed_ms*0.7,
         wind_35 = Windspeed_ms*0.65,
         Baseline = fruit_surface_temp(Air_temp_C,Windspeed_ms,SOLAR_RAD_WM2,Dew_point_c),
         Control = fruit_surface_temp(Air_temp_C,Windspeed_ms,Control_flux_Wm2,Dew_point_c),
         C_WS10 = fruit_surface_temp(Air_temp_C,wind_10,Control_flux_Wm2,Dew_point_c),
         C_WS15 = fruit_surface_temp(Air_temp_C,wind_15,Control_flux_Wm2,Dew_point_c),
         C_WS20 = fruit_surface_temp(Air_temp_C,wind_20,Control_flux_Wm2,Dew_point_c),
         C_WS25 = fruit_surface_temp(Air_temp_C,wind_25,Control_flux_Wm2,Dew_point_c),
         C_WS30 = fruit_surface_temp(Air_temp_C,wind_30,Control_flux_Wm2,Dew_point_c),
         C_WS35 = fruit_surface_temp(Air_temp_C,wind_35,Control_flux_Wm2,Dew_point_c),
         V1_WS10 = fruit_surface_temp(Air_temp_C,wind_10,Variant_1_flux_Wm2,Dew_point_c),
         V1_WS15 = fruit_surface_temp(Air_temp_C,wind_15,Variant_1_flux_Wm2,Dew_point_c),
         V1_WS20 = fruit_surface_temp(Air_temp_C,wind_20,Variant_1_flux_Wm2,Dew_point_c),
         V1_WS25 = fruit_surface_temp(Air_temp_C,wind_25,Variant_1_flux_Wm2,Dew_point_c),
         V1_WS30 = fruit_surface_temp(Air_temp_C,wind_30,Variant_1_flux_Wm2,Dew_point_c),
         V1_WS35 = fruit_surface_temp(Air_temp_C,wind_35,Variant_1_flux_Wm2,Dew_point_c)) %>% 
  dplyr::select(-c(wind_10,wind_15,wind_20,wind_25,wind_30,wind_35))
## write the dataset back
write.csv(AWN_FST, file.path(fst_awn_root, "AWN_FST.csv"), row.names = F)
rm(AWN_PVsyT)



# -- For each scenario, keep only hours with FST >= 43 deg C. --
# Aggregate to hours per month and number of distinct exceedance days.
# No rows are generated for months with zero exceedance hours.
############ Filter FST higher than 43 using the loops
## simulation category
Category <- c("Baseline","Control", "C_WS20","C_WS10", "C_WS15","C_WS25", "C_WS30","C_WS35", 
              "V1_WS20","V1_WS10","V1_WS15","V1_WS25", "V1_WS30","V1_WS35")

## define null dataframe
sensitivity_cat_AWN <- NULL
## loop to find out FST higher than 43 and do required summary
for (i in seq_along(Category)) {
  FST <- AWN_FST %>%
    filter(.data[[Category[i]]] >= 43) %>%
    group_by(variants, station_id, Year, Month, Day) %>%
    summarise(number_hours = n(), .groups = "drop") %>%
    group_by(variants, station_id,Year, Month) %>%
    summarise(
      number_hours = sum(number_hours, na.rm = TRUE),
      number_days = n(),
      .groups = "drop"
    ) %>%
    mutate(sensitivity_category = Category[i])
  
  sensitivity_cat_AWN <- bind_rows(sensitivity_cat_AWN, FST)
}

## read the dataset back
write.csv(sensitivity_cat_AWN, file.path(fst_awn_root, "sensitivity_cat_AWN.csv"), row.names = F)


# -- Inspect shading flux differences for growing-season hours. --
# Only April-October and 06:00-20:00 are included in this plot.
### Check solar comparison
####
Mn <- c(4:10)
hr <- c(6:20)
Check_solar <- AWN_FST %>% 
  dplyr::select(c( "variants","station_id","Year","Month","Day","Hour","SOLAR_RAD_WM2","Control_flux_Wm2",
                   "Variant_1_flux_Wm2")) %>% 
  filter(Month %in% Mn, Hour %in% hr) 

Check_solar1<- reshape2::melt(Check_solar, id =c("variants","station_id","Year","Month","Day","Hour")) %>% 
  mutate(Hour = factor(as.character(Hour), levels = as.character(6:20), ordered = TRUE))




# -- Plot monthly pooled hourly radiation distributions by variant; --
# suppressed outlier symbols do not imply filtered records.
###### Prepare figure for that
## Create plots to check the range
P1<- ggplot(Check_solar1, aes(x=Hour,y= value, fill = variable))+ 
  geom_boxplot(outlier.shape = NA)+ facet_wrap(~variants)+ #outlier.shape = NA
  labs(x= " ", y= "Comparison of GHI, COntrol, Variant 1 in W/m2")+ theme_bw()+
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
        legend.text = element_text(size=18), 
        strip.text = element_text(size=20), legend.title = element_blank(),
        axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=0.5, vjust = 0.5,face="bold"),
        axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),
        legend.position = "bottom")

ggsave(P1, file=file.path(fst_awn_root, "SR_comp_AWN.png"), width = 40, height = 30,dpi = 300, units = "cm")
