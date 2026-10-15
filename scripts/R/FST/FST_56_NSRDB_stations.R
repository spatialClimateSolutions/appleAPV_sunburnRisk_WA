

# ==== PORTABLE PATH CONFIGURATION =======================================
# Set APPLE_APV_DATA_ROOT to the supplementary-data directory when data are
# stored outside this repository. Otherwise, use the current working folder.
data_root <- Sys.getenv("APPLE_APV_DATA_ROOT", unset = getwd())
data_root <- normalizePath(data_root, mustWork = FALSE)

nsrdb_weather_root <- file.path(data_root, "weather", "NSRDB_Cleaned_Data_56_Sites")
pvsyst_nsrdb_root <- file.path(data_root, "pvsyst_results")
fst_nsrdb_root <- file.path(data_root, "fst_results", "NSRDB")
dir.create(fst_nsrdb_root, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(nsrdb_weather_root)) {
  stop("NSRDB weather directory does not exist: ", nsrdb_weather_root)
}
if (!dir.exists(pvsyst_nsrdb_root)) {
  stop("NSRDB PVsyst-results directory does not exist: ", pvsyst_nsrdb_root)
}

# ========================================================================
# FST SIMULATIONS: NSRDB WEATHER + PVSYST SHADING
# Purpose: process weather from the National Solar Radiation Database
# (NSRDB), join hourly PVsyst shading radiation, simulate FST for scenario
# comparisons, summarize threshold exceedances, and plot radiation inputs.
# Inputs: NSRDB station files and PVsyst simulation outputs (56-site study).
# Outputs: combined meteorology/radiation CSVs, hourly modeled FST,
# monthly threshold exceedances, and a diagnostic solar-radiation plot.
# Documentation-only edition: original executable statements are unchanged.
# ========================================================================

## Load all packages
## Before Running this script, please download NSRDB weather and PVsyst output as described in README
library(agclimtools,warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(viridisLite, warn.conflicts = FALSE)
library(viridis, warn.conflicts = FALSE) 
library(reshape2, warn.conflicts = FALSE)



# -- Read the NSRDB weather --
# The first two *rows after read.csv()* are treated as metadata/header;
#### Write loop to read all NSRDB 100 stations data together
# NSRDB weather files are read from nsrdb_weather_root above.
weather_path <- nsrdb_weather_root
stations<- list.files(weather_path)
combine_data_subset <- NULL
for ( i in seq_along(stations)){
  files <- list.files(paste0(weather_path,stations[i]))
  if (!length(files)) next
  for ( j in seq_along(files)){
    ## read the data
    data<- read.csv(paste0(weather_path,stations[i],"/",files[j]))
    if (is.null(data) || nrow(data) < 3) {  # need ≥2 header/meta rows + data
      message("Too few rows: ", files[j]); next
    }
    
    ## rename column number
    names(data) <- data[2,]
    lat <- data[1,6]
    long<- data[1,7]
    ## required modification and keep required value
    data<- data[-c(1:2),c(1:18)] %>% 
      dplyr::select(c("Year","Month","Day","Hour","GHI","Wind Speed","Temperature","Relative Humidity","Dew Point" )) %>% 
      rename("Wind_speed" = "Wind Speed", "RH"="Relative Humidity", "Dew_point" = "Dew Point") %>% 
      mutate(across(everything(),~as.numeric(.))) %>% 
      mutate(station = stations[i]) %>% 
      dplyr::select(c("station","Year","Month","Day","Hour","GHI","Wind_speed","Temperature","RH","Dew_point")) %>% 
      mutate(lat = lat,
             long = long)
    
    ## append dataset
    combine_data_subset <- bind_rows(combine_data_subset,data)
    
  }
}



# -- Traverse PVsyst design/variant folders and station subfolders. --
# The two PVsyst fluxes represent Control and Variant 1 radiation;
# the exact geometry/design associated with each variant are described in manuscript
#### Write loop to read all reduced solar radiation data from Pvsyst simulation
## Some of these stations doesn't have any consistent pattern for this data
Pvsyst_Path <- pvsyst_nsrdb_root
variants <- list.files(Pvsyst_Path)
combine_data_solar <- NULL
for ( i in seq_along(variants)){
  stations<- list.files(paste0(Pvsyst_Path,variants[i],"/"))
  for ( j in seq_along(stations)){
    files <- list.files(paste0(Pvsyst_Path,variants[i],"/",stations[j],"/"))
    if (!length(files)) next
    for ( k in seq_along(files)){
      ## read the data
      radiation <- read.csv(paste0(Pvsyst_Path,variants[i],"/",stations[j],"/",files[k])) 
      if (is.null(radiation) || nrow(radiation) < 2) {  # need ≥2 header/meta rows + data
        message("Too few rows: ", files[j]); next
      }

      # -- Parse PVsyst date/time and reshape flux columns for joining. --
      # Date format is day/month/two-digit-year in this NSRDB branch.
      radiation <- radiation%>% 
        rename("Control_flux_Wm2" = "Control.Flux..W.m.2.", "Variant_1_flux_Wm2"="Variant.1.Flux..W.m.2.") %>% 
        separate(Date, into = c("date","time"),sep = " ") %>% 
        mutate(date = as.Date(date, format = "%d/%m/%y")) %>% 
        separate(date, into = c("Year","Month","Day"),sep = "-") %>% 
        separate(time, into = c("Hour","Minute"),sep = ":") %>% 
        mutate(across(everything(),~as.numeric(.)),
               station = stations[j],
               variants = variants[i]) %>% 
        dplyr::select(c("station","Year","Month","Day","Hour","Control_flux_Wm2","Variant_1_flux_Wm2","variants"))
      
      ## append dataset
      combine_data_solar <- bind_rows(combine_data_solar,radiation)
      
    }
  }
}



# -- TIME-ALIGNMENT ASSUMPTION: shift PVsyst hour labels back by one. --
# Original interpretation: PVsyst half-hour-centered labels (e.g. 08:30)
# were rounded to 09:00, then assigned to the 08:00 model interval.
## Adjust the hourly shift, PVsyst returns reduced radiation with a 30 minute shift, but here I have accounted hourly as the simulation is done here at hourly timescale
## 8:30 becomes 9, sop if I do an hourly backward shift like 9 to 8 its same
combine_data_solar1<- combine_data_solar %>% 
  mutate(Hour = Hour-1)




# -- Join PVsyst radiation and NSRDB weather on their shared field names. --
# column names; review join keys and row counts before interpreting results.
# inner_join discards timestamps absent from either input.
## Combine both weather and reduced SR data
combine_together_11 <- combine_data_solar1 %>% inner_join(combine_data_subset) %>% 
  dplyr::select(-c(lat,long))
write.csv(combine_together_11, file.path(fst_nsrdb_root, "combine_together_11_NSRDB_v1.csv"), row.names = F)



###

# -- Run fruit_surface_temp(air temperature, wind speed, radiation, dew point). --
# Baseline uses unshaded NSRDB GHI and measured/source wind.
# Control uses PVsyst Control flux with unchanged wind.
# C_WSxx uses Control flux and an xx% reduction in wind speed.
# V1_WSxx uses Variant 1 flux and an xx% reduction in wind speed.
# A 20% reduction is tagged as reference in original comments; all six
# wind reductions (10,15,20,25,30,35%) are simulated.
# Temperature and dew point are held constant across shading scenarios.
# Run the fruit surface temperature model for baseline and reduced SR for sensitivity test
FST_Calc <- combine_together_11 %>% 
  mutate(wind_10 = Wind_speed*0.9,
         wind_15 = Wind_speed*0.85,
         wind_20 = Wind_speed*0.8, #20% reduction in windspeed is the baseline
         wind_25 = Wind_speed*0.75,
         wind_30 = Wind_speed*0.7,
         wind_35 = Wind_speed*0.65,
         Baseline = fruit_surface_temp(Temperature,Wind_speed,GHI,Dew_point),
         Control = fruit_surface_temp(Temperature,Wind_speed,Control_flux_Wm2,Dew_point),
         C_WS20 = fruit_surface_temp(Temperature,wind_20,Control_flux_Wm2,Dew_point),
         C_WS10 = fruit_surface_temp(Temperature,wind_10,Control_flux_Wm2,Dew_point),
         C_WS15 = fruit_surface_temp(Temperature,wind_15,Control_flux_Wm2,Dew_point),
         C_WS25 = fruit_surface_temp(Temperature,wind_25,Control_flux_Wm2,Dew_point),
         C_WS30 = fruit_surface_temp(Temperature,wind_30,Control_flux_Wm2,Dew_point),
         C_WS35 = fruit_surface_temp(Temperature,wind_35,Control_flux_Wm2,Dew_point),
         V1_WS20 = fruit_surface_temp(Temperature,wind_20,Variant_1_flux_Wm2,Dew_point),
         V1_WS10 = fruit_surface_temp(Temperature,wind_10,Variant_1_flux_Wm2,Dew_point),
         V1_WS15 = fruit_surface_temp(Temperature,wind_15,Variant_1_flux_Wm2,Dew_point),
         V1_WS25 = fruit_surface_temp(Temperature,wind_25,Variant_1_flux_Wm2,Dew_point),
         V1_WS30 = fruit_surface_temp(Temperature,wind_30,Variant_1_flux_Wm2,Dew_point),
         V1_WS35 = fruit_surface_temp(Temperature,wind_35,Variant_1_flux_Wm2,Dew_point)) %>% 
  dplyr::select(-c(wind_10,wind_15,wind_20,wind_25,wind_30,wind_35))
write.csv(FST_Calc, file.path(fst_nsrdb_root, "FST_Calc_NSRDB_56_v1.csv"), row.names = F)
rm(combine_together_11)





# -- Count hourly modeled FST values at or above the 43 deg C threshold. --
### Check the sensitivity
############ Filter FST higher than 43 using the loops
## simulation category
Category <- c("Baseline","Control", "C_WS20","C_WS10", "C_WS15","C_WS25", "C_WS30","C_WS35", 
              "V1_WS20","V1_WS10","V1_WS15","V1_WS25", "V1_WS30","V1_WS35")

## define null dataframe
sensitivity_cat_10_NSRDB <- NULL
## loop to find out FST higher than 43 and do required summary
for (i in seq_along(Category)) {
  FST <- FST_Calc %>%
    filter(.data[[Category[i]]] >= 43) %>%
    group_by(variants, station, Year, Month, Day) %>%
    summarise(number_hours = n(), .groups = "drop") %>%
    group_by(variants, station,Year, Month) %>%
    summarise(
      number_hours = sum(number_hours, na.rm = TRUE),
      number_days = n(),
      .groups = "drop"
    ) %>%
    mutate(sensitivity_category = Category[i])
  
  sensitivity_cat_10_NSRDB <- bind_rows(sensitivity_cat_10_NSRDB, FST)
}

write.csv(sensitivity_cat_10_NSRDB, file.path(fst_nsrdb_root, "sensitivity_cat_56_NSRDB.csv"), row.names = F)





# -- Compare incoming NSRDB GHI and the two PVsyst flux series for --
# April-October, hours 06-20, as a diagnostic of shading effects.
### Check solar comparison
####
Mn <- c(4:10)
hr <- c(6:20)
Check_solar <- FST_Calc %>% 
  dplyr::select(c( "variants","station","Year","Month","Day","Hour","GHI","Control_flux_Wm2",
                   "Variant_1_flux_Wm2")) %>% 
  filter(Month %in% Mn, Hour %in% hr) 

Check_solar1<- reshape2::melt(Check_solar, id =c("variants","station","Year","Month","Day","Hour")) %>% 
  mutate(Hour = factor(as.character(Hour), levels = as.character(6:20), ordered = TRUE))




# -- Draw a per-variant hourly distribution of radiation via boxplots. --
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

ggsave(P1, file=file.path(fst_nsrdb_root, "SR_comp_NSRDB56.png"), width = 40, height = 30,dpi = 300, units = "cm")
