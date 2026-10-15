

# ==== PORTABLE PATH CONFIGURATION =======================================
# Set APPLE_APV_DATA_ROOT to the supplementary-data directory when data are
# stored outside this repository. Otherwise, use the current working folder.
data_root <- Sys.getenv("APPLE_APV_DATA_ROOT", unset = getwd())
data_root <- normalizePath(data_root, mustWork = FALSE)
awn_raw_root <- file.path(data_root, "awn_raw")
awn_qaqc_root <- file.path(data_root, "awn_qaqc")
awn_complete_root <- file.path(awn_qaqc_root, "Combined_data", "Complete_data")
awn_cleaned_root <- file.path(awn_qaqc_root, "Combined_data", "Cleaned_data")
dir.create(awn_complete_root, recursive = TRUE, showWarnings = FALSE)
dir.create(awn_cleaned_root, recursive = TRUE, showWarnings = FALSE)

# ========================================================================
# AWN WEATHER DATA QUALITY ASSURANCE / QUALITY CONTROL
# Purpose: inspect raw AgWeatherNet (AWN) station observations, convert
# measurement units, identify missing/implausible records, select station-years,
# and produce diagnostic figures and cleaned weather data for FST and PVSyst shading simulations.
# Input: AWN station CSV files obtained for the underlying study.
# Outputs: station-level cleaned CSVs, completeness summaries (optional),
# and diagnostic weather plots in the original directory structure.
# IMPORTANT: This is an archival research script; some sections are exploratory
# and contain unresolved references. 
# ========================================================================

## Load all packages
## This QA/QC script requires AWN dataset used in Savalkar et al. (2024), it requires obtaining dataset from teh correspondence
library(agclimtools,warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(viridisLite, warn.conflicts = FALSE)
library(viridis, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)


# Single-station exploratory check: parse hourly AWN timestamps,
# remove selected years and convert F, mph to deg C, m/s.
# This preliminary section does not feed the later combined-data loop.
## Load  a single station dataset for exploratory analysis
data<- read.csv(file.path(awn_raw_root, "AWN_60_100014.csv")) %>% 
  separate(TSTAMP_HOUR, into = c("date","Time"), sep = " ") %>% 
  mutate(date = as.POSIXct(date),
         Year = format(date, "%Y"),
         Doy = format(date, "%j")) %>% filter(!Year %in% c("1994","2021") ) %>% 
  dplyr::select(c("date","Time","UNIT_ID","STATION_NAME","LATITUDE","LONGITUDE", "ELEVATION_FEET","AIR_TEMP_F","RELATIVE_HUMIDITY_.",
                  "DEWPOINT_F","WIND_SPEED_MPH","SOLAR_RAD_WM2","Year","Doy")) %>% 
  mutate(date_1 = date,
         Air_temp_C = (AIR_TEMP_F - 32)*(5/9),
         Dew_point_c = (DEWPOINT_F - 32) * (5/9),
         Windspeed_ms = (WIND_SPEED_MPH*1.609*1000)/3600) %>% 
  separate(date_1, into = c("Year", "Month","Day"), sep = "-") %>% 
  separate(Time, into = c("Hour","Minute"), sep = ":")


# Detect any row with one or more NA values; subsequent filtering 
# drops an ENTIRE station-year if any row was incomplete.
## Check NA values
na_check <- data[!complete.cases(data),]
## remove years which has NA
data1<- data %>% filter(! Year %in% na_check$Year)
## Find years 
data2 <- data1 %>% 
  group_by(STATION_NAME) %>% 
  summarise(n_years = n_distinct(Year),
            Years = unique(Year))




# Batch-import all AWN CSV files and preserve source filenames. 
# Only the variables needed for QA/QC and FST forcing are retained.
### Load all datasets and clean them steps by steps
Path <- awn_raw_root
AWN <- list.files(Path, pattern = "\\.csv$", full.names = FALSE)
Combined_data_1 <- NULL
for ( i in 1: length(AWN)){
  ## Read every datafile
  data<- read.csv(paste0(Path,AWN[i])) %>% 
    mutate(file_name = AWN[i]) %>% 
    dplyr::select(c("TSTAMP_HOUR","STATION_NAME","LATITUDE","LONGITUDE", "ELEVATION_FEET","AIR_TEMP_F","RELATIVE_HUMIDITY_.",
                    "PRECIP_INCHES","DEWPOINT_F","WIND_SPEED_MPH","SOLAR_RAD_WM2","file_name"))
  
  ##  Combined dataset
  Combined_data_1<- bind_rows(Combined_data_1,data)
}



# Harmonize two assumed timestamp layouts (ymd HM, mdy HM).
# All parsed timestamps are marked UTC in this code. 
# Convert temperature F->deg C, wind mph->m/s, precipitation in->mm.
### Fix the date time format
Combined_data_1<- Combined_data_1 %>% 
  ## make the Tiemstamp into a consistent pattern
  mutate(t_stamp = parse_date_time(TSTAMP_HOUR, orders = c("ymd HM", "mdy HM"),
                                   tz = "UTC"),
         TSTAMP_HOUR = t_stamp) %>% 
  separate(t_stamp, into = c("date","Time"), sep = " ") %>% 
  mutate(date1 = date) %>% 
  separate(date1, into = c("Year","Month","Day"), sep = "-") %>% 
  separate(Time, into = c("Hour","Minute","Seconds"), sep = ":") %>% 
  dplyr::select(c("TSTAMP_HOUR","Year","Month","Day","Hour","STATION_NAME","LATITUDE","LONGITUDE", "ELEVATION_FEET","AIR_TEMP_F","RELATIVE_HUMIDITY_.",
                  "PRECIP_INCHES","DEWPOINT_F","WIND_SPEED_MPH","SOLAR_RAD_WM2","file_name")) %>% 
  mutate(Air_temp_C = (AIR_TEMP_F - 32)*(5/9),
         Dew_point_c = (DEWPOINT_F - 32) * (5/9),
         Windspeed_ms = (WIND_SPEED_MPH*1.609*1000)/3600,
         Precip_mm = PRECIP_INCHES*25.4)

Combined_data_1<- Combined_data_1 %>% 
  mutate(Hour = ifelse(is.na(Hour),"00",Hour))
#write.csv(Combined_data_1, file.path(awn_qaqc_root, "Combined_data", "Combined_complete_data_raw.csv"))


# -- Generate exploratory distributions for each station after exclusion --
# of station-years containing explicit NA values.
# For April-September plot by month and hour: RH, air temperature,
# solar radiation, wind speed, and precipitation.
# The original plotting code is intentionally preserved verbatim.
### Loops to create required figures for analysis
Years_data_new_2 <- NULL
stations <- unique(Combined_data_1$STATION_NAME)
for ( i in 1: length(stations)){
  
  # Filter each station
  data1<- Combined_data_1 %>% filter( STATION_NAME == stations[i]) %>% 
    mutate(Hour = ifelse(is.na(Hour),"00",Hour),
           Hour = factor(Hour, labels = c("00","01","02" ,"03","04","05","06","07","08","09","10","11","12",
                                          "13","14","15","16","17","18","19","20","21","22","23")))
  
  ## Check NA values
  na_check <- data1[!complete.cases(data1),]
  ## remove years which has NA
  data2<- data1 %>% filter(! Year %in% na_check$Year) 
  #Write back the complete dataset
  awn_file <- unique(data2$file_name)
  write.csv(data2, file.path(awn_complete_root, awn_file), row.names = FALSE)
  ## Find years 
  data3 <- data2 %>% 
    group_by(STATION_NAME,file_name) %>% 
    summarise(n_full_years = n_distinct(Year),
              Years_has_complete_data = unique(Year))
  
  ## Store into in a single dataframe 
  Years_data_new_2 <- bind_rows(Years_data_new_2,data3)
  
  ## remove years which has NA
  months<- c("04","05","06","07","08","09")
  data4<- data2 %>% filter(! Year %in% na_check$Year, Month %in% months)

  
 
  
  ## Prepare plots
  ## Relative humidity
  P1<- ggplot(data4, aes(x=Month,y=`RELATIVE_HUMIDITY_.`,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    labs(x= "", y= "Relative humidity in %", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  
  P2<- ggplot(data4, aes(x=Hour,y=`RELATIVE_HUMIDITY_.` ,fill=Hour))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    labs(x= "", y= "Relative humidity in %", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P3 <- P1+P2
  
  # Save image
  Pr <- file.path(awn_qaqc_root, "diagnostics", "Rh", "Month")
  dir.create(Pr, recursive = TRUE, showWarnings = FALSE)
  ggsave(P3, file=file.path(Pr, paste0(stations[i], "_Rh.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  ## Air temperature
  P1<- ggplot(data4, aes(x=Month,y= Air_temp_C,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    labs(x= "", y= "Air temperature in °C", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P2<- ggplot(data4, aes(x=Hour,y= Air_temp_C,fill=Hour))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Air temperature in °C", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P3 <- P1+P2
  ## Save plot
  Pt <- file.path(awn_qaqc_root, "diagnostics", "temp", "Month")
  dir.create(Pt, recursive = TRUE, showWarnings = FALSE)
  ggsave(P3, file=file.path(Pt, paste0(stations[i], "temp.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  ## Solar radiation
  P1<- ggplot(data4, aes(x=Month,y= SOLAR_RAD_WM2,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Solar radiatio in W/m2", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 90, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P2<- ggplot(data4, aes(x=Hour,y= SOLAR_RAD_WM2,fill=Hour))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Solar radiatio in W/m2", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 90, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P3 <- P1+P2
  # Save the plot
  Ps <- file.path(awn_qaqc_root, "diagnostics", "Srad", "Month")
  dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
  ggsave(P3, file=file.path(Ps, paste0(stations[i], "Srad.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  
  ## Windspeed
  P1<- ggplot(data4, aes(x=Month,y= Windspeed_ms,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Windspeed in m/s", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P2<- ggplot(data4, aes(x=Hour,y= Windspeed_ms,fill=Hour))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Windspeed in m/s", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P3 <- P1+P2
  # Save the plot
  Pw <- file.path(awn_qaqc_root, "diagnostics", "windspeed", "Month")
  dir.create(Pw, recursive = TRUE, showWarnings = FALSE)
  ggsave(P3, file=file.path(Pw, paste0(stations[i], "wind.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  
  
  ## Precipitation
  P1<- ggplot(data4, aes(x=Month,y= Precip_mm,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Precipitation in mm", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P2<- ggplot(data4, aes(x=Hour,y= Precip_mm,fill=Hour))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Precipitation in mm", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P3 <- P1+P2
  # Save the plot
  Pp <- file.path(awn_qaqc_root, "diagnostics", "Precip", "Month")
  dir.create(Pp, recursive = TRUE, showWarnings = FALSE)
  ggsave(P3, file=file.path(Pp, paste0(stations[i], "precip.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  
  
  
  

  # Second diagnostics suite: hourly distributions colored by month. --
  # Figures are station-specific and target the April-September season.
  ## Save another set of dataset
  ## Prepare plots
  ## Relative humidity
  P1<- ggplot(data4, aes(x=Hour,y=`RELATIVE_HUMIDITY_.`,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Relative humidity in %", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "bottom",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  # Save image
  Pr <- file.path(awn_qaqc_root, "diagnostics", "Rh", "Hours")
  dir.create(Pr, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Pr, paste0(stations[i], "_Rh.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  ## Air temperature
  P1<- ggplot(data4, aes(x=Hour,y= Air_temp_C,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Air temperature in °C", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "bottom",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  ## Save plot
  Pt <- file.path(awn_qaqc_root, "diagnostics", "temp", "Hours")
  dir.create(Pt, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Pt, paste0(stations[i], "temp.png")), width = 25, height = 15,dpi = 300, units = "cm")
  
  ## Solar radiation
  P1<- ggplot(data4, aes(x=Hour,y= SOLAR_RAD_WM2,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Solar radiatio in W/m2", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "bottom",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  
  # Save the plot
  Ps <- file.path(awn_qaqc_root, "diagnostics", "Srad", "Hours")
  dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Ps, paste0(stations[i], "Srad.png")), width = 25, height = 15,dpi = 300, units = "cm")
  
  
  ## Windspeed
  
  P1<- ggplot(data4, aes(x=Hour,y= Windspeed_ms,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Windspeed in m/s", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "bottom",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  Pw <- file.path(awn_qaqc_root, "diagnostics", "windspeed", "Hours")
  dir.create(Pw, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Pw, paste0(stations[i], "wind.png")), width = 25, height = 15,dpi = 300, units = "cm")
  
  
  ## Precip
  
  P1<- ggplot(data4, aes(x=Hour,y= Precip_mm,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Windspeed in m/s", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "bottom",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  Pw <- file.path(awn_qaqc_root, "diagnostics", "Precip", "Hours")
  dir.create(Pw, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Pw, paste0(stations[i], "Precip.png")), width = 25, height = 15,dpi = 300, units = "cm")
}





# Separate explicit missing values (NA fields) from implicit missing 
# timestamps (hours absent entirely from a station-year).
# Precipitation and elevation are excluded from this NA-based pass.
## Checck implicit and explicit missing data
Years_completeness <- NULL
Combined_data_af_explicit_missing <- NULL
stations <- unique(Combined_data_1$STATION_NAME)
for ( i in 1: length(stations)){
  # Filter each station
  data1<- Combined_data_1 %>% filter( STATION_NAME == stations[i]) %>% 
    mutate(Hour = ifelse(is.na(Hour),"00",Hour),
           Hour = factor(Hour, labels = c("00","01","02" ,"03","04","05","06","07","08","09","10","11","12",
                                          "13","14","15","16","17","18","19","20","21","22","23"))) %>% 
    dplyr::select(-c("PRECIP_INCHES", "Precip_mm", "ELEVATION_FEET"))
  
  ## Check NA values
  na_check <- data1[!complete.cases(data1),]
  ## remove years which has NA
  data2<- data1 %>% filter(! Year %in% na_check$Year) 
  
  #Implicit Check
  data5<- data2 %>% 
    group_by(STATION_NAME,LATITUDE,LONGITUDE,file_name,Year) %>% 
    summarise(Entry = n())
  
  ## bind them together
  Years_completeness<- bind_rows(Years_completeness,data5)
  
  ## Combined all dataset after removing explicit missing data
  Combined_data_af_explicit_missing <- bind_rows(Combined_data_af_explicit_missing,data2)

  
}




#  Flag years with >=8760 observations; partially filled leap years 
# (8761..8783 entries) are identified and removed separately.
# NOTE: >=8760 does not guarantee complete unique hourly timestamps;
# a robust revision should validate timestamps and expected leap-year hours.
## Check if there is implicit missing data, if number of rows less than 8760 has been removed (60 rwos has been removed)
Years_completeness_v1 <- Years_completeness %>% 
  filter(Year != 2021, Entry >= 8760) %>% 
  mutate(Year_Station = paste0(STATION_NAME,"_",Year))

## Checking for leap year
Years_completeness_v2 <- Years_completeness %>% 
  filter(Entry > 8760 & Entry < 8784)

## adding column to the data 
Years_completeness_v2 <- Years_completeness_v2 %>% 
  mutate(Year_Station = paste0(STATION_NAME,"_",Year))

## Removed years and stations which leap years are not complete
Years_completeness_v3 <- Years_completeness_v1 %>% 
  filter(!Year_Station %in% Years_completeness_v2$Year_Station)
#write.csv(Years_completeness_v3, file.path(awn_qaqc_root, "Combined_data", "Complete_years.csv"))


# -- Restrict the completeness summary to 2009-2020 and retain stations --
# with at least 10 qualifying station-years; this summary is not
# automatically propagated to the next step of the original script.
## Complete data years
Yr<- c(2009 : 2020)
Years_completeness_v4<- Years_completeness_v3 %>% 
  filter(Year %in% Yr) %>% 
  group_by(STATION_NAME,file_name) %>% 
  summarise(Year_complete = n()) %>% filter(Year_complete >= 10)
#write.csv(Years_completeness_v4, file.path(awn_qaqc_root, "Combined_data", "Complete_full_years_09_20.csv"))


# -- Reject entire station-years if any radiation observation exceeds --
# 1250 W/m2, a research screening threshold in the original script.
### Remove high solar year during day times 
### Radiation above 1250 W/m2 are removed, assumed erreneous based on radiation values in the dataset
high_soladay_hr <- Combined_data_af_explicit_missing_1 %>% filter(SOLAR_RAD_WM2> 1250) %>% 
  mutate(day_st_yr = paste0(STATION_NAME,"_",Year,"_",Month,"_",Day), 
         Year_Station = paste0(STATION_NAME,"_",Year))

### remove these years and stations which have radiation above 1250 W/m2
Years_completeness_v5 <- Years_completeness_v3 %>% filter(!Year_Station %in% high_soladay_hr$Year_Station)


# -- Additional year-level rejection for unusually high solar radiation --
# (>100 W/m2) during designated overnight/evening clock hours.
# Nighttime window is fixed; season, longitude and timezone matter.
# Remove high solar years during morning/evening hours
## Radiation values above zero in the middle of the night removed assumed erreneous or sensor issues
morning_hours<- c("00","01","02" ,"03","04","05","21","22","23")
high_solar <- Combined_data_af_explicit_missing %>% filter(Hour %in% morning_hours, SOLAR_RAD_WM2> 100) %>% 
  mutate(day_st_yr = paste0(STATION_NAME,"_",Year,"_",Month,"_",Day),
         Year_Station = paste0(STATION_NAME,"_",Year))

### remove these years and stations which have night time radiation values above zero
Years_completeness_v5 <- Years_completeness_v5 %>% filter(!Year_Station %in% high_solar$Year_Station)
## write.csv(Years_completeness_v5, file.path(awn_qaqc_root, "Combined_data", "Complete_years_1.csv"))


# -- Write retained station-year records to station-specific cleaned CSVs. --
# Existing output directory hierarchy is assumed to exist.
## Save clean data years
stations <- unique(Years_completeness_v5$STATION_NAME)
for ( i in 1: length(stations)){
  Years_st <- Years_completeness_v5 %>% filter(STATION_NAME == stations[i])
  file_name <- unique(Years_st$file_name)
  
  ## filter stations and complete years
  station_data <- Combined_data_af_explicit_missing %>% filter(STATION_NAME == stations[i], Year %in% Years_st$Year)
  ## write the databack
  write.csv(station_data, file.path(awn_cleaned_root, file_name), row.names = FALSE)
  
}




# -- Exploratory anomaly plots for unusual radiation, wind, temperature --
# and relative-humidity patterns; not a systematic filtering stage.
## Check QA_QC to answer higher solar, higher temp, and higher Rh question

morning_hours<- c("00","01","02" ,"03","04","05","21","22","23")
day_hours <- c("08","09","10","11","12","13","14","15","16","17","18")

## Add a variable to filter out data
Combined_data_af_explicit_missing_1 <- Combined_data_af_explicit_missing %>% 
  mutate(day_st_yr = paste0(STATION_NAME,"_",Year,"_",Month,"_",Day),
         year_month_day = paste0(Year,"_",Month,"_",Day))


# -- Inspect nighttime radiation anomaly days. An explicit Puyallup --
# example is embedded within a station loop, so filenames repeat.
## High Solar in the evening
high_solar <- Combined_data_af_explicit_missing %>% filter(Hour %in% morning_hours, SOLAR_RAD_WM2> 100) %>% 
  mutate(day_st_yr = paste0(STATION_NAME,"_",Year,"_",Month,"_",Day))

high_solar_1<- Combined_data_af_explicit_missing_1 %>% filter(day_st_yr %in% high_solar$day_st_yr)
stations<- unique(high_solar_1$STATION_NAME)
for ( i in 1: length(stations)){
  
  #data<- high_solar_1 %>% filter(STATION_NAME == stations[i])
  data<- high_solar_1 %>% filter(STATION_NAME == "Puyallup", Year == "2005", Month == "06", Day %in% c("05","06"))
  P1<- ggplot(data)+
    geom_line( aes(x=Hour,y= SOLAR_RAD_WM2, group = 1))+ facet_wrap(~year_month_day)+
    #geom_hline(yintercept = 3, color = "blue")+
    scale_y_continuous("Solar radiation in W/m2",limits=c(0,1100), breaks=seq(0,1100, by=110))+
    #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
    labs(x= "Hour", y= "Solar radiation in W/m2", title = paste0("Puyallup"))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 18,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
          legend.justification = c("right", "bottom"))
  
  Ps <- file.path(awn_qaqc_root, "diagnostics", "Srad", "Anomaly", "Morning_hrs")
  dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Ps, "Puyallup_SR.png"), width = 30, height = 20,dpi = 300, units = "cm")
  
  #`RELATIVE_HUMIDITY_.`
  P1<- ggplot(data)+
    geom_line( aes(x=Hour,y= Windspeed_ms, group = 1))+ facet_wrap(~year_month_day)+ #Air_temp_C
    #geom_hline(yintercept = 3, color = "blue")+
    #scale_y_continuous("Height (m)",limits=c(0,850), breaks=seq(0,850, by=85))+
    #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
    labs(x= "Hour", y= "Windspeed m/s", title = paste0("Puyallup"))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = c(0.95, 0.05),legend.justification = c("right", "bottom"))
  
  Ps <- file.path(awn_qaqc_root, "diagnostics", "Srad", "Anomaly", "Morning_hrs")
  dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Ps, "Puyallup_ws.png"), width = 30, height = 20,dpi = 300, units = "cm") #,stations[i]
}




#### 

# -- Inspect station-days with incoming radiation above 1250 W/m2. --
## High Solar in the day
high_soladay_hr <- Combined_data_af_explicit_missing_1 %>% filter(SOLAR_RAD_WM2> 1250) %>% 
  mutate(day_st_yr = paste0(STATION_NAME,"_",Year,"_",Month,"_",Day))

high_soladay_hr_1<- Combined_data_af_explicit_missing_1 %>% filter(day_st_yr %in% high_soladay_hr$day_st_yr)
stations<- unique(high_soladay_hr_1$STATION_NAME)
for ( i in 1: length(stations)){
  
  data<- high_soladay_hr_1 %>% filter(STATION_NAME == stations[i], Year == "2007",Month == "07", Day %in% c("11","12","13"))
  P1<- ggplot(data)+
    geom_line( aes(x=Hour,y= SOLAR_RAD_WM2, group = 1))+ facet_wrap(~year_month_day)+
    #geom_hline(yintercept = 3, color = "blue")+
    scale_y_continuous("Solar radiation in W/m2",limits=c(0,3000), breaks=seq(0,3000, by=300))+
    #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
    labs(x= "Hour", y= "Solar radiation in W/m2", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
          legend.justification = c("right", "bottom"))
  
  Ps <- file.path(awn_qaqc_root, "diagnostics", "Srad", "Anomaly", "Day_hours")
  dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Ps, paste0(stations[i], "SR.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  
  P1<- ggplot(data)+
    geom_line( aes(x=Hour,y= Air_temp_C, group = 1))+ facet_wrap(~year_month_day)+
    #geom_hline(yintercept = 3, color = "blue")+
    #scale_y_continuous("Height (m)",limits=c(0,850), breaks=seq(0,850, by=85))+
    #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
    labs(x= "Hour", y= "Air temperature °C", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
          legend.justification = c("right", "bottom"))
  
  Ps <- file.path(awn_qaqc_root, "diagnostics", "Srad", "Anomaly", "Day_hours")
  dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Ps, paste0(stations[i], "temp.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  P1<- ggplot(data)+
    geom_line( aes(x=Hour,y= `RELATIVE_HUMIDITY_.`, group = 1))+ facet_wrap(~year_month_day)+
    #geom_hline(yintercept = 3, color = "blue")+
    #scale_y_continuous("Height (m)",limits=c(0,850), breaks=seq(0,850, by=85))+
    #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
    labs(x= "Hour", y= "Relative humidity", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
          legend.justification = c("right", "bottom"))
  
  Ps <- file.path(awn_qaqc_root, "diagnostics", "Srad", "Anomaly", "Day_hours")
  dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Ps, paste0(stations[i], "_Rh.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  P1<- ggplot(data)+
    geom_line( aes(x=Hour,y= Windspeed_ms, group = 1))+ facet_wrap(~year_month_day)+
    #geom_hline(yintercept = 3, color = "blue")+
    #scale_y_continuous("Height (m)",limits=c(0,850), breaks=seq(0,850, by=85))+
    #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
    labs(x= "Hour", y= "Windspeed m/s", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
          legend.justification = c("right", "bottom"))
  
  Ps <- file.path(awn_qaqc_root, "diagnostics", "Srad", "Anomaly", "Day_hours")
  dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Ps, paste0(stations[i], "_WS.png")), width = 40, height = 20,dpi = 300, units = "cm")
}




# -- Despite heading Higher temperature, this filter identifies --
# records with wind speed >= 6 m/s for the selected season/years.
# The following selection references high_wind, not high_temp;
# high_wind is not defined within this script.
### Higher temperature
high_temp <- Combined_data_af_explicit_missing_1 %>% filter(!Hour %in% morning_hours, Month %in% months, Windspeed_ms >= 6, Year %in% c(2009:2020))


high_temp_1<- Combined_data_af_explicit_missing_1 %>% filter(day_st_yr %in% high_wind$day_st_yr)
stations<- unique(high_temp_1$STATION_NAME)
for ( i in 1: length(stations)){
  
  #data<- Combined_data_af_explicit_missing_1 %>% filter(STATION_NAME == "K2H", Year == "2013",Month == "05", Day %in% c("01","02"))
  #data<- Combined_data_af_explicit_missing_1 %>% filter(STATION_NAME == "Mattawa.E", year_month_day %in% c("2012_06_08","2012_06_09"))
  data<- high_temp_1 %>% filter(STATION_NAME == stations[i])
   P1<- ggplot(data)+
     geom_line( aes(x=Hour,y= SOLAR_RAD_WM2, group = 1))+ facet_wrap(~year_month_day)+
     #geom_hline(yintercept = 3, color = "blue")+
     #scale_y_continuous("Height (m)",limits=c(0,850), breaks=seq(0,850, by=85))+
     #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
     labs(x= "Hour", y= "Solar radiation in W/m2", title = paste0(stations[i]))+theme_bw()+ 
     theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
           legend.text = element_text(size=18), 
           strip.text = element_text(size=20), legend.title = element_blank(),
           axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
           axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
           legend.justification = c("right", "bottom"))
   
   Ps <- file.path(awn_qaqc_root, "diagnostics", "windspeed", "Anomaly", "check")
   dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
   ggsave(P1, file=file.path(Ps, paste0(stations[i], "SR.png")), width = 30, height = 20,dpi = 300, units = "cm")
  
  
   P1<- ggplot(data)+
     geom_line( aes(x=Hour,y= Air_temp_C, group = 1))+ facet_wrap(~year_month_day)+
     #geom_hline(yintercept = 3, color = "blue")+
     #scale_y_continuous("Height (m)",limits=c(0,850), breaks=seq(0,850, by=85))+
     #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
     labs(x= "Hour", y= "Air temperature °C", title = paste0(stations[i]))+theme_bw()+ 
     theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
           legend.text = element_text(size=18), 
           strip.text = element_text(size=20), legend.title = element_blank(),
           axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
           axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
           legend.justification = c("right", "bottom"))
   
   Ps <- file.path(awn_qaqc_root, "diagnostics", "windspeed", "Anomaly", "check")
   dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
   ggsave(P1, file=file.path(Ps, paste0(stations[i], "_K2h_temp_v1.png")), width = 30, height = 20,dpi = 300, units = "cm")
  
    P1<- ggplot(data)+
      geom_line( aes(x=Hour,y= `RELATIVE_HUMIDITY_.`, group = 1))+ facet_wrap(~year_month_day)+
      #geom_hline(yintercept = 3, color = "blue")+
      #scale_y_continuous("Height (m)",limits=c(0,850), breaks=seq(0,850, by=85))+
      #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
      labs(x= "Hour", y= "Relative humidity", title = paste0(stations[i]))+theme_bw()+ 
      theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
            legend.text = element_text(size=18), 
            strip.text = element_text(size=20), legend.title = element_blank(),
            axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
            axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
            legend.justification = c("right", "bottom"))
    
    Ps <- file.path(awn_qaqc_root, "diagnostics", "windspeed", "Anomaly", "check")
    dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
    ggsave(P1, file=file.path(Ps, paste0(stations[i], "_sample_Rh.png")), width = 30, height = 20,dpi = 300, units = "cm")
  
    P1<- ggplot(data)+
      geom_line( aes(x=Hour,y= Windspeed_ms, group = 1))+ facet_wrap(~year_month_day)+
      #geom_hline(yintercept = 3, color = "blue")+
      #scale_y_continuous("Height (m)",limits=c(0,850), breaks=seq(0,850, by=85))+
      #scale_x_continuous("Windspeed (m/s)",limits=c(0,5), breaks=seq(0,5, by=1))+
      labs(x= "Hour", y= "Windspeed m/s", title = paste0(stations[i]))+theme_bw()+ 
      theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
            legend.text = element_text(size=18), 
            strip.text = element_text(size=20), legend.title = element_blank(),
            axis.text.x = element_text(size = 20,color = "black",angle = 45, hjust=1,face="bold"),
            axis.text.y = element_text(size = 20,color = "black",angle = 0, hjust=1,face="bold"),legend.position = c(0.95, 0.05),
            legend.justification = c("right", "bottom"))
    
   Ps <- file.path(awn_qaqc_root, "diagnostics", "windspeed", "Anomaly", "check")
   dir.create(Ps, recursive = TRUE, showWarnings = FALSE)
   ggsave(P1, file=file.path(Ps, paste0(stations[i], "_WS.png")), width = 40, height = 20,dpi = 300, units = "cm")
}





# -- Explore daily precipitation totals by month; group by date and --
# sum hourly precipitation, then create station-level boxplots.
## Precip

stations <- unique(Combined_data_1$STATION_NAME)
for ( i in 1: length(stations)){
  
  # Filter each station
  data1<- Combined_data_1 %>% filter( STATION_NAME == stations[i]) %>% 
    mutate(Hour = ifelse(is.na(Hour),"00",Hour),
           Hour = factor(Hour, labels = c("00","01","02" ,"03","04","05","06","07","08","09","10","11","12",
                                          "13","14","15","16","17","18","19","20","21","22","23")))
  
  ## Check NA values
  na_check <- data1[!complete.cases(data1),]
  ## remove years which has NA
  data2<- data1 %>% filter(! Year %in% na_check$Year) 
  
  ## remove years which has NA
  months<- c("04","05","06","07","08","09")
  data4<- data2 %>% filter(! Year %in% na_check$Year, Month %in% months)
  
  data5 <- data4 %>% 
    group_by(STATION_NAME,Year,Month,Day) %>% 
    summarise(Precip_mm  = sum(Precip_mm, na.rm=T))

  
  
  
  ## Precipitation
  P1<- ggplot(data5, aes(x=Month,y= Precip_mm,fill=Month))+
    geom_boxplot()+ #facet_wrap(~site)+ #outlier.shape = NA
    #scale_y_continuous("Wind speed (m/s)",limits=c(0,10), breaks=seq(0,10, by=2))+
    labs(x= "", y= "Precipitation in mm", title = paste0(stations[i]))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), 
          strip.text = element_text(size=20), legend.title = element_blank(),
          axis.text.x = element_text(size = 20,color = "black",angle = 90, hjust=1,face="bold"),
          axis.text.y = element_text(size = 22,color = "black",angle = 0, hjust=1,face="bold"),
          legend.position = "none",legend.spacing = unit(-5, "pt"),
          legend.margin = margin(t = -15))
  
  P3 <- P1+P2
  # Save the plot
  Pp <- file.path(awn_qaqc_root, "diagnostics", "Precip", "Month1")
  dir.create(Pp, recursive = TRUE, showWarnings = FALSE)
  ggsave(P1, file=file.path(Pp, paste0(stations[i], "precip.png")), width = 40, height = 20,dpi = 300, units = "cm")
  
  
}


