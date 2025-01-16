################################################################################
# @title T-Van data pre-processing

# @author
# J. Bryan Curtis \email{John.Curtis@colorado.edu}, Hannah Holland-Moritz \email{Hannah.HollandMoritz@colorado.edu}

# @description 
# Workflow for downloading raw 30-minute Tvan data from ftp, and preprocessing it so it is ready for gap-filling and ReddyProc modifications.

# changelog and author contributions / copyrights
# J. Bryan Curtis (2019-08-07)
#   original creation - Adopted from Matlab code written by Peter Blanken and John Knowles
# Hannah Holland-Moritz (2020-06-09)
#   Modifying to only preprocess tvan data (no gapfilling) and to handle data from both
#   tower sites and all years. Also added automatic download of data from online lter ftp
#   and EDI
# Meghan Hayden (2021-05-11)
#   Modified to include additional cleaning for Ameriflux submissions,
#   as well as creation of abridged file for submission. 
# Sarah Elmendorf 2021-01-18
#   Modified to: adjust timestamps and snow processing for 2021 submission;
#   read in PAR/par_density if found in file and process correctly; set
#   filtering function to read filters by name rather than position to 
#   accomodate files with different column orders and content
#   moved plotting of filtered vs unfiltered code to AFTER the additional
#   cleaning for Ameriflux submission is done; updated max Ta theshold for east
#   from 25C to 30c; added plots for timestamp checks
# Miles Moore 2023
#   Modified to: a) Use a dictionary and reference columns by name rather than position 
#   when reading in data via loadfluxdata(). b) Grab saddle 10min rad data and join to
#   the dataset. c) Fix some misapplied soil heat flux calibration coefficients.

#TODO WARNING: Soil Heat Flux data is currently being "nuked" (all set to NA) at
# The east tower from 2022-07-21 on (when plates were replaced). See ln 850ish...

################################################################################

rm(list = ls())

################################################################################
# Dependencies
################################################################################
# Install dependencies

packReq <- c("RCurl", "magrittr", "EML", "ggplot2", "dplyr", "tidyr", 
             "xts", "lubridate")

# Install and load all required packages
lapply(packReq, function(x) {
  print(x)
  if (require(x, character.only = TRUE) == FALSE) {
    install.packages(x)
    library(x, character.only = TRUE)
  }})

# List of dependencies and their purposes
# library(RCurl) # For downloading Tvan files from ftp
# library(magrittr) # for the functions that download EDI files
# library(EML) # For downloading EDI files
# library(ggplot2) # only required if makeplots = true in user options
# library(dplyr) # for dataframe processing
# library(xts) # Extensible time series. Also loads 'zoo' package - used for
#               interpolating met data which is recorded in hourly rather than
#               half-hourly increments.
# library(tidyr) # for dataframe processing
# library(lubridate) # for easy handling of dates and times


################################################################################
# User options
################################################################################
# Set the following user options that are specific to your system and/or needs.
user = 'milesmoore'

if (user=='wwieder') {
  data_dir <- "~/Desktop/Working_files/Niwot/TVan_in_new/" # The location where data will be downloaded (directory path should include a final "/" character)
  output_dir_base <- "~/Desktop/Working_files/Niwot/Tvan_out_new/" # the location of the directory where output should be placed (directory path should include a final "/" character)
} else if (user == 'hannah') {
  data_dir <- "/home/hannah/Downloads/TVan_in/" # The location where data will be downloaded (directory path should include a final "/" character)
  output_dir_base <- "/home/hannah/Downloads/Tvan_out/" # the location of the directory where output should be placed (directory path should include a final "/" character)
} else if (user == 'scelmendorf'){
  data_dir <- 'C:/Users/Sarah/Documents/TVan_in/'
  output_dir_base <- 'C:/Users/Sarah/Documents/TVan_out/'
} else if (user == 'meghanhay') {
  data_dir <- "C:/Users/meha3816/Desktop/CLM_WD/data/" 
  output_dir_base <- "C:/Users/meha3816/Desktop/CLM_WD/data/cleaned/"
}else if (user == 'ruderalis') {
  data_dir <- '/data2/sarahdata/TVan_in/'
  output_dir_base- "/data2/sarahdata/TVan_out/"
} else if (user == 'milesmoore'){
  data_dir <- '../tvan_in/'
  output_dir_base <- "../tvan_out/"
}

# Which Tvan tower do you want to process?
tower <- "West" # "East" or "West"

# What are the start and end dates of the data you want to process?
# dates of the data to process - if you don't want to process all data at once
start_date <- "1999-01-01" # Ex: "2012-01-01" # set to NA to get data from earliest date possible
end_date <- "2023-12-31"   # Ex: "2013-12-31" # set to NA to get data from latest date possible

# Should the script overwite existing Tvan files from the ftp or should it only 
# download files that aren't in the current download directory ("data_dir")?
overwrite <- FALSE

# Prepare an ReddyProc-ready file
# If this is set to TRUE, then an ReddyProc-ready file will also be produced; If this is 
# set to FALSE, no ReddyProc-ready file will be produced; Suggested default is FALSE, 
# this setting is intended only for temporary use until data can be loaded to AmeriFlux
produce_reddyproc_file <- TRUE

# Prepare an file nicely formatted for Ameriflux
# If this is set to TRUE, then a file for Ameriflux submission file will also be
# produced; If this is  set to FALSE, no Ameriflux submission will be produced;
# Suggested default is TRUE. This file is trimmed to the exact dates specified
ameri_submit <-TRUE

# Should the script pull down the latest data from EDI if available? The saddle
# met data comes from EDI and is used to fill in gaps in the data and add radiation
# if the user sets include_saddle_rad = TRUE
getNewData <- TRUE

# Should the script generate plots of the data? (Note: plotting is quite time intensive)
makeplots <- FALSE
plot_filetype <- ".png" # should plots end in ".pdf" or ".png" (png takes longer)

################################################################################
# Parameters derived from user options and parameters that shouldn't be changed
################################################################################
# Create directories for data
ifelse(!dir.exists(file.path(data_dir)), 
       dir.create(file.path(data_dir)), FALSE)

ifelse(!dir.exists(file.path(output_dir_base)), 
       dir.create(file.path(output_dir_base)), FALSE)

ifelse(!dir.exists(file.path(paste0(output_dir_base, "Raw_compiledData"))), 
       dir.create(file.path(paste0(output_dir_base, "Raw_compiledData"))), FALSE)

ifelse(!dir.exists(file.path(paste0(output_dir_base, "AmeriFlux_readyData"))), 
       dir.create(file.path(paste0(output_dir_base, "AmeriFlux_readyData"))), FALSE)

ifelse(!dir.exists(file.path(paste0(output_dir_base, "Reddy_proc_readyData"))), 
       dir.create(file.path(paste0(output_dir_base, "Reddy_proc_readyData"))), FALSE)

# Create plots directories
if (makeplots == TRUE) {
  plots_dir <- paste0(output_dir_base, "plots")
  ifelse(!dir.exists(file.path(plots_dir)), 
         dir.create(file.path(plots_dir)), FALSE)
  # create directory for plots of all-years
  allyrs_dir <- paste0(plots_dir, "/all_years")
  ifelse(!dir.exists(file.path(allyrs_dir)), 
         dir.create(file.path(allyrs_dir)), FALSE)
} 

# create ftp download directory
ftp_download_dir <- paste0(data_dir, "tvan/", tower)

# File name elements for tvan file - User can change if file naming schemes change
# Currently, in the ftp East tower files contain "53" or "_e_", or "east_", while 
# West tower files have "54", or "_w_" or "west_"
# Set the tower id number
# ifelse(tower == "West", twr <- 54, twr <- 53)
ifelse(tower == "West", twr <- "54|_w_|west_", twr <- "53|_e_|east_")

# The ftp location of the Tvan data
# Note, if you already have data and don't want to download it from the ftp,
# you can place the files in the following location <data_dir>/tvan/<tower>
tvan.ftp.url <- "ftp://niwotlter.colorado.edu/Tvan/"

# the EDI id for hourly meterological data from the saddle weather stations
saddle_met_data <- "57" # NWT LTER EDI id
saddle_ten_minute <- "274"

#create a list of column names with syntax "loggerName" = "fluxName"
# columns that are set to NULL are dropped (e.g. 'RECORD' = NULL)

flux_column_dict <- list("TIMESTAMP" = "time", "RECORD" = NULL, "Hs" = NULL, "Fc_wpl" = "Fc",
"LE_wpl" = "LE", "Hc" = "H", "tau" = NULL, "u_star" = "u_star",
"Ts_mean" = "Ts", "stdev_Ts" = NULL, "cov_Ts_Ux" = "u_T", "cov_Ts_Uy" = "v_T",
"cov_Ts_Uz" = "w_T", "co2_mean" = "rho_c", "stdev_co2" = NULL, "cov_co2_Ux" = "u_c",
"cov_co2_Uy" = "v_c", "cov_co2_Uz" = "w_c", "h2o_Avg" = NULL, "stdev_h2o" = NULL,
"cov_h2o_Ux" = "u_q", "cov_h2o_Uy" = "v_q", "cov_h2o_Uz" = "w_q", "Ux_Avg" = "u1",
"stdev_Ux" = NULL,"cov_Ux_Uy" = NULL, "cov_Ux_Uz" = NULL, "Uy_Avg" = "v1", 
"stdev_Uy" = NULL,"cov_Uy_Uz" = NULL, "Uz_Avg" = "w1", "stdev_Uz" = NULL,
"press_mean" = "P", "t_hmp_mean" = "Ta", "h2o_hmp_mean" = "rho_v", "rho_a_mean" = NULL,
"wnd_dir_compass" = "dir", "wnd_dir_csat3" = NULL, "wnd_spd" = NULL, "rslt_wnd_spd" = "U",
"std_wnd_dir" = NULL, "Fc_irga" = NULL, "LE_irga" = NULL, "co2_wpl_LE" = NULL, 
"co2_wpl_H" = NULL, "h2o_wpl_LE" = NULL, "h2o_wpl_H" = NULL, "n_Tot" = NULL, "csat_warnings" = NULL,
"irga_warnings" = NULL, "del_T_f_Tot" = NULL, "sig_lck_f_Tot" = NULL,"amp_h_f_Tot" = NULL, 
"amp_l_f_Tot" = NULL, "chopper_f_Tot" = NULL, "detector_f_Tot" = NULL, "pll_f_Tot" = NULL,
"sync_f_Tot" = NULL, "agc_Avg" = "agc_Avg", "panel_temp_Avg" = NULL, "batt_volt_Avg" = NULL,
"Rn_meas_Avg" = NULL, "Rn_cor_Avg" = "Rn", "par_Avg" = "par","par_density_Avg" = "par_density",
"shf_Avg.1." = "G_1_1_1", "shf_Avg.2."= "G_2_1_1","del_Tsoil" = NULL, "Tsoil_avg"='soil_temp',
# starting at horizontal position 2 because tvan already had one grouping installed
"vwc_1_Avg" = "SWC_2_1_1", "ec_1_Avg" = NULL, "tsoil_1_Avg" = "TS_2_1_1",
"vwc_2_Avg" = "SWC_2_2_1", "ec_2_Avg" = NULL, "tsoil_2_Avg" = "TS_2_2_1",
"vwc_3_Avg" = "SWC_2_3_1", "ec_3_Avg" = NULL, "tsoil_3_Avg" = "TS_2_3_1",
"vwc_4_Avg" = "SWC_3_1_1", "ec_4_Avg" = NULL, "tsoil_4_Avg" = "TS_3_1_1",
"vwc_5_Avg" = "SWC_3_2_1", "ec_5_Avg" = NULL, "tsoil_5_Avg" = "TS_3_2_1",
"vwc_6_Avg" = "SWC_3_3_1", "ec_6_Avg" = NULL, "tsoil_6_Avg" = "TS_3_3_1",
"vwc_7_Avg" = "SWC_4_1_1", "ec_7_Avg" = NULL, "tsoil_7_Avg" = "TS_4_1_1",
"vwc_8_Avg" = "SWC_4_2_1", "ec_8_Avg" = NULL, "tsoil_8_Avg" = "TS_4_2_1",
"vwc_9_Avg" = "SWC_4_3_1", "ec_9_Avg" = NULL, "tsoil_9_Avg" = "TS_4_3_1",
"DT_Avg" = NULL, "DT_Max" = NULL, "DT_TMx"= NULL, "DT_Min"= NULL, "DT_TMn"= NULL, 
"TCDT_Avg"= NULL, "TCDT_Max"= NULL, "TCDT_TMx"= NULL, "TCDT_Min"= NULL, 
"TCDT_TMn"= NULL, "DBTCDT_Avg" = "DBTCDT_Avg", "DBTCDT_Max"= NULL, "DBTCDT_TMx"= NULL, 
"DBTCDT_Min"= NULL, "DBTCDT_TMn"= NULL, "ES1_ID"= NULL,"ES1_SF.1." = NULL,"ES1_SF.2." = NULL,
"ES1_SF.3." = NULL,"ES1_SF.4." = NULL,"ES1_SF.5." = NULL,"ES1_SF.6." = NULL,"ES1_SF.7." = NULL,
"ES1_SF.8." = NULL,"ES1_WC.1." = "wc10","ES1_WC.2." = "wc20","ES1_WC.3." = "wc30",
"ES1_WC.4." = "wc50","ES1_WC.5." = "wc70","ES1_WC.6." = "wc100","ES1_WC.7." = "wc150",
"ES1_WC.8." = "wc200", "tc_10_Avg" = "tc10","tc_20_Avg" = "tc20","tc_30_Avg" = "tc30",
"tc_50_Avg" = "tc50","tc_70_Avg" = "tc70","tc_100_Avg" = "tc100","tc_150_Avg" = "tc150",
"tc_200_Avg" = "tc200")

# only 6 cs655s were installed at West, adjust naming conventions accordingly
if (tower == 'West') {
  flux_column_dict['vwc_7_Avg'] = NULL 
  flux_column_dict['vwc_8_Avg'] = NULL
  flux_column_dict['vwc_9_Avg'] = NULL
  flux_column_dict['tsoil_7_Avg'] = NULL
  flux_column_dict['tsoil_8_Avg'] = NULL
  flux_column_dict['tsoil_9_Avg'] = NULL
}

################################################################################
## Variable Descriptions and Function definitions
################################################################################
# time: day of year (GMT), timestamp corresponds to end of 30-min averaging period
# Fc: carbon dioxide flux (mg CO2 m^2 s^-1)
# LE: latent heat flux (W m^-2)
# H: sensible heat flux (W m^-2)
# u_star: friction velocity (m s^-1)
# Ts: sonic air temp (deg C)
# u_T: flux covariance of air temperature with x coord (sonic) flow speed
# v_T: flux covariance of air temperature with y coord (sonic) flow speed
# w_T: flux covariance of air temperature with z coord (sonic) flow speed
# rho_c: CO2 density (mg m^-3)
# u_c: flux covariance of CO2 with x coord (sonic) flow speed
# v_c: flux covariance of CO2 with y coord (sonic) flow speed
# w_c: flux covariance of CO2 with z coord (sonic) flow speed
# u_q: flux covariance of H2O with x coord (sonic) flow speed
# v_q: flux covariance of H2O with y coord (sonic) flow speed
# w_q: flux covariance of H2O with z coord (sonic) flow speed
# u1: mean x coord (sonic) flow speed
# v1: mean y coord (sonic) flow speed
# w1: mean z coord (sonic) flow speed
# P: atmospheric pressure (kPa)
# Ta: HMP air temp (deg C)
# rho_v: vapor density from HMP (g^m-3)
# dir: CSAT sonic wind direction
# U: resultant wind speed (m s^-1)
# agc_Avg used to filter data
# Rn: net radiation (W m^-2)
# par: photosynthetically active radiation *NEW in 2021 EAST tower
# par_density: photosynthetically active radiation density *NEW in 2021 EAST tower
# G_1_1_1: soil heat flux 1 (W m^-2)
# G_2_1_1: soil heat flux 2 (W m^-2)
# soil_temp: temperature at 10 cm below surface
# DBTCDT_Avg: snow depth *NEW
# wc10: soil volumetric water content (mm/mm) at 10 cm below surface
# wc20: soil volumetric water content (mm/mm) at 20 cm below surface
# wc30: soil volumetric water content (mm/mm) at 30 cm below surface
# wc50: soil volumetric water content (mm/mm) at 50 cm below surface
# wc70: soil volumetric water content (mm/mm) at 70 cm below surface
# wc100: soil volumetric water content (mm/mm) at 100 cm below surface
# wc150: soil volumetric water content (mm/mm) at 150 cm below surface
# wc200: soil volumetric water content (mm/mm) at 200 cm below surface
# tc10: soil temperature (deg C) at 10 cm below surface
# tc20: soil temperature (deg C) at 20 cm below surface
# tc30: soil temperature (deg C) at 30 cm below surface
# tc50: soil temperature (deg C) at 50 cm below surface
# tc70: soil temperature (deg C) at 70 cm below surface
# tc100: soil temperature (deg C) at 100 cm below surface
# tc150: soil temperature (deg C) at 150 cm below surface
# tc200: soil temperature (deg C) at 200 cm below surface

# Rg: Saddle met incoming solar radiation (W/m^2)

## Define Constants
Mw = 18.02 # Molecular weight of water
R = 8.3143e-3 # universal gas constant [kPa m^3/(K mol)]
RD = R/29 # gas constant dry air [kPa m^3/(K g)]
RV = R/18 # gas constant water vapor [kPa m^3/(K g)]
mu = 29/18 # mass dry air/water vapor

## Define Functions
# This function calculates specific heat of moist air
spe_heat <- function(e,P){
  # Specific heat of moist air (J kg^-1 Deg C^-1)
  # e = water vapor pressure in kPa
  # P = barometric pressur in kPa
  
  epsilon = 0.622
  specific_heat_dry_air = 1003
  specific_heat_water_vapour = 1810
  mixing_ratio = e*epsilon/(P-e)
  specific_heat <- (specific_heat_dry_air + 
                      mixing_ratio*specific_heat_water_vapour)/(1+mixing_ratio)
  
  return(specific_heat)
  
} 

# This function downloads tvan files from ftp
download_ftp <- function(url, dest_dir, twr, overwrite = FALSE) {
  # This function downloads tvan files from the Niwot LTER ftp
  # Variable definitions
  # url -------- the url for the ftp where the tvan data is stored
  #
  # dest_dir --- the location where the tvan data will be downloaded to.
  #
  # twr -------- the file-name number associated with the tower; 54 for West,
  #              53 for East, as this is currently the naming scheme in the ftp
  #              directory. NOTE: this will break if files do not follow this 
  #              naming convention!
  #
  # overwrite -- An option to not overwrite files that have already been downloaded
  #              These files are detected with a simple name-match, so don't expect
  #              anything fancy if you rename files, or want to check timestamps.
  
  library(RCurl)
  
  # Check if the url exists
  if (!url.exists(url)) {
    stop("FTP url is not accessible.")
  }
  
  # make a directory to download files to, if it doesn't exist
  made_dir <- ifelse(!dir.exists(file.path(dest_dir)), 
         dir.create(file.path(dest_dir), recursive = TRUE), FALSE)
  if (!made_dir) {
    writeLines("Data download directory not created, it already exists.")
  }
  
  # get a list of available files from the ftp
  ftpfilenames <- unlist(
    strsplit(
      getURL(url,
             ftp.use.epsv = FALSE, dirlistonly = TRUE),
      split = "\n"))
  
  # Get a list of the files that aren't already in dest_dir
  current_files <- list.files(dest_dir, include.dirs = FALSE)
  
  #when downloading on windows, \r\n is default line ending, remove terminal
  # \r in line ending
  if (Sys.info()['sysname']=='Windows'){
    ftpfilenames <- gsub('\\\r$', '', ftpfilenames)
  }
  
  # Should the current files in the directory be overwritten?
  if (overwrite == TRUE) {
    # yes overwrite them all!
    files_to_download <- ftpfilenames
  } else {
    # only download files that aren't already in directory
    files_to_download <- ftpfilenames[which(!(ftpfilenames %in% current_files))]
  }
  
  
  # Filter files_to_download based off of "_flux" in name and tower keyword search
  # patterns
  files_to_download <- files_to_download[grep("_flux", files_to_download)]
  files_to_download <- files_to_download[grep(twr, files_to_download)]
  
  # Download the files or tell the user that there's nothing new to download
  if (length(files_to_download) > 0) {
    # Create destination directory if it doesn't exist
    ifelse(!dir.exists(file.path(dest_dir)),
           dir.create(file.path(dest_dir)), FALSE)
    lapply(files_to_download, function(x) {
      writeLines(paste0("Downloading ", x))
      try(download.file(paste0(url, x),
                        destfile = paste0(dest_dir, "/", x),
                        method = "curl"))
    })
    writeLines(paste0("Files can be found in: ", dest_dir))
  } else {
    writeLines("No new Tvan files to download.")
  }
  
  return(files_to_download)
}

# This function reads in tvan flux data
loadfluxdata <- function(data_dir, filename) {
  # This function loads raw 30-minute flux data (comma-delimited format),
  # and selects variables from it for further processing.
  # The variables it selects are defined by their column location in the tvan file, and
  # are listed above in the variable definition section. It loads one file at a time.
  #
  #
  # This function accounts for the following formatting of tvan files:
  #   - tvan files with headers above the columns
  #   - tvan files without headers (assumes column order is the same)
  #   - tvan files with 101 columns (these files are missing tc10-tc200)
  #   - tvan files with with 109 columns
  #
  # Function variable definitions
  #
  #   data_dir = a directory that a file to be loaded can be found in
  #   filename = the name of the file to be loaded
  
  print(paste0("Reading in ", filename))
  
  # Define date format and use it in read.table
  setClass("myPOSIXct")
  setAs("character","myPOSIXct", 
        function(from) as.POSIXct(from, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
  
  # Check if the first line of the file is a header or not
  # Files with headers have the word "flux" in the first line
  ifelse(grepl("flux", readLines(paste0(data_dir, filename), n = 1)),
         flux_skip <- 4, flux_skip <- 0)
  
  # Check for updated format that has par in the 2nd line
  # Starting in Tvan East in 2021
  header <- read.csv(paste0(data_dir, filename),
                    skip =0, header = F,
                    nrows =4)
  
  has_par <- ifelse((any(grepl('par', header[2,]))),
                    TRUE, FALSE)
  
  #check for the cs655's that were first added to the array in July of 2022.
  #need to specify '_5' because tvan west already had a vwc, we only want to 
  #detect the newer installations to inform our read and stack workflow below
  has_vwc <- ifelse((any(grepl('vwc_5', header[2,]))),
                    TRUE, FALSE)
  
  # Read in the data:
  # Set up a special case for the file "Tvan_West_54_flux_up_to_2010-03-01-2100.dat" 
  # This file has different widths of data starting at line 43536 (timestamp: 
  # 2009-12-11 21:00:00); So we read it in in two parts, then combine the 
  # parts together. Otherwise, we'll just use read.table as normal.
  if(filename == "Tvan_West_54_flux_up_to_2010-03-01-2100.dat") {
    flux1 <- read.table(file = paste0(data_dir, filename),
                        sep = ",", nrows = 43529,
                        na.strings = c("NAN","NaN","NA", "INF", "-INF"),
                        skip = flux_skip, as.is = TRUE)
    flux2 <- read.table(file = paste0(data_dir, filename),
                        sep = ",",
                        na.strings = c("NAN","NaN","NA", "INF", "-INF"),
                        skip = 43535, as.is = TRUE)
    # Add blank columns so binding data frames will be easier later
    flux1[c("V102", "V103", "V104", "V105", "V106", "V107", "V108", "V109")] <- NA
    flux <- rbind(flux1, flux2)
  } else {
    # Read in flux data as normal
    flux <- read.table(file = paste0(data_dir, filename), 
                       sep = ",", 
                       na.strings = c("NAN","NaN","NA", "INF", "-INF"),
                       skip = flux_skip, as.is = TRUE)
  }# flux$yday = lubridate::ymd_hms(flux$V1) %>%
#   lubridate::date(.)
  # Before naming variables, we'll need to check the number of columns in the data.
  # We change all columns except first to character type, then add on columns until 
  # the total for the file is 109. This standaradizes the width of our data and helps
  # deal with some of the inconsistencies in how R interprets the values in the data
  if(ncol(flux) == 101) { 
    # for 2007-2010, the soil temperature at 10-200 cm depth is missing for some files
    flux[,1] <- as.POSIXct(flux[,1], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    flux[,2:101] <- lapply(flux[,2:101], as.character)

    # add blank columns so binding data frames will be easier later
    flux[c("tc10", "tc20", "tc30", "tc50", "tc70", "tc100", "tc150", "tc200")] <- NA
  } else {
    # if 109 columns present
    flux[,1] <- as.POSIXct(flux[,1], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    flux[,2:109] <- lapply(flux[,2:109], as.character)
  }
  
  # Read in units and headers if present 
  # (this is for reference/debugging and not used later in script)
  if (flux_skip == 4) {
    flux_headers_units <- read.table(file = paste0(data_dir, filename),
                                     sep = ",", na.strings = "NaN",nrows = 2, skip = 1)
  }
  
  # Create output dataframe with selected variables
  # adding agc_Avg=59 & DBTCDT_Avg=80
  if (!has_par & !has_vwc){
  flux_P <- flux[,(c(1,4,5,6,8,9,11:13,14,16:18,21:23,24,28,31,33:35,37,
                     40,59,63,64,65,68,80,94:101,102:109))]
  colnames(flux_P) <- c("time","Fc", "LE", "H", "u_star", "Ts", "u_T", "v_T", "w_T",
                        "rho_c", "u_c", "v_c", "w_c", "u_q", "v_q", "w_q", "u1", "v1",
                        "w1", "P", "Ta", "rho_v", "dir", "U", 'agc_Avg',"Rn", "G_1_1_1", "G_2_1_1",
                        "soil_temp", "DBTCDT_Avg", "wc10", "wc20", "wc30", "wc50", "wc70", "wc100",
                        "wc150", "wc200", "tc10", "tc20", "tc30", "tc50", "tc70", 
                        "tc100", "tc150", "tc200")
  
  } else if (flux_skip == 4) { #added in 2023
    #here, using a workflow which finds columns using their names, instead of their 
    #indexes as this is more robust to accidental column mismatching.
    
    #get header
    flux3_headers <- read.table(file = paste0(data_dir, filename), 
                       sep = ",", 
                       skip = 1, nrows = 1,header = F, as.is = TRUE)
    
    #read in data and assign flux3_headerss as column names
    flux_3 <- read.table(file =paste0(data_dir, filename), 
                         sep = ",", 
                         skip = flux_skip,header = F, col.names = flux3_headers, 
                         as.is = TRUE)
    
    #rename columns & drop ones that are NULL in flux_column_dict
    for (entry in 1:length(flux_column_dict)){
      fname = names(flux_column_dict[entry])
      if(!is.null(flux_column_dict[[fname]])){ 
        colnames(flux_3)[colnames(flux_3) == fname] = flux_column_dict[[fname]]
        
      } else {
        flux_3[fname] = NULL
      }
    }
    flux_3[,1] <- as.POSIXct(flux_3[,1], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    flux_P <- flux_3
  } else {
    stop('Doh! An unexpected edge case has arisen. Reading ',filename,' halted.')
  }
  # Redefine numeric columns as numeric, now that zeros can't be erased willy-nilly by R
  flux_P[,2:ncol(flux_P)] <- sapply(flux_P[,2:ncol(flux_P)], as.numeric)
  
  return(flux_P)
}

# Functions for downloading saddle meterological data from EDI:
# These functions are from Sarah Elmendorf's utility_functions_all.R script
# https://github.com/NWTlter/long-term-trends/blob/master/utility_functions/utility_functions_all.R
# function to determine current version of data package on EDI
getCurrentVersion <- function(edi_id){
  # This function checks an EDI id to determine the most recent available
  # version. It returns the id of the most recent version.
  library(magrittr)
  versions = readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-nwt/', edi_id), 
                     warn = FALSE) %>%
    as.numeric() %>% (max)
  packageid=paste0('knb-lter-nwt.', edi_id, '.', versions)
  return (packageid)
}

#function to download the EML file from EDI
getEML <- function(packageid){
  
  require(magrittr)
  
  myurl<-paste0("https://portal.edirepository.org/nis/metadataviewer?packageid=",
                packageid,
                "&contentType=application/xml")
  
  myeml<-xml2::read_xml(paste0("https://portal.edirepository.org/nis/metadataviewer?packageid=",
                               packageid,
                               "&contentType=application/xml"))%>%EML::read_eml()
  
}

# Function for downloading from EDI
download_EDI <- function(edi_id, dest_dir, getNewData = TRUE) {
  # This section heavily borrowed from Sarah Elmendorf's generic_timeseries_workflow.R script
  # https://github.com/NWTlter/long-term-trends/blob/master/plotting_scripts/generic_timeseries_workflow.R
  
  # Depends on getCurrentVersion() and getEML()
  
  packageid = getCurrentVersion(edi_id)
  
  if (any(grepl(packageid, list.files(dest_dir)) == TRUE)) {
    writeLines(paste0("Most recent package version ", packageid, " is already downloaded."))
    return(list.files(dest_dir, pattern = paste0(packageid, ".{1,}csv"), full.names = T))
  } else if (getNewData == FALSE) {
    writeLines(paste0("A more recent version of the data (version ", packageid, ") is available.",
                      " But since you have specified getNewData = FALSE, the latest version will not be downloaded."))
    return(list.files(dest_dir, pattern = paste0(".{1,}csv"), full.names = T))
  } else {
    
    writeLines(paste0("Downloading package ", packageid, " from EDI."))
    
    myeml=getEML(packageid)

    # Create output directory for data
    ifelse(!dir.exists(file.path(dest_dir)),
           dir.create(file.path(dest_dir)), FALSE)

    ### eml reading and downloading of csv
    if (is.null(names(myeml$dataset$dataTable))){
      attributeList=lapply(myeml$dataset$dataTable, function(x){
        EML::get_attributes(x$attributeList)
      })
      names(attributeList)=lapply(myeml$dataset$dataTable, function(x){
        x$physical$objectName})
      if(getNewData){
        #download all the datatables in the package
        csv_list <- list()
        csv_list <- lapply(myeml$dataset$dataTable, function(x){
          url_to_get=x$physical$distribution$online$url$url
          download.file(url_to_get,
                        destfile=paste0(dest_dir, "/",
                                        packageid, "_",
                                        x$physical$objectName),
                        method = "curl")
          output_csv_file <-  paste0(dest_dir, "/",
                                     packageid, "_",
                                     x$physical$objectName)
        })
      }

    }else{
      #if only one data table
      attributeList=list(EML::get_attributes(myeml$dataset$dataTable$attributeList))
      names(attributeList)=myeml$dataset$dataTable$physical$objectName
      if(getNewData){
        url_to_get=myeml$dataset$dataTable$physical$distribution$online$url$url
        download.file(url_to_get,
                      destfile=paste0(dest_dir, "/",
                                      packageid, "_",
                                      myeml$dataset$dataTable$physical$objectName),
                      method = "curl")
        output_csv_file <- paste0(dest_dir, "/",
               packageid, "_",
               myeml$dataset$dataTable$physical$objectName)
      }
    }

    # Also save the full xml
    write_eml(myeml, file = paste0(dest_dir, "/", packageid, ".xml"))

    writeLines(paste0("Downloaded data can be found in: ", dest_dir))
    return(output_csv_file)
     }
  }

################################################################################
# Download Data
################################################################################

# Download the raw tvan half-hourly flux measurements
download_ftp(url = tvan.ftp.url, 
             dest_dir = ftp_download_dir, 
             twr = twr,
             overwrite = overwrite)

# Download saddle met data
#hourly
# saddle_met_data_hr_fp <- download_EDI(edi_id = saddle_met_data, 
#                                    dest_dir = paste0(data_dir, "saddle_met_data"),
#                                    getNewData = getNewData)
#tenminute
saddle_met_data_tm_fp <- download_EDI(edi_id = saddle_ten_minute, 
                                   dest_dir = paste0(data_dir, "saddle_met_data"),
                                   getNewData = getNewData)

################################################################################
# Load in Tvan data
################################################################################
## Load Data
# get a list of all flux files in the download directory
allfilelist <- list.files(ftp_download_dir, pattern = paste0("_flux"))

# subset file list based on tower id (in case both towers' files
# are in the same directory)
filelist <- allfilelist[grepl(twr, allfilelist)]

# if logger programs get updated again, sanity check that the files with
# par in them will be processed correctly by inspection, this has been checked
# thru 1/20/2021, but since columns load by position will need to make sure if
# further program updates occur

# withpar = list()
# for (filenm in list.files(paste0(data_dir, '/tvan/East'))){
#   header <-read.csv(paste0(data_dir, '/tvan/East/', filenm),
#                     skip =0, header = F,
#                     nrows =4)
#   if (any(grepl('par', header[2,]))){
#     withpar[[filenm]] <-filenm
#   }
# }
# 
# withpar = list()
# for (filenm in list.files(paste0(data_dir, '/tvan/West'))){
#   header <-read.csv(paste0(data_dir, '/tvan/West/', filenm),
#                     skip =0, header = F,
#                     nrows =4)
#   if (any(grepl('par', header[2,]))){
#     withpar[[filenm]] <-filenm
#   }
# }

# Load each raw flux file into a list of dataframes
flux_P_list <- vector(mode = "list", length = length(filelist))
flux_P_list <- lapply(filelist, loadfluxdata, data_dir = paste0(ftp_download_dir, "/"))

# collapse list into one dataframe
flux_P_all <- dplyr::bind_rows(flux_P_list)

# Subset flux_P_all by start and end dates, if start and end dates = NA, use all
# available data
# ***
# NOTE about time filtering: this solution is inelegant because it first loads all 
# the tvan data in and then subsets it by time. This takes a long time but because we
# can't rely on the data files to have a specific start and end date, be a known length,
# or not have overlapping dates with each other, this is the easiest solution for now. 
# In the future, it would be nice to modify the loading script to take start and end 
# dates into account, as this method will become unsustainable as more and more years
# of data are added
# ****

# Convert start and end dates to GMT from MST
# Times are read in as GMT, but the user is likely assuming MST for start date.
# Therefore, change the start date and end date from MST format to GMT format for 
# filtering - The final data product will eventually be changed back to MST

if (!is.na(start_date)) {
  start_date <- as.POSIXct(paste0(start_date, " 00:00:00"), tz = "MST")
  lubridate::with_tz(start_date, "UTC")
  start_buffer <- start_date - lubridate::hours(24)
  flux_P_all <- flux_P_all[flux_P_all[,"time"] >= start_buffer,]
}
if (!is.na(end_date)) {
  end_date <- as.POSIXct(paste0(end_date, " 23:59:59"), tz = "MST")
  lubridate::with_tz(end_date, "UTC")
  end_buffer <- end_date + lubridate::hours(24)
  flux_P_all <- flux_P_all[flux_P_all[,"time"] <= end_buffer,]
}

# Remove duplicate time stamps (since we don't rely on tvan files not having overlapping 
# time periods)

flux_P <- flux_P_all[!duplicated(flux_P_all$time),]

# Generate a warning message if there are any duplicated timestamps after removing rows
# which are exact copies of each other.
if (any(duplicated(flux_P$time)) == TRUE) {
  warning("Duplicate timestamps present!")
}


# Fix the 2015 time difference in the second half of the year in the west tower:
# Note this timefix was not necessary over all time, but because specific regions
# were identified for supplmental filtering using this shift, left here
# Moved this to after the duplicate removal as this creates some duplicates

if (tower == "West") {
  flux_P <- flux_P %>%
    mutate(date = lubridate::date(time),
           month = month(date),
           shifted = if_else(month %in% c(7:12), TRUE, FALSE), 
           time = if_else(month %in% c(7:12), time +
                            lubridate::hours(7), time)) %>%
    select(-date, -month)
}

############################################################################
# All Post-hoc Recalibration logic lives here
############################################################################

# Correct HFT REBS for week where instrumentation was updated but program was not
# date range 20211019 1200MST - 20211028 11:40 MST
# To apply the new soil heat flux (shf) sensor calibrations, divide by the
# old multiplier and multiply by the new e.g., for the new W tower G_1_1_1,
# divide by 33.4 and then multiply by 56.17. JK will update the BADM to
# reflect these new sensors and the new snow depth sensors when submit to AmeriFlux.   

# note the timestamp shifts are in standard time but need to convert
# to mtn here to make it adjust correctly against the
# datetime in the data.

if (tower == 'West'){
  #but first...
  #flip all shf data from before the sensors were replaced (and installed correctly)
  #on 2021-10-10 at noon MST
  flux_P <- flux_P %>% 
    mutate(
      G_1_1_1 = ifelse(time >= (lubridate::ymd_hms("2021-10-19 12:00:00 UTC")+
                               lubridate::hours(7)), G_1_1_1, G_1_1_1*-1),
      G_2_1_1 = ifelse(time >= (lubridate::ymd_hms("2021-10-19 12:00:00 UTC")+
                               lubridate::hours(7)), G_2_1_1, G_2_1_1*-1)
    )
  flux_P <- flux_P %>%
    mutate(G_1_1_1 = ifelse (time >=(lubridate::ymd_hms ('2021-10-19 12:00:00 UTC')+
                                    lubridate::hours(7)) & 
                            time <=(lubridate::ymd_hms ('2021-10-28 11:40:00 UTC')+
                                      lubridate::hours(7)),
                          ((G_1_1_1/33.4)*(1000/56.17)), G_1_1_1),
           G_2_1_1 = ifelse (time  >=(lubridate::ymd_hms ('2021-10-19 12:00:00 UTC')+
                                     lubridate::hours(7)) & 
                            time <= (lubridate::ymd_hms ('2021-10-28 11:40:00 UTC')+
                                       lubridate::hours(7)),
                          ((G_2_1_1/33.3)*(1000/56.59)), G_2_1_1))

  flux_P <- flux_P %>%
    mutate(G_1_1_1 = ifelse (time >=(lubridate::ymd_hms ('2021-10-28 12:00:00 UTC')+
                                    lubridate::hours(7)),
                          ((G_1_1_1/56.17)*(1000/56.17)), G_1_1_1),
           G_2_1_1 = ifelse (time  >=(lubridate::ymd_hms ('2021-10-28 12:00:00 UTC')+
                                     lubridate::hours(7)),
                          ((G_2_1_1/56.59)*(1000/56.59)), G_2_1_1))
  
} else if (tower == 'East'){ 
  #do the same for east for different date range & constants
  # The following note was copied from the metadata in the field maintenance logs:
  
  # 20220721 1130 MST replaced HFT3 REBS soil heat flux plates with Hukseflux HFP01-15 
  # soil heat flux plates.  Previous constants shf1 = 33.8, G_2_1_1 = 31.4. New SHF1= serial 
  # 18912, sensitivity S = 56.63 x 10^-6 V/(W/m^2), New G_2_1_1 = serial # 19346, 
  # sensitivity S = 57.04*10^-6 V/(W/m^2).
  # *Note, program was not updated with new sensitivity (constants) until 20220726,  
  # and data between these times will need to be updated with correct constants. 
  # (calibration certificates can be found at data/data2/rawarchive/nwt_protocols).
  
  #but first...
  #flip all shf data from East tower as they are upsidedown as of April 2023
  flux_P <- flux_P %>% 
    mutate(
      G_1_1_1 = ifelse(time >= (lubridate::ymd_hms("2022-07-21 00:00:00")+
                               lubridate::hours(7)), G_1_1_1, G_1_1_1*-1),
      G_2_1_1 = ifelse(time >= (lubridate::ymd_hms("2022-07-21 00:00:00")+
                               lubridate::hours(7)), G_2_1_1, G_2_1_1*-1)
    ) %>%
    #now fixing cal coeff's as described above. Calibration certificates can be found
    #on the NWT server in data/data2/nwt_protocol/certificates/
    mutate(G_1_1_1 = ifelse (time >= (lubridate::ymd_hms ('2022-07-21 11:30:00 UTC')+
                                     lubridate::hours(7)) &
                            time <= (lubridate::ymd_hms ('2022-07-26 12:00:00 UTC')+
                                       lubridate::hours(7)),
                          (G_1_1_1/33.8)*(1000/56.63), G_1_1_1), 
           G_2_1_1 = ifelse (time >= (lubridate::ymd_hms ('2022-07-21 11:30:00 UTC')+
                                     lubridate::hours(7)) &
                            time <= (lubridate::ymd_hms ('2022-07-26 12:00:00 UTC')+
                                       lubridate::hours(7)),
                          (G_2_1_1/31.4)*(1000/57.04), G_2_1_1)
    )
  # likewise, there was an issue where the CR Basic program for Tvan_East was
  # reverted to an older version & it wasn't noticed that this older version did
  # not have the correct calibration coefficients in it yet.
  # 1 = old:17.65
  # 2 = old:17.53
  flux_P <- flux_P %>%
    ## TODO 
    # 1- Maybe extend this time range to the logger change ( this would work if 1075_21 ) **
    # 2 - Try and undo this completely ( this would work if 20220919 program was loaded )
    
    mutate(G_1_1_1 = ifelse (time >=(lubridate::ymd_hms ('2022-09-19 15:45:00 UTC')+
                                    lubridate::hours(7)) &
                            time<=(lubridate::ymd_hms ('2023-07-13 12:00:00 UTC')+
                                     lubridate::hours(7)),
                          (G_1_1_1/33.8)*(1000/56.63), G_1_1_1),
           G_2_1_1 = ifelse (time >=(lubridate::ymd_hms ('2022-09-19 15:45:00 UTC')+
                                    lubridate::hours(7)) &
                            time<(lubridate::ymd_hms ('2023-07-13 12:00:00 UTC')+
                                    lubridate::hours(7)),
                          (G_2_1_1/31.4)*(1000/57.04), G_2_1_1)
    ) #%>% 
  
  # TODO WARNING: Remove this block once this issue is solved. 
    # Jen said she would look at the tower itself to see if anything is
    # wired incorrectly. Jen, Sarah, and Miles quadruple checked the calibration
    # coefficients and are super stumped as to what is causing the East tower's
    # SHF data to have a substantially smaller stretch that it seems like it should
  ######## THIS BLOCK (BELOW) SHOULD BE COMMENTED OUT AND FIXED WHEN WE KNOW
  ## WHAT IS UP W THE SHF T THIS TOWER ####################################
  # mutate(G_1_1_1 = ifelse (time >= (lubridate::ymd_hms ('2022-07-21 11:30:00 UTC')+
  #                                  lubridate::hours(7)),
  #                       NaN, G_1_1_1),
  #        G_2_1_1 = ifelse (time >= (lubridate::ymd_hms ('2022-07-21 11:30:00 UTC')+
  #                                  lubridate::hours(7)),
  #                       NaN, G_2_1_1),
  # )
}

# Wind Direction
if (tower == 'East'){
  # We think that the East tower's wind direction was not correctly aligned to 
  # 'True' north until 2022-07-29 (Guessing 1200MST ish), when the flux sensor
  # boom was rotated from 345 to 0. In order to back-correct the wind direction
  # data, 15 degrees should be subtracted from all wind direction data before
  # the boom was rotated. I'll use a modular arithmetic approach for this;
  # 15 degrees will be subtracted from all data and then data points >360
  # will ave 360 subtracted from them, and vice verse for data < 0.
  flux_P <- flux_P |> 
    # Convert from compass rose to circular
    dplyr::mutate(dir = dir + 90) |> 
    dplyr::mutate(
      dir = ifelse(time < lubridate::ymd_hms("2022-07-29 12:00:00"), dir - 15, dir)
    ) |> 
    dplyr::mutate(
      dir = dplyr::case_when(
        dir > 360 ~ dir - 360,
        dir < 0 ~ dir + 360, 
        TRUE ~ dir # otherwise don't adjust
      )
    )|> 
    # put back in circular from compass rose
    dplyr::mutate(dir = dir - 90)
} else if (tower == 'West'){
  # Similarly, at West - on 2022-07-29 JK also rotated the flux sensor crossbar 15 degrees
  # COUNTER CLOCKWISE ( from 15 to 0). This was necesary as previously, a rock 
  # was used to prevent the crossbar from drifting in the wind. It appears
  # the rock failed on approximately 2020-11-29, allowing the cross bar to rotate
  # from 0 to 15. Data collected between 2020-11-29 and when JK/JM fixed the
  # crossbar's heading (and fastened it more securly) on 2022-07-29 will need
  # to be corrected. I will again use a modular arithmetic approach.
  flux_P <- flux_P |> 
    # Convert from compass rose to circular
    dplyr::mutate(dir = dir + 90) |> 
    dplyr::mutate(
      dir = ifelse(time > lubridate::ymd_hms("2020-11-29 00:00:00") &
                     time < lubridate::ymd_hms("2022-07-29 12:00:00"), dir + 15, dir)
    ) |> 
    dplyr::mutate(
      dir = dplyr::case_when(
        dir > 360 ~ dir - 360,
        dir < 0 ~ dir + 360, 
        TRUE ~ dir # otherwise don't adjust
      )
    ) |> 
    # put back in circular from compass rose
    dplyr::mutate(dir = dir - 90)
}

################################################################################
## Plot the unfiltered data and save the compiled raw tvan data
################################################################################
# Plot unfiltered data
if (makeplots == TRUE) {
  # List of units and variable explanations (to add to plots):
  flux_units <- c("Fc" = "carbon dioxide flux (mg CO2 m^2 s^-1)",
  "LE" = "latent heat flux (W m^-2)",
  "H" = "sensible heat flux (W m^-2)",
  "u_star" = "friction velocity (m ,s^-1)",
  "Ts" = "sonic air temp (deg C)",
  "u_T" = "flux covariance of air temperature with x coord (sonic) flow speed",
  "v_T" = "flux covariance of air temperature with y coord (sonic) flow speed",
  "w_T" = "flux covariance of air temperature with z coord (sonic) flow speed",
  "rho_c" = "CO2 density (mg m^-3)",
  "u_c" = "flux covariance of CO2 with x coord (sonic) flow speed",
  "v_c" = "flux covariance of CO2 with y coord (sonic) flow speed",
  "w_c" = "flux covariance of CO2 with z coord (sonic) flow speed",
  "u_q" = "flux covariance of H2O with x coord (sonic) flow speed",
  "v_q" = "flux covariance of H2O with y coord (sonic) flow speed",
  "w_q" = "flux covariance of H2O with z coord (sonic) flow speed",
  "u1" = "mean x coord (sonic) flow speed",
  "v1" = "mean y coord (sonic) flow speed",
  "w1" = "mean z coord (sonic) flow speed",
  "P" = "atmospheric pressure (kPa)",
  "Ta" = "HMP air temp (deg C)",
  "rho_v" = "vapor density from HMP (g m^-3)",
  "dir" = "CSAT sonic wind direction",
  "U" = "resultant wind speed (m s^-1)",
  "Rn" = "net radiation (W m^-2)",
  "par" = "photosynthetically active radiation (mV)",
  "par_density" =	"photosynthetically active radiation density (umol s^-1 m^-2)",
  "G_1_1_1" = "soil heat flux 1 (W m^-2)",
  "G_2_1_1" = "soil heat flux 2 (W m^-2)",
  "G_1_1_2" = "soil heat flux 1 (W m^-2)",
  "G_2_1_2" = "soil heat flux 2 (W m^-2)",
  "soil_temp" = "temperature at 10 cm below surface",
  "wc10" = "soil volumetric water content (mm/mm) at 10 cm below surface",
  "wc20" = "soil volumetric water content (mm/mm) at 20 cm below surface",
  "wc30" = "soil volumetric water content (mm/mm) at 30 cm below surface",
  "wc50" = "soil volumetric water content (mm/mm) at 50 cm below surface",
  "wc70" = "soil volumetric water content (mm/mm) at 70 cm below surface",
  "wc100" = "soil volumetric water content (mm/mm) at 100 cm below surface",
  "wc150" = "soil volumetric water content (mm/mm) at 150 cm below surface",
  "wc200" = "soil volumetric water content (mm/mm) at 200 cm below surface",
  "tc10" = "soil temperature (deg C) at 10 cm below surface",
  "tc20" = "soil temperature (deg C) at 20 cm below surface",
  "tc30" = "soil temperature (deg C) at 30 cm below surface",
  "tc50" = "soil temperature (deg C) at 50 cm below surface",
  "tc70" = "soil temperature (deg C) at 70 cm below surface",
  "tc100" = "soil temperature (deg C) at 100 cm below surface",
  "tc150" = "soil temperature (deg C) at 150 cm below surface",
  "tc200" = "soil temperature (deg C) at 200 cm below surface",
  "DBTCDT_Avg" = "snow depth (cm)",
  "SWC_2_1_1" = "soil volumetric water content (%) in West pit 10cm below surface",
  "SWC_2_2_1" = "soil volumetric water content (%) in West pit 20cm below surface",
  "SWC_2_3_1" = "soil volumetric water content (%) in West pit 30cm below surface",
  "SWC_3_1_1" = "soil volumetric water content (%) in North pit 10cm below surface",
  "SWC_3_2_1" = "soil volumetric water content (%) in North pit 20cm  below surface",
  "SWC_3_3_1" = "soil volumetric water content (%) in North pit 30cm  below surface",
  "SWC_4_1_1" = "soil volumetric water content (%) in South pit 10cm below surface",
  "SWC_4_2_1" = "soil volumetric water content (%) in South pit 20cm below surface",
  "SWC_4_3_1" = "soil volumetric water content (%) in South pit 30cm below surface",
  "TS_2_1_1" = "soil temperature (deg C) in West pit 10cm below surface",
  "TS_2_2_1" = "soil temperature (deg C) in West pit 20cm below surface",
  "TS_2_3_1" = "soil temperature (deg C) in West pit 30cm below surface",
  "TS_3_1_1" = "soil temperature (deg C) in North pit 10cm below surface",
  "TS_3_2_1" = "soil temperature (deg C) in North pit 20cm  below surface",
  "TS_3_3_1" = "soil temperature (deg C) in North pit 30cm  below surface",
  "TS_4_1_1" = "soil temperature (deg C) in South pit 10cm below surface",
  "TS_4_2_1" = "soil temperature (deg C) in South pit 20cm below surface",
  "TS_4_3_1" = "soil temperature (deg C) in South pit 30cm below surface",
  #supplemental derived units
  "G" = "average the two soil heat fluxes (W/m^2)",
  "ea" = "vapor pressure (kPa)",
  "es" = "saturation vapor pressure (kPa)",
  "vpd" = "vapor pressure deficit (kPa)",
  "RH" = "relative humidity (unitless proportion)",
  "h2o_hmp_mean" = "h2o_hmp_mean (units not yet specified)",
  "rho_d_mean" = "rho_d_mean (units not yet specified)",
  "rho_a_mean" = "rho_a_mean (units not yet specified)",
  "sigma" = "sigma (units not yet specified)",
  "lambda" = "latent heat of vaporization (J kg^-1)",
  "FC" = "(mg CO2 m-2 s-1)",
  "cS" = "cS (units not yet specified)",
  "co2_wpl_H" = "co2_wpl_H (units not yet specified)",
  "co2_wpl_LE" = "co2_wpl_LE (units not yet specified)",
  "cS" = "cS (units not yet specified)",
  "cT" = "cT (units not yet specified)",
  "Fc_rot_wpl" = "Fc_rot_wpl (mg CO2 m-2 s-1)",
  "H_rot_wpl" = "H_rot_wpl (W m^-2)",
  "LE_rot_wpl" = "LE_rot_wpl (W m^-2)",
  "gC" = "gC"
  )
  
  for(i in 2:ncol(flux_P)) {
    y_name = names(flux_P)[i]
    p <- flux_P %>% 
      dplyr::filter(!is.na(!!dplyr::sym(y_name))) %>% 
      dplyr::select(time, all_of(y_name)) %>% 
      ggplot(aes_string(x = "time", y = y_name)) +
      geom_point() +
      ylab(flux_units[y_name]) +
      ggtitle(paste0(tower, " ", y_name, " - ", flux_units[y_name], " - raw"))
    ggsave(paste0(allyrs_dir,"/", tower, "_", y_name, "_raw_data", plot_filetype),
           plot = p)
    
  }
}

# save an object with unfiltered data for comparison later
flux_P.unfilt <- flux_P
flux_P.unfilt$Filtered <- "unfiltered"

# set up dates for naming the file
time1 <- gsub(":", "-", gsub(" ", "_", lubridate::with_tz(min(flux_P$time), tz = "MST")))
time2 <- gsub(":", "-", gsub(" ", "_", lubridate::with_tz(max(flux_P$time), tz = "MST")))
period <- paste0(time1, "_to_", time2)
writeLines(paste0("Saving unfiltered concatenated 30-minute tvan data for the time period
                  ",
                  lubridate::with_tz(min(flux_P$time), tz = "MST"), " to ",
                  lubridate::with_tz(max(flux_P$time), tz = "MST")))

writeLines(paste0("The concatentated file can be found here ", 
                  paste0(output_dir_base, "Raw_compiledData/",
                         "tvan_raw_data_", tower,"_", period, "_flux_P.csv")))
# Save the raw data from the whole processing period in one csv
write.csv(flux_P.unfilt, file = paste0(output_dir_base, "Raw_compiledData/",
                                "tvan_raw_data_", tower,"_", period, "_flux_P.csv"),
          row.names = FALSE)


################################################################################
## Filter Tvan Data
################################################################################
# This section filters data in two steps. First it filters extreme values from data by 
# setting common-sense cutoffs that apply to the whole 12 year period, then, in a second 
# step, it filters particular locations in the data that need more tailored filtering. 
# This section of code is laid out so that the common-sense cutoffs for east and west
# towers are specified in two lists below (and can be easily changed, if needed). 
# Then the proposed common-sense cutoffs are plotted before the data is filtered. Finally, 
# a dataframe of flagged points is generated to hold information about which points are 
# filtered out and the data is filtered.

# Filter cutoffs with reasoning behind them added in comments when appropriate:
# To the user: update these cutoffs if new data requires it. For most variables,
# these cutoffs should be appropriate and are unlikely to need to be changed. Change
# with caution, if you are only working with a couple of years of data.
#### West cutoffs ####
cutoffs_west <- list(
            # Fc - CO2 flux (mg CO2 m^2 s^-1)
            Fc = c(Fc_upr = 5, 
                   Fc_lwr = -5),
            # LE - Latent heat flux, W/m^2
            LE = c(LE_lwr = 500, 
                   LE_upr = -250),
            # H - Sensible heat flux, W/m^2 - should be in same ballpark as latent heat
            # flux (LE)
            H = c(H_upr = 500, 
                  H_lwr = -250),
            # u_star - Friction velocity (m/s) >10, can't be less than zero
            u_star = c(u_star_upr = 20, 
                       u_star_lwr = 0),
            # Ts - sonic air temperature (C) - should be within a degree or two of air
            # temperature
            Ts = c(Ts_upr = 25,
                   Ts_lwr = -40),
            # u_T - w_T - flux covariance of air temperature in x,y,&z direction
            # should aim to fill plot
            u_T = c(u_T_upr = 2, 
                    u_T_lwr = -2),
            v_T = c(v_T_upr = 2.5, 
                    v_T_lwr = -2.5),
            w_T = c(w_T_upr = 0.56, 
                    w_T_lwr = -0.56),
            # Can't be less than atmospheric CO2 ~415 ppm (same as mg*m^-3) but 
            # we are keeping lower points as they might be useful to some users
            rho_c = c(rho_c_upr = 2500, 
                      rho_c_lwr = 0),
            # u_c - w_c - Flux covariance of CO2 in x,y,&z direction - should fill plot;
            u_c = c(u_c_upr = 50, 
                    u_c_lwr = -50),
            v_c = c(v_c_upr = 50, 
                    v_c_lwr = -50),
            w_c = c(w_c_upr = 5, 
                    w_c_lwr = -5),
            # u_c - w_c - Flux covariance of H20 in x, y, & z direction - should fill plot
            u_q = c(u_q_upr = 4, 
                    u_q_lwr = -4),
            v_q = c(v_q_upr = 4, 
                    v_q_lwr = -4),
            w_q = c(w_q_upr = 0.2, 
                    w_q_lwr = -0.2),
            # u1 - mean x coord flow spead - remove points greater than 25 or less 
            # than -25
            u1 = c(u1_upr = 25, 
                    u1_lwr = -25),
            # v1 - mean y coord flow spead - remove points greater than 37.5 or less 
            # than -12.5
            v1 = c(v1_upr = 37.5, 
                   v1_lwr = -12.5),
            # w1 - mean z coord flow spead - remove points greater than 37.5 or less 
            # than -12.5
            w1 = c(w1_upr = 2.5, 
                   w1_lwr = -2.5),
            # P - pressure looks good as is. We use cut-offs of 
            # 70 as an upper limit and 60 as a lower limit (these cutoffs exceed maximum
            # values from past extremes, so they are likely to retain all data in the
            # future).
            P = c(P_upr = 70, 
                   P_lwr = 60),
            # Ta - Air temperature, it's tricky, and mostly can't be solved with simple 
            # cutoffs. For right now, I will use the sonic air temp cutoffs and deal 
            # with the rest later. Note: -39- -40 are sensor's way of recording a 
            # bad value.
            Ta = c(Ta_upr = 30,
                   Ta_lwr = -50),
            # rho_v - Similar situation to Ta, going to use 0 as common-sense low boundary
            # and 12 as an upper boundary that removes the crazy spike in 2019
            rho_v = c(rho_v_upr = 12,
                      rho_v_lwr = 0),
            # dir - This is a compass-rose so values are -90 to 270
            dir = c(dir_upr = 270,
                    dir_lwr = -90),
            # U - wind; 40 m/s ~ 90 mph; It is windy up there, so a high cutoff is
            # reasonable lower cutoff is 0, since negative values make no sense in 
            # this context. 
            # Note: we raised the cutoff to 75. There were only a few points above 45,
            # but they all seemed reasonable and it *is* pretty windy up there.
            U = c(U_upr = 75,
                    U_lwr = 0),
            # additional filter, no filter applied here
            agc_Avg = c(agc_Avg_upr = 9999,
                        agc_Avg_lwr = -9999),
            # Rn - net radiation (W m^-2) - lower level of -250 or so, upper high enough
            # that should it does not cutoff any points
            Rn = c(Rn_upr = 1500,
                   Rn_lwr = -250),
            # G_1_1_1 - soil heat flux 1 (W m^-2) - suggest cutoff of -150 and 300
            G_1_1_1 = c(G_1_1_1_upr = 300,
                   G_1_1_1_lwr = -150),
            # G_2_1_1 - soil heat flux 2 (W m^-2) - suggest cutoff of -150 and 300
            G_2_1_1 = c(G_2_1_1_upr = 300,
                     G_2_1_1_lwr = -150),
            # soil_temp - soil temp 10 cm depth (C) - raw data look great so we'll
            # maintain that by giving an extreme high cutoff value of 30 deg C and 
            # an extreme low cutoff value of -30
            soil_temp = c(soil_temp_upr = 30,
                     soil_temp_lwr = -30),
            # snow depth, no filter applied here, see filtering below
            DBTCDT_Avg = c(DBTCDT_Avg_upr = 9999,
                           DBTCDT_Avg_lwr = -9999),
            # wc10 - volumetric water content at 10cm (mm/mm) - remove zeros
            # Need to add code to deal with offset in 2017+
            wc10 = c(wc10_upr = 45,
                     wc10_lwr = 1),
            # wc20 - volumetric water content at 20cm (mm/mm) - remove zeros
            wc20 = c(wc20_upr = 45,
                     wc20_lwr = 1),
            # wc30 - volumetric water content at 30cm (mm/mm) - remove individual 
            # points; <8 or so
            # Need to add code to deal with offset in 2017+
            wc30 = c(wc30_upr = 50,
                     wc30_lwr = 5),
            # wc50 - volumetric water content at 50cm (mm/mm) - remove individual 
            # points <5 or so
            wc50 = c(wc50_upr = 50,
                     wc50_lwr = 5),
            # wc70 - volumetric water content at 70cm (mm/mm) - remove anything <0.1, 
            # to preserve cycling in 2010-2011
            wc70 = c(wc70_upr = 50,
                     wc70_lwr = 0.1),
            # wc100 - volumetric water content at 100cm (mm/mm) - remove anything <0.1, 
            # to preserve cycling in 2009-2010
            wc100 = c(wc100_upr = 50,
                      wc100_lwr = 0.1),
            # wc150 -  volumetric water content at 150cm (mm/mm) - remove anything <8,
            # this removes the "fishhook" in 2009, the average yearly minimum is around 
            # 9.25 with a standard deviation of 0.27 - so pretty consistent
            wc150 = c(wc150_upr = 50,
                      wc150_lwr = 0.1),
            # wc200 -  volumetric water content at 200cm (mm/mm) - remove anything <1
            wc200 = c(wc200_upr = 50,
                      wc200_lwr = 1),
            # tc10 -  soil temperature at 10cm (C) - keep values between 30 and -30
            tc10 = c(tc10_upr = 30,
                      tc10_lwr = -30),
            # tc20 -  soil temperature at 20cm (C) - keep values between 30 and -30
            tc20 = c(tc20_upr = 30,
                     tc20_lwr = -30),
            # tc30 -  soil temperature at 30cm (C) - keep values between 30 and -30
            tc30 = c(tc30_upr = 30,
                     tc30_lwr = -30),
            # tc50 -  soil temperature at 50cm (C) - keep values between 30 and -30
            tc50 = c(tc50_upr = 30,
                     tc50_lwr = -30),
            # tc70 -  soil temperature at 70cm (C) - keep values between 30 and -30
            tc70 = c(tc70_upr = 30,
                     tc70_lwr = -30),
            # tc100 -  soil temperature at 100cm (C) - keep values between 30 and -30
            tc100 = c(tc100_upr = 30,
                      tc100_lwr = -30),
            # tc150 -  soil temperature at 150cm (C) - keep values between 30 and -30
            tc150 = c(tc150_upr = 30,
                     tc150_lwr = -30),
            # tc200 -  soil temperature at 200cm (C) - keep values between 35 and -30
            tc200 = c(tc200_upr = 35,
                     tc200_lwr = -30),
            #SWC logically can not exceed the 0 to 100% range.
            SWC_2_1_1 = c(SWC_2_1_1_upr = 100,
                          SWC_2_1_1_lwr = 0),
            SWC_2_2_1 = c(SWC_2_2_1_upr = 100,
                          SWC_2_2_1_lwr = 0),
            SWC_2_3_1 = c(SWC_2_3_1_upr = 100,
                          SWC_2_3_1_lwr = 0),
            SWC_3_1_1 = c(SWC_3_1_1_upr = 100,
                          SWC_3_1_1_lwr = 0),
            SWC_3_2_1 = c(SWC_3_2_1_upr = 100,
                          SWC_3_2_1_lwr = 0),
            SWC_3_3_1 = c(SWC_3_3_1_upr = 100,
                          SWC_3_3_1_lwr = 0),
            SWC_4_1_1 = c(SWC_4_1_1_upr = 100,
                          SWC_4_1_1_lwr = 0),
            SWC_4_2_1 = c(SWC_4_2_1_upr = 100,
                          SWC_4_2_1_lwr = 0),
            SWC_4_3_1 = c(SWC_4_3_1_upr = 100,
                          SWC_4_3_1_lwr = 0),
            #using same logic as above for soil_temp
            TS_2_1_1 = c(TS_2_1_1_upr = 30,
                         TS_2_1_1_lwr = -30),
            TS_2_2_1 = c(TS_2_2_1_upr = 30,
                         TS_2_2_1_lwr = -30),
            TS_2_3_1 = c(TS_2_3_1_upr = 30,
                         TS_2_3_1_lwr = -30),
            TS_3_1_1 = c(TS_3_1_1_upr = 30,
                         TS_3_1_1_lwr = -30),
            TS_3_2_1 = c(TS_3_2_1_upr = 30,
                         TS_3_2_1_lwr = -30),
            TS_3_3_1 = c(TS_3_3_1_upr = 30,
                         TS_3_3_1_lwr = -30),
            TS_4_1_1 = c(TS_4_1_1_upr = 30,
                         TS_4_1_1_lwr = -30),
            TS_4_2_1 = c(TS_4_2_1_upr = 30,
                         TS_4_2_1_lwr = -30),
            TS_4_3_1 = c(TS_4_3_1_upr = 30,
                         TS_4_3_1_lwr = -30)
            )
#### East cutoffs ####
cutoffs_east <- list(
            # Fc - CO2 flux (mg CO2 m^2 s^-1)
            Fc = c(Fc_upr = 5, 
                   Fc_lwr = -5),
            # LE - Latent heat flux, W/m^2 - 
            LE = c(LE_lwr = 500, 
                   LE_upr = -250),
            # H - Sensible heat flux, W/m^2 - should be in same ballpark as latent heat
            # flux (LE)
            H = c(H_upr = 500, 
                  H_lwr = -250),
            # u_star - Friction velocity (m/s) >10, can't be less than zero
            u_star = c(u_star_upr = 20, 
                       u_star_lwr = 0),
            # Ts - sonic air temperature (C) - should be within a degree or two of air
            # temperature; need to clear outliers from 2017 later
            Ts = c(Ts_upr = 25,
                   Ts_lwr = -40),
            # u_T - w_T - flux covariance of air temperature in x,y,&z direction
            # should fill plot
            u_T = c(u_T_upr = 5, 
                    u_T_lwr = -5),
            v_T = c(v_T_upr = 2.5, 
                    v_T_lwr = -2.5),
            w_T = c(w_T_upr = 0.75, 
                    w_T_lwr = -0.75),
            # Can't be less than atmospheric CO2 ~415 ppm (same as mg*m^-3)
            rho_c = c(rho_c_upr = 2500, 
                      rho_c_lwr = 0),
            # u_c - w_c - Flux covariance of CO2 in x,y,&z direction - should fill plot;
            u_c = c(u_c_upr = 100, 
                    u_c_lwr = -100),
            v_c = c(v_c_upr = 100, 
                    v_c_lwr = -100),
            w_c = c(w_c_upr = 25, 
                    w_c_lwr = -25),
            # u_c - w_c - Flux covariance of H20 in x, y, & z direction - should fill plot
            u_q = c(u_q_upr = 15, 
                    u_q_lwr = -15),
            v_q = c(v_q_upr = 15, 
                    v_q_lwr = -15),
            w_q = c(w_q_upr = 0.5, 
                    w_q_lwr = -0.5),
            # u1 - mean x coord flow spead - remove points greater than 15 or less 
            # than -15
            u1 = c(u1_upr = 15, 
                   u1_lwr = -15),
            # v1 - mean y coord flow spead - remove points greater than 40 or less 
            # than -20; this removes some outlier data
            v1 = c(v1_upr = 40, 
                   v1_lwr = -20),
            # w1 - mean z coord flow spead - remove points greater than 5 or less 
            # than -5; this removes some clusers of outlier data
            w1 = c(w1_upr = 5, 
                   w1_lwr = -5),
            # P - remove points lower than 63 as these points seem to be outliers and
            # don't follow the seasonal trend. 
            P = c(P_upr = 70, 
                  P_lwr = 63),
            # Ta - Air temperature I will use the sonic air temp cutoffs and deal 
            # with the rest later.
            Ta = c(Ta_upr = 30,
                   Ta_lwr = -50),
            # Similar situation to Ta, going to use 0 as common-sense low boundary
            # and 20 as an upper boundary that keeps all values
            rho_v = c(rho_v_upr = 12,
                      rho_v_lwr = 0),
            # dir - This is a compass-rose so values are -90 to 270
            dir = c(dir_upr = 270,
                    dir_lwr = -90),
            # U - wind; 40 m/s ~ 90 mph; the values seem reasonalble
            U = c(U_upr = 45,
                  U_lwr = 0),
            # additional filter, no filter applied here
            agc_Avg = c(agc_Avg_upr = 9999,
                        agc_Avg_lwr = -9999),
            # Rn - net radiation (W m^-2) - lower level of -250 or so, upper should 
            # not cutoff any points
            Rn = c(Rn_upr = 1500,
                   Rn_lwr = -250),
            # par - cutoffs need updating
            par = c(par_upr = 20,
                    par_lwr = -10),
            # par_density - cutoffs need updating if we want to use them
            par_density = c(par_density_upr = 3000,
                            par_density_lwr = -100),
            # G_1_1_1 - soil heat flux 1 (W m^-2) - suggest cutoff of -150 and 300
            G_1_1_1 = c(G_1_1_1_upr = 300,
                     G_1_1_1_lwr = -150),
            # G_2_1_1 - soil heat flux 2 (W m^-2) - suggest cutoff of -150 and 300
            G_2_1_1 = c(G_2_1_1_upr = 300,
                     G_2_1_1_lwr = -150),
            # soil_temp - soil temp 10 cm depth (C) - raw data look great so we'll
            # maintain that by giving an extreme high cutoff value of 30 deg C and 
            # an extreme low cutoff value of -30
            soil_temp = c(soil_temp_upr = 30,
                          soil_temp_lwr = -30),
            # snow depth, no filter applied here, see adjustments below
            DBTCDT_Avg = c(DBTCDT_Avg_upr = 9999,
                           DBTCDT_Avg_lwr = -9999),
            # Probe at these sites has never worked, so NAN at all sites for wc10-200
            # to do this the upper limit will be set to -1 and the lower limit to 1
            # wc10 - volumetric water content at 10cm (mm/mm) - All NA
            wc10 = c(wc10_upr = -1,
                     wc10_lwr = 1),
            # wc20 - volumetric water content at 20cm (mm/mm) - All NA
            wc20 = c(wc20_upr = -1,
                     wc20_lwr = 1),
            # wc30 - volumetric water content at 30cm (mm/mm) - All NA
            wc30 = c(wc30_upr = -1,
                     wc30_lwr = 1),
            # wc50 - volumetric water content at 50cm (mm/mm) - All NA
            wc50 = c(wc50_upr = -1,
                     wc50_lwr = 1),
            # wc70 - volumetric water content at 70cm (mm/mm) - All NA
            wc70 = c(wc70_upr = -1,
                     wc70_lwr = 1),
            # wc100 - volumetric water content at 100cm (mm/mm) - All NA
            wc100 = c(wc100_upr = -1,
                      wc100_lwr = 1),
            # wc150 -  volumetric water content at 150cm (mm/mm) - All NA
            wc150 = c(wc150_upr = -1,
                      wc150_lwr = 1),
            # wc200 -  volumetric water content at 200cm (mm/mm) - All NA
            wc200 = c(wc200_upr = -1,
                      wc200_lwr = 1),
            # tc10 -  soil temperature at 10cm (C) - keep values between 30 and -30
            tc10 = c(tc10_upr = 30,
                     tc10_lwr = -30),
            # tc20 -  soil temperature at 20cm (C) - keep values between 20 and -20
            tc20 = c(tc20_upr = 20,
                     tc20_lwr = -20),
            # tc30 -  soil temperature at 30cm (C) - keep values between 30 and ~-20
            tc30 = c(tc30_upr = 30,
                     tc30_lwr = -20),
            # tc50 -  soil temperature at 50cm (C) - keep values between 30 and -30
            tc50 = c(tc50_upr = 30,
                     tc50_lwr = -30),
            # tc70 -  soil temperature at 70cm (C) - keep values between 30 and -30
            tc70 = c(tc70_upr = 30,
                     tc70_lwr = -30),
            # tc100 -  soil temperature at 100cm (C) - keep values between 30 and -24
            tc100 = c(tc100_upr = 30,
                      tc100_lwr = -24),
            # tc150 -  soil temperature at 150cm (C) - keep values between 30 and -30
            tc150 = c(tc150_upr = 30,
                      tc150_lwr = -30),
            # tc200 -  soil temperature at 200cm (C) - keep values between 35 and -30
            tc200 = c(tc200_upr = 35,
                      tc200_lwr = -30),
            #SWC logically can not exceed the 0 to 100% range.
            SWC_2_1_1 = c(SWC_2_1_1_upr = 100,
                          SWC_2_1_1_lwr = 0),
            SWC_2_2_1 = c(SWC_2_2_1_upr = 100,
                          SWC_2_2_1_lwr = 0),
            SWC_2_3_1 = c(SWC_2_3_1_upr = 100,
                          SWC_2_3_1_lwr = 0),
            SWC_3_1_1 = c(SWC_3_1_1_upr = 100,
                          SWC_3_1_1_lwr = 0),
            SWC_3_2_1 = c(SWC_3_2_1_upr = 100,
                          SWC_3_2_1_lwr = 0),
            SWC_3_3_1 = c(SWC_3_3_1_upr = 100,
                          SWC_3_3_1_lwr = 0),
            SWC_4_1_1 = c(SWC_4_1_1_upr = 100,
                          SWC_4_1_1_lwr = 0),
            SWC_4_2_1 = c(SWC_4_2_1_upr = 100,
                          SWC_4_2_1_lwr = 0),
            SWC_4_3_1 = c(SWC_4_3_1_upr = 100,
                          SWC_4_3_1_lwr = 0),
            #using same logic as above for soil_temp
            TS_2_1_1 = c(TS_2_1_1_upr = 30,
                         TS_2_1_1_lwr = -30),
            TS_2_2_1 = c(TS_2_2_1_upr = 30,
                         TS_2_2_1_lwr = -30),
            TS_2_3_1 = c(TS_2_3_1_upr = 30,
                         TS_2_3_1_lwr = -30),
            TS_3_1_1 = c(TS_3_1_1_upr = 30,
                         TS_3_1_1_lwr = -30),
            TS_3_2_1 = c(TS_3_2_1_upr = 30,
                         TS_3_2_1_lwr = -30),
            TS_3_3_1 = c(TS_3_3_1_upr = 30,
                         TS_3_3_1_lwr = -30),
            TS_4_1_1 = c(TS_4_1_1_upr = 30,
                         TS_4_1_1_lwr = -30),
            TS_4_2_1 = c(TS_4_2_1_upr = 30,
                         TS_4_2_1_lwr = -30),
            TS_4_3_1 = c(TS_4_3_1_upr = 30,
                         TS_4_3_1_lwr = -30)
)

#### Plotting cutoffs and then filtering based on them ####
# Select the appropriate cutoffs based on the tower

if (tower == "West") {
  cutoffs <- cutoffs_west
  print("Using West tower cutoffs")
} else {
  cutoffs <- cutoffs_east
  print("Using East tower cutoffs")
}

# Pad out df with all variable names in case you are processing a data subset
# where that var is missing

vars_to_add <- setdiff(names(cutoffs), names(flux_P))

if (length(vars_to_add) > 0) {
  for (var in (vars_to_add)) {
    flux_P[[var]] <- NA
    flux_P.unfilt[[var]] <- NA
  }
}

# Plot the proposed cutoffs - note for readability, these plots omit some extreme data
# when that happens, a warning is printed to the screen, and the number of missing 
# points is noted on the plot. "extreme data" that omitted is anything that's > 15 % of
# the difference between the high and low cutoffs

if (makeplots == TRUE) {
  cutoffs_dir <- paste0(plots_dir, "/cutoffs")
  ifelse(!dir.exists(file.path(cutoffs_dir)), 
         dir.create(file.path(cutoffs_dir)), FALSE)
  plot_cutoffs <- function(var, verbose = FALSE) {
    # Variable explanations
    # var - a variable that you would like to plot cutoffs for
    # verbose - Should data about the removed points be plotted to the screen?
    
    # extract cutoffs from list
    cut_upr = cutoffs[[var]][1]
    cut_lwr = cutoffs[[var]][2]
    
    # Test for cut_upr > cut_lwr; if false, no plot should be made since all data will 
    # be discarded; for example wc10-200 in the east tower has a bad sensor and all data
    # is changed to NAs
    if (cut_upr > cut_lwr) {
      writeLines(paste0("Making ", var, " plot..."))
      # Calculate plot limits:
      y_upr_lim <- (cut_upr+cut_lwr)/2 + (cut_upr-cut_lwr)*1.15 # mean plus range*1.15
      y_lwr_lim <- (cut_upr+cut_lwr)/2 - (cut_upr-cut_lwr)*1.15
    
      # Warn the user about filtered values that will be omited from the plot by the
      # y_upr_lim and y_lwr_lim
      rm_upr <- 0
      rm_lwr <- 0
      if(any(flux_P.unfilt[[var]] > y_upr_lim, na.rm = TRUE) | 
         any(flux_P.unfilt[[var]] < y_lwr_lim, na.rm = TRUE)) {
        rm_upr <- length(flux_P.unfilt[[var]][which(flux_P.unfilt[[var]] > y_upr_lim)])
        rm_lwr <- length(flux_P.unfilt[[var]][which(flux_P.unfilt[[var]] < y_lwr_lim)])
        writeLines(paste0("WARNING: ", rm_upr + rm_lwr, " values exceed the plot limits. \n"))
      }
      if (verbose) {
        # Print summary statistics of the missing points to the screen
        # summary of upper points
        if (rm_upr > 0) {
          sumry_upr <- summary(flux_P.unfilt[[var]][which(flux_P.unfilt[[var]] > y_upr_lim)])
          writeLines(paste0("Summary of ", rm_upr, 
                            " upper values greater than plot limit of ", 
                            y_upr_lim, " is: \n", 
                            "Min: ", sumry_upr[1], "\n",
                            "1st Qu.: ", sumry_upr[2], "\n",
                            "Median: ", sumry_upr[3], "\n",
                            "Mean: ", sumry_upr[4], "\n",
                            "3rd Qu.: ", sumry_upr[5], "\n",
                            "Max: ", sumry_upr[6], "\n\n"))
        }
        # summary of lower points
        if (rm_lwr > 0) {
          sumry_lwr <- summary(flux_P.unfilt[[var]][which(flux_P.unfilt[[var]] < y_lwr_lim)])
          
          writeLines(paste0("Summary of ", rm_lwr, 
                            " lower values less than plot limit of ", 
                            y_lwr_lim, " is: \n", 
                            "Min: ", sumry_lwr[1], "\n",
                            "1st Qu.: ", sumry_lwr[2], "\n",
                            "Median: ", sumry_lwr[3], "\n",
                            "Mean: ", sumry_lwr[4], "\n",
                            "3rd Qu.: ", sumry_lwr[5], "\n",
                            "Max: ", sumry_lwr[6], "\n\n"))
          
        }
      }
      
      # Filter data for plotting, and set plot limits
      
      p.plot <- flux_P.unfilt %>%
        dplyr::select(time, all_of(var)) %>%
        dplyr::filter(!!dplyr::sym(var) <= y_upr_lim &
                        !!dplyr::sym(var) >= y_lwr_lim)
      
      if (nrow(p.plot) > 0){
      xmin <- min(p.plot$time)
      ymax <- max(p.plot[,var])
      ymin <- min(p.plot[,var])
      
      # Plot the cutoffs and save as pdf
      p1 <- ggplot(data = p.plot, aes_string(x = "time", y = var)) +
        geom_hline(aes(yintercept = cut_upr), color = "red") +
        geom_hline(aes(yintercept = cut_lwr), color = "red") +
        geom_text(data = data.frame(x = xmin, y = ymax, 
                                    label = paste0(rm_upr, " points not shown")),
                  aes(x = x, y = y, label = label),
                  hjust = 0, vjust = 0) +
        geom_text(data = data.frame(x = xmin, y = ymin, 
                                    label = paste0(rm_lwr, " points not shown")),
                  aes(x = x, y = y, label = label),
                  hjust = 0, vjust = 1) +
        ylim(y_lwr_lim, y_upr_lim) +
        ylab(flux_units[var]) +
        ggtitle(paste0(tower, " ", var, " - ", flux_units[var], " - cutoffs"))
      
      # Points are plotted separately from canvas to save plotting time
      p <- p1 + 
        geom_point()
  
      ggsave(paste0(cutoffs_dir,"/", tower, "_", var, "_cutoffs", plot_filetype),
             plot = p, width = 7, height = 7)
      } else {
        writeLines(paste0("WARNING: no data for variable", var, " in date range specified. \n"))
      }
    }
  }
  
  # run the plotting function over all variables
  lapply(names(cutoffs), plot_cutoffs)
}

# Make a dataframe to hold flagged values in preparation for filtering
# Flag descriptions:
# m = missing (original value in raw data is NA or missing)
# q1 = questionable type 1: questionable because it falls below or above a cutoff (and the raw value is filtered out due to quality concerns)
# q2 = questionable type 2: questionable because displays odd behavior but cannot be filtered out by a single cutoff. These are filtered below.
# n = no flag, the raw data is of acceptable quality
flux_P_flag <- as.data.frame(matrix(rep("n", nrow(flux_P)), 
                                    nrow = nrow(flux_P), 
                                    ncol = ncol(flux_P)), stringsAsFactors = FALSE)
names(flux_P_flag) <- c("time", paste0("flag_", names(flux_P)[2:length(names(flux_P))]))

# Filter the data based on the list of cutoffs
for (var in 2:length(flux_P)) {
  varname <- names(flux_P)[var]
  #for (var in 2:length(flux_P)) {
  if (varname == 'shifted') {next} else{
  cut_upr <- cutoffs[[varname]][1]
  cut_lwr <- cutoffs[[varname]][2]
  
  writeLines(paste0("Filtering ", names(flux_P)[var]))
  # print(names(flux_P)[var])
  # print(names(cutoffs)[var-1])
  # print(names(flux_P_flag)[var])
  
  # Set the flags for missing data
  flux_P_flag[[var]][is.na(flux_P[[var]])] <- "m"
  
  # Set the flags for questionable data
  flux_P_flag[[var]][flux_P[[var]] >= cut_upr | 
                       flux_P[[var]] <= cut_lwr] <- "q1"
  
  flux_P[[var]] <- ifelse(!is.na(flux_P[[var]]) & 
                            flux_P[[var]] >= cut_upr | 
                            flux_P[[var]] <= cut_lwr, 
                      NaN, flux_P[[var]])
}
}
#### Filtering of specific regions that cannot be solved by cutoffs ####
# Define a helpful filtering function 
filtering_function <- function(data = flux_P, data_flag = flux_P_flag, 
                               variable, start_date, end_date,
                               upper_cutoff, lower_cutoff) {
  # function for filtering points that can't be solved by one general cutoff
  # if a cutoff is NA, then no cutoff will be applied, otherwise the cutoff will
  # be applied only within the starting and ending dates
  # 
  # Variable definitions:
  # data --------- the dataframe to be filtered
  # data_flag ---- the flag dataframe so flags can be added
  # variable ----- the variable to be filtered
  # start_date --- the start date for the filtering period
  # end_date ----- the end date for the filtering period
  # upper_cutoff - the upper cutoff for filtering data; if NA, no cutoff will be applied
  # lower_cutoff - the lower cutoff for filtering data; if NA, no cutoff will be applied
  require(dplyr)
  
  variable_flag <- paste0("flag_", variable)
  # testing
  # print(variable_flag)
  # print(names(data))
  # print(head(data[[variable]]))
  
  # if there is both an upper and lower cutoff
  if ( !is.na(upper_cutoff) & !is.na(lower_cutoff)) {

    # print("if 1")
    # Set the flags
    data_flag[[variable_flag]][as.Date(data[["time"]]) > as.Date(start_date) & 
                                 as.Date(data[["time"]]) < as.Date(end_date) & 
                                 (data[[variable]] > upper_cutoff |
                                    data[[variable]] < lower_cutoff)] <- "q2"
    
    # filter the data
    data[[variable]] <- ifelse(as.Date(data[["time"]]) > as.Date(start_date) & 
                                 as.Date(data[["time"]]) < as.Date(end_date) & 
                                 (data[[variable]] > upper_cutoff |
                                 data[[variable]] < lower_cutoff), 
                               NaN, data[[variable]])
  } 
  # if there is no upper cutoff
  else if ( is.na(upper_cutoff)) {
    # print("if 2")
    
    # set the flags
    data_flag[[variable_flag]][as.Date(data[["time"]]) > as.Date(start_date) & 
                                 as.Date(data[["time"]]) < as.Date(end_date) & 
                                 data[[variable]] < lower_cutoff] <- "q2"
    
    # Filter the data
    data[[variable]] <- ifelse(as.Date(data[["time"]]) > as.Date(start_date) & 
                                 as.Date(data[["time"]]) < as.Date(end_date) & 
                                 data[[variable]] < lower_cutoff, 
                               NaN, data[[variable]])
  } 
  # if there is no lower_cutoff
  else if ( is.na(lower_cutoff)) {
    # print("if 3")
    
    # Set the flags
    data_flag[[variable_flag]][as.Date(data[["time"]]) > as.Date(start_date) & 
                                 as.Date(data[["time"]]) < as.Date(end_date) & 
                                 data[[variable]] > upper_cutoff] <- "q2"
    # Filter the data
    data[[variable]] <- ifelse(as.Date(data[["time"]]) > as.Date(start_date) & 
                                 as.Date(data[["time"]]) < as.Date(end_date) & 
                                 data[[variable]] > upper_cutoff, 
                               NaN, data[[variable]])
  }
  
  
  return(list(variable = data[[variable]], 
              variable_flag = data_flag[[variable_flag]]))
   
}

# Define a helpful plotting function
plot_specific_region <- function(tmp_var = tmp$variable, var_name,
                                 start_date, end_date) {
  # function for comparing the output of filtering_function to the unfiltered
  # variable. Regions of dat thah have been filtered out, are bright blue,
  # while regions that still have data are an overlay of red/blue that is
  # darker.
  # 
  # Variable definitions:
  # tmp_var ------ the temporary variable created by the filtering function
  # var_name ----- a string specifying the name of the variable in flux_P; ex: "rho_v"
  # start_date --- the start date for the plotting period; ex: 2018-10-11
  # end_date ----- the end date for the plotting period; ex: 2019-04-01
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  data.frame(time = flux_P[["time"]], filtvariable = tmp$variable,
             variable = flux_P[[var_name]]) %>%
    pivot_longer(contains("variable"), names_to = "OriginalFiltered",
                 values_to = "value") %>%
    filter(as.Date(time) > as.Date(start_date) &
             as.Date(time) < as.Date(end_date)) %>%
    ggplot(aes(x = time, y = value, color = OriginalFiltered)) +
    geom_point(alpha = 0.5)
}

# Filter specific regions for the West tower
if (tower == "West") {
  # West:
  #### Ta ####
  # Remove points that are inplausible or clearly annomoulous readings
  # remove points < -10 degrees in May-July 2007
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "Ta",
                     start_date = "2007-05-01", end_date = "2007-07-01",
                     upper_cutoff = NA, lower_cutoff = -10)
  flux_P$Ta <- tmp$variable
  flux_P_flag$flag_Ta <- tmp$variable_flag
  # remove "L" of bad data around -40 degrees in Nov 2011
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "Ta",
                            start_date = "2011-10-01", end_date = "2011-12-01",
                            upper_cutoff = NA, lower_cutoff = -38)
  flux_P$Ta <- tmp$variable
  flux_P_flag$flag_Ta <- tmp$variable_flag
  
  # Remove the wonky stretch between Jan and Sept of 2012
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "Ta",
                            start_date = "2012-01-01", end_date = "2012-09-01",
                            upper_cutoff = NA, lower_cutoff = 20)
  flux_P$Ta <- tmp$variable
  flux_P_flag$flag_Ta <- tmp$variable_flag
  
  # Remove the 4 points below -4 degrees in June of 2013
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "Ta",
                            start_date = "2013-06-01", end_date = "2013-07-01",
                            upper_cutoff = NA, lower_cutoff = -4)
  flux_P$Ta <- tmp$variable
  flux_P_flag$flag_Ta <- tmp$variable_flag
  
  # Remove the 11 points below -5 degrees in July of 2014
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "Ta",
                            start_date = "2014-07-01", end_date = "2014-08-01",
                            upper_cutoff = NA, lower_cutoff = -5)
  flux_P$Ta <- tmp$variable
  flux_P_flag$flag_Ta <- tmp$variable_flag

  #### rho_v ####  
  # Remove the points above 10 in 2019
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "rho_v",
                            start_date = "2019-01-11", end_date = "2019-03-20",
                            upper_cutoff = 9.8, lower_cutoff = NA)
  # Check filtered region
  # plot_specific_region(var_name = "rho_v", 
  #                      start_date = "2018-10-11",
  #                      end_date = "2019-04-01")
  flux_P$rho_v <- tmp$variable
  flux_P_flag$flag_rho_v <- tmp$variable_flag
  
  #### wc10 ####
  # Remove the four points that are above the trend in Dec 2008 wc10
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc10",
                            start_date = "2008-12-01", end_date = "2009-12-14",
                            upper_cutoff = 9, lower_cutoff = NA)
  flux_P$wc10 <- tmp$variable
  flux_P_flag$flag_wc10 <- tmp$variable_flag
  
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc10",
                            start_date = "2008-12-14", end_date = "2009-12-30",
                            upper_cutoff = 8, lower_cutoff = NA)
  flux_P$wc10 <- tmp$variable
  flux_P_flag$flag_wc10 <- tmp$variable_flag
  
  # Remove 12 points greater than 21 in April - December 2009
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc10",
                            start_date = "2009-04-01", end_date = "2009-12-30",
                            upper_cutoff = 21, lower_cutoff = NA)
  flux_P$wc10 <- tmp$variable
  flux_P_flag$flag_wc10 <- tmp$variable_flag
  
  # Remove 3 points, 3 in end of May-June 2010, and 1 in July 2010 
  # that are greater than 20
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc10",
                            start_date = "2010-05-30", end_date = "2010-08-01",
                            upper_cutoff = 20, lower_cutoff = NA)
  flux_P$wc10 <- tmp$variable
  flux_P_flag$flag_wc10 <- tmp$variable_flag
  
  # Remove 3 points greater than 20 in July-Dec 2011
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc10",
                            start_date = "2011-07-01", end_date = "2011-12-31",
                            upper_cutoff = 20, lower_cutoff = NA)
  flux_P$wc10 <- tmp$variable
  flux_P_flag$flag_wc10 <- tmp$variable_flag

  #### wc20 ####
  # Remove 4 points over 11 in Dec 2008
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2008-12-01", end_date = "2008-12-31",
                            upper_cutoff = 11, lower_cutoff = NA)
  
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  # Remove 10 points above 11 in Jan-Mar 2009
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2009-01-01", end_date = "2009-03-20",
                            upper_cutoff = 11, lower_cutoff = NA)
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  # Remove 1 points above 11 in the last 5 days of Mar 2009
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2009-03-25", end_date = "2009-03-30",
                            upper_cutoff = 11, lower_cutoff = NA)
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  # Remove 10 points above 30 in Jun 30-Nov 30 2009
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2009-06-30", end_date = "2009-11-30",
                            upper_cutoff = 30, lower_cutoff = NA)
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  # Remove 10 points above 30 in Jun 30-Nov 30 2009
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2009-06-30", end_date = "2009-11-30",
                            upper_cutoff = 30, lower_cutoff = NA)
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  # Remove 4 points breater than 26 from late May-August 2010
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2010-05-29", end_date = "2010-09-01",
                            upper_cutoff = 26, lower_cutoff = NA)
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  
  # Remove 4 points greater than 26 from late May-August 2010
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2010-05-29", end_date = "2010-09-01",
                            upper_cutoff = 26, lower_cutoff = NA)
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  # Remove 5 points greater than 30 in 2011 and 2012
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2011-01-01", end_date = "2012-12-31",
                            upper_cutoff = 30, lower_cutoff = NA)
  
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  # Remove 1 point greater than 30 in Oct 2013
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc20",
                            start_date = "2013-09-30", end_date = "2013-10-31",
                            upper_cutoff = 30, lower_cutoff = NA)
  
  flux_P$wc20 <- tmp$variable
  flux_P_flag$flag_wc20 <- tmp$variable_flag
  
  #### wc30 ####
  # Remove 2 points less than 10 in Jul-Sept 2012
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc30",
                            start_date = "2012-07-01", end_date = "2012-08-31",
                            upper_cutoff = NA, lower_cutoff = 10)
  
  flux_P$wc30 <- tmp$variable
  flux_P_flag$flag_wc30 <- tmp$variable_flag
  
  # Remove 1 point less than 15 in Oct 2013
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc30",
                            start_date = "2013-09-20", end_date = "2013-10-30",
                            upper_cutoff = NA, lower_cutoff = 15)
  
  flux_P$wc30 <- tmp$variable
  flux_P_flag$flag_wc30 <- tmp$variable_flag
  
  # Remove point less than 10 in April 2015
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc30",
                            start_date = "2015-04-01", end_date = "2015-05-01",
                            upper_cutoff = NA, lower_cutoff = 10)
  
  flux_P$wc30 <- tmp$variable
  flux_P_flag$flag_wc30 <- tmp$variable_flag
  
  #### wc50 ####
  # Remove points less than 20 in Jun-Nov 2009
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc50",
                            start_date = "2009-06-01", end_date = "2009-11-01",
                            upper_cutoff = NA, lower_cutoff = 20)
  
  flux_P$wc50 <- tmp$variable
  flux_P_flag$flag_wc50 <- tmp$variable_flag
  
  # Remove points <10 in Mar-Jul 2010
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc50",
                            start_date = "2010-03-01", end_date = "2010-08-01",
                            upper_cutoff = NA, lower_cutoff = 10)
  flux_P$wc50 <- tmp$variable
  flux_P_flag$flag_wc50 <- tmp$variable_flag
  
  # Remove points <20 in Jul-Aug 2010
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc50",
                            start_date = "2010-07-01", end_date = "2010-08-30",
                            upper_cutoff = NA, lower_cutoff = 20)
  flux_P$wc50 <- tmp$variable
  flux_P_flag$flag_wc50 <- tmp$variable_flag
  
  # Remove point <10 in Oct-Nov 2011
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc50",
                            start_date = "2011-10-01", end_date = "2011-11-30",
                            upper_cutoff = NA, lower_cutoff = 10)
  flux_P$wc50 <- tmp$variable
  flux_P_flag$flag_wc50 <- tmp$variable_flag
  
  # Remove point <25 in Sept 2013
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc50",
                            start_date = "2013-09-30", end_date = "2013-10-02",
                            upper_cutoff = NA, lower_cutoff = 25)
  flux_P$wc50 <- tmp$variable
  flux_P_flag$flag_wc50 <- tmp$variable_flag
  
  # Remove point <10 in April 2014
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc50",
                            start_date = "2014-04-01", end_date = "2014-05-01",
                            upper_cutoff = NA, lower_cutoff = 10)
  flux_P$wc50 <- tmp$variable
  flux_P_flag$flag_wc50 <- tmp$variable_flag
  
  # Remove point less than 26 in August 2014
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc50",
                            start_date = "2014-08-01", end_date = "2014-09-30",
                            upper_cutoff = NA, lower_cutoff = 26)
  flux_P$wc50 <- tmp$variable
  flux_P_flag$flag_wc50 <- tmp$variable_flag
  
  # Remove point less than 8 in Jan-Apr 2015
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc50",
                            start_date = "2015-01-01", end_date = "2015-04-30",
                            upper_cutoff = NA, lower_cutoff = 8)
  flux_P$wc50 <- tmp$variable
  flux_P_flag$flag_wc50 <- tmp$variable_flag
  
  #### wc150 ####
  # Remove "fishook" between late 2008 and early 2009
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc150",
                            start_date = "2008-01-01", end_date = "2008-12-31",
                            upper_cutoff = NA, lower_cutoff = 15)
  flux_P$wc150 <- tmp$variable
  flux_P_flag$flag_wc150 <- tmp$variable_flag
  
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "wc150",
                            start_date = "2008-12-31", end_date = "2009-12-31",
                            upper_cutoff = NA, lower_cutoff = 10)
  flux_P$wc150 <- tmp$variable
  flux_P_flag$flag_wc150 <- tmp$variable_flag


  # remove all red points in tc10-200 (6 spots - start slind 366)
  #### tc70 ####
  # Flat area in August-Dec 2014
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc70",
                            start_date = "2014-08-01", end_date = "2014-12-10",
                            upper_cutoff = NA, lower_cutoff = 10)
  flux_P$tc70 <- tmp$variable
  flux_P_flag$flag_tc70 <- tmp$variable_flag
  
  # Anomolous spike in Fall 2017
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc70",
                            start_date = "2017-11-11", end_date = "2017-11-29",
                            upper_cutoff = NA, lower_cutoff = 40)
  flux_P$tc70 <- tmp$variable
  flux_P_flag$flag_tc70 <- tmp$variable_flag
  
  #### tc100 ####
  # flat region in Oct-Jan 2012
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc100",
                            start_date = "2011-10-01", end_date = "2011-12-31",
                            upper_cutoff = NA, lower_cutoff = 40)
  flux_P$tc100 <- tmp$variable
  flux_P_flag$flag_tc100 <- tmp$variable_flag
  
  #### tc150 ####
  # November spike 2017
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc150",
                            start_date = "2017-11-11", end_date = "2017-11-29",
                            upper_cutoff = NA, lower_cutoff = 40)
  flux_P$tc150 <- tmp$variable
  flux_P_flag$flag_tc150 <- tmp$variable_flag
  
  #### tc200 ####
  # November spike 2017
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc200",
                            start_date = "2017-11-11", end_date = "2017-11-29",
                            upper_cutoff = NA, lower_cutoff = 40)
  flux_P$tc200 <- tmp$variable
  flux_P_flag$flag_tc200 <- tmp$variable_flag
  
}
# Filter specific regions for the East tower
if (tower == "East") {
  # East: 
  #### LE ####
  # Remove anomoulous points in late Nov early Dec 2011
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "LE",
                            start_date = "2011-11-28", end_date = "2011-12-11",
                            upper_cutoff = NA, lower_cutoff = 3000)
  flux_P$LE <- tmp$variable
  flux_P_flag$flag_LE <- tmp$variable_flag
  
  #### H ####
  # Remove anomoulous points in late Nov early Dec 2011
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "H",
                            start_date = "2011-11-30", end_date = "2011-12-10",
                            upper_cutoff = NA, lower_cutoff = 3000)
  flux_P$H <- tmp$variable
  flux_P_flag$flag_H <- tmp$variable_flag
  
  #### Ts #### - slide 476 - red points
  # Anomolous data in April-Oct 2017
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "Ts",
                            start_date = "2017-04-01", end_date = "2017-10-10",
                            upper_cutoff = NA, lower_cutoff = -15)
  flux_P$Ts <- tmp$variable
  flux_P_flag$flag_Ts <- tmp$variable_flag
  
  # Anomolous data in Nov-Dec 2017
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "Ts",
                            start_date = "2017-11-01", end_date = "2017-12-30",
                            upper_cutoff = NA, lower_cutoff = -25)
  flux_P$Ts <- tmp$variable
  flux_P_flag$flag_Ts <- tmp$variable_flag
  
  # data.frame(time = flux_P[["time"]], filtvariable = tmp$variable,
  #            variable = flux_P[["rho_v"]]) %>%
  #   pivot_longer(contains("variable"), names_to = "OriginalFiltered",
  #                values_to = "value") %>%
  #   filter(as.Date(time) > as.Date("2014-10-01") &
  #            as.Date(time) < as.Date("2020-12-31")) %>%
  #   ggplot(aes(x = time, y = value, color = OriginalFiltered)) +
  #   geom_point(alpha = 0.5)
  
  #### w1 ####
  # "flat" section in 2008
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "w1",
                            start_date = "2007-05-01", end_date = "2008-07-30",
                            upper_cutoff = NA, lower_cutoff = 5)
  flux_P$w1 <- tmp$variable
  flux_P_flag$flag_w1 <- tmp$variable_flag
  
  #### U ####
  # Remove the bottom dot from the outlier string in late 2017; it wasn't fully
  # filtered out by cutoffs
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "U",
                            start_date = "2017-10-01", end_date = "2017-11-30",
                            upper_cutoff = 40, lower_cutoff = NA)
  
  flux_P$U <- tmp$variable
  flux_P_flag$flag_U <- tmp$variable_flag
  
  #### tc150 ####
  # remove abnormal spike at end of 2017
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc150",
                            start_date = "2017-10-01", end_date = "2019-11-30",
                            upper_cutoff = 24, lower_cutoff = NA)
  flux_P$tc150 <- tmp$variable
  flux_P_flag$flag_tc150 <- tmp$variable_flag
  
  #### tc200 ####
  # remove abnormal spike at end of 2017
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc200",
                            start_date = "2017-10-01", end_date = "2019-11-30",
                            upper_cutoff = 24, lower_cutoff = NA)
  
  flux_P$tc200 <- tmp$variable
  flux_P_flag$flag_tc200 <- tmp$variable_flag
  
  # remove abnormal spike at early 2020
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc200",
                            start_date = "2020-01-11", end_date = "2020-02-03",
                            upper_cutoff = 20, lower_cutoff = NA)
  
  flux_P$tc200 <- tmp$variable
  flux_P_flag$flag_tc200 <- tmp$variable_flag
  
  tmp <- filtering_function(data = flux_P, data_flag = flux_P_flag, variable = "tc200",
                            start_date = "2020-02-18", end_date = "2020-03-13",
                            upper_cutoff = 15, lower_cutoff = NA)
  flux_P$tc200 <- tmp$variable
  flux_P_flag$flag_tc200 <- tmp$variable_flag
  
  # Helpful plotting of filtered data
  # data.frame(time = flux_P[["time"]], filtvariable = tmp$variable,
  #            variable = flux_P[["tc200"]]) %>%
  #   pivot_longer(contains("variable"), names_to = "OriginalFiltered",
  #                values_to = "value") %>%
  #   filter(as.Date(time) > as.Date("2019-12-01") &
  #            as.Date(time) < as.Date("2020-12-31")) %>%
  #   ggplot(aes(x = time, y = value, color = OriginalFiltered)) +
  #   geom_point(alpha = 0.5)

}

################################################################################
## Calculate Derived Variables
################################################################################
# Variables used in this section: (redefined here for convenience)
# flux_P - a dataframe containing a timestamp column and all filtered variables from above
# dir - wind direction
# G_1_1_1 = soil heat flux 1 (W m^-2)
# rho_v = vapor density from HMP (g^m-3)
# R = universal gas constant [kPa m^3/(K mol)]
# Ta = HMP air temp (deg C)
# Mw = molecular weight of water
# ea = derived variable - vapor pressure (kPa)
# es = saturation vapor pressure (kPa)
# vpd = vapor pressure deficit (kPa)
# RH = relative humidity
# P = atmospheric pressure (kPa)
# RD = gas constant dry air [kPa m^3/(K g)]
# 
writeLines("Calculating derived variables...")

attributes(flux_P$time)$tzone <- "MST" # convert to MST (local)

flux_P$dir <- flux_P$dir + 90 # correct CSAT wind direction to compass direction

# flux_P$G <- rowMeans(flux_P[,c("shf1", "shf2")], na.rm = TRUE) # average the two soil heat fluxes (W/m^2) #No longer calculating a single G.

flux_P$ea <- flux_P$rho_v*R*(flux_P$Ta+273.15)/Mw # vapor pressure (kPa)

flux_P$es <- 0.61365*exp((flux_P$Ta*17.502)/(240.97+flux_P$Ta)) # saturation vapor pressure (kPa)

flux_P$vpd <- flux_P$es - flux_P$ea # vapor pressure deficit (kPa)

flux_P$RH <- flux_P$ea/flux_P$es # unitless proportion

flux_P$h2o_hmp_mean <- flux_P$ea/((flux_P$Ta + 273.15)*RV) 

flux_P$rho_d_mean <- (flux_P$P - flux_P$ea)/((flux_P$Ta + 273.15)*RD) # dry air density g m^-3

flux_P$rho_a_mean <- (flux_P$rho_d_mean + flux_P$h2o_hmp_mean)/1000

flux_P$sigma <- flux_P$h2o_hmp_mean/flux_P$rho_d_mean

flux_P$lambda <- (2501-2.38*flux_P$Ta)*1000 # latent heat of vaporization J kg^-1


################################################################################
## Tvan Data Processing - Calculations performed on data
################################################################################
# Variables used in this section: (redefined here for convenience)
# u1: mean x coord (sonic) flow speed
# v1: mean y coord (sonic) flow speed
# w1: mean z coord (sonic) flow speed
# cT: cosine theta
# sT: sine theta
# cS: cosine sigma
# sS: sine sigma
# u_q: flux covariance of H2O with x coord (sonic) flow speed
# v_q: flux covariance of H2O with y coord (sonic) flow speed
# w_q: flux covariance of H2O with z coord (sonic) flow speed
# u_T: flux covariance of air temperature with x coord (sonic) flow speed
# v_T: flux covariance of air temperature with y coord (sonic) flow speed
# w_T: flux covariance of air temperature with z coord (sonic) flow speed
# u_c: flux covariance of CO2 with x coord (sonic) flow speed
# v_c: flux covariance of CO2 with y coord (sonic) flow speed
# w_c: flux covariance of CO2 with z coord (sonic) flow speed
# spe_heat(); calculates specific heat of moist air from barometric pressure (kPa) and water vapor pressure (kPa)
# mu: mass dry air/water vapor
# LE: Latent heat flux (W m^-2)
# H: sensible heat flux (W m^-2)
# Fc: carbon dioxide flux (mg CO2 m^2 s^-1)

# 2-d coordinate rotation (note no WPL)
# Baldocchi et al, Ecology, 1988, 69:1331-1340 (pg 1335)

flux_P$ubar2 <- (flux_P$u1)^2 # m^2/s^2
flux_P$vbar2 <- (flux_P$v1)^2 # m^2/s^2
flux_P$wbar2 <- (flux_P$w1)^2 # m^2/s^2

# Cosine theta
flux_P$cT <- sqrt(flux_P$ubar2 + flux_P$vbar2)/
  sqrt(flux_P$ubar2 + flux_P$vbar2 + flux_P$wbar2) # unitless

# Sine theta
flux_P$sT <- flux_P$w1/sqrt(flux_P$ubar2 + flux_P$vbar2 + flux_P$wbar2)  # unitless

# Cosine sigma
flux_P$cS <- flux_P$u1/sqrt(flux_P$ubar2 + flux_P$vbar2)  # unitless

# Sine sigma
flux_P$sS <- flux_P$v1/sqrt(flux_P$ubar2 + flux_P$vbar2)  # unitless

# Rotated turbulence flux covariances:
flux_P$w_q_rot <- flux_P$w_q*flux_P$cT -
  flux_P$u_q*flux_P$sT*flux_P$cS -
  flux_P$v_q*flux_P$sT*flux_P$sS # (gH2O/ m^2 s)

flux_P$w_T_rot <- flux_P$w_T*flux_P$cT -
  flux_P$u_T*flux_P$sT*flux_P$cS -
  flux_P$v_T*flux_P$sT*flux_P$sS # mdegCs-1

flux_P$w_c_rot <- flux_P$w_c*flux_P$cT -
  flux_P$u_c*flux_P$sT*flux_P$cS -
  flux_P$v_c*flux_P$sT*flux_P$sS # mgCO2m-2s-1

## WPL (Webb, Pearman, and Leuning) corrected fluxes
# Accounts for effects of temperature and humidity fluctuations on [ ]s
# Uses rotated covariances (above) => rotated WPL fluxes 

# Webb terms for LE (eqn 25) using the rotated covariances
flux_P$h2o_wpl_LE <- mu*flux_P$sigma*flux_P$lambda/1000*flux_P$w_q_rot # Wm-2
flux_P$h2o_wpl_H <- (1+(mu*flux_P$sigma))*flux_P$h2o_hmp_mean/
  (flux_P$Ta+273.15)*flux_P$lambda/1000*flux_P$w_T_rot # Wm-2
flux_P$LE_rot_wpl <- flux_P$lambda/1000*flux_P$w_q_rot + flux_P$h2o_wpl_LE + flux_P$h2o_wpl_H # Wm-2

# Filter calculated LE
flux_P$LE_rot_wpl <- ifelse(!is.na(flux_P$LE_rot_wpl) &
                              flux_P$LE_rot_wpl <= -200 | flux_P$LE_rot_wpl >= 450, NA, flux_P$LE_rot_wpl)

# Webb terms for H using rotated w_T and LE_rot_wpl (corrected LE)
flux_P$H_rot_wpl <- ((flux_P$rho_a_mean*spe_heat(flux_P$ea,flux_P$P)*flux_P$w_T_rot)-
                       (flux_P$rho_a_mean*spe_heat(flux_P$ea,flux_P$P)*0.51*RD*(flux_P$Ta+273.15)*flux_P$LE_rot_wpl)/
                       (flux_P$P*flux_P$lambda))*((flux_P$Ta+273.15)/(flux_P$Ts +273.15))

# Filter calculated H
flux_P$H_rot_wpl <- ifelse(!is.na(flux_P$H_rot_wpl) &
                             flux_P$H_rot_wpl <= -200 | flux_P$H_rot_wpl >= 400, NA, flux_P$H_rot_wpl)

# Webb terms for CO2 (equation 24)
flux_P$co2_wpl_LE <- mu*flux_P$rho_c/flux_P$rho_d_mean*flux_P$w_q_rot
flux_P$co2_wpl_H  <- (1+(mu*flux_P$sigma))*flux_P$rho_c/(flux_P$Ta+273.15)*flux_P$H_rot_wpl/
  (flux_P$rho_a_mean*spe_heat(flux_P$ea,flux_P$P))
flux_P$Fc_rot_wpl = flux_P$w_c_rot + flux_P$co2_wpl_LE + flux_P$co2_wpl_H

# Filter calculated Fc
flux_P$Fc_rot_wpl <- ifelse(!is.na(flux_P$Fc_rot_wpl) &
                              flux_P$Fc_rot_wpl <= -0.5 | flux_P$Fc_rot_wpl >= 0.5, NA, flux_P$Fc_rot_wpl)

# Add NEE variable in umol m^2 s^-1 (convert from mg m^2 s^-1)
# 1000 mg/g; 44.0095 g/mol CO2; 10^-6 mol/umol
flux_P$NEE <- (flux_P$Fc_rot_wpl)*(1/1000)*(1/44.01)*(1/10^-6)

# Calculate total grams of carbon for 1/2 hour period
# multiply by the ratio of the molecular mass of C to CO2: 12.01/44.01

flux_P$g_C <- (((flux_P$Fc_rot_wpl)*(1/1000))*(60*30))*(12.01/44.01)

################################################################################
## Ameriflux Data Fixes and Cleanup
################################################################################
# This section makes several alterations to the data, including fixes to units and 
# column headers, as well as deletions of incorrect data points by time range. 
# This section of code prepares data for submission to Ameriflux.
### Column additions ####

# Add decimal day of year column
flux_P$doy <- format(flux_P$time, '%j.%H')

# Add year column
flux_P <- flux_P %>%
  dplyr::mutate(year = lubridate::year(time))

#### Data fixes - individual areas with known issued identified by J. Knowles####

# 2021 max wind speeds at 3 timestamps exceed historical values by several SDs
# checking Saddle windspeed at correponding timeframe and windspeeds 
# unremarkable, so most likely cause is instrument error at Tvan. Remove
# windspeed and winddirection for these 2 timeperiods

if (tower == "West") {
  flux_P$dir[flux_P$time >= "2021-04-24 00:00:00" &
    flux_P$time <= "2021-04-28 00:00:00" &
    flux_P$U > 45 &
    !is.na(flux_P$U)] <- NaN

  flux_P$U[flux_P$time >= "2021-04-24 00:00:00" &
    flux_P$time <= "2021-04-28 00:00:00" &
    flux_P$U > 45 &
    !is.na(flux_P$U)] <- NaN

# Inspection of Rn suggests timestampe might have been off for the first few
# days after west sensor was redeployed in March 2021
# Jknowles (email to SE 1/2021 says just NaN the first couple of days to err
# on the cautious side
  mar_2021_filter <- which(flux_P$time >= "2021-03-10 00:00:00" &
    flux_P$time <= "2021-03-15 00:00:00")
  flux_P[mar_2021_filter, setdiff(names(flux_P), c("time", "year", "doy"))] <- NaN
}

## AGC fix
# Filter periods when IRGA was dirty or obscured (agc > 56; 10% of data).
agc_filter <- which(flux_P$agc_Avg > 56.00)

# For agc > 56, change the following variables to NaN: 
# LE_rot_wpl, Fc_rot_wpl_mg, Fc_rot_wpl_umol, rho_c; rho_v.
flux_P$Fc_rot_wpl[agc_filter] <- NaN
flux_P$NEE[agc_filter] <- NaN
flux_P$LE_rot_wpl[agc_filter] <- NaN
flux_P$rho_c[agc_filter] <- NaN
flux_P$rho_v[agc_filter] <- NaN 




if (tower == 'West') {
  
  ## Changes only apply to T-Van West ##
  
  ## Wind direction fix
  # Adjust wind direction for when Sonic re-oriented from 320 deg to 360 deg on 12 Aug 2008.
  for (i in which((flux_P$time >= "2007-05-09 19:00:00") & 
                  (flux_P$time <= "2008-08-12 12:30:00"))){
    # Subtract 40 from all dir values between 05-09-2007 19:00:00 and 08-12-2008 12:00:00
    flux_P$dir[i] <- flux_P$dir[i] - 40
    # Find all values in this range that are now negative and add 360
    for (i in which(flux_P$dir < 0)) {
      flux_P$dir[i] <- flux_P$dir[i] + 360
    }
  }
  
} 

#### Data cleanup ####
## Note, this code runs pretty slowly...

# Variable modifications
if (tower == 'West'){
  
  ## T-Van West Changes ##
  
  # Change Ta, rho_v, ea, es, h2o_hmp_mean, lambda, sigma, vpd in range ---------
  # 09/13/16 10:00 – 03/19/19 10:30  to NaN
  for (i in which((flux_P$time >= "2016-09-13 10:00:00") & (flux_P$time <= "2019-03-19 10:30:00"))){
    # Change values to NaN for indexed dates
    flux_P$Ta[i] <- NaN
    flux_P$rho_v[i] <- NaN
    flux_P$ea[i] <- NaN
    flux_P$es[i] <- NaN
    flux_P$h2o_hmp_mean[i] <- NaN
    flux_P$lambda[i] <- NaN
    flux_P$sigma[i] <- NaN
    flux_P$vpd[i] <- NaN 
  }
  
  
  # Change Ta, RH, and rho_v to NaN in all the following periods: -----------
  # 11/29/11 14:30
  for (i in which(flux_P$time == "2011-11-29 14:30:00")){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #11/30/11 11:30 – 11/30/11 12:30
  for (i in which((flux_P$time >= "2011-11-30 11:30:00") & 
                  (flux_P$time <= "2011-11-30 12:30:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #11/30/11 14:00 – 12/01/11 13:00
  for (i in which((flux_P$time >= "2011-11-30 14:00:00") & 
                  (flux_P$time <= "2011-12-01 13:00:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/01/11 23:00 – 12/02/11 02:30
  for (i in which((flux_P$time >= "2011-12-01 23:00:00") & 
                  (flux_P$time <= "2011-12-02 02:30:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/02/11 06:00 – 12/02/11 07:30
  for (i in which((flux_P$time >= "2011-12-02 06:00:00") & 
                  (flux_P$time <= "2011-12-02 07:30:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/03/11 19:00 – 12/04/11 01:00
  for (i in which((flux_P$time >= "2011-12-03 19:00:00") & 
                  (flux_P$time <= "2011-12-04 01:00:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/10/11 14:30
  for (i in which(flux_P$time == "2011-12-10 14:30:00")){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/10/11 19:30 - 12/10/11 21:00
  for (i in which((flux_P$time >= "2011-12-10 19:30:00") & 
                  (flux_P$time <= "2011-12-10 21:00:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/12/11 12:30 - 12/12/11 15:00
  for (i in which((flux_P$time >= "2011-12-12 12:30:00") & 
                  (flux_P$time <= "2011-12-12 15:00:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/12/11 22:30 - 12/13/11 03:00
  for (i in which((flux_P$time >= "2011-12-12 22:30:00") & 
                  (flux_P$time <= "2011-12-13 03:00:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/13/11 11:30 - 12/13/11 15:30
  for (i in which((flux_P$time >= "2011-12-13 11:30:00") & 
                  (flux_P$time <= "2011-12-13 15:30:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/13/11 22:30 - 12/14/11 02:30
  for (i in which((flux_P$time >= "2011-12-13 22:30:00") & 
                  (flux_P$time <= "2011-12-14 02:30:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/17/11 12:00
  for (i in which(flux_P$time == "2011-12-17 12:00:00")){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/25/11 12:00 - 12/25/11 22:00
  for (i in which((flux_P$time >= "2011-12-25 12:00:00") & 
                  (flux_P$time <= "2011-12-25 22:00:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  #12/28/11 07:00 - 12/31/11 05:00
  for (i in which((flux_P$time >= "2011-12-28 07:00:00") & 
                  (flux_P$time <= "2011-12-31 05:00:00"))){
    flux_P$Ta[i] <- NaN
    flux_P$RH[i] <- NaN
    flux_P$rho_v[i] <- NaN
  }
  
  
  # Change RH values for ranges: ------
  # Subtract 0.0671 from all RH values in this range:
  # 03/31/09 04:30 – 08/04/11 08:00
  for (i in which((flux_P$time >= "2009-03-31 04:30:00") & 
                  (flux_P$time <= "2011-08-04 08:00:00"))){
    flux_P$RH[i] <- flux_P$RH[i] - 0.0671
  }
  
  # Change all RH values > 1 or < 0 to NaN.
  for (i in which(flux_P$RH > 1 | flux_P$RH < 0)){
    flux_P$RH[i] <- NaN
  }
  
  # Change rho_c/v values for value ranges: ------------
  # Change all rho_c values > 650 or < 400 to NaN.
  for (i in which(flux_P$rho_c > 650 | flux_P$rho_c < 400)){
    flux_P$rho_c[i] <- NaN
  }
  
  # Change all rho_v values to NaN for the following date & value ranges: ------
  # 08/21/12 14:30 – 09/10/12 00:00
  for (i in which((flux_P$time >= "2012-08-21 14:30:00") & 
                  (flux_P$time <= "2012-09-10 00:00:00"))){
    flux_P$rho_v[i] <- NaN
  }
  # 11/13/11 00:00 – 11/31/11 23:30: Change to NaN
  for (i in which((flux_P$time >= "2011-11-13 00:00:00") & 
                  (flux_P$time <= "2011-11-30 23:30:00"))){
    flux_P$rho_v[i] <- NaN
  }
  # 01/01/12 00:00 – 02/28/12 23:30 & rho_v is less than 0.25 
  for (i in which((flux_P$time >= "2012-01-01 00:00:00") & 
                  (flux_P$time <= "2012-02-28 23:30:00") & 
                  (flux_P$rho_v < 0.25))){
    flux_P$rho_v[i] <- NaN
  }
  # 02/29/12 00:00 – 04/18/12 23:30 & rho_v is less than 0.5 
  for (i in which((flux_P$time >= "2012-02-29 00:00:00") & 
                  (flux_P$time <= "2012-04-18 23:30:00") & 
                  (flux_P$rho_v < 0.50))){
    flux_P$rho_v[i] <- NaN
  }
  # 04/19/12 00:00 – 09/05/12 23:30 & rho_v is less than 1 
  for (i in which((flux_P$time >= "2012-04-19 00:00:00") & 
                  (flux_P$time <= "2012-09-05 23:30:00") & 
                  (flux_P$rho_v < 1))){
    flux_P$rho_v[i] <- NaN
  }
  # Change wc10-200 values for time & value ranges: ------
  # Add 1 to the value of Wc10 from 05/06/15 17:00 until end of record
  for (i in which(flux_P$time >= "2015-05-06 17:00:00")){
    flux_P$wc10[i] <- flux_P$wc10[i] + 1
  }
  
  # Add 2.5 to the value of Wc50 from 11/11/14 09:00 until end of record
  for (i in which(flux_P$time >= "2014-11-11 09:00:00")){
    flux_P$wc50[i] <- flux_P$wc50[i] + 2.5
  }
  
  # Change Wc70 to NaN for 06/06/09 00:00 – 10/21/11 13:30
  for (i in which((flux_P$time >= "2009-06-06 00:00:00") & 
                  (flux_P$time <= "2011-10-21 13:30:00"))){
    flux_P$wc70[i] <- NaN
  }
  
  # Change wc100 to NaN for 05/09/07 19:00 – 05/29/09 17:30 AND
  # for 10/17/12 19:30 – 06/09/13 01:00
  for (i in which((flux_P$time >= "2007-05-09 19:00:00") & 
                  (flux_P$time <= "2009-05-29 17:30:00") | 
                  ((flux_P$time >= "2012-10-17 19:30:00") & 
                   (flux_P$time <= "2013-06-09 01:00:00")))){
    flux_P$wc100[i] <- NaN
  }
  
  # Change wc150 to NaN for 05/09/07 19:00 – 07/30/09 15:30
  for (i in which((flux_P$time >= "2007-05-09 19:00:00") & 
                  (flux_P$time <= "2009-07-30 15:30:00"))){
    flux_P$wc150[i] <- NaN
  }
  
  # Change G to NaN for time ranges: ------
  # 08/03/12 12:00 – 08/21/12 17:00
  for (i in which((flux_P$time >= "2012-08-03 12:00:00") & 
                  (flux_P$time <= "2012-08-21 17:00:00"))){
    flux_P$G_1_1_1[i] <- NaN
    flux_P$G_2_1_1[i] <- NaN
  }
  # 05/20/17 21:30 – 05/30/17 12:30
  for (i in which((flux_P$time >= "2017-05-20 21:30:00") & 
                  (flux_P$time <= "2017-05-30 12:30:00"))){
    flux_P$G_1_1_1[i] <- NaN
    flux_P$G_2_1_1[i] <- NaN
  }
  # Change rho_a/d_mean for time ranges: ---------
  # Change rho_a_mean and rho_d_mean to NaN for 10/21/11 14:00 – 12/31/11 05:00
  for (i in which((flux_P$time >= "2011-10-21 14:00:00") & 
                  (flux_P$time <= "2011-12-31 05:00:00"))){
    flux_P$rho_a_mean[i] <- NaN
    flux_P$rho_d_mean[i] <- NaN
  }
  # Add 0.025 to rho_a_mean values and 20 to rho_d_mean values
  # for 09/13/16 10:00 – 03/19/19 10:30
  for (i in which((flux_P$time >= "2019-09-13 10:00:00") & 
                  (flux_P$time <= "2019-03-19 10:30:00"))){
    flux_P$rho_a_mean[i] <- flux_P$rho_a_mean[i] + 0.025
    flux_P$rho_d_mean[i] <- flux_P$rho_d_mean[i] + 20
  }
  
  
  # Change G_2_1_1 to NaN for all values > 100 ----------
  for (i in which(flux_P$G_2_1_1 < -100)){
    flux_P$G_2_1_1[i] <- NaN
  }
  # Change all vpd < 0 to vpd = 0 ------------
  for (i in which(flux_P$vpd < 0)){
    flux_P$vpd[i] <- 0
  }
  
  flux_P <- flux_P |> 
    dplyr::mutate(
      Ta = ifelse(time > lubridate::ymd_hms("2023-10-15 00:00:00") & 
                    time < lubridate::ymd_hms("2024-01-31 00:00:00"),
                  NaN, Ta),
      RH = ifelse(time > lubridate::ymd_hms("2023-10-15 00:00:00") & 
                    time < lubridate::ymd_hms("2024-01-31 00:00:00"),
                  NaN, RH),
      vpd = ifelse(time > lubridate::ymd_hms("2023-10-15 00:00:00") & 
                    time < lubridate::ymd_hms("2024-01-31 00:00:00"),
                  NaN, vpd)
    )
  
  # Soil Heat Flux plates were replaced on 2021/10/19 at West with different
  # sensors. The data do not appear consist with each other, and the previous
  # sensors failed, so we can not cross calibrate. We think the newer plates' data
  # are systematically different from the previous plates due to size and
  # even sensitivity or how the plates are sitting in the soil. Thus, we are
  # creating a separate data stream (by moving the data to a different column) for
  # the newer plates' data. This is the code to rename the data after the 
  # installation of new plates.
  
  flux_P <- flux_P |> 
    dplyr::mutate(
      # Unfortunately, when EXACTLY the plates were replaced is ambiguous, NAing
      # data for the whole day that the sensors were replaced to be clean about this.
      G_1_1_1 = ifelse(lubridate::date(time) == lubridate::date("2021-10-29"),
                       NA, G_1_1_1),
      G_2_1_1 = ifelse(lubridate::date(time) == lubridate::date("2021-10-29"), 
                       NA, G_2_1_1)
    ) |> 
    dplyr::mutate(
      # First pull out the new data and put it in a different column name
      G_1_1_2 = ifelse(time > lubridate::ymd_hms("2021-10-29 00:00:00"), G_1_1_1, NA),
      G_2_1_2 = ifelse(time > lubridate::ymd_hms("2021-10-29 00:00:00"), G_2_1_1, NA)
    ) |> 
    dplyr::mutate(
      # Next, remove data from the original columns after replacement
      G_1_1_1 = ifelse(time > lubridate::ymd_hms("2021-10-29 00:00:00"),
                       NA, G_1_1_1),
      G_2_1_1 = ifelse(time > lubridate::ymd_hms("2021-10-29 00:00:00"),
                       NA, G_2_1_1)
    )
  
  
} else if (tower == 'East'){
  
  ## T-Van East Changes ##
  
  for (i in which(flux_P$G_1_1_1 < -150)){
    flux_P$G_1_1_1[i] <- NaN
  }
  for (i in which(flux_P$G_2_1_1 < -100)){
    flux_P$G_2_1_1[i] <- NaN
  }
  
  # Change RH for date ranges: ----------------
  # Subtract 0.0124 from RH values for 08/29/07 09:00 – 04/16/08 13:00
  for (i in which((flux_P$time >= "2007-08-29 09:00:00") & 
                  (flux_P$time <= "2008-04-16 13:00:00"))){
    flux_P$RH[i] <- flux_P$RH[i] - 0.0124
  }
  # Subtract 0.0793 from RH values for 04/16/08 13:30 – 10/04/18 12:00
  for (i in which((flux_P$time >= "2008-04-16 13:30:00") & 
                  (flux_P$time <= "2018-10-04 12:00:00"))){
    flux_P$RH[i] <- flux_P$RH[i] - 0.0793
  }
  # Subtract 0.0394 from RH values for 10/04/18 12:30 – 12/31/20 23:30
  for (i in which((flux_P$time >= "2018-10-04 12:30:00") & 
                  (flux_P$time <= "2020-12-31 23:30:00"))){
    flux_P$RH[i] <- flux_P$RH[i] - 0.0394
  }
  # Change all RH values > 1 or < 0 to NaN
  for (i in which((flux_P$RH >1) | (flux_P$RH < 0))){
    flux_P$RH[i] <- NaN
  }
  # Change following variables to NaN for 07/01/2017 - 04/01/2019: -----
  # Ta; ea; es; h2o_hmp_mean; lambda; rho_a_mean; rho_d_mean; rho_v; sigma; vpd.
  for (i in which((flux_P$time >= "2017-07-01") & 
                  (flux_P$time <= "2019-04-01"))){
    flux_P$Ta[i] <- NaN
    flux_P$ea[i] <- NaN
    flux_P$es[i] <- NaN
    flux_P$h2o_hmp_mean[i] <- NaN
    flux_P$lambda[i] <- NaN
    flux_P$rho_a_mean[i] <- NaN
    flux_P$rho_d_mean[i] <- NaN
    flux_P$rho_v[i] <- NaN
    flux_P$sigma[i] <- NaN
    flux_P$vpd[i] <- NaN
  }
  # Change G to NaN for 05/05/2020 12:00 – 05/05/2020 12:30 ---------
  for (i in which((flux_P$time >= "2020-05-05 12:00:00") & 
                  (flux_P$time <= "2020-05-05 12:30:00"))){
    flux_P$G_1_1_1[i] <- NaN
    flux_P$G_2_1_1[i] <- NaN
  }
  # Change rho_c to NaN for all values > 650 and < 400 ----------
  for (i in which((flux_P$rho_c > 650) | 
                  (flux_P$rho_c < 400))){
    flux_P$rho_c[i] <- NaN
  }
  # Change Rn to NaN for 04/24/2017 12:30 ----------
  for (i in which(flux_P$time == "2017-04-24 12:30:00")){
    flux_P$Rn[i] <- NaN
  }
  # Change vpd to 0 for all vpd < 0 ------------
  for (i in which(flux_P$vpd < 0)){
    flux_P$vpd[i] <- 0
  }
  
  # Soil Heat Flux plates were replaced on 2022/07/21 at East with different
  # sensors. The data do not appear consist with each other, and the previous
  # sensors failed, so we can not cross calibrate. We think the newer plates' data
  # are systematically different from the previous plates due to size and
  # even sensitivity or how the plates are sitting in the soil. Thus, we are
  # creating a separate data stream (by moving the data to a different column) for
  # the newer plates' data. This is the code to rename the data after the 
  # installation of new plates.
  
  flux_P <- flux_P |> 
    dplyr::mutate(
      # Unfortunately, when EXACTLY the plates were replaced is ambiguous, NAing
      # data for the whole day that the sensors were replaced to be clean about this.
      G_1_1_1 = ifelse(lubridate::date(time) == lubridate::date("2022-07-21"),
                       NA, G_1_1_1),
      G_2_1_1 = ifelse(lubridate::date(time) == lubridate::date("2022-07-21"), 
                       NA, G_2_1_1)
    ) |> 
    dplyr::mutate(
      # First pull out the new data and put it in a different column name
      G_1_1_2 = ifelse(time > lubridate::ymd_hms("2022-07-21 00:00:00"), G_1_1_1, NA),
      G_2_1_2 = ifelse(time > lubridate::ymd_hms("2022-07-21 00:00:00"), G_2_1_1, NA)
    ) |> 
    dplyr::mutate(
      # Next, remove data from the original columns.
      G_1_1_1 = ifelse(time > lubridate::ymd_hms("2022-07-21 00:00:00"), NA, G_1_1_1),
      G_2_1_1 = ifelse(time > lubridate::ymd_hms("2022-07-21 00:00:00"), NA, G_2_1_1)
    ) |> 
    # Move the columns together
    dplyr::relocate(G_1_1_2, .after = G_1_1_1) |> 
    dplyr::relocate(G_2_1_2, .after = G_2_1_1)
print('--done cleaning everything except snow--')  
}

print('--cleaning snow--') 

# Clean snow data
if(tower == 'West'){
  flux_P <- flux_P %>%
    arrange(time) %>%
    # until 2008-08-12 03:00:00 sensor misbehaving
    mutate(
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2008-08-12 03:00:00") < time,
        DBTCDT_Avg, NaN
      ),
      # also junk from Jan 2012 to Oct 2012
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2012-01-01 00:00:00") < time &
          lubridate::ymd_hms("2012-10-15 00:00:00") > time,
        NaN, DBTCDT_Avg_clean
      ),
      # sensor position shift adjustements
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2008-08-12 03:00:00") < time &
          lubridate::ymd_hms("2009-12-11 13:30:00") > time,
        DBTCDT_Avg + 23, DBTCDT_Avg_clean
      ),
      DBTCDT_Avg_clean = dplyr::if_else(
        time > lubridate::ymd_hms("2012-10-01 00:00:00") &
          time < lubridate::ymd_hms("2013-06-10 13:00:00"),
        DBTCDT_Avg + 2, DBTCDT_Avg_clean
      ),
      DBTCDT_Avg_clean = dplyr::if_else(
        time %in% c(
          lubridate::ymd_hms("2013-10-07 12:30:00"),
          lubridate::ymd_hms("2013-10-07 13:00:00")
        ),
        NaN, DBTCDT_Avg_clean
      ),
      # several jumps in here but it's all snow free period
      DBTCDT_Avg_clean = dplyr::if_else(
        time > lubridate::ymd_hms("2013-06-10 12:30:00") &
          time < lubridate::ymd_hms("2013-10-07 13:00:00"),
        NaN, DBTCDT_Avg_clean
      ),
      #grab summer median for offset
      summer22_median = median(DBTCDT_Avg[which(time >= lubridate::date("2022-05-01") & 
                                                  time < lubridate::date("2022-10-07"))],
                               na.rm=T
      ),
      #apply offset 
      DBTCDT_Avg = dplyr::if_else(time >= lubridate::date("2021-11-01") &
                                    time <= lubridate::date("2022-12-31") &
                                    !is.na(DBTCDT_Avg),
                                  DBTCDT_Avg+abs(summer22_median),
                                  DBTCDT_Avg),
      summer22_median = NULL, #remove column
      
      #all junk between these periods, then ok with no adj needed
      DBTCDT_Avg_clean = dplyr::if_else(
        time >= lubridate::ymd_hms("2013-10-07 13:00:00") &
        time <= lubridate::ymd_hms("2021-08-26 08:00:00"),
        NaN, DBTCDT_Avg_clean
      ),
      #junk as well after august 2022 - dec 31
      DBTCDT_Avg_clean = dplyr::if_else(
        time >= lubridate::ymd_hms("2022-08-01 00:00:00") &
          time <= lubridate::ymd_hms("2022-12-31 23:59:59"),
        NaN, DBTCDT_Avg_clean
      ),
      
      
      # after adjustments, clear values outside of range
      DBTCDT_Avg_clean = ifelse(DBTCDT_Avg_clean > 75, NA, DBTCDT_Avg_clean),
      DBTCDT_Avg_clean = ifelse(-10 > DBTCDT_Avg_clean, NA, DBTCDT_Avg_clean),
      # calculate lag1 values forward and backward
      snow_lag = DBTCDT_Avg_clean - lag(DBTCDT_Avg_clean, 1),
      snow_lead = DBTCDT_Avg_clean - lead(DBTCDT_Avg_clean, 1),
      # cannot reasonably jump >5cm per half hour
      DBTCDT_Avg_clean = ifelse(snow_lead < 5 & -5 < snow_lag & !is.na(snow_lag) & !is.na(snow_lead),
                                DBTCDT_Avg_clean, NA
      ),
      # if snow lead or lag is missing but the other looks bad, also scrub
      DBTCDT_Avg_clean = ifelse(snow_lead > 5 & is.na(snow_lag),
                                NA, DBTCDT_Avg_clean
      ),
      DBTCDT_Avg_clean = ifelse(-5 > snow_lag & is.na(snow_lead),
                                NA, DBTCDT_Avg_clean
      ),
      snow_lag_2 = DBTCDT_Avg_clean - lag(DBTCDT_Avg_clean, 2),
      snow_lead_2 = DBTCDT_Avg_clean - lead(DBTCDT_Avg_clean, 2),
      DBTCDT_Avg_clean = ifelse(snow_lead_2 < 10 & -10 < snow_lag_2 & !is.na(snow_lag_2) & !is.na(snow_lead_2),
                                DBTCDT_Avg_clean, NA
      ),
      # if snow lead or lag is missing but the other looks bad, also scrub
      DBTCDT_Avg_clean = ifelse(snow_lead_2 > 10 & is.na(snow_lag_2),
                                NA, DBTCDT_Avg_clean
      ),
      DBTCDT_Avg_clean = ifelse(-10 > snow_lag_2 & is.na(snow_lead_2),
                                NA, DBTCDT_Avg_clean
      ),
      # calculate moving average over 24 timestamp period forward and reverse
      snow_depth_mean = slider::slide_dbl(DBTCDT_Avg_clean,
                                          function(x) mean(x, na.rm = TRUE),
                                          .before = 24,
                                          .after = 24
      ),
      snow_depth_sd = slider::slide_dbl(DBTCDT_Avg_clean, function(x) sd(x, na.rm = TRUE),
                                        .before = 24,
                                        .after = 24
      ),
      # remove any lingering spikes outside of the moving window mean +/-2sd
      lo = snow_depth_mean - 2.5 * snow_depth_sd,
      hi = snow_depth_mean + 2.5 * snow_depth_sd,
      DBTCDT_Avg_clean = ifelse(DBTCDT_Avg_clean < hi & !is.na(hi), DBTCDT_Avg_clean, NA),
      DBTCDT_Avg_clean = ifelse(DBTCDT_Avg_clean > lo & !is.na(lo), DBTCDT_Avg_clean, NA)
    ) %>%
    select(-DBTCDT_Avg) %>%
    rename(DBTCDT_Avg = DBTCDT_Avg_clean) %>%
    select(
      -snow_lag, -snow_lead, -snow_lag_2, -snow_lead_2, -snow_depth_mean,
      -snow_depth_sd, -lo, -hi)
} else if(tower == "East"){
  flux_P <- flux_P %>%
    arrange(time) %>%
    # early offset is bad
    mutate(
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2008-07-15 12:00:00") > time,
        DBTCDT_Avg + 22, DBTCDT_Avg
      ),
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2008-07-15 11:20:00") < time &
          time < lubridate::ymd_hms("2009-06-16 12:00:00"),
        DBTCDT_Avg + 21, DBTCDT_Avg_clean
      ),
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2009-06-16 12:30:00") < time &
          time < lubridate::ymd_hms("2010-03-16 12:00:00"),
        DBTCDT_Avg + 20, DBTCDT_Avg_clean
      ),
      # after July - Sept 2017 has spotty bad bits in summer
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2017-07-01 00:00:00") < time & time < "2017-09-28 00:00:00",
        NaN, DBTCDT_Avg_clean
      ),
      # 2018 (~14 May - 1 October)
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2018-05-14 00:00:00") < time & time < "2018-10-01 00:00:00",
        NaN, DBTCDT_Avg_clean
      ),
      # 2019 (~14 May - 1 October)
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2018-05-14 00:00:00") < time & time < "2018-10-01 00:00:00",
        NaN, DBTCDT_Avg_clean
      ),
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2019-08-01 00:00:00") < time & time < "2020-01-01 00:00:00",
        NaN, DBTCDT_Avg_clean
      ),
      # need to just ditch the high phantom data in this period otherwise algorithm can't clean, also needs about 2cm more
      DBTCDT_Avg_clean = dplyr::if_else(
        lubridate::ymd_hms("2017-07-01 00:00:00") < time & (DBTCDT_Avg_clean > 30 | -10 > DBTCDT_Avg_clean),
        NaN, DBTCDT_Avg_clean
      ),
      # last part just impossible
      DBTCDT_Avg_clean = dplyr::if_else(
        time >lubridate::ymd_hms("2018-05-01 00:00:00") & 
          time < lubridate::ymd_hms("2021-07-18 00:00:00"),
        NaN, DBTCDT_Avg_clean
      ),
      #nuke this spike
      DBTCDT_Avg_clean = dplyr::if_else(
        (time > lubridate::date("2022-07-09") & 
           time < lubridate::date("2022-07-24")) |
          ( time > lubridate::date("2022-07-10") & 
              time < lubridate::date("2022-09-20")), 
        NaN, DBTCDT_Avg_clean
      ),
      summer22_median = median(DBTCDT_Avg_clean[which(time >= lubridate::date("2022-05-01") & 
                                                  time < lubridate::date("2022-10-07"))],
                               na.rm=T
      ),
      DBTCDT_Avg = dplyr::if_else(time >= lubridate::date("2021-07-01") &
                                    time <= lubridate::date("2022-12-31"),
                                  DBTCDT_Avg_clean+abs(summer22_median),
                                  DBTCDT_Avg_clean),
      summer22_median = NULL, #remove column
      # lag is diff vs last timestamp
      DBTCDT_Avg_clean = ifelse(DBTCDT_Avg_clean > 75, NA, DBTCDT_Avg_clean),
      DBTCDT_Avg_clean = ifelse(-10 > DBTCDT_Avg_clean, NA, DBTCDT_Avg_clean),
      snow_lag = DBTCDT_Avg_clean - lag(DBTCDT_Avg_clean, 1),
      snow_lead = DBTCDT_Avg_clean - lead(DBTCDT_Avg_clean, 1),
      DBTCDT_Avg_clean = ifelse(snow_lead < 5 & -5 < snow_lag & !is.na(snow_lag) & !is.na(snow_lead),
                                DBTCDT_Avg_clean, NA
      ),
      # if snow lead or lag is missing but the other looks bad, also scrub
      DBTCDT_Avg_clean = ifelse(snow_lead > 5 & is.na(snow_lag),
                                NA, DBTCDT_Avg_clean
      ),
      DBTCDT_Avg_clean = ifelse(-5 > snow_lag & is.na(snow_lead),
                                NA, DBTCDT_Avg_clean
      ),
      snow_lag_2 = DBTCDT_Avg_clean - lag(DBTCDT_Avg_clean, 2),
      snow_lead_2 = DBTCDT_Avg_clean - lead(DBTCDT_Avg_clean, 2),
      DBTCDT_Avg_clean = ifelse(snow_lead_2 < 10 & -10 < snow_lag_2 & !is.na(snow_lag_2) & !is.na(snow_lead_2),
                                DBTCDT_Avg_clean, NA
      ),
      # if snow lead or lag is missing but the other looks bad, also scrub
      DBTCDT_Avg_clean = ifelse(snow_lead_2 > 10 & is.na(snow_lag_2),
                                NA, DBTCDT_Avg_clean
      ),
      DBTCDT_Avg_clean = ifelse(-10 > snow_lag_2 & is.na(snow_lead_2),
                                NA, DBTCDT_Avg_clean
      ),
      snow_depth_mean = slider::slide_dbl(DBTCDT_Avg_clean,
                                          function(x) mean(x, na.rm = TRUE),
                                          .before = 24,
                                          .after = 24
      ),
      snow_depth_sd = slider::slide_dbl(DBTCDT_Avg_clean, function(x) sd(x, na.rm = TRUE),
                                        .before = 24,
                                        .after = 24
      ),
      lo = snow_depth_mean - 2.5 * snow_depth_sd,
      hi = snow_depth_mean + 2.5 * snow_depth_sd,
      DBTCDT_Avg_clean = ifelse(DBTCDT_Avg_clean < hi & !is.na(hi), DBTCDT_Avg_clean, NA),
      DBTCDT_Avg_clean = ifelse(DBTCDT_Avg_clean > lo & !is.na(lo), DBTCDT_Avg_clean, NA)
    ) %>%
    select(-DBTCDT_Avg) %>%
    rename(DBTCDT_Avg = DBTCDT_Avg_clean) %>%
    select(
      -snow_lag, -snow_lead, -snow_lag_2, -snow_lead_2, -snow_depth_mean,
      -snow_depth_sd, -lo, -hi
    )
}


################################################################################
## Plot comparisons between filtered and unfiltered data
################################################################################

# Plot all years of filtered data for each variable
if (makeplots == TRUE) {
  # Combine unfiltered and filtered data:
  flux_P.plot <- flux_P %>%
    mutate(Filtered = "filtered") %>%
    bind_rows(flux_P.unfilt) %>%
    mutate(Filtered = factor(Filtered, levels = c("unfiltered", "filtered")))
  
  plot_filtering_comparison <- function(var, data, outdir, plot_filetype) {
    # Variable definitions
    # var ----------- the variable (character string format) being plotted
    # data ---------- the dataframe containing the variable in both filtered and 
    #                 unfiltered states; Also needs a column "Filtered" to specify
    #                 which state the variable is in.
    # outdir -------- the location that the plots should be saved to
    # plot_filetype - the filtype (pdf or png) that should be saved
    
    library(dplyr)
    library(ggplot2)
    
    p.plot <- data %>% 
      dplyr::filter(!is.na(!!dplyr::sym(var))) %>% 
      dplyr::select(time, all_of(var), Filtered)
    xmin <- min(data$time)
    xmax <- max(data$time)
    # only plot variables that have data
    if (nrow(p.plot) > 2) {
      p <- ggplot(p.plot, aes_string(x = "time", y = var)) +
        geom_point() + 
        facet_wrap(~ Filtered, scales = "free") +
        ylab(flux_units[var]) +
        xlim(xmin, xmax) +
        ggtitle(paste0(tower, " ", var, " - ", flux_units[var],
                       " - filtering comparison"))
      # save plot
      ggsave(paste0(outdir,"/", tower, "_", y_name, 
                    "_filtering_comparison", plot_filetype),
             width = 10, height = 5,
             plot = p)
    }
  }
  
  for (i in 2:ncol(flux_P)) {
    print(paste0("Plotting ", names(flux_P)[i]))
    y_name = names(flux_P)[i]
    if (!y_name %in% c('doy', 'year', 'shifted')) {
      plot_filtering_comparison(data = flux_P.plot,
                              outdir = allyrs_dir,
                              var = y_name,
                              plot_filetype = plot_filetype)
    }
  }
}

# Plot year-by-year comparison plots for each variable
if (makeplots == TRUE) {
  # Combine unfiltered and filtered data:
  flux_P.plot <- flux_P %>%
    mutate(Filtered = "filtered") %>%
    bind_rows(flux_P.unfilt) %>%
    mutate(Filtered = factor(Filtered, levels = c("unfiltered", "filtered")))
  
  # get sequence of years:
  year_list <- lubridate::year(seq.POSIXt(min(flux_P.plot$time), 
                                          max(flux_P.plot$time), by = "1 year"))
  
  # write a function to plot a comparison of filtered and unfiltered data for each year
  # separately
  plot_comparison_by_years <- function(year, data, outdir, var, plot_filetype) {
    # Variable definitions
    # year ---------- The year of data to plot
    # data ---------- the dataframe containing the variable in both filtered and 
    #                 unfiltered states; Also needs a column "Filtered" to specify
    #                 which state the variable is in.
    # outdir -------- the location that the plots should be saved to
    # var ----------- the variable (character string format) being plotted
    # plot_filetype - the filtype (pdf or png) that should be saved
    
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    # Subset data by year and variable
    p.plot <- data %>% 
      select(-year) %>%
      dplyr::filter(!is.na(!!dplyr::sym(var))) %>% 
      dplyr::mutate(Year = lubridate::year(time)) %>%
      filter(Year == year) %>%
      dplyr::select(time, Year, all_of(var), Filtered)
    xmin <- as.POSIXct(paste0(year, "-01-01 00:00:00"), tz = "GMT")
    xmax <- as.POSIXct(paste0(year + 1, "-01-01 00:00:00"), tz = "GMT")
    # only plot years that have data
    if ( nrow(p.plot) > 2) {
      p <- ggplot(p.plot, aes_string(x = "time", y = var)) +
        geom_point() + 
        facet_wrap(Year ~ Filtered, scales = "free") +
        ylab(flux_units[var]) +
        xlim(xmin, xmax) +
        ggtitle(paste0(tower, " ", var, " - ", flux_units[var], " - ", year))
      #p
      ggsave(paste0(outdir,"/", tower, "_", var, "_", year, plot_filetype),
             plot = p, width = 14, height = 7)
    }
  }
  
  # Plot by year for each variable
  for (i in which(!names(flux_P) %in% c('time', 'doy', 'year', 'shifted'))) {
    # Don't bother plotting any variable that is all NAs after filtering
    if (!all(is.na(flux_P[[i]]))) {
      print(paste0("Plotting ", names(flux_P)[i]))
      y_name = names(flux_P)[i]
      yr_plots_dir <- paste0(plots_dir, "/",y_name, "_yearly_plots")
      ifelse(!dir.exists(file.path(yr_plots_dir)), 
             dir.create(file.path(yr_plots_dir)), FALSE)
      writeLines(paste0("Saving yearly plots for ", y_name))
      lapply(year_list, plot_comparison_by_years, 
             data = flux_P.plot, 
             outdir = yr_plots_dir, 
             var = y_name, plot_filetype = plot_filetype)
    }
    
  }
  
}


#### Ameriflux unit conversions and corrections ####

# Multiply RH by 100 (fraction to percent)
flux_P$RH <- flux_P$RH*100

# Multiply vpd by 10 (kPa to hPa)
flux_P$vpd <- flux_P$vpd*10

# Create new variables after Ta, ea, and P have been cleaned up in the previous section.
flux_P$co2_ppm <- flux_P$rho_c/44*8.314*(flux_P$Ta+273.15)/(flux_P$P-flux_P$ea) # co2_ppm is a new variable for Ameriflux
flux_P$h2o_ppt <- flux_P$rho_v/18*8.314*(flux_P$Ta+273.15)/(flux_P$P-flux_P$ea) # h2o_ppt is a new variable for Ameriflux

# Multiply all SWC vars by 100 to convert from frac to percent as well
flux_P <- flux_P |>
  mutate(across(starts_with("SWC"), ~ . * 100))

#### Time stamp shift in 2015 -------------------------
# Undo Hannah time-stamp shift in 2015
if (tower == "West") {
  flux_P <- flux_P %>%
    mutate(time = if_else((shifted == TRUE&!is.nan(shifted)), 
                          time - lubridate::hours(7), time)) 
}

#sum(duplicated(flux_P$time))


################################################################################
# Time Stamp Fixes
################################################################################
# Fix time stamp shifts

#plot unshifted
# turning on plots for this...
makeplots = TRUE
# Create plots directories
if (makeplots == TRUE) {
  plots_dir <- paste0(output_dir_base, "plots")
  ifelse(!dir.exists(file.path(plots_dir)), 
         dir.create(file.path(plots_dir)), FALSE)
  # create directory for plots of all-years
  allyrs_dir <- paste0(plots_dir, "/all_years")
  ifelse(!dir.exists(file.path(allyrs_dir)), 
         dir.create(file.path(allyrs_dir)), FALSE)
} 

# read potrad for set of yrs as provided by Ameriflux
potrad <- read.csv('potrad/US-NR3_HH_2007_2020.csv') %>%
  # Hannah's notes say time is END of 30min averaging period
  # so we'll join that way
  mutate(time = lubridate::ymd_hm(TIMESTAMP_END)) %>%
  mutate(year = lubridate::year(time))

# add on another year as nec
if ((max(potrad$year)-1) <
    max(lubridate::year(flux_P$time))){
  newyrs = setdiff(unique(lubridate::year(flux_P$time)),
                   max(potrad$year)-1)
}

yrs2add <- list()
for (yr in newyrs) {
  #detect leap years
  if ((yr %% 4) !=0) {
    yrs2add [[yr]] = potrad %>%
      filter(year == 2019) %>%
      mutate(TIMESTAMP_END = 
               gsub('^[0-9]{4}', yr, TIMESTAMP_END)) %>%
      select(TIMESTAMP_END, SW_IN_POT)
  }
}

potrad = yrs2add %>%
  data.table::rbindlist(.) %>%
  mutate(TIMESTAMP_END= as.double(TIMESTAMP_END)) %>%
  bind_rows(potrad %>%
              select(TIMESTAMP_END, SW_IN_POT), .)

timecheck <- flux_P
timecheck$TIMESTAMP_END <- as.character(timecheck$time)
timecheck$TIMESTAMP_END <- gsub("[^[:alnum:]]", "", timecheck$TIMESTAMP_END)
timecheck$TIMESTAMP_END <- as.numeric(timecheck$TIMESTAMP_END)/100

# start time shifted a half hour forward
timecheck$TIMESTAMP_START <- as.character(timecheck$time - (60*30)) # subtract 30 minutes
timecheck$TIMESTAMP_START <- gsub("[^[:alnum:]]", "", timecheck$TIMESTAMP_START )
timecheck$TIMESTAMP_START <- as.numeric(timecheck$TIMESTAMP_START)/100

#join to data for testing
timecheck <- timecheck  %>%
  left_join(., potrad, by = 'TIMESTAMP_END')

times <- timecheck %>%
  filter(!is.na(SW_IN_POT)) %>%
  mutate(hour = lubridate::hour(time),
         minute = lubridate::minute(time),
         yday = lubridate::yday(time),
         year = lubridate::year(time),
         yday = lubridate::yday(time),
         week_pair = round((lubridate::week(time))/2))%>%
  group_by(year, week_pair) %>%
  summarise(min_date = min(lubridate::date(time)),
            max_date = max(lubridate::date(time)))

rn_deltas <- timecheck %>%
  #time end slightly different for the ameriflux submit so start off
  # with just the times that match
  filter(!is.na(SW_IN_POT)) %>%
  mutate(hour = lubridate::hour(time),
         minute = lubridate::minute(time),
         yday = lubridate::yday(time),
         year = lubridate::year(time),
         yday = lubridate::yday(time),
         week_pair = round((lubridate::week(time))/2))%>%
  group_by(year, hour, minute, week_pair) %>%
  summarize(SW_IN_POT = mean (SW_IN_POT, na.rm = TRUE),
            Rn = mean (Rn, na.rm = TRUE), .groups = 'drop')%>%
  mutate(frac_hour = ifelse(minute == 0, 0, 0.5))%>%
  mutate(hour_min = hour + frac_hour) %>%
  left_join(., times, by = c('year', 'week_pair'))

#plot to file
rad_comparison <- ggplot (rn_deltas,
                         aes (x=hour_min, y=SW_IN_POT))+
  geom_line(color = 'red')+
  geom_line(aes(y=Rn), color = 'blue')+
  facet_grid(year ~ week_pair)+
  ylab("radiation (blue = NET; red = SW_POT)")

ggsave(paste0(plots_dir,"/", tower, "_rad_comparison_unshifted", plot_filetype),
       plot = rad_comparison,
       width =24, height = 12)

if (tower == "East") {
  flux_P <- flux_P %>%
    # NA out all data in the time around the shift
    filter(time >= lubridate::ymd_hm("2017-06-27 00:00") &
             time <= lubridate::ymd_hm("2017-06-28 00:00")) %>%
    mutate_if(is.numeric, function(x) (NA)) %>%
    # prepend these to the real data and then remove duplicates
    bind_rows(., flux_P) %>%
    filter(!duplicated(time)) %>%
    # time shift everything back
    mutate(time = dplyr::if_else(
      time >= lubridate::ymd_hm("2017-06-27 00:00") &
        time <= lubridate::ymd_hm("2019-02-18 00:00"),
      time + lubridate::hours(7), time
    )) %>%
    # remove duplicates
    filter(!duplicated(time)) %>%
    # pad out time series
    full_join(., flux_P %>% select(time) %>% distinct()) %>%
    arrange(time)
  
  #fix 2nd time shift. goes to 12 hours off
  flux_P <- flux_P %>%
    filter(time>=lubridate::ymd_hm('2019-02-18 00:00')&
             time<=lubridate::ymd_hm('2019-02-20 00:00')) %>%
    mutate_if(is.numeric, function(x) (NA)) %>%
    #prepend these to the real data and then remove duplicates
    bind_rows(., flux_P) %>%
    filter(!duplicated(time)) %>%
    #time shift everything 12 hrs next until 2021-03-10 12:01:17
    mutate(time = dplyr::if_else(time>=lubridate::ymd_hm('2019-02-18 00:00')&
                                time<=lubridate::ymd_hms('2021-03-10 12:01:17'), 
                                 time + lubridate::hours(12), time)) %>%
    #remove duplicates
    filter(!duplicated(time)) %>%
    #pad out time series
    full_join(., flux_P %>%select(time)%>%distinct())%>%
    arrange(time)
   
   #last shift should be -7 just to permanently go to MST to utc
   flux_P <- flux_P %>%
    mutate(time = dplyr::if_else(time>=lubridate::ymd_hms('2021-03-10 12:01:17'), 
                                 time + lubridate::hours(7), time)) %>%
    #this was during a missing data phase so no duplicates introduced here
    #pad out time series
    full_join(., flux_P %>%select(time)%>%distinct())%>%
    arrange(time)
}

if (tower == "West") {
  # NA values for May 7-9 2015, went bad and maybe even 12 hours bad
  # there the 2nd day, then just 7 hrs (we thought)
  # but per comment from J Knowles email to sce 3/21/2021
  # it appears it's a 6 not 7 hour shift here, perhaps some confusion
  # on the reset to std vs daylight time?
  flux_P <- flux_P %>%
    filter(time>=lubridate::ymd_hm('2015-05-07 00:00')&
             time<=lubridate::ymd_hm('2015-05-10 00:00')) %>%
    mutate_if(is.numeric, function(x) (NA)) %>%
    #prepend these to the real data and then remove duplicates
    bind_rows(., flux_P) %>%
    filter(!duplicated(time)) %>%
    #time shift everything back
    mutate(time = dplyr::if_else(time>=lubridate::ymd_hm('2015-05-07 00:00')&
                                   time<=lubridate::ymd_hm('2016-09-12 00:00'), 
                                 time + lubridate::hours(6), time)) %>%
    #remove duplicates
    filter(!duplicated(time)) %>%
    #pad out time series
    full_join(., flux_P %>%select(time)%>%distinct())%>%
    arrange(time)
  
  flux_P <- flux_P %>%
    #time shift everything 7 hrs to UTC
    mutate(time = dplyr::if_else(time>=lubridate::ymd_hms('2021-03-08 00:00:00'), 
                                 time + lubridate::hours(7), time)) %>%
    #this was during a missing data phase so no duplicates introduced here
    #pad out time series
    full_join(., flux_P %>%select(time)%>%distinct())%>%
    arrange(time)
}

#replot to check
timecheck <- flux_P
timecheck$TIMESTAMP_END <- as.character(timecheck$time)
timecheck$TIMESTAMP_END <- gsub("[^[:alnum:]]", "", timecheck$TIMESTAMP_END)
timecheck$TIMESTAMP_END <- as.numeric(timecheck$TIMESTAMP_END)/100

# start time shifted a half hour forward
timecheck$TIMESTAMP_START <- as.character(timecheck$time - (60*30)) # subtract 30 minutes
timecheck$TIMESTAMP_START <- gsub("[^[:alnum:]]", "", timecheck$TIMESTAMP_START )
timecheck$TIMESTAMP_START <- as.numeric(timecheck$TIMESTAMP_START)/100

#join to data for testing
timecheck <- timecheck  %>%
  left_join(., potrad, by = "TIMESTAMP_END")

times <- timecheck %>%
  filter(!is.na(SW_IN_POT)) %>%
  mutate(hour = lubridate::hour(time),
         minute = lubridate::minute(time),
         yday = lubridate::yday(time),
         year = lubridate::year(time),
         yday = lubridate::yday(time),
         week_pair = round((lubridate::week(time))/2))%>%
  group_by(year, week_pair) %>%
  summarise(min_date = min(lubridate::date(time)),
            max_date = max(lubridate::date(time)), .groups = 'drop')

rn_deltas <- timecheck %>%
  #time end slightly different for the ameriflux submit so start off
  # with just the times that match
  filter(!is.na(SW_IN_POT)) %>%
  mutate(hour = lubridate::hour(time),
         minute = lubridate::minute(time),
         yday = lubridate::yday(time),
         year = lubridate::year(time),
         yday = lubridate::yday(time),
         week_pair = round((lubridate::week(time))/2))%>%
  group_by(year, hour, minute, week_pair) %>%
  summarize(SW_IN_POT = mean (SW_IN_POT, na.rm = TRUE),
            Rn = mean (Rn, na.rm = TRUE), .groups = 'drop')%>%
  mutate(frac_hour = ifelse(minute == 0, 0, 0.5))%>%
  mutate(hour_min = hour + frac_hour) %>%
  left_join(., times, by = c('year', 'week_pair'))

#pplot to file
rad_comparison <- ggplot (rn_deltas,
                         aes (x=hour_min, y=SW_IN_POT))+
  geom_line(color = 'red')+
  geom_line(aes(y=Rn), color = 'blue')+
  facet_grid(year ~ week_pair)+
  ylab("radiation (blue = NET; red = SW_POT)")

ggsave(paste0(plots_dir,"/", tower, "_rad_comparison_corrected", plot_filetype),
       plot = rad_comparison,
       width =24, height = 12)


################################################################################
# Plotting Cumulative NEE data
################################################################################
# Plot cumulative NEE plots before further processing
#  NOTE, previously this pointed to g_c, which was removed above with the following comment
# # Delete Column 75 (g_C) due to inaccuracy.
if (makeplots == TRUE) {
  writeLines("Saving Cumulative NEE plots.")
  # Cumulative NEE plots
  flux_P.plot <- flux_P %>%
    dplyr::mutate(year = lubridate::year(time)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(cumsum_C = cumsum(ifelse(is.na(NEE), 0, NEE)) + NEE * 0) %>%
    dplyr::ungroup()
  
  
  plotbyyear <- function(Year, flux_plot, tower) {
    # Variable Defintions
    # Year ------ the year you want to plot NEE for
    # flux_plot - the dataframe with cumulative carbon for plotting must have 
    #             column cumsum_C and time
    # tower ----- the tower you want to plot ("East" or "West")
    print(paste0("Making plot for ", Year))
    
    flux_plot.p <- dplyr::filter( flux_plot, year == Year)
    # convert units to gC/m2/30 minutes (is this done correctly)?
    #p <- ggplot(flux_plot.p, aes(time, (cumsum_C*(1/1000)*(1/44.01)*(10^-6)*(60*30)))) +
    p <- ggplot(flux_plot.p, aes(time, (cumsum_C*10^-6* #mol/umol (convert to mols)
                                        44.0095* # g/mol CO2 (convert to gC)
                                        (60*30)))) + #seconds  per half hour
                                        
      geom_point() +
      ggtitle(paste0(tower, " Tower ", Year)) +
      xlab("Time") +
      ylab("Cumulative NEE (gC m^-2)") +
      theme(axis.text.x=element_text(size=14, colour="black"),
            axis.text.y=element_text(size=14, colour="black"),
            axis.title.y=element_text(size=22, colour="black", margin = margin(0, 20, 0, 0)),
            axis.title.x=element_text(size=22, colour="black", margin = margin(20, 0, 0, 0)),
            panel.border=element_blank(), 
            axis.line=element_line(colour="black"),
            plot.title = element_text(size = 22, lineheight=.8, face="bold"))
    ggsave(paste0(plots_dir,"/", tower, "_", Year,"_g_C", plot_filetype), plot = p,
           width = 6, height =6)
    }
  
  years <- unique(flux_P.plot$year)
  lapply(years, plotbyyear, flux_plot = flux_P.plot, tower = tower)
  print(plots_dir)
  }


################################################################################
# Preparing for combining Tvan and Saddle Met data
################################################################################
# Create equidistant flux time series and fill gaps w/ NA
start_dt <- as.POSIXct(min(flux_P$time), tz = "MST", format = "%Y-%m-%d %H:%M:%S")
end_dt <- as.POSIXct(max(flux_P$time), tz = "MST", format = "%Y-%m-%d %H:%M:%S")
posix_complete <- as.data.frame(seq.POSIXt(start_dt, end_dt, by = "30 mins"))
colnames(posix_complete) <- "time"

flux_P_com <- merge(flux_P, posix_complete, all.y = TRUE)
flux_P_com <- unique(flux_P_com) 
################################################################################
# Load Met data
################################################################################

# In Feb 2023 this section was updated to incorporate the new ten minute saddle
# met data product from NWT LTER. Ten minute resolution radiation data are available
# from ~2014 on. Therefore, from 2007 on the interpolated hourly data is used,
# after which the ten minute (aggregated instead of interpolated to 30mins) is used.

writeLines("Reading in saddle met data...")
# Use incoming solar radiation data from Saddle Climate data
# just read the files WITH their header
# saddle_met_hr <- read.csv(saddle_met_data_hr_fp)[,c("date_time_start", 
                                                    # "solrad_avg", "flag_solrad_avg")]

saddle_met_tm <- read.csv(saddle_met_data_tm_fp)[,c("date.time_start", 
                                                    "solrad_avg", "flag_solrad_avg")]


################################################################################
# Merge Met and Processed Flux Data
################################################################################
# This section merges the saddle Met incoming solar radiation data with the tvan
# data.
writeLines("Filtering met radiation data and interpolating to 30-minute time steps...")



########### Preparing the 10 minute data #######################################

### grab only datums with flag == 'n' for 'no flag', reformat datetime, 
# aggregate to 30 minutes
saddle_met_tm_clean <- saddle_met_tm  %>% mutate(
  solrad_avg = ifelse(flag_solrad_avg == 'n', solrad_avg, NA),
  time = lubridate::ymd_hms(date.time_start, tz = 'MST'),
  interval = lubridate::ceiling_date(time, '30 mins')) %>%
  select(time, interval, solrad_avg) %>% 
  group_by(interval) %>%
  summarise(avg_Rg = ifelse(n() == 3, mean(solrad_avg, na.rm=TRUE), NA))

## start time check

times = saddle_met_tm_clean  %>%
  #left_join(., potrad %>%
  #            select(time, SW_IN_POT)) %>%
  #time end slightly different for the ameriflux submit so start off
  # with just the times that match
  #filter(!is.na(SW_IN_POT)) %>%
  mutate(hour = lubridate::hour(interval),
         minute = lubridate::minute(interval),
         yday = lubridate::yday(interval),
         year = lubridate::year(interval),
         yday = lubridate::yday(interval),
         week_pair = round((lubridate::week(interval))/2))%>%
  group_by(year, week_pair) %>%
  summarise(min_date = min(lubridate::date(interval)),
            max_date = max(lubridate::date(interval)))

rn_deltas = saddle_met_tm_clean %>%
  #left_join(., potrad %>%
  #            select(time, SW_IN_POT)) %>%
  #time end slightly different for the ameriflux submit so start off
  # with just the times that match
  #filter(!is.na(SW_IN_POT)) %>%
  mutate(hour = lubridate::hour(interval),
         minute = lubridate::minute(interval),
         yday = lubridate::yday(interval),
         year = lubridate::year(interval),
         yday = lubridate::yday(interval),
         week_pair = round((lubridate::week(interval))/2))%>%
  group_by(year, hour, minute, week_pair) %>%
  summarize(#SW_IN_POT = mean (SW_IN_POT, na.rm = TRUE),
            Rg = mean (avg_Rg, na.rm = TRUE))%>%
  mutate(frac_hour = ifelse(minute == 0, 0, 0.5))%>%
  mutate(hour_min = hour + frac_hour) %>%
  left_join(., times)

#pplot to file
e_rad_comparison = ggplot (rn_deltas,
                           aes (x=hour_min, y=Rg))+
  geom_line(color = 'red')+
  #geom_line(aes(y=Rn), color = 'blue')+
  facet_grid(year ~ week_pair)#+
  #ylab("radiation (blue = NET; red = SW_POT)")


## end time check

########### Join sdl ten minute met and flux_P together ########################

## Align Met times with tvan times
flux_times <- as.data.frame(flux_P_com$time)

met_times <- merge(flux_times, saddle_met_tm_clean, by.x = "flux_P_com$time", 
                   by.y = "interval", all.x = TRUE)

colnames(met_times) <- (c("time", "Rg"))

# Combine the full time series of flux_P with the met data from the saddle in 
# one dataframe
flux_P_met <- merge(flux_P_com, met_times, by = "time", all.x = TRUE)



################################################################################
# Save Tvan Filtered Output (with all derived variables)
################################################################################
# Save filtered flux file (ongoing)
# set up dates for saving data
time1 <- gsub(":", "-", gsub(" ", "_", lubridate::with_tz(min(flux_P$time), tz = "MST")))
time2 <- gsub(":", "-", gsub(" ", "_", lubridate::with_tz(max(flux_P$time), tz = "MST")))
period <- paste0(time1, "_to_", time2)

# Change all NAs to NaNs
flux_P_filt <- flux_P_met
flux_P_filt[is.na(flux_P_filt)] <- NaN

# Convert time to character so it will be written out properly
flux_P_filt$time <- as.character(flux_P_filt$time)


writeLines(paste0("Saving filtered flux data and derived tvan variables to ",
                  paste0(output_dir_base, "AmeriFlux_readyData","/", 
                         "tvan_", tower, "_", period, "_flux_P.csv")))

# Still need to add units for rho_d_mean and h2o_hmp_mean when issues are resolved
# added variables agc_Avg and DBTCDT_Avg
flux_P_vars <- c("time", "Fc",	"LE",	"H",	"u_star",	"Ts",	"u_T",	"v_T",	
                 "w_T",	"rho_c", "u_c", "v_c", "w_c", "u_q", "v_q", "w_q",
                 "u1", "v1",	"w1",	"P", "Ta", "rho_v", "dir", "U", 'agc_Avg',"Rn",	
                 "G_1_1_1", "G_1_1_2", "G_2_1_1", "G_2_1_2", "soil_temp","wc10",
                 "wc20",	"wc30",	"wc50", "wc70", "wc100", "wc150", "wc200", "tc10", 
                 "tc20", "tc30", "tc50", "tc70", "tc100", "tc150", "tc200", "ea",
                 "es", "vpd",	"RH", "h2o_hmp_mean",	"rho_d_mean", "rho_a_mean",
                 "sigma", "lambda", "ubar2", "vbar2", "wbar2", "cT", "sT", "cS", 
                 "sS", "w_q_rot", "w_T_rot", "w_c_rot", "h2o_wpl_LE",	"h2o_wpl_H",
                 "LE_rot_wpl", "H_rot_wpl", "co2_wpl_LE", "co2_wpl_H", 
                 "Fc_rot_wpl",'DBTCDT_Avg', "NEE", "g_C", "Rg", "par", "par_density")


units_flux_P_met <- data.frame("MST", "mgCO2m-2s-1", "Wm-2", "Wm-2", "ms-1", "degC",
                           "mdegCs-1", "mdegCs-1","mdegCs-1", "mgCO2m-3","mgCO2m-2s-1",
                           "mgCO2m-2s-1","mgCO2m-2s-1", "gH2Om-2s-1", "gH2Om-2s-1",
                           "gH2Om-2s-1", "ms-1", "ms-1", "ms-1", "kPa", "degC", "gm-3",
                           "deg", "m/s", 'unitless',"Wm-2", "Wm-2","Wm-2", "Wm-2","Wm-2",
                           "degC","mmmm-1","mmmm-1","mmmm-1", "mmmm-1", "mmmm-1", "mmmm-1", 
                           "mmmm-1", "mmmm-1","degC", "degC","degC","degC","degC","degC",
                           "degC","degC", "kPa", "kPa", "hPa", "%", "-", "gm-3",
                           "-", "-", "Jkg-1","m2s-2","m2s-2","m2s-2", "-", "-", "-", "-",
                           "gH2Om-2s-1", "gH2Om-2s-1", "mdegCs-1","mgCO2m-2s-1", "Wm-2",
                           "Wm-2","Wm-2","Wm-2", "Wm-2", "Wm-2","-","umolm2s-1", "gC", 
                           "Wm-2","mV",
                           "umolm-2s-1",
                           stringsAsFactors = FALSE)

names(units_flux_P_met) <- flux_P_vars
  
write.table(units_flux_P_met, file = paste0(output_dir_base, "AmeriFlux_readyData","/", 
                                        "tvan_", tower, "_", period, "_flux_P.csv"),
            row.names = FALSE, sep = ",")

#if par or par_density not present (bc West or old East, add as Na)
if (!'par' %in% names(flux_P_filt)){
  flux_P_filt$par <-NaN
  flux_P_filt$par_density <-NaN
}
write.table(flux_P_filt %>% select(all_of(flux_P_vars)), file = paste0(output_dir_base, "AmeriFlux_readyData","/", 
                                       "tvan_", tower, "_", period, "_flux_P.csv"),
            row.names = FALSE, append = TRUE, sep = ",", col.names = FALSE)


# Save flagged data
writeLines(paste0("Saving flagged flux data to ",
                  paste0(output_dir_base, "AmeriFlux_readyData","/", 
                         "tvan_", tower, "_", period, "_flagged_flux_P.csv")))

write.table(flux_P_flag, 
            file = paste0(output_dir_base, "AmeriFlux_readyData","/", 
                          "tvan_", tower, "_", period, 
                          "_flagged_flux_P.csv"),
            row.names = FALSE, sep = ",", col.names = TRUE)

################################################################################
# Save Ameriflux formatted output
################################################################################
# This output is abridged to include only those variables relevant
# to Ameriflux submission. An additional timestamp formatted column
# is also included.

if(ameri_submit){
  ## Add Ameriflux formatted time stamp column
  # End time remains the same
  flux_P_filt <- flux_P_met 
  
  # trim final file to intended start/end dates
  # can only do lubridate hour math 
  flux_P_filt <- flux_P_filt[flux_P_filt[,"time"] >= start_date + lubridate::minutes(30),]
  flux_P_filt <- flux_P_filt[flux_P_filt[,"time"] <= end_date + lubridate::minutes(30),]
  
  flux_P_filt[is.na(flux_P_filt)] <- NaN
  
  # This timestamp workflow is convoluted because there is a weird thing about 
  #the POSIXct class where midnight timestamps drop all the time component 
  # when converted to char. Just bear with me here and trust the process (MM).
  
  flux_P_filt$TIMESTAMP_END <- as.character(flux_P_filt$time)
  flux_P_filt$TIMESTAMP_END <- gsub("[^[:alnum:]]", "", flux_P_filt$TIMESTAMP_END)
  flux_P_filt$TIMESTAMP_END <- as.character(flux_P_filt$TIMESTAMP_END)
  flux_P_filt <- flux_P_filt |> 
    dplyr::mutate(TIMESTAMP_END = ifelse(nchar(TIMESTAMP_END) == 8, 
                                         paste0(TIMESTAMP_END, "000000"), TIMESTAMP_END))
  flux_P_filt$TIMESTAMP_END <- as.numeric(flux_P_filt$TIMESTAMP_END)/100
  
  # start time shifted a half hour forward
  flux_P_filt$TIMESTAMP_START <- as.character(flux_P_filt$time - (60*30)) # subtract 30 minutes
  flux_P_filt$TIMESTAMP_START <- gsub("[^[:alnum:]]", "", flux_P_filt$TIMESTAMP_START )
  flux_P_filt$TIMESTAMP_START <- as.character(flux_P_filt$TIMESTAMP_START)
  flux_P_filt <- flux_P_filt |> 
    dplyr::mutate(TIMESTAMP_START = ifelse(nchar(TIMESTAMP_START) == 8, 
                                         paste0(TIMESTAMP_START, "000000"), TIMESTAMP_START))
  flux_P_filt$TIMESTAMP_START <- as.numeric(flux_P_filt$TIMESTAMP_START)/100

if(tower == 'West'){
  # Rename variables for Ameriflux cleaning output
  flux_P_filt <- flux_P_filt %>%
  select(-H, -LE) %>% #Ameriflux wants the _rot_wpl versions of these
    rename (
            LE = LE_rot_wpl,
            H = H_rot_wpl,
            FC = NEE, # NEE in umol; note Fc_rot_wpl is in mg
            #G = G # no rename needed 
            NETRAD = Rn,
            T_SONIC = Ts,
            TA = Ta,
            #RH = RH # no rename needed
            VPD = vpd,
            PA = P,
            USTAR = u_star,
            WS = U,
            WD = dir,
            CO2 = co2_ppm,
            H2O = h2o_ppt,
            TS_1_1_1 = soil_temp,
            D_SNOW = DBTCDT_Avg,
            SW_IN = Rg,
            SWC_1_1_1 = wc10,
            SWC_1_2_1 = wc20,
            SWC_1_3_1 = wc30,
            SWC_1_4_1 = wc50,
            SWC_1_5_1 = wc70,
            SWC_1_6_1 = wc100,
            SWC_1_7_1 = wc150,
            SWC_1_8_1 = wc200,
            #SWC_2_1_1 =SWC_2_1_1
            #SWC_2_2_1 = SWC_2_2_1,
            #SWC_2_3_1 = SWC_2_3_1,
            #SWC_3_1_1 =SWC_3_1_1,
            #SWC_3_2_1 =SWC_3_2_1,
            #SWC_3_3_1 =SWC_3_3_1,
            #SWC_4_1_1 =SWC_4_1_1,
            #SWC_4_2_1 =SWC_4_2_1,
            #SWC_4_3_1 =SWC_4_3_1
            #TS_2_1_1 =TS_2_1_1
            #TS_2_2_1 = TS_2_2_1,
            #TS_2_3_1 = TS_2_3_1,
            #TS_3_1_1 =TS_3_1_1,
            #TS_3_2_1 =TS_3_2_1,
            #TS_3_3_1 =TS_3_3_1,
            #TS_4_1_1 =TS_4_1_1,
            #TS_4_2_1 =TS_4_2_1,
            #TS_4_3_1 =TS_4_3_1
            )
  
  
  flux_P_amsubmit_vars <- c("TIMESTAMP_START", "TIMESTAMP_END",	"LE", "H",
        "FC",	"G_1_1_1","G_1_1_2","G_2_1_1","G_2_1_2","NETRAD",	"T_SONIC",
        "TA",	"RH", "VPD", "PA",	"USTAR",	"WS",	"WD", "CO2", "H2O",
        "TS_1_1_1", "D_SNOW","SW_IN",	"SWC_1_1_1",	"SWC_1_2_1",	"SWC_1_3_1",
        "SWC_1_4_1",	"SWC_1_5_1",	"SWC_1_6_1",	"SWC_1_7_1","SWC_1_8_1",
        "SWC_2_1_1","SWC_2_2_1","SWC_2_3_1","SWC_3_1_1","SWC_3_2_1","SWC_3_3_1",
        "TS_2_1_1","TS_2_2_1","TS_2_3_1","TS_3_1_1","TS_3_2_1","TS_3_3_1")

  #original plan was manual renaming, this seems like an error prone plan
  # flux_P_amsubmit_vars <- c("TIMESTAMP_START", "TIMESTAMP_END",	"LE_rot_wpl", "H_rot_wpl", 
  #                           "Fc_rot_wpl_umol",	"G",	"Rn",	"Ts",	"Ta",	"RH",
  #                           "vpd", "P",	"u_star",	"U",	"dir", "co2_ppm", "h2o_ppt",
  #                           "soil_temp","DBTCDT_Avg","Rg","wc10",	"wc20",	"wc30",
  #                           "wc50",	"wc70",	"wc100","wc150","wc200", "SWC_2_1_1","SWC_2_2_1",
  #                           "SWC_2_3_1","SWC_3_1_1","SWC_3_2_1","SWC_3_3_1","TS_2_1_1",
  #                           "TS_2_2_1","TS_2_3_1","TS_3_1_1","TS_3_2_1","TS_3_3_1")
  # 
  # names_flux_P_amsubmit <- data.frame("TIMESTAMP_START", "TIMESTAMP_END",	"LE", "H", 
  #                                     "FC",	"G",	"NETRAD",	"T_SONIC","TA",	"RH",
  #                                     "VPD", "PA",	"USTAR",	"WS",	"WD", "CO2", "H2O",
  #                                     "TS", "D_SNOW","SW_IN",	"SWC_1_1_1",	"SWC_1_2_1",	"SWC_1_3_1",
  #                                     "SWC_1_4_1",	"SWC_1_5_1",	"SWC_1_6_1",	"SWC_1_7_1","SWC_1_8_1",
  #                                     "SWC_2_1_1","SWC_2_2_1","SWC_2_3_1","SWC_3_1_1","SWC_3_2_1","SWC_3_3_1",
  #                                     "TS_2_1_1","TS_2_2_1","TS_2_3_1","TS_3_1_1","TS_3_2_1","TS_3_3_1",
  #                                     stringsAsFactors = FALSE)
  
} else if(tower == 'East'){
  flux_P_filt <- flux_P_filt %>%
    select(-H, -LE) %>% #Ameriflux wants the _rot_wpl versions of these
    rename (
      LE = LE_rot_wpl,
      H = H_rot_wpl,
      FC = NEE, # NEE in umol; note Fc_rot_wpl is in mg
      #G_1_1_1 = G_1_1_1 # no rename needed 
      #G_1_1_2 = G_1_1_2 # no rename needed 
      #G_2_1_1 = G_2_1_1 # no rename needed 
      #G_2_1_2 = G_2_1_2 # no rename needed 
      NETRAD = Rn,
      T_SONIC = Ts,
      TA = Ta,
      #RH = RH # no rename needed
      VPD = vpd,
      PA = P,
      USTAR = u_star,
      WS = U,
      WD = dir,
      CO2 = co2_ppm,
      H2O = h2o_ppt,
      TS_1_1_1 = soil_temp,
      D_SNOW = DBTCDT_Avg,
      PPFD_IN = par_density,
      SW_IN = Rg,
      # no rename needed
      #SWC_2_1_1 =SWC_2_1_1
      #SWC_2_2_1 = SWC_2_2_1,
      #SWC_2_3_1 = SWC_2_3_1,
      #SWC_3_1_1 =SWC_3_1_1,
      #SWC_3_2_1 =SWC_3_2_1,
      #SWC_3_3_1 =SWC_3_3_1,
      #SWC_4_1_1 =SWC_4_1_1,
      #SWC_4_2_1 =SWC_4_2_1,
      #SWC_4_3_1 =SWC_4_3_1
      #TS_2_1_1 =TS_2_1_1
      #TS_2_2_1 = TS_2_2_1,
      #TS_2_3_1 = TS_2_3_1,
      #TS_3_1_1 =TS_3_1_1,
      #TS_3_2_1 =TS_3_2_1,
      #TS_3_3_1 =TS_3_3_1,
      #TS_4_1_1 =TS_4_1_1,
      #TS_4_2_1 =TS_4_2_1,
      #TS_4_3_1 =TS_4_3_1
    )
  
  
  flux_P_amsubmit_vars <- c("TIMESTAMP_START", "TIMESTAMP_END",	"LE", "H", 
    "FC",	"G_1_1_1", "G_1_1_2","G_2_1_1", "G_2_1_2","NETRAD",	"T_SONIC",	"TA",	"RH",
    "VPD", "PA",	"USTAR",	"WS",	"WD", "CO2", "H2O",
    "TS_1_1_1", "D_SNOW", "PPFD_IN", "SW_IN","SWC_2_1_1",
    "SWC_2_2_1",	"SWC_2_3_1","SWC_3_1_1","SWC_3_2_1","SWC_3_3_1",
    "SWC_4_1_1","SWC_4_2_1","SWC_4_3_1", "TS_2_1_1","TS_2_2_1",
    "TS_2_3_1","TS_3_1_1","TS_3_2_1","TS_3_3_1", "TS_4_1_1","TS_4_2_1","TS_4_3_1")

  
  
  
  
  # flux_P_amsubmit_vars <- c("TIMESTAMP_START", "TIMESTAMP_END",	"LE_rot_wpl", "H_rot_wpl", 
  #                           "Fc_rot_wpl_umol",	"G",	"Rn",	"Ts",	"Ta",	"RH",
  #                           "vpd", "P",	"u_star",	"U",	"dir", "co2_ppm", "h2o_ppt",
  #                           "soil_temp", "DBTCDT_Avg", "par_density", "Rg", "SWC_1_1_1",
  #                           "SWC_1_2_1",	"SWC_1_3_1","SWC_2_1_1","SWC_2_2_1","SWC_2_3_1",
  #                           "SWC_3_1_1","SWC_3_2_1","SWC_3_3_1", "TS_1_1_1",	#"TS_1_2_1",	
  #                           "TS_1_3_1", "TS_2_1_1","TS_2_2_1","TS_2_3_1","TS_3_1_1",
  #                           "TS_3_2_1","TS_3_3_1")
  # 
  # names_flux_P_amsubmit <- data.frame("TIMESTAMP_START", "TIMESTAMP_END",	"LE", "H", 
  #                                     "FC",	"G",	"NETRAD",	"T_SONIC",	"TA",	"RH",
  #                                     "VPD", "PA",	"USTAR",	"WS",	"WD", "CO2", "H2O",
  #                                     "TS", "D_SNOW", "PPFD_IN", "SW_IN", "SWC_1_1_1","SWC_1_2_1",	"SWC_1_3_1",
  #                                     "SWC_2_1_1","SWC_2_2_1","SWC_2_3_1","SWC_3_1_1","SWC_3_2_1","SWC_3_3_1",
  #                                     "TS_1_1_1",	"TS_1_2_1",	"TS_1_3_1", "TS_2_1_1","TS_2_2_1",
  #                                     "TS_2_3_1","TS_3_1_1","TS_3_2_1","TS_3_3_1",
  #                                     stringsAsFactors = FALSE) 
}

#find final times for naming
time1 <- gsub(":", "-", gsub(" ", "_", lubridate::with_tz(min(flux_P_filt$time), tz = "MST")))
time2 <- gsub(":", "-", gsub(" ", "_", lubridate::with_tz(max(flux_P_filt$time), tz = "MST")))
period <- paste0(time1, "_to_", time2)


## add one more final timestamp check
# to sanity test on the rg and rn both being right
timecheck <- flux_P_filt

#join to data for testing
timecheck <- timecheck  %>%
  left_join(., potrad, by = "TIMESTAMP_END")

times <- timecheck %>%
  filter(!(is.na(SW_IN)&is.na(NETRAD)))%>%
  mutate(hour = lubridate::hour(time),
         minute = lubridate::minute(time),
         yday = lubridate::yday(time),
         year = lubridate::year(time),
         yday = lubridate::yday(time),
         week_pair = round((lubridate::week(time))/2))%>%
  group_by(year, week_pair) %>%
  summarise(min_date = min(lubridate::date(time)),
            max_date = max(lubridate::date(time)), .groups = 'drop')

rn_deltas <- timecheck %>%
  #time end slightly different for the ameriflux submit so start off
  # with just the times that match
  filter(!(is.na(NETRAD)&is.na(SW_IN)))%>%
  mutate(hour = lubridate::hour(time),
         minute = lubridate::minute(time),
         yday = lubridate::yday(time),
         year = lubridate::year(time),
         yday = lubridate::yday(time),
         week_pair = round((lubridate::week(time))/2))%>%
  group_by(year, hour, minute, week_pair) %>%
  summarize(SW_IN_POT = mean (SW_IN_POT, na.rm = TRUE),
            NETRAD = mean (NETRAD, na.rm = TRUE),
            SW_IN = mean (SW_IN, na.rm = TRUE),.groups = 'drop')%>%
  mutate(frac_hour = ifelse(minute == 0, 0, 0.5))%>%
  mutate(hour_min = hour + frac_hour) %>%
  left_join(., times, by = c('year', 'week_pair'))

#pplot to file
rad_comparison <- ggplot (rn_deltas,
                          aes (x=hour_min, y=SW_IN_POT))+
  geom_line(color = 'red')+
  geom_line(aes(y=NETRAD), color = 'blue')+
  geom_line(aes(y=SW_IN), color = 'purple')+
  facet_grid(year ~ week_pair)+
  ylab("radiation (blue = Rn/NETRAD; red = SW_POT, purple = Rg/SW_IN)")

ggsave(paste0(plots_dir,"/", tower, "_rad_comparison_corrected_with_Rg", plot_filetype),
       plot = rad_comparison,
       width =24, height = 12)


# Save file
#write out with headers now
# write.table(names_flux_P_amsubmit, file = paste0(output_dir_base, "AmeriFlux_readyData","/", 
#                                                  "tvan_", tower, "_", period, "_flux_P_ameriflux_submit.csv"),
#             row.names = FALSE, sep = ",", col.names = FALSE)
write.table(flux_P_filt |>
              select(all_of(flux_P_amsubmit_vars)),
            file = paste0(output_dir_base, "AmeriFlux_readyData","/", 
                                           "tvan_", tower, "_", period, "_flux_P_ameriflux_submit.csv"),
            row.names = FALSE, append = FALSE, sep = ",", col.names = TRUE,
            na = '-9999')  
}
################################################################################
# Create ReddyProc-ready File - *** (Updated to include radiation in 2023) ***
################################################################################
# If the user would like to use only Tvan data this section will produce a 
# ReddyProc-ready file that does not use any meterological data from the Saddle
# to correct or gap-fill any of the tvan tower data. 

if (produce_reddyproc_file) {
  writeLines("Preparing text files for ReddyProc...")
  
  # Select variables of interest to write out
  flux_P_sub <- flux_P_met[,c("time", "NEE", "LE_rot_wpl", "H_rot_wpl", "u_star", 
                          "Ta", "vpd", "RH", "U", "P","soil_temp", "Rg")]
  #retrim to correct date times
  flux_P_sub <- flux_P_sub[flux_P_sub[,"time"] >= start_date + lubridate::minutes(30),]
  flux_P_sub <- flux_P_sub[flux_P_sub[,"time"] <= end_date + lubridate::minutes(30),]
  
  #find final times for naming
  time1 <- gsub(":", "-", gsub(" ", "_", lubridate::with_tz(min(flux_P_sub$time), tz = "MST")))
  time2 <- gsub(":", "-", gsub(" ", "_", lubridate::with_tz(max(flux_P_sub$time), tz = "MST")))
  period <- paste0(time1, "_to_", time2)

  # Shift time 30 min forward to represent end of flux period
  flux_P_sub$time <- as.POSIXct(flux_P_sub$time + (3600/2), tz = "MST", 
                                format = "%Y-%m-%d %H:%M:%S") 
  flux_P_sub$Year <- as.numeric(strftime(flux_P_sub$time, format = "%Y", tz = "MST"))
  
  flux_P_sub$DoY <- as.integer(strftime(flux_P_sub$time, format = "%j", tz = "MST"))
  
  flux_P_sub$Hour <- (as.numeric(strftime(flux_P_sub$time, format = "%H", tz = "MST"))) +
    ((as.numeric(strftime(flux_P_sub$time, format = "%M", tz = "MST")))/60)
  
  # Handle Day and Year transitions: Start Hour = 0.5 End Hour = 24
  # Flux time represents end of measurement period
  flux_P_sub$Hour <- ifelse(flux_P_sub$Hour == 0, flux_P_sub$Hour + 24, flux_P_sub$Hour)
  
  flux_P_sub$DoY <- ifelse(flux_P_sub$Hour == 24, flux_P_sub$DoY - 1, flux_P_sub$DoY)
  
  locf <- function(x) {
    v <- !is.na(x)
    c(NA, x[v])[cumsum(v) + 1]
  }
  
  flux_P_sub$Year <- ifelse(flux_P_sub$DoY == 0, flux_P_sub$Year == NA, flux_P_sub$Year)
  
  flux_P_sub$Year <- locf(flux_P_sub$Year)
  
  flux_P_sub$DoY <- ifelse(flux_P_sub$DoY == 0, flux_P_sub$DoY == NA, flux_P_sub$DoY)
  
  flux_P_sub$DoY <- locf(flux_P_sub$DoY)
  
  flux_P_sub$Hour <- formatC(flux_P_sub$Hour, digits = 3, format = "fg")
  
  flux_P_sub_posix <- flux_P_sub$time
  
  flux_P_sub[is.na(flux_P_sub)] <- -9999
  
  # Select columns and rename them according to ReddyProc rules
  # Note no radiation in this data!
  flux_P_sub <- flux_P_sub[,c("NEE", "LE_rot_wpl", "H_rot_wpl", "u_star", "Ta",
                              "vpd", "RH", "U", "P", "soil_temp", "Rg",
                              "Year", "DoY", "Hour")]
  
  colnames(flux_P_sub) <- c("NEE", "LE", "H", "Ustar", "Tair", "VPD", "rH", "U", "P",
                            "Tsoil","Rg", "Year", "DoY", "Hour")
  units.df <- data.frame("umolm-2s-1", "Wm-2", "Wm-2", "ms-1", "degC", "hPa", "%", 
                         "ms-1","kPa","degC", "Wm-2", "-", "-", "-",
                         stringsAsFactors = FALSE)
  colnames(units.df) <- c("NEE", "LE", "H", "Ustar", "Tair", "VPD", "rH", "U", "P", 
                          "Tsoil", "Rg", "Year", "DoY", "Hour")
  
  flux_P_sub_units <- rbind(units.df, flux_P_sub)
  
  # Write out tvan and met data
  writeLines(paste0("Saving ReddyProc-ready files to ",
                    paste0(output_dir_base, "Reddy_proc_readyData","/", 
                           "tvan_", tower,"_", period, "_flux_P_reddyproc.txt")))
  
  write.table(flux_P_sub_units, 
              file = paste0(output_dir_base, "Reddy_proc_readyData","/",
                            "tvan_", tower,"_", period, "_flux_P_reddyproc.txt"),
              row.names = FALSE, sep = "\t")
}

writeLines("Tvan L1 processing finished. \n Suggested next steps: Go forth with your clean data and rejoice!")
