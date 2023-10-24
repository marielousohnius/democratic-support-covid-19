
################################################################################
# Author: Marie-Lou Sohnius                                                    #
################################################################################

## ---- Setup ----
rm(list = ls())

# set working directory
setwd("C:/Users/bauma/Dropbox/regional")

## Save package names as a vector of strings
pkgs <-  c(
  "foreign",
  "dplyr",
  "magrittr",
  "tidyr",
  "cartogram",
  "sf",
  "raster",
  "spData",
  "lwgeom",
  "tmap",
  "leaflet",
  "mapview",
  "readxl",
  "reshape2",
  "rgdal",
  "rmarkdown",
  "knitr",
  "zoo",
  "stringr",
  "pillar",
  "xlsx",
  "haven"
)

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Load all packages to library and adjust options
lapply(pkgs, library, character.only = TRUE)

## Load data (test for one file) 

# Read all files from folder in list 
filenames <- list.files("C:/Users/bauma/Dropbox/regional", pattern="*.xlsx", full.names=TRUE)

# Define function to read all files
read_function <- function(filename){
  x <- read.xlsx(filename, sheetName = "Daten", skip = 8) %>% 
    dplyr::select(NA., NA..1, Arbeitsmarkt.kommunal)
  return(x)
}

# Apply to read all files
ldf <- lapply(filenames, read_function) 

df <- as.data.frame(NA)

# define function to extract variables from list into new dataframe   
transform_function <- function(list_name) {
  for (i in seq(1, 11941)) {
    df[i,1] <- as.character(list_name[[i]][[3,1]])
    df[i,2] <- substr(df[i,1], 1, 8)
    df[i,3] <- substr(df[i,1], 9, 25)
    df[i,4] <- as.character(list_name[[i]][[23,3]])
    df[i,5] <- as.character(list_name[[i]][[24,3]])
    df[i,6] <- as.character(list_name[[i]][[25,3]])
    df[i,7] <- as.character(list_name[[i]][[26,3]])
    df[i,8] <- as.character(list_name[[i]][[27,3]])
    df[i,9] <- as.character(list_name[[i]][[28,3]])
    df[i,10] <- as.character(list_name[[i]][[38,3]])
    df[i,11] <- as.character(list_name[[i]][[39,3]])
    df[i,12] <- as.character(list_name[[i]][[40,3]])
    df[i,13] <- as.character(list_name[[i]][[41,3]])
    df[i,14] <- as.character(list_name[[i]][[42,3]])
    df[i,15] <- as.character(list_name[[i]][[43,3]])
  }
  return(df)
}

# Define empty data frame to insert values
df <- as.data.frame(NA)

# Apply function to fill dataset
final_data <- transform_function(ldf)  

# Rename columns
final_data <- final_data %>% 
  rename(ags8 = V2) %>% 
  rename(name = V3) %>% 
  rename(males_working = V4) %>% 
  rename(females_working = V5) %>% 
  rename(foreigners_working = V6) %>% 
  rename(younger25_working = V7) %>% 
  rename(older55_working = V8) %>% 
  rename(commuter_working = V9) %>% 
  rename(males_nonworking = V10) %>% 
  rename(females_nonworking = V11) %>% 
  rename(foreigners_nonworking = V12) %>% 
  rename(younger25_nonworking = V13) %>% 
  rename(older55_non_working = V14) %>% 
  rename(longterm_nonworking = V15) %>% 
  dplyr::select(-"NA")
  

# Transform to numeric
cols <- names(final_data)[c(3:14)]
final_data[cols] <- lapply(final_data[cols], as.numeric)
final_data[cols] <- lapply(final_data[cols], round)

# Generate AGS5 from AGS8 (Kreise)
final_data$AGS5 <- substr(final_data$ags8, 1, 5)

# Generate Kreis sums 
final_data <- final_data %>% 
  group_by(AGS5) %>% 
  mutate(population = sum(males_working + females_working + males_nonworking + females_nonworking, na.rm = TRUE),
    males_working_kreis = sum(males_working, na.rm = TRUE),
         females_working_kreis = sum(females_working, na.rm = TRUE),
         foreigners_working_kreis = sum(foreigners_working, na.rm = TRUE),
         younger25_working_kreis = sum(younger25_working, na.rm = TRUE),
         older55_working_kreis = sum(older55_working, na.rm = TRUE),
         commuter_working_kreis = sum(commuter_working, na.rm = TRUE),
         males_nonworking_kreis = sum(males_nonworking, na.rm = TRUE),
         females_nonworking_kreis = sum(females_nonworking, na.rm = TRUE),
         foreigners_nonworking_kreis = sum(foreigners_nonworking, na.rm = TRUE),
         younger25_nonworking_kreis = sum(younger25_nonworking, na.rm = TRUE),
         older55_non_working_kreis = sum(older55_non_working, na.rm = TRUE),
         longterm_nonworking_kreis = sum(longterm_nonworking, na.rm = TRUE)) %>% 
  ungroup()

# Generate Kreis Percentages
final_data <- final_data %>% 
  group_by(AGS5) %>% 
  mutate(males_working_kreis_perc = (males_working_kreis / population),
         females_working_kreis_perc = (females_working_kreis / population),
         foreigners_working_kreis_perc = (foreigners_working_kreis / population),
         younger25_working_kreis_perc = (younger25_working_kreis / population),
         older55_working_kreis_perc = (older55_working_kreis / population),
         commuter_working_kreis_perc = (commuter_working_kreis / population),
         males_nonworking_kreis_perc = (males_nonworking_kreis / population),
         females_nonworking_kreis_perc = (females_nonworking_kreis / population),
         foreigners_nonworking_kreis_perc = (foreigners_nonworking_kreis / population),
         younger25_nonworking_kreis_perc = (younger25_nonworking_kreis / population),
         older55_non_working_kreis_perc = (older55_non_working_kreis / population),
         longterm_nonworking_kreis_perc = (longterm_nonworking_kreis / population)) %>% 
  ungroup() %>% 
  dplyr::select(males_working_kreis_perc,
                females_working_kreis_perc,
                foreigners_working_kreis_perc,
                younger25_working_kreis_perc,
                older55_working_kreis_perc,
                commuter_working_kreis_perc,
                males_nonworking_kreis_perc,
                females_nonworking_kreis_perc,
                foreigners_nonworking_kreis_perc,
                younger25_nonworking_kreis_perc,
                older55_non_working_kreis_perc,
                longterm_nonworking_kreis_perc,
                AGS5, ags8
                )


## Join with BTW 2017 turnout data
btw2017 <- read.csv("C:/Users/bauma/Dropbox/AQM fun/final_paper/data/btw2017_AGS8.csv", sep = ";",
                    colClasses=c(AGS8="character"))

# Add leading zero for AGS with 7 chars
btw2017$AGS8 <- 
  ifelse((nchar(btw2017$AGS8) == 7),
         paste0("0",btw2017$AGS8),
         btw2017$AGS8)

# Generate AGS5 Kreis from AGS8 
btw2017$AGS5 <- substr(btw2017$AGS8, 1, 5)

# generate sum of votes by AGS5
btw2017 <- btw2017 %>% 
  group_by(AGS5) %>% 
  mutate(
         sum_cdu = sum(CDU, na.rm = TRUE),
         sum_csu = sum(CSU, na.rm = TRUE),
         sum_spd = sum(SPD, na.rm = TRUE),
         sum_fdp = sum(FDP, na.rm = TRUE),
         sum_green = sum(GR?NE, na.rm = TRUE),
         sum_afd = sum(AfD, na.rm = TRUE),
         sum_left = sum(DIE.LINKE, na.rm = TRUE)
         ) %>% 
  ungroup() %>% 
  distinct(AGS8, .keep_all = TRUE) %>% 
  dplyr::select( AGS8, sum_cdu, sum_cdu, sum_csu, sum_spd, sum_fdp, sum_green, sum_afd, sum_left, AGS5)


# generate sums of voters by AGS5
btw2017 <- btw2017 %>% 
  group_by(AGS5) %>% 
  mutate(
    sum_voters = sum_cdu + sum_csu + sum_spd + sum_fdp + sum_green + sum_afd + sum_left) %>% 
  ungroup() 

# Calculate proportions by AGS5
btw2017 <- btw2017 %>% 
  group_by(AGS5) %>% 
  mutate(
    vs_cdu_kreis = (sum_cdu / sum_voters),
    vs_csu_kreis = (sum_csu / sum_voters),
    vs_spd_kreis = (sum_spd / sum_voters),
    vs_green_kreis = (sum_green / sum_voters),
    vs_fdp_kreis = (sum_fdp / sum_voters),
    vs_afd_kreis = (sum_afd / sum_voters),
    vs_left_kreis = (sum_left / sum_voters)
  ) %>% 
  ungroup() %>% 
  dplyr::select(vs_cdu_kreis, vs_csu_kreis, vs_spd_kreis, vs_green_kreis, vs_fdp_kreis, vs_afd_kreis, vs_left_kreis, AGS8) 

final_data <- final_data %>% 
  rename(AGS8 = ags8)


# Merge both data sets by AGS8
combined <- final_data %>% 
  left_join(btw2017, by = "AGS8")


# Select relevant columns and keep distinct AGS5
combined <- combined %>% 
  distinct()

# Generate bundesland from AGS8
combined$bundesland <- 
  ifelse((nchar(combined$AGS5) < 5),
         combined$bundesland <- substr(combined$AGS5, 1, 1),
         combined$bundesland <- substr(combined$AGS5, 1, 2))

# Save final dataset
save(combined, file = "C:/Users/marie/Dropbox/AQM fun/final_paper/data/regional_data_percentages.RData")


