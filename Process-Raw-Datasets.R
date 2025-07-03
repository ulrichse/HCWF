
# Script: Process-Raw-Datasets.R -----------------------------------------------
# Purpose: Process raw data files into a county-level population database 
#          for the HealthCare Workforce Demand Projection Model
# Last updated by: Sarah Ulrich
# ______________________________________________________________________________

# Load required libraries

library(tidyverse)
library(readr)
library(tidycensus)
library(stringr)
library(magrittr)
library(httr)
library(janitor)
library(readxl)

# Define file path to NCAHEC_Center for HealthCare Workforce folder in Teams

# IMPORTANT: Modify this path if running on a different machine - this is my personal path 
personal_path <- "C:/Users/ulric/University of North Carolina at Chapel Hill"

# Set the working directory to the folder containing data files
# NOTE: Use file.path() for cross-platform compatibility
setwd(file.path(personal_path, 
                "cd_general - NCAHEC_Center on the Workforce for Health"
                ))

# Confirm the working directory is set correctly
print(paste("Working directory set to:", getwd()))

# SECTION: PLACES Data ---------------------------------------------------------
# Import PLACES County-Level Public Health Data
# Source: CDC's PLACES Project
# URL: https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb/about_data
# ______________________________________________________________________________

# Define API endpoint for PLACES 2024 county-level CSV dataset
# This data includes modeled estimates for 29 chronic disease measures, 
# health outcomes, and health risk behaviors
api_url <- "https://data.cdc.gov/api/views/swc5-untb/rows.csv?accessType=DOWNLOAD"

# Read the CSV data directly from the CDC API
# read_csv() from the readr package (part of tidyverse) efficiently imports large CSV files
places_county <- read_csv(api_url)

# Preview the structure and contents of the dataset
head(places_county)

# Filter and select relevant variables
places_county <- places_county %>%
  # Keep only observations for North Carolina
  filter(StateAbbr == "NC") %>%
  # Select essential columns for reshaping and analysis
  dplyr::select(Year, DataSource, LocationName, Data_Value, MeasureId, DataValueTypeID)

# Reshape from long to wide format
# Each combination of DataSource, MeasureId, and DataValueTypeID becomes its own column
places_county_pivot <- pivot_wider(
  places_county,
  names_from = c("DataSource", "MeasureId", "DataValueTypeID"),
  values_from = Data_Value
)

# Keep only the most recent data (2022) and create a standardized county name
dataset1 <- places_county_pivot %>%
  # Keep only rows from the 2022 data release
  filter(Year == 2022) %>%
  # Convert LocationName to a character column named 'County_Name'
  mutate(county_name = as.character(LocationName))%>%
  dplyr::select(-Year, -LocationName)

rm(places_county_pivot, places_county)
gc()
  
# SECTION: RUCC DATA -----------------------------------------------------------
# Description: Read and filter county-level rural-urban status 
# Source: USDA ERS Rural-Urban Continuum Codes (2023)
# URL: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes
# ______________________________________________________________________________

# Code to restrict to just counties in NC, and filter for only RUCC variable:

url <- "https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/53251/Ruralurbancontinuumcodes2023.csv?v=95832"

dataset2 <- read_csv(url)%>%
  pivot_wider(names_from = Attribute, values_from = Value) %>%
  filter(State == "NC") %>%
  mutate(
    county_name = str_remove(County_Name, " County$"),
    RUCC_2023 = as.numeric(RUCC_2023)
  ) %>%
  select(county_name, RUCC_2023)
  
# SECTION: ACS DATA ------------------------------------------------------------
# Read and filter county-level socio-demographic variables
# Source: American Community Survey with 5-year estimates (2019-2023)
# ______________________________________________________________________________

# REPLACE WITH YOUR OWN CENSUS API KEY!

# Set Census API key for accessing the American Community Survey (ACS) data
# You can request your key at: https://api.census.gov/data/key_signup.html
# Setting install = TRUE stores the key in your .Renviron file for future sessions
census_api_key("6ad8347ab5d52e0137f9587e7aef3abb9f9cf8f4", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")  # Reload environment variables to make the key immediately available

# ACS variable names: 

acs_var_names <- load_variables(year = 2023, dataset = "acs5", cache = TRUE)

# Retrieve ACS 5-year estimates for North Carolina counties (2023)
# 'output = "wide"' returns estimates and margins of error (MOEs) in separate columns

# ACS Subsection: Age and race categories -------------------------------------- 
# Male and female age categories, with white, Hispanic, and 
# 'Other' (non-white, non-Hispanic) to match the groups in the NC population 
# projection file.

# Total population male and female age categories: 

age <- get_acs(
  geography = "county",
  state = "NC",
  table = "B01001",
  year = 2023,
  survey = "acs5",
  output = "wide"
)%>%
  mutate(totalpop = B01001_001E, 
         totalpop_m = B01001_002E, 
         totalpop_f = B01001_026E,
    total_m_under5 = B01001_003E,
    total_m_5to9 = B01001_004E,
    total_m_10to14 = B01001_005E, 
    total_m_15to17 = B01001_006E,
    total_m_18to19 = B01001_007E, 
    total_m_20to24 = B01001_008E + B01001_009E + B01001_010E, 
    total_m_25to29 = B01001_011E, 
    total_m_30to34 = B01001_012E, 
    total_m_35to44 = B01001_013E + B01001_014E, 
    total_m_45to54 = B01001_015E + B01001_016E, 
    total_m_55to64 = B01001_017E + B01001_018E + B01001_019E, 
    total_m_65to74 = B01001_020E + B01001_021E + B01001_022E, 
    total_m_75to84 = B01001_023E + B01001_024E,
    total_m_85above = B01001_025E,
    total_f_under5 = B01001_027E,
    total_f_5to9 = B01001_028E,
    total_f_10to14 = B01001_029E, 
    total_f_15to17 = B01001_030E,
    total_f_18to19 = B01001_031E, 
    total_f_20to24 = B01001_032E + B01001_033E + B01001_034E, 
    total_f_25to29 = B01001_035E, 
    total_f_30to34 = B01001_036E, 
    total_f_35to44 = B01001_037E + B01001_038E, 
    total_f_45to54 = B01001_039E + B01001_040E, 
    total_f_55to64 = B01001_041E + B01001_042E + B01001_043E, 
    total_f_65to74 = B01001_044E + B01001_045E + B01001_046E, 
    total_f_75to84 = B01001_047E + B01001_048E,
    total_f_85above = B01001_049E
)%>%
  select(GEOID, starts_with("total"))

# Total white population male and female age categories: 

white <- get_acs(
  geography = "county",
  state = "NC",
  table = "B01001H",
  year = 2023,
  survey = "acs5",
  output = "wide"
)%>%
  mutate(
    white_m_under5 = B01001H_003E,
    white_m_5to9 = B01001H_004E,
    white_m_10to14 = B01001H_005E, 
    white_m_15to17 = B01001H_006E,
    white_m_18to19 = B01001H_007E, 
    white_m_20to24 = B01001H_008E,  
    white_m_25to29 = B01001H_009E, 
    white_m_30to34 = B01001H_010E, 
    white_m_35to44 = B01001H_011E, 
    white_m_45to54 = B01001H_012E,
    white_m_55to64 = B01001H_013E,
    white_m_65to74 = B01001H_014E,
    white_m_75to84 = B01001H_015E,
    white_m_85above = B01001H_016E,
    white_f_under5 = B01001H_018E,
    white_f_5to9 = B01001H_019E,
    white_f_10to14 = B01001H_020E, 
    white_f_15to17 = B01001H_021E,
    white_f_18to19 = B01001H_022E, 
    white_f_20to24 = B01001H_023E, 
    white_f_25to29 = B01001H_024E, 
    white_f_30to34 = B01001H_025E, 
    white_f_35to44 = B01001H_026E, 
    white_f_45to54 = B01001H_027E, 
    white_f_55to64 = B01001H_028E, 
    white_f_65to74 = B01001H_029E, 
    white_f_75to84 = B01001H_030E,
    white_f_85above = B01001H_031E
  )%>%
  select(GEOID, starts_with("white"))

# Hispanic population male and female age categories: 

hispanic <- get_acs(
  geography = "county",
  state = "NC",
  table = "B01001I",
  year = 2023,
  survey = "acs5",
  output = "wide"
)%>%
  mutate(
    hispanic_m_under5 = B01001I_003E,
    hispanic_m_5to9 = B01001I_004E,
    hispanic_m_10to14 = B01001I_005E, 
    hispanic_m_15to17 = B01001I_006E,
    hispanic_m_18to19 = B01001I_007E, 
    hispanic_m_20to24 = B01001I_008E,  
    hispanic_m_25to29 = B01001I_009E, 
    hispanic_m_30to34 = B01001I_010E, 
    hispanic_m_35to44 = B01001I_011E, 
    hispanic_m_45to54 = B01001I_012E,
    hispanic_m_55to64 = B01001I_013E,
    hispanic_m_65to74 = B01001I_014E,
    hispanic_m_75to84 = B01001I_015E,
    hispanic_m_85above = B01001I_016E,
    hispanic_f_under5 = B01001I_018E,
    hispanic_f_5to9 = B01001I_019E,
    hispanic_f_10to14 = B01001I_020E, 
    hispanic_f_15to17 = B01001I_021E,
    hispanic_f_18to19 = B01001I_022E, 
    hispanic_f_20to24 = B01001I_023E, 
    hispanic_f_25to29 = B01001I_024E, 
    hispanic_f_30to34 = B01001I_025E, 
    hispanic_f_35to44 = B01001I_026E, 
    hispanic_f_45to54 = B01001I_027E, 
    hispanic_f_55to64 = B01001I_028E, 
    hispanic_f_65to74 = B01001I_029E, 
    hispanic_f_75to84 = B01001I_030E,
    hispanic_f_85above = B01001I_031E
  )%>%
  select(GEOID, starts_with("hispanic"))

# Merge age datasets: 

nc_acs_data <- age %>%
  left_join(white, by = c('GEOID')) %>%
  left_join(hispanic, by = c('GEOID'))

nc_acs_data <- nc_acs_data %>%
  mutate(other_m_under5 = total_m_under5 - white_m_under5 - hispanic_m_under5,
         other_m_5to9 = total_m_5to9 - white_m_5to9 - hispanic_m_5to9,
         other_m_10to14 = total_m_10to14 - white_m_10to14 - hispanic_m_10to14,
         other_m_15to17 = total_m_15to17 - white_m_15to17 - hispanic_m_15to17,
         other_m_18to19 = total_m_18to19 - white_m_18to19 - hispanic_m_18to19,
         other_m_20to24 = total_m_20to24 - white_m_20to24 - hispanic_m_20to24,
         other_m_25to29 = total_m_25to29 - white_m_25to29 - hispanic_m_25to29,
         other_m_30to34 = total_m_30to34 - white_m_30to34 - hispanic_m_30to34,
         other_m_35to44 = total_m_35to44 - white_m_35to44 - hispanic_m_35to44,
         other_m_45to54 = total_m_45to54 - white_m_45to54 - hispanic_m_45to54,
         other_m_55to64 = total_m_55to64 - white_m_55to64 - hispanic_m_55to64,
         other_m_65to74 = total_m_65to74 - white_m_65to74 - hispanic_m_65to74,
         other_m_75to84 = total_m_75to84 - white_m_75to84 - hispanic_m_75to84,
         other_m_85above = total_m_85above - white_m_85above - hispanic_m_85above,
         other_f_under5 = total_f_under5 - white_f_under5 - hispanic_f_under5,
         other_f_5to9 = total_f_5to9 - white_f_5to9 - hispanic_f_5to9,
         other_f_10to14 = total_f_10to14 - white_f_10to14 - hispanic_f_10to14,
         other_f_15to17 = total_f_15to17 - white_f_15to17 - hispanic_f_15to17,
         other_f_18to19 = total_f_18to19 - white_f_18to19 - hispanic_f_18to19,
         other_f_20to24 = total_f_20to24 - white_f_20to24 - hispanic_f_20to24,
         other_f_25to29 = total_f_25to29 - white_f_25to29 - hispanic_f_25to29,
         other_f_30to34 = total_f_30to34 - white_f_30to34 - hispanic_f_30to34,
         other_f_35to44 = total_f_35to44 - white_f_35to44 - hispanic_f_35to44,
         other_f_45to54 = total_f_45to54 - white_f_45to54 - hispanic_f_45to54,
         other_f_55to64 = total_f_55to64 - white_f_55to64 - hispanic_f_55to64,
         other_f_65to74 = total_f_65to74 - white_f_65to74 - hispanic_f_65to74,
         other_f_75to84 = total_f_75to84 - white_f_75to84 - hispanic_f_75to84,
         other_f_85above = total_f_85above - white_f_85above - hispanic_f_85above
)

rm(age)
rm(hispanic)
rm(totalpop)
rm(white)

# ACS Subsection: Household income categories -----------------------

income <- get_acs(
  geography = "county",
  state = "NC",
  table = "B19001",
  year = 2023,
  survey = "acs5",
  output = "wide"
)%>%
  mutate(total_hh = B19001_001E,
         hh_income_under10k = B19001_002E, 
         hh_income_10to20k = B19001_003E + B19001_004E, 
         hh_income_20to35k = B19001_005E + B19001_006E + B19001_007E,
         hh_income_35to50k = B19001_008E + B19001_009E + B19001_010E,
         hh_income_50to75k = B19001_011E + B19001_012E,
         hh_income_75to100k = B19001_013E,
         hh_income_100to150k = B19001_014E + B19001_015E,
         hh_income_above150k = B19001_016E + B19001_017E
         )%>%
  select(GEOID, total_hh, starts_with("hh"))

# ACS Subsection: Insurance categories ---------------------------------------------
# Use ACS data to create insurance categories delineated by age (18 
# and under, 19 to 34, 35 to 64, and 65 and above) and insurance type (public, 
# private, public and private, and uninsured). 

insurance <- get_acs(
  geography = "county",
  state = "NC",
  table = "B27010",
  year = 2023,
  survey = "acs5"
)

insurance$county_name <- gsub(" County, North Carolina", "", insurance$NAME)

## Drop NAME, MOE
insurance <- insurance %>% select(c("county_name", "variable", "estimate"))

## Convert to wide
insurance <- insurance %>% spread(variable, estimate)

## We're going to turn this into
## Private Only, Pub Only, Priv and Pub, No Ins

## Rename a few columns
names(insurance)[3] <- "POP_A0018"
names(insurance)[19] <- "POP_A1934"
names(insurance)[35] <- "POP_A3564"
names(insurance)[52] <- "POP_A65p"

## Calculate vars
insurance$PRIV_INS_A0018 <- insurance$B27010_004 + insurance$B27010_005 + insurance$B27010_008 + insurance$B27010_011 + insurance$B27010_014
insurance$PUB_INS_A0018 <- insurance$B27010_006 + insurance$B27010_007 + insurance$B27010_009 + insurance$B27010_013 + insurance$B27010_015
insurance$PRIVPUB_INS_A0018 <- insurance$B27010_012 + insurance$B27010_016
names(insurance)[18] <- "UNINS_A0018"

insurance$PRIV_INS_A1934 <- insurance$B27010_020 + insurance$B27010_021 + insurance$B27010_024 + insurance$B27010_027 + insurance$B27010_030
insurance$PUB_INS_A1934 <- insurance$B27010_022 + insurance$B27010_023 + insurance$B27010_025 + insurance$B27010_029 + insurance$B27010_031
insurance$PRIVPUB_INS_A1934 <- insurance$B27010_028 + insurance$B27010_032
names(insurance)[34] <- "UNINS_A1934"

insurance$PRIV_INS_A3564 <- insurance$B27010_036 + insurance$B27010_037 + insurance$B27010_040 + insurance$B27010_043 + insurance$B27010_047
insurance$PUB_INS_A3564 <- insurance$B27010_038 + insurance$B27010_039 + insurance$B27010_041 + insurance$B27010_046 + insurance$B27010_048
insurance$PRIVPUB_INS_A3564 <- insurance$B27010_044 + insurance$B27010_045 + insurance$B27010_049
names(insurance)[51] <- "UNINS_A3564"

insurance$PRIV_INS_A65p <- insurance$B27010_053 + insurance$B27010_054 + insurance$B27010_056 + insurance$B27010_059 + insurance$B27010_063
insurance$PUB_INS_A65p <- insurance$B27010_055 + insurance$B27010_057 + insurance$B27010_062 + insurance$B27010_064
insurance$PRIVPUB_INS_A65p <- insurance$B27010_060 + insurance$B27010_061 + insurance$B27010_065
names(insurance)[67] <- "UNINS_A65p"

# Subset and reorder
insurance <- insurance[,c(1,3,68:70,18,19,71:73,34,35,74:76,51,52,77:79,67)]

# ACS Subsection: Merge and clean the ACS datasets ---------------------------------

# Clean and transform the dataset:
# 1. Remove "County, North Carolina" from the NAME field for readability
# 2. Drop MOE (margin of error) columns to retain only estimates
dataset3 <- nc_acs_data %>%
  mutate(county_name = str_remove(NAME, " County, North Carolina$")) %>%
  left_join(insurance, by = c('county_name')) %>%
  left_join(income, by = c('GEOID'))

rm(insurance)
rm(income)
rm(nc_acs_data)

# SECTION: HRSA UNS Score ------------------------------------------------------
# Import HRSA Unmet Need Score (UNS), aggregate (weighted scores) from zip code 
# to county level. 
# Source: HRSA
# URL: https://data.hrsa.gov/topics/health-centers/sanam
# Requires HUD crosswalk file from zip codes to county, information is here: 
# https://www.huduser.gov/portal/dataset/uspszip-api.html
# ______________________________________________________________________________

# Input API credentials to access the HUD crosswalk

key <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiI2IiwianRpIjoiNjc0NzAyZGIxNjBhMDkxMDcyMGU0NTBlZWU3OWZmZmY1YjdlZmE4MjU0ZTZjZTU1YjJiNzYxZGU3NDU5ZGY2MDQzN2FkYjY3ZjkyM2M5OTIiLCJpYXQiOjE3NTEzODExOTcuNTk0Mzc5LCJuYmYiOjE3NTEzODExOTcuNTk0MzgxLCJleHAiOjIwNjY5MTM5OTcuNTkwMzE0LCJzdWIiOiIxMDIwNTYiLCJzY29wZXMiOltdfQ.iHqaN1leqbAbnHlxYxvxp9gb0N_kzZbWuZxIrZySoTtpnX22CEgGDEoHcKBEW7PglkVC7XT4ciIHkRjrZWMNtg"
url <- "https://www.huduser.gov/hudapi/public/usps"

# GET request for ZIP-to-county crosswalk for NC (type = 2)

response <- GET(url, query = list(type = 2, query = "NC"),
                add_headers(Authorization = paste("Bearer", key)))

# Parse response content into a dataframe

output <- content(response)
df <- map_dfr(output$data$results, ~as.data.frame(.x, stringsAsFactors = FALSE)) %>%
  mutate(
    zip_code = as.numeric(zip),
    geoid = as.numeric(geoid)
  )

# Read in zip-level UNS data from HRSA

uns <- read_xlsx("raw/full_UNS.xlsx") %>%
  filter(State == "NC") %>%
  clean_names() %>%
  rename(zcta = zip_code_tabulation_area_zcta_map) %>%
  select(zip_code, zcta, zcta_uns)

# Join UNS data to the crosswalk file and and compute weighted county-level score

cross <- df %>%
  left_join(uns, by = "zip_code")

county_weighted_uns <- cross %>%
  filter(!is.na(zcta_uns)) %>%
  group_by(geoid) %>%
  summarise(
    weighted_uns = sum(zcta_uns * res_ratio, na.rm = TRUE) / sum(res_ratio, na.rm = TRUE),
    .groups = "drop"
  )

# Add county names using FIPS lookup from tidycensus

fips_lookup <- fips_codes %>%
  filter(state_code == "37") %>%  # NC only
  transmute(
    geoid = as.numeric(paste0("37", county_code)),
    county_name = str_remove(county, " County$")
  )

# Add county names to weighted UNS

dataset4 <- county_weighted_uns %>%
  left_join(fips_lookup, by = "geoid")%>%
  select(-geoid)

# SECTION: Merge datasets into population database file ------------------------
# Merge PLACES, RUCC, and ACS-based data files into a single population database
# ______________________________________________________________________________

countydat <- dataset2 %>%
  left_join(dataset3, by=c('county_name'))%>%
  left_join(dataset1, by=c('county_name'))%>%
  left_join(dataset4, by=c('county_name'))
head(countydat)
rm(dataset1, dataset2, dataset3)
gc()

write.csv(countydat, "clean/NC_County_Population_Database.csv")



