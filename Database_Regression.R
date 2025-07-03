# ------------------------------------------------------------------------------
# Script: Database_Regression.R
# Purpose: Regression models to predict utilization based on community variables.
# Last updated by: Sarah Ulrich
# ------------------------------------------------------------------------------

# Load required libraries

library(tigris)
library(tidyverse)
library(readr)
library(tidycensus)
library(stringr)
library(sjPlot)
library(corrplot)

# Define file path to NCAHEC_Center for HealthCare Workforce folder in Teams

# IMPORTANT: Modify this path if running on a different machine - this is my personal path 
personal_path <- "C:/Users/ulric/University of North Carolina at Chapel Hill/"

# Set the working directory to the folder containing data files
# NOTE: Use file.path() for cross-platform compatibility
setwd(file.path(personal_path, 
                "cd_general - NCAHEC_Center on the Workforce for Health"
))

# Confirm the working directory is set correctly
print(paste("Working directory set to:", getwd()))

# Section: Import data ----

# Read in population database from clean data folder
dat <- read.csv("data/clean/NC_County_Population_Database.csv")%>%
  select(-X, -X.x)

# Section: Select regression variables ----

model_dat <- dat %>%
  mutate(
    HHINC_UNDER25k = (
      B19001_002E + # Less than $10,000
        B19001_003E + # $10,000 to $14,999
        B19001_004E + # $15,000 to $19,999
        B19001_005E   # $20,000 to $24,999
    ) / B19001_001E,  # Total number of households
    
    HHINC_OVER100k = (
      B19001_014E + # $100,000 to $124,999
        B19001_015E + # $125,000 to $149,000
        B19001_016E + # $150,000 to $199,000
        B19001_017E   # Over $200,000
    ) / B19001_001E,  # Total number of households
    
    PUB_INS_PCT = (
      PUB_INS_A1934 + 
        PUB_INS_A3564   
    ) / (POP_A1934 + POP_A3564),
    
    
    PRIV_INS_PCT = ( 
      PRIV_INS_A1934 +
        PRIV_INS_A3564   
    ) / (POP_A1934 + POP_A3564),
    
    UNINS_PCT = ( 
      UNINS_A1934 + 
        UNINS_A3564 
    ) / (POP_A1934 + POP_A3564),
    
    ABOVE65 = (
      B01001_020E + # Male 65 and 66
        B01001_021E + # Male 67 to 69
        B01001_022E + # Male 70 to 74
        B01001_023E + # Male 75 to 79
        B01001_024E + # Male 80 to 84
        B01001_025E + # Male 85 and abvoe
        B01001_044E + # Female 65 and 66
        B01001_045E + # Female 67 to 69
        B01001_046E + # Female 70 to 74
        B01001_047E + # Female 75 to 79
        B01001_048E + # Female 80 to 84
        B01001_049E   # Female 85 and above
    ) / B01001_001E,  # Total population
    
    RUCC_CAT = ifelse(RUCC_2023 < 4, "Urban", 
                      ifelse(RUCC_2023 < 7, "Micro", 
                             ifelse(RUCC_2023 <= 10, "Rural",
                                    "Missing"))),
    
    CANCER = BRFSS_CANCER_CrdPrv / 100, 
    HEARTDIS = BRFSS_CHD_CrdPrv / 100,
    SMOKING = BRFSS_CSMOKING_CrdPrv / 100,
    OBESITY = BRFSS_OBESITY_CrdPrv / 100,
    STROKE = BRFSS_STROKE_CrdPrv / 100,
    COPD = BRFSS_COPD_CrdPrv / 100,
    LACKTRPT = BRFSS_LACKTRPT_CrdPrv / 100,
    
    HOSP_ADMIT = Acute_HospAdmit / B01001_001E,
    
    HOMEHEALTH = total_homehealth_visits / B01001_001E,
    
  ) %>%
  select(
    County_Name,
    #HHINC_UNDER25k,
    #HHINC_OVER100k,
    #PUB_INS_PCT,
    #PRIV_INS_PCT,
    UNINS_PCT,
    ABOVE65,
    RUCC_CAT,
    CANCER, 
    HEARTDIS,
    SMOKING,
    OBESITY,
    STROKE,
    COPD,
    LACKTRPT,
    #HOSP_ADMIT,
    HOMEHEALTH
  )


# ------------------------------------------------------------------------------
# Section: Explore data
# ------------------------------------------------------------------------------

# View summary statistics
summary(model_dat)

# Correlation matrix
cor_mat <- cor(model_dat %>% dplyr::select(where(is.numeric)), use = "complete.obs")

corrplot(cor_mat, 
         method = "color", 
         type = "lower", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45)

# Remove HOSP_ADMIT outliers and plot distribution:  

model_dat_filtered <- model_dat %>%
  mutate(z_score = scale(HOSP_ADMIT)) %>%
  filter(abs(z_score) <= 3)

ggplot(model_dat_filtered, aes(x = HOSP_ADMIT)) + 
  geom_histogram()

# Remove HOMEHEALTH outliers and plot distribution:  

model_dat_filtered <- model_dat %>%
  mutate(z_score = scale(HOMEHEALTH)) %>%
  filter(abs(z_score) <= 3)

ggplot(model_dat_filtered, aes(x = CANCER)) + 
  geom_histogram()

# ------------------------------------------------------------------------------
# Section: Mapping
# ------------------------------------------------------------------------------

library(tigris)
library(tmap)

nc_counties <- counties(state = "NC",
                        cb = TRUE)%>%
  select(NAME, geometry)

model_dat_spatial <- nc_counties %>%
  left_join(model_dat, by = c("NAME" = "County_Name"))

tm_shape(model_dat_spatial) +
  tm_polygons(fill = "HOMEHEALTH", palette = "blues")

tm_shape(model_dat_spatial) +
  tm_polygons(fill = "HOSP_ADMIT", palette = "blues")

tm_shape(model_dat_spatial) +
  tm_polygons(fill = "PUB_INS_PCT", palette = "blues")

tm_shape(model_dat_spatial) +
  tm_polygons(fill = "PRIV_INS_PCT", palette = "blues")

tm_shape(model_dat_spatial) +
  tm_polygons(fill = "UNINS_PCT", palette = "reds")

# ------------------------------------------------------------------------------
# Section: Regression 
# ------------------------------------------------------------------------------

model <- lm(HOMEHEALTH ~ )

model <- glm(HOMEHEALTH ~ 
             RUCC_CAT + 
             HHINC_UNDER25k +
             HHINC_OVER100k +
             PUB_INS_PCT +
             PRIV_INS_PCT +
             UNINS_PCT +
             #ABOVE65 +
             #CANCER + 
             #HEARTDIS +
             #SMOKING +
             #OBESITY +
             #STROKE +
             #COPD +
             LACKTRPT,
             family = quasipoisson(link = "log"),
             data = model_dat)
tab_model(model)

model <- glm(acute_hospadmit ~ RUCC_Cat, 
             family = quasipoisson(link = "log"), 
             data = model_dat)
tab_model(model)

model <- glm(homehealth ~ RUCC_Cat + pct_above65, 
             family = quasipoisson(link = "log"), 
             data = model_dat)
tab_model(model)



rucc_model <- lm(acute_hospadmit ~ factor(RUCC_Cat), data=model_dat)
tab_model(rucc_model)


cancer_model <- lm(cancer ~ factor(RUCC_2023), data = model_dat)
tab_model(cancer_model)

smoking_model <- lm(smoking ~ factor(RUCC_2023), data = model_dat)
tab_model(smoking_model)


#Stepwise regression

vars <- c(RUCC_2023, 
          pctnoins,
          above_65_pct,
          low_income_pct,
          cancer,
          heartdisease,
          smoking,
          obesity,
          arthritis,
          checkup,
          stroke)

null_model <- lm(acute_hospadmit_rate ~ 1, data = model_dat)

forward_model <- step(null_model, scope = list(lower = ~ 1, upper = ~ . - 1), 
                      direction = "forward")

summary(forward_model)

initial_model <- lm(acute_hospadmit_rate ~ ., data=model_dat)

backward_model <- step(initial_model, direction = "backward")


reduced_model <- lm(acute_hospadmit_rate ~ factor(RUCC_2023) + pctnoins + low_income_pct + above_65_pct + smoking + cancer + checkup, data = model_dat)
step(reduced_model, direction = "backward")



# Mapping






