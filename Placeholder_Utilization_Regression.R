
# Test regressions with placeholder utilization data

# Start with county-level population database file and add utilization data 
# to test some regression models prior to recieving Sheps claims dataset

# SECTION: Setup libraries and working directory -------------------------------

library(tidyverse)
library(corrplot)
library(sjPlot)

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

# Section: Import and merge data -----------------------------------------------

# Read in population database from clean data folder
countydat <- read.csv("data/clean/NC_County_Population_Database.csv")

# Add acute care hospital admissions data.  
# Source: NC Division of Health Service Regulation Healthcare Planning 
#         and Certificate of Need Section (2023)
# URL: https://info.ncdhhs.gov/dhsr/mfp/patientoriginreports.html
# 
# NOTE: The script Destination_agg.R processes data from .txt file format 
# into the .csv files that are read in below. 

acute_admissions <- read.csv("data/raw/ncdhsr_hpcon/acute_hospital/Destination_agg.csv") %>%
  rename(Acute_HospAdmit = Number_of_Patients)%>%
  rename(county_name = Patient_Origin)

# Add home health visitation data.  
# Source: NC Division of Health Service Regulation Healthcare Planning 
#         and Certificate of Need Section (2023)
# URL: https://info.ncdhhs.gov/dhsr/mfp/patientoriginreports.html
# 
# NOTE: The script Homehealth_agg.R processes data from .xlsx file format 
# into the .csv files that are read in below. 

home_health <- read.csv("data/raw/ncdhsr_hpcon/home_health/Homehealth_agg.csv") 

# Merge datasets

countydat <- countydat %>%
  left_join(acute_admissions, by=c('county_name'))%>%
  left_join(home_health, by=c('county_name' = 'County'))


# Section: Select regression variables -----------------------------------------

model_dat <- countydat %>%
  mutate(
   
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
       total_m_65to74 + 
       total_m_75to84 + 
       total_m_85above + 
       total_f_65to74 + 
       total_f_75to84 +
       total_f_85above) / totalpop,  # Total population
    
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
    
    HOSP_ADMIT = Acute_HospAdmit / totalpop,
    
    HOMEHEALTH = total_homehealth_visits / totalpop,
    
  ) %>%
  select(
    county_name,
    PUB_INS_PCT,
    PRIV_INS_PCT,
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
    HOSP_ADMIT,
    HOMEHEALTH,
    weighted_uns
  )


# SECTION: Explore correlation -------------------------------------------------

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

# Section: Mapping -------------------------------------------------------------

library(tigris)
library(tmap)

nc_counties <- counties(state = "NC",
                        cb = TRUE)%>%
  select(NAME, geometry)

model_dat_spatial <- nc_counties %>%
  left_join(model_dat, by = c("NAME" = "county_name"))

tmap_mode("view")

tm_shape(model_dat_spatial) +
  tm_polygons("HOMEHEALTH", id = "NAME", palette = "Blues", group = "Home Health") +
  tm_polygons("HOSP_ADMIT", id = "NAME", palette = "Blues", group = "Hospital Admits") +
  tm_polygons("PUB_INS_PCT", id = "NAME", palette = "Blues", group = "Public Insurance %") +
  tm_polygons("PRIV_INS_PCT", id = "NAME", palette = "Blues", group = "Private Insurance %") +
  tm_polygons("UNINS_PCT", id = "NAME", palette = "Reds", group = "Uninsured %") +
  tm_polygons("STROKE", id = "NAME", palette = "Blues", group = "Stroke") +
  tm_polygons("CANCER", id = "NAME", palette = "Blues", group = "Cancer") +
  tm_polygons("HEARTDIS", id = "NAME", palette = "Blues", group = "Heart Disease") +
  tm_polygons("COPD", id = "NAME", palette = "Blues", group = "COPD") +
  tm_polygons("weighted_uns", id = "NAME", palette = "Blues", group = "UNS") +
  tm_facets(sync = TRUE)  

# Section: Regression ----------------------------------------------------------

model <- glm(HOMEHEALTH ~ weighted_uns, 
             family = quasipoisson(link = "log"), 
             data = model_dat)

summary(model)
tab_model(model)

model <- glm(HOSP_ADMIT ~ 
               RUCC_CAT + 
               #HHINC_UNDER25k +
               #HHINC_OVER100k +
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
               LACKTRPT +
               weighted_uns,
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


# Identify high-access counties...







