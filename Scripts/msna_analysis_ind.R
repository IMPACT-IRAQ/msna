###PURPOSE: MMSNA Individual Analysis (2023), use MSNA cleaned data to generate ind refugee and host community analyses
###AUTHOR: Cody Adelson (cody.adelson@impact-initiatives.org)
###DATE CREATED: July 2023
###NOTES: Refugee and host community analysis separate. Host community analysis indicative, therefore not weighted

library(tidyverse)
library(readxl)
library(openxlsx)
library(illuminate)
library(stringr)
library(srvyr)

# Identify personal information columns
PII_ind <- c("individual_UUID")

# Read in household data for join to get geographical information
df_hh_join <- read_xlsx("Dataout/msna_data_hh.xlsx") %>% 
  distinct(governorate_residence, district, hostcom_refugee, camp_out_of_camp, camp_name, `_uuid`) %>% 
  rename(`_submission__uuid`=`_uuid`) %>% 
  mutate(strata = paste(hostcom_refugee, governorate_residence, camp_out_of_camp, sep = "_"))

# Read in individual data, remove PII and join with hh information
df_ind_raw <- read_xlsx("Dataout/msna_data_ind.xlsx") %>% 
  select(-PII_ind) %>% 
  left_join(df_hh_join)

# Read population data to calculate weights, join with data
df_pop_ind <- read_xlsx("Data/population_size.xlsx", sheet = "weights")

df_weights_ind <- df_ind_raw %>% 
  filter(hostcom_refugee  != "host_community") %>% 
  group_by(strata) %>% 
  summarise(survey_count = n()) %>%
  left_join(df_pop_ind) %>% 
  mutate(sample_global = sum(survey_count, na.rm = T),
         pop_global = sum(pop_size, na.rm = T),
         survey_weight= (pop_size/pop_global)/(survey_count/sample_global))

df_ind <- df_ind_raw %>% 
  left_join(df_weights_ind) %>%
  filter(!is.na(survey_weight))

### analysis

# Remove columns not needed for analysis
df_ind_analysis <- df_ind %>% 
  select(-`_index`:-`_submission__tags`)

df_ind_analysis_fix <- fix_data_type(df = df_ind_analysis)

analysis_ind_overall <- survey_analysis(df = df_ind_analysis_fix, weights = T,  weight_column = "survey_weight",
                                        strata = "governorate", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_ind_gov <- survey_analysis(df = df_ind_analysis_fix, weights = T,  weight_column = "survey_weight",
                                    strata = "governorate", disag = "governorate_residence", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_ind_dis <- survey_analysis(df = df_ind_analysis_fix, weights = T,  weight_column = "survey_weight",
                                    strata = "governorate", disag = "district", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_ind_camp <- survey_analysis(df = df_ind_analysis_fix, weights = T,  weight_column = "survey_weight",
                                     strata = "governorate", disag = "camp_out_of_camp", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")


write_list_ind <- list(analysis_ind_overall = "analysis_ind_overall",
                       analysis_ind_gov = "analysis_ind_gov",
                       analysis_ind_dis = "analysis_ind_dis",
                       analysis_ind_camp = "analysis_ind_camp")

write_formatted_excel(write_list_ind, output_path = "Dataout/msna_analysis_refugee_ind.xlsx")

## Host community analysis

# Clean
df_hc_ind <- df_ind_raw %>% 
  filter(hostcom_refugee  == "host_community") %>%
  select(-camp_name, -camp_out_of_camp) 

# Remove columns not needed for analysis
df_ind_hc_analysis <- df_hc_ind %>% 
  select(-`_index`:-`_submission__tags`)

df_ind_hc_analysis_fix <- fix_data_type(df = df_ind_hc_analysis)

analysis_ind_hc_overall <- survey_analysis(df = df_ind_hc_analysis_fix, weights = F,
                                           strata = "governorate", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_ind_hc_gov <- survey_analysis(df = df_ind_hc_analysis_fix, weights = F,
                                       strata = "governorate", disag = "governorate_residence", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_ind_hc_dis <- survey_analysis(df = df_ind_hc_analysis_fix, weights = F,
                                       strata = "governorate", disag = "district", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")


write_list_ind_hc <- list(analysis_ind_hc_overall = "analysis_ind_hc_overall",
                          analysis_ind_hc_gov = "analysis_ind_hc_gov",
                          analysis_ind_hc_dis = "analysis_ind_hc_dis")

write_formatted_excel(write_list_ind_hc, output_path = "Dataout/msna_analysis_hc_ind.xlsx")

