###PURPOSE: MSNA Household Analysis (2023), use MSNA cleaned data to generate hh refugee and host community analyses
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
PII_hh <- c("enumerator_num", "name_of_respondent_ki", "telephone_number_ki", "gps_point_ki", "_gps_point_ki_latitude",
            "_gps_point_ki_longitude", "_gps_point_ki_altitude", "_gps_point_ki_precision", "_id", "_uuid")

# Read in household data, add strata column and remove PII
df_hh_raw <- read_xlsx("Dataout/msna_data_hh.xlsx") %>% 
  mutate(strata = paste(hostcom_refugee, governorate_residence, camp_out_of_camp, sep = "_")) %>% 
  select(-PII_hh)

# Read in population data to calculate weights, convert to hh, join with data
df_pop_hh <- read_xlsx("Data/population_size.xlsx", sheet = "weights") %>% 
  mutate(pop_size = pop_size/6)

df_weights_hh <- df_hh_raw %>% 
  filter(hostcom_refugee  != "host_community") %>%
  group_by(strata) %>% 
  summarise(survey_count = n()) %>%
  left_join(df_pop_hh) %>% 
  mutate(sample_global = sum(survey_count, na.rm = T),
         pop_global = sum(pop_size, na.rm = T),
         survey_weight= (pop_size/pop_global)/(survey_count/sample_global))

df_hh <- df_hh_raw %>% 
  left_join(df_weights_hh) %>%
  filter(!is.na(survey_weight))

### Refugee analysis

# Remove columns not needed for analysis
df_hh_analysis <- df_hh %>% 
  select(-start:-information, -`_submission_time`:-`_index`)


df_hh_analysis_fix <- fix_data_type(df = df_hh_analysis)

analysis_hh_overall <- survey_analysis(df = df_hh_analysis_fix, weights = T,  weight_column = "survey_weight",
                                       strata = "governorate", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_hh_gov <- survey_analysis(df = df_hh_analysis_fix, weights = T,  weight_column = "survey_weight",
                                   strata = "governorate", disag = "governorate_residence", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_hh_dis <- survey_analysis(df = df_hh_analysis_fix, weights = T,  weight_column = "survey_weight",
                                   strata = "governorate", disag = "district", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_hh_camp <- survey_analysis(df = df_hh_analysis_fix, weights = T,  weight_column = "survey_weight",
                                    strata = "governorate", disag = "camp_out_of_camp", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

write_list_hh <- list(analysis_hh_overall = "analysis_hh_overall",
                      analysis_hh_gov = "analysis_hh_gov",
                      analysis_hh_dis = "analysis_hh_dis",
                      analysis_hh_camp = "analysis_hh_camp")

write_formatted_excel(write_list_hh, output_path = "Dataout/msna_analysis_refugee_hh.xlsx")

## Host community analysis

# Clean
df_hc_hh <- df_hh_raw %>% 
  filter(hostcom_refugee  == "host_community") %>%
  select(-camp_name, -camp_out_of_camp) 

# Remove columns not needed for analysis
df_hh_hc_analysis <- df_hc_hh %>% 
  select(-start:-information, -`_submission_time`:-`_index`)

df_hh_hc_analysis_fix <- fix_data_type(df = df_hh_hc_analysis)

analysis_hh_hc_overall <- survey_analysis(df = df_hh_hc_analysis_fix, weights = F,
                                          strata = "governorate", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_hh_hc_gov <- survey_analysis(df = df_hh_hc_analysis_fix, weights = F,
                                      strata = "governorate", disag = "governorate_residence", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")

analysis_hh_hc_dis <- survey_analysis(df = df_hh_hc_analysis_fix, weights = F,
                                      strata = "governorate", disag = "district", sm_sep = "/", question_lable = TRUE, kobo_path = "Data/kobo.xlsx")


write_list_hh_hc <- list(analysis_hh_hc_overall = "analysis_hh_hc_overall",
                         analysis_hh_hc_gov = "analysis_hh_hc_gov",
                         analysis_hh_hc_dis = "analysis_hh_hc_dis")

write_formatted_excel(write_list_hh_hc, output_path = "Dataout/msna_analysis_hc_hh.xlsx")
