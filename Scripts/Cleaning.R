# PURPOSE: create cleaning log for MSNA 2023 , the input are daily data and kobo form so the output a cleaning log 

# AUTHOR: Mardy Palanadar  | Database assistant 

# DATE CREATED: July-10, 2023

# NOTES: 

library(lubridate)
library(readxl)
library(writexl)
library(stringr)
library(illuminate)
library(purrr)
library(tidyverse)

msna_kobo <- read_excel('./input/kobo.xlsx',sheet =1,guess_max = Inf) 
msna_kobo_choices <- read_excel('./input/kobo.xlsx',sheet =2,guess_max = Inf)
msna_data <- read_excel('input/data.xlsx',sheet = 1,guess_max =Inf)
### change camp name from ninawa to erbil 




#msna_data$governorate_residence <- if_else(msna_data$camp_name == "akre_settlement" & !is.na(msna_data$camp_name), "erbil", msna_data$governorate_residence)

msna_data$governorate_residence <- msna_data$governorate_residence %>% str_replace_all("ninewa","duhok")



#msna_data$governorate_residence <- if_else(msna_data$governorate_residence == "ninewa" & !is.na(msna_data$camp_name), "duhok", msna_data$governorate_residence)




write_xlsx(msna_data,'./Output/msna_data.xlsx')

msna_data$today_sub <- date(msna_data$`_submission_time`)

#######################################################################




zip_file <- './input/audit.zip'

duration <- survey_duration_from_audit(data = msna_data, 
                                       audit_zip_path = zip_file,
                                       filter = T,
                                       filter_column = "Consent_to_collect_contact_inf",
                                       filter_choice = "yes",
                                       uuid_col_name = "_uuid",
                                       start_question = "hostcom_refugee",
                                       end_question ="means_feedback") 

duration <- data.frame(duration)

duration <- replace(duration, duration == -Inf, NA)

# Remove rows containing NA
duration <- na.omit(duration)








### combine 
duration <- duration %>% select(uuid,duration_minutes)  

final_cleaning_10 <- left_join(msna_data,duration,by= c('_uuid'="uuid"))  %>%  mutate(new_value='',old_value=duration_minutes,status="",issue='the duration of surevy is less than 15 ',comment_from_SFO='') %>% filter(old_value <15) %>%  select(start,end,'_uuid',enumerator_num,old_value,issue,comment_from_SFO,new_value,status,governorate_residence,gps_point_ki,today_sub) 

view(final_cleaning_10)


write_xlsx(final_cleaning_10,'./Output/survey need to be deleted .xlsx')








############################################################################### other checks 



cl1 <- function(data)
{
  col_names <- names(data)
  col_names_others <- col_names[endsWith(col_names, "_other") & !grepl("/", col_names)]
  pivot_longer(data,col_names_others, names_to = "question", values_to = "old_value") %>% filter(!is.na(old_value)) %>% mutate(issue="other fields (validate and/or translate)") %>% mutate_all(as.character)
}

cl1(msna_data)



clean1 <- cl1(msna_data) %>% mutate(new_value='',status='',comment_from_SFO='') %>% select(start,end,enumerator_num,'_uuid',question,issue,old_value,comment_from_SFO,new_value,status,governorate_residence,today_sub)


# caling question that has other 
quesion_no_other <- clean1$question %>% str_remove('_other')




#seperate type from listname 
separate_type <-  msna_kobo %>% select(type,name,`label::English`,`label::Arabic`) %>%  separate(type,sep=" ",c("type","listname")) 

separate_type <- na.omit(separate_type)



# cobine choices in one cell 

group_and_concat <- msna_kobo_choices %>%
  group_by(list_name) %>%
  summarise(all_choices = paste(name, collapse = " | "))


#combine choices with questions 

choices_questions <- left_join(group_and_concat,separate_type,by=c("list_name"="listname")) %>% select(all_choices,name,`label::English`,`label::Arabic`) %>% mutate(name=str_c(name,'_other'))



final_cleaning_1 <- left_join(clean1,choices_questions,by=c('question'='name')) %>% select(start,end,'_uuid',enumerator_num,question,`label::English`,`label::Arabic`,old_value,issue,comment_from_SFO,new_value,status,governorate_residence,today_sub) %>% rename(question_name=question)

view(final_cleaning_1)






####################################################################### outliers checks

         
## caling only numeric data 

data_numeric <- msna_data %>% select(start,end,enumerator_num,'_uuid',today_sub,governorate_residence, where(~identical(class(.), c("numeric"))) &!contains("/") &!contains("_gps") &!contains("_id") &!contains("first_arrive") &!contains("cereals") &!contains("nuts_seed")&!contains("milk_dairy")&!contains("meat")&!contains("vegetables")&!contains("fruits") &!contains("oil_fats") &!contains("sweets") &!contains("spices_condiments") )


data_pivot <- data_numeric %>% pivot_longer(c(-1,-2,-3,-4,-5,-6),names_to ='questions',values_to = 'values') %>% filter(!is.na(values))




data_groupby_question <- data_pivot %>% group_by(questions) %>% mutate(outlier=rep(list(boxplot.stats(values)$out), length(questions))) %>% rowwise() %>% mutate(is_outlier = values %in% outlier)




cleaning_2 <- data_groupby_question %>% mutate(old_value=values,new_value='',status='',issue='This value seems exceptionally high/low, please check with enumerator/respondent to verify?',comment_from_SFO='') %>% filter(is_outlier==TRUE)  %>%   select(start,end,enumerator_num,'_uuid',comment_from_SFO,question=questions,issue,old_value,new_value,status,governorate_residence,today_sub)



#unite_phone_number_from_cleaning_2 <-   cleaning_2 %>%  unite(telephone,telephone_number_ki,col = telephone_number,remove =TRUE,na.rm =TRUE)



question_label <- msna_kobo %>% select(name,`label::English`,`label::Arabic`)

final_cleaning_2 <- left_join(cleaning_2,question_label,by=c('question'='name'))  %>% select(start,end,'_uuid',enumerator_num,question,`label::English`,`label::Arabic`,old_value,issue,comment_from_SFO,new_value,status,governorate_residence,today_sub) %>% rename(question_name=question)

view(final_cleaning_2)




##################################################### combine other with oulier check            

final_cleaning_2$start <- as.character(final_cleaning_2$start)
final_cleaning_2$end <- as.character(final_cleaning_2$end)
final_cleaning_2$old_value <- as.character(final_cleaning_2$old_value)
final_cleaning_2$today_sub <- as.character(final_cleaning_2$today_sub)







############################### calculating duration with audit file 

zip_file <- './input/audit.zip'

duration <- survey_duration_from_audit(data = msna_data, 
                                          audit_zip_path = zip_file,
                                          filter = T,
                                          filter_column = "Consent_to_collect_contact_inf",
                                          filter_choice = "yes",
                                          uuid_col_name = "_uuid",
                                          start_question = "hostcom_refugee",
                                          end_question ="means_feedback") 

duration <- data.frame(duration)

duration <- replace(duration, duration == -Inf, NA)

# Remove rows containing NA
duration <- na.omit(duration)








### combine 
duration <- duration %>% select(uuid,duration_minutes)  


final_cleaning_3 <- left_join(msna_data,duration,by= c('_uuid'="uuid"))  %>%  mutate(new_value='',old_value=duration_minutes,status="",issue='the duration of surevy is less than 30 or more than 120 minutes',comment_from_SFO='') %>% filter(old_value <30 | old_value > 120) %>%  select(start,end,'_uuid',enumerator_num,old_value,issue,comment_from_SFO,new_value,status,governorate_residence,today_sub) 


view(final_cleaning_3)

##########################

final_cleaning_3$start <- as.character(final_cleaning_3$start)
final_cleaning_3$end <- as.character(final_cleaning_3$end)
final_cleaning_3$old_value <- as.character(final_cleaning_3$old_value)
final_cleaning_3$today_sub <- as.character(final_cleaning_3$today_sub)

######################## f_c_s





##################################################3 f_c_s questions

manual_outlier <- msna_data %>%  select(start,end,'_uuid',enumerator_num,cereals,nuts_seed,milk_dairy,meat,vegetables,fruits,oil_fats,sweets,spices_condiments,governorate_residence) %>% mutate_all(as.character) 


pivot_manual_outlier <-  pivot_longer(manual_outlier,cols = 5:13,names_to = 'f_c_s',values_to = 'how_many_days')


pivot_manual_outlier <- na.omit(pivot_manual_outlier)




filterted_manual_outlier <-  pivot_manual_outlier %>% filter(
  
  (f_c_s=='cereals'      &  how_many_days < 5) |
    (f_c_s=='nuts_seed'    &  how_many_days == 0) |
    ( f_c_s=='milk_dairy'      &  how_many_days < 5) |
    ( f_c_s=='meat'      &  how_many_days == 0 )|
    ( f_c_s=='vegetables' &  how_many_days < 5) |
    (f_c_s== 'fruits'    &  how_many_days == 0) |
    (f_c_s=='sweets'        &  how_many_days < 5) |
    (f_c_s=='spices_condiments'      &  how_many_days < 5) ) %>%
  select(start,end,'_uuid',enumerator_num,f_c_s,how_many_days,governorate_residence)


f_c_s_cleaning <-   filterted_manual_outlier %>% mutate(question_name=f_c_s,new_value='',old_value=how_many_days,issue='please check the value of f_c_s questions',comment_from_SFO='',status='') %>% select(start,end,'_uuid',enumerator_num,question_name,old_value,issue,comment_from_SFO,new_value,status,governorate_residence)
view(f_c_s_cleaning)
write_xlsx(f_c_s_cleaning,'./Output/f_c_s_cleaning.xlsx')








########### combine all the cleaning into one cleaning 


final_cleaning_all_with_sub <- bind_rows(final_cleaning_1, final_cleaning_2,final_cleaning_3) %>% mutate_all(as.character) 

final_cleaning_all <- final_cleaning_all_with_sub  %>% select(-today_sub)

view(final_cleaning_all)

write_xlsx(final_cleaning_all,'./Output/cleaning_log_all_2023.xlsx')



##### filter only today date 


#dates <- c("2023-07-26", "2023-07-27","2023-07-28","2023-07-29") #//// filter(today_sub %in% dates) filter(today_sub=="2023-07-11")



final_cleaning_today_data_only <- final_cleaning_all_with_sub %>% filter(today_sub=="2023-07-30") %>% select(-today_sub) #%>% rename('sub_date'='today_sub')

view(final_cleaning_today_data_only)

write_xlsx(final_cleaning_today_data_only,'./Output/cleaning_log_july_30_2023.xlsx')



















####################################################################################################### member 

msna_data_member <- read_excel('input/data.xlsx',sheet = 2,guess_max =Inf)





msna_data_member$today_sub <- date(msna_data_member$`_submission__submission_time`)


############################################################################### other checks 



cl1 <- function(data)
{
  col_names <- names(data)
  col_names_others <- col_names[endsWith(col_names, "_other") & !grepl("/", col_names)]
  pivot_longer(data,col_names_others, names_to = "question", values_to = "old_value") %>% filter(!is.na(old_value)) %>% mutate(issue="other fields (validate and/or translate)") %>% mutate_all(as.character)
}

cl1(msna_data_member)



clean1 <- cl1(msna_data_member) %>% mutate(new_value='',status='',comment_from_SFO='')%>% select(individual_UUID,'_submission__uuid',question,issue,old_value,comment_from_SFO,new_value,status,today_sub)


# caling question that has other 
quesion_no_other <- clean1$question %>% str_remove('_other')




#seperate type from listname 
separate_type <-  msna_kobo %>% select(type,name,`label::English`,`label::Arabic`) %>%  separate(type,sep=" ",c("type","listname")) 

separate_type <- na.omit(separate_type)



# cobine choices in one cell 

group_and_concat <- msna_kobo_choices %>%
  group_by(list_name) %>%
  summarise(all_choices = paste(name, collapse = " | "))


#combine choices with questions 

choices_questions <- left_join(group_and_concat,separate_type,by=c("list_name"="listname")) %>% select(all_choices,name,`label::English`,`label::Arabic`) %>% mutate(name=str_c(name,'_other'))



final_cleaning_1 <- left_join(clean1,choices_questions,by=c('question'='name')) %>% select(individual_UUID,'_submission__uuid',question,`label::English`,`label::Arabic`,old_value,issue,comment_from_SFO,new_value,status,today_sub) %>%  rename(question_name=question)

view(final_cleaning_1)






####################################################################### outliers checks


## caling only numeric data 

data_numeric <- msna_data_member %>% select(individual_UUID,'_submission__uuid',today_sub, where(~identical(class(.), c("numeric"))) &!contains("/") &!contains("_gps") &!contains("_id") &!contains("first_arrive") )


data_pivot <- data_numeric %>% pivot_longer(c(-1,-2,-3,-4),names_to ='questions',values_to = 'values') %>% filter(!is.na(values))




data_groupby_question <- data_pivot %>% group_by(questions) %>% mutate(outlier=rep(list(boxplot.stats(values)$out), length(questions))) %>% rowwise() %>% mutate(is_outlier = values %in% outlier)




cleaning_2 <- data_groupby_question %>% mutate(old_value=values,new_value='',status='',issue='This value seems exceptionally high/low, please check with enumerator/respondent to verify?',comment_from_SFO='') %>% filter(is_outlier==TRUE) %>%   select(individual_UUID,'_submission__uuid',comment_from_SFO,question=questions,issue,old_value,new_value,status,today_sub)



#unite_phone_number_from_cleaning_2 <-   cleaning_2 %>%  unite(telephone,telephone_number_ki,col = telephone_number,remove =TRUE,na.rm =TRUE)



question_label <- msna_kobo %>% select(name,`label::English`,`label::Arabic`)

final_cleaning_2 <- left_join(cleaning_2,question_label,by=c('question'='name'))  %>% select(individual_UUID,'_submission__uuid',question,`label::English`,`label::Arabic`,old_value,issue,comment_from_SFO,new_value,status,today_sub) %>% rename(question_name=question)

view(final_cleaning_2)




##################################################### combine other with oulier check            


final_cleaning_2$old_value <- as.character(final_cleaning_2$old_value)
final_cleaning_2$today_sub <- as.character(final_cleaning_2$today_sub)











########### combine all the cleaning into one cleaning 


final_cleaning_all_with_sub <- bind_rows(final_cleaning_1, final_cleaning_2) %>% mutate_all(as.character) 

final_cleaning_all_member <- final_cleaning_all_with_sub  %>% select(-today_sub)

view(final_cleaning_all_member)

final_cleaning_all_member_gov <- left_join(final_cleaning_all_member,msna_data,by=c('_submission__uuid'='_uuid'))  %>% select(start,end,individual_UUID,'_submission__uuid',enumerator_num,question_name,`label::English`,`label::Arabic`,old_value,issue,comment_from_SFO,new_value,status,governorate_residence,today_sub) %>% select (-today_sub)

view(final_cleaning_all_member_gov)

write_xlsx(final_cleaning_all_member,'./Output/Cleaning_log_all_member_2023.xlsx')



##### filter only today date 


#dates <- c("2023-07-26", "2023-07-27","2023-07-28","2023-07-29") #//// filter(today_sub %in% dates) filter(today_sub=="2023-07-11")



final_cleaning_all_member_gov_today <- final_cleaning_all_with_sub %>% filter(today_sub=="2023-07-30") %>% select(-today_sub) #%>% rename('sub_date'='today_sub')

view(final_cleaning_today_data_only)

write_xlsx(final_cleaning_all_member_gov_today,'./Output/cleaning_log_member_july_30_2023.xlsx')


