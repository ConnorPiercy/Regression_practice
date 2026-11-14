source("Packages.R")
source("SQL_Server_Import.r")

str(pracitce_data)
#Change varibles listed from characters to factors

practice_data.1 <- pracitce_data %>% 
  mutate(across(c(site_id, region, site_type, treatment_arm, employment, 
                  smoker, diet_type, sex, education, family_history, rare_variant, 
                  hypertension, rare_event, primary_condition, event_observed
                  ), as.factor))
#Change variables to numerical

practice_data.2 <- practice_data.1 %>% 
  mutate(across(c(income_k, waist_cm, crp, hba1c_wk12, weight_wk12, hdl), as.numeric))
#Change Variables to integers

practice_data.3 <- practice_data.2 %>% 
  mutate(across(c(sbp_wk6, sbp_wk12, ldl_wk12), as.integer))

str(practice_data.3)
#Scales wv, st, ss, self-rated-health - keep order

practice_data.4 <- practice_data.3 %>% 
  mutate(across(starts_with("wb"), as.ordered)) %>% 
  mutate(across(starts_with("st"), as.ordered)) %>% 
  mutate(across(starts_with("ss"), as.ordered)) %>% 
  mutate(across("self_rated_health", as.ordered))
str(practice_data.4)
#Get columns with NA
names(practice_data.4)[colSums(is.na(practice_data.4)) > 0]

#Replace NA with Random Forest Imputation

miss_forest <- missForest(practice_data.4, maxiter = 10, ntree = 100)
practice_data.5 <- miss_forest$ximp

#Deal with rounding to 1
practice_data.6 <- practice_data.5 %>% 
  mutate(across(c(income_k, waist_cm, hba1c_wk12, weight_wk12), 
                ~ round(., digits = 1)))
#Deal with rounding to 0
practice_data.7 <- practice_data.6 %>% 
  mutate(across(c(hdl, sbp_wk6, sbp_wk12, 
                  sbp_wk6, ldl_wk12),
                ~ round(., digits = 0)))
#Round to 2
practice_data.8 <- practice_data.7 %>% 
  mutate(across(c(crp), ~round(., digits = 2)))
view(practice_data.8)

names(practice_data.8)[colSums(is.na(practice_data.8)) > 0]

path_data_folder <-  "/Users/connorpiercy/Multivariate_Practice/Data/"
file_name <- "practice_data.clean.csv"
full_path <- file.path(path_data_folder, file_name)


if (!file.exists(full_path)) {
  write_csv(practice_data.8, file = full_path)
  message("File saved successfully to: ", full_path)
} else {
  message("Skipped: A file named '",file_name , "' already exists in this folder.")
}



