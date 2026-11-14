source("Cleaning_Script.R")

practice_data.8 %>% 
  select(where(is.numeric), -c(id, starts_with("lab"))) %>% 
  datasummary_skim(, fmt = 2) 
#No missing Data in dataset 
practice_data.8 %>% 
  select(where(is.factor)) %>% 
  datasummary_skim() 

practice_data.8 %>% 
  select(-id, -starts_with(c("lab", "wb", "st", "ss"))) %>% 
  datasummary_skim(, fmt = 2, by = "treatment_arm")

practice_data.8 %>%
  select(-id, -starts_with(c("lab", "wb", "st", "ss"))) %>%
  datasummary_balance(~ region, data = .)