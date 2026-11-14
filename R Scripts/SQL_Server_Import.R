con <- dbConnect(
  RMariaDB::MariaDB(),
  host     = "127.0.0.1",
  username = "root",
  password = "rootpassword",
  dbname   = "Multivariate_data",
  port     = 3306
)

sql_query <- "
SELECT `date`, day_of_week, mean_temp_c, flu_index, public_holiday, daily_clinic_visits
FROM Multivariate_data.daily_series;"

daily_series_data <- dbGetQuery(con, sql_query)

sql_query_2 <- "
SELECT id, site_id, region, site_type, 
treatment_arm, age, sex, education, employment, 
income_k, height_cm, weight_kg, bmi, waist_cm, smoker, 
diet_type, diet_score, sleep_hours, screen_hours, 
family_history, rare_variant, training_min_week, 
exercise_hours_week, days_worn, mean_daily_steps_k, 
cigs_per_day, alcohol_units_week, 
wb1, wb2, wb3, wb4, st1, st2, st3, st4, ss1, ss2, 
ss3, ss4, sbp, dbp, total_chol, ldl, hdl, triglycerides, 
glucose, hba1c, crp, resting_hr, lab_marker_01, 
lab_marker_02, lab_marker_03, lab_marker_04, lab_marker_05, 
lab_marker_06, lab_marker_07, lab_marker_08, lab_marker_09, 
lab_marker_10, lab_marker_11, lab_marker_12, lab_marker_13, 
lab_marker_14, lab_marker_15, sbp_wk6, sbp_wk12, ldl_wk12, 
hba1c_wk12, weight_wk12, hypertension, rare_event, primary_condition, 
self_rated_health, exposure_years, gp_visits, er_visits, annual_cost, 
fitness_score, adherence_prop, bp_in_target_prop, time_to_event_months, 
event_observed
FROM Multivariate_data.practice_data;"

pracitce_data <- dbGetQuery(con, sql_query_2)

dbDisconnect(con)
