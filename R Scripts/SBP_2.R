source("SBP_analysis.r")

summary(model8.lme)


vc <- as.data.frame(VarCorr(model8.lme))
var_within_person <- vc$vcov[vc$grp == "Residual"]

var_between_people <- vc$vcov[vc$grp == "id" & vc$var1 == "(Intercept)" & is.na(vc$var2)]

var_between_clinics <- vc$vcov[vc$grp == "site_id" & vc$var1 == "(Intercept)" & is.na(vc$var2)]
pander(cbind(
  "Variation within Person" = var_within_person,
  "Variation between People" = var_between_people,
  "Variation between Clinics" = var_between_clinics
))

r2(model8.lme)
# 29.1% of variation explained by fixed variables variables
#94.0% of variation explained by fixed effects and random effects

re <- ranef(model8.lme)$site_id
pander(re[order(re[, "(Intercept)"], decreasing = TRUE), ][1:10, ])
pander(re[order(re[, "(Intercept)"], decreasing = FALSE), ][1:10, ])
pander(re[order(re[, "age_c"], decreasing = TRUE), ][1:10, ])
pander(re[order(re[, "age_c"], decreasing = FALSE), ][1:10, ])

re.person <- ranef(model8.lme)$id
pander(re.person[order(re.person[, "(Intercept)"], decreasing = TRUE), ][1:10, ])
pander(re.person[order(re.person[, "(Intercept)"], decreasing = FALSE), ][1:10, ])
pander(re.person[order(re.person[, "time_num"], decreasing = TRUE), ][1:10, ])
pander(re.person[order(re.person[, "time_num"], decreasing = FALSE), ][1:10, ])

check_model(model8.lme)


set.seed(3960)

train_ids <- sample(unique(sbp_long$id), size = 0.8 * n_distinct(sbp_long$id))
#Trainging the model on this data - 24,000 observations from long version
#Of original data. 
training_data <- sbp_long %>% filter(id %in% train_ids)
#Then testing the model on this data - 6000 observations from data set of long
#Where each participant recorded 3 times, for each sbp check at baseline, 6 and 12
test_data     <- sbp_long %>% filter(!id %in% train_ids)
nrow(training_data)
nrow(test_data)

model8_training <- lmer(
  sbp_value ~
    time_num +
    age_c +
    bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    lab_marker_03 +
    lab_marker_07 +
    lab_marker_12 +
    (1 + time_num | id) +
    (1 + age_c | site_id),
  data = training_data
)

test_data$predicted_sbp <- predict(
  model8_training,
  newdata = test_data,
  re.form = NULL,
  allow.new.levels = TRUE
)

RMSE <- sqrt(mean((test_data$sbp_value - test_data$predicted_sbp)^2, na.rm = TRUE))
MAE <- mean(abs(test_data$sbp_value - test_data$predicted_sbp), na.rm = TRUE)

#baseline check
mean_sbp <- mean(training_data$sbp_value, na.rm = TRUE)
null_rmse <- sqrt(mean((test_data$sbp_value - mean_sbp)^2, na.rm = TRUE))


pander(cbind(
  "Test MAE (mmHg):" = round(MAE, 2),
  "Test RMSE (mmHg):" = round(RMSE, 2),
  "Null Model RMSE (mmHg):" = round(null_rmse, 2),
  "Out of Sample R^2 (%):", (round(1 - (RMSE^2 / null_rmse^2), 5))*100
))

ggplot(test_data, aes(x = predicted_sbp, y = sbp_value)) +
  geom_point(alpha = 0.25, color = "black") + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "12-Week SBP: Observed vs. Full Mixed Model Predictions",
    subtitle = "Includes both patient (id) and clinic (site_id) random intercepts & slopes",
    x = "Predicted SBP (mmHg)",
    y = "Observed SBP (mmHg)"
  ) +
  theme_minimal()

# Train on baseline and 6-week visits (plus all visits from training_data)
#Do this to then predict sbp at 12 weeks, and see the RMSE of prediction. 
train_longitudinal <- sbp_long %>%
  filter(id %in% train_ids | time_num < 12)

test_month12 <- sbp_long %>%
  filter(!id %in% train_ids & time_num == 12)

model_long <- update(model8_training, data = train_longitudinal)

# Include patient-level random effects (re.form = NULL)
test_month12$pred_m12 <- predict(model_long, newdata = test_month12, re.form = NULL)

rmse_m12 <- sqrt(mean((test_month12$sbp_value - test_month12$pred_m12)^2))
cat("12-Week Visit RMSE (Existing Patients):", round(rmse_m12, 2), "mmHg\n")

#Testing non linear time model
sbp_long$age_c <- as.numeric(sbp_long$age_c)

model9.lme <- lmer(
  sbp_value ~
    time + 
    bmi +
    age_c +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    lab_marker_03 +
    lab_marker_07 +
    lab_marker_12 +
    (1 + time_num | id) +
    (1 + age_c | site_id),
  data = sbp_long
)
summary(model8.lme)
summary(model9.lme)

anova(model8.lme, model9.lme)
#Checking time cateogrical variable spread and variance across visits

sbp_long$resid_9 <- resid(model9.lme)


ggplot(sbp_long, aes(x = time, y = resid_9)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_boxplot(width = 0.25, outlier.shape = NA, alpha = 0.3) +
  geom_jitter(width = 0.12, alpha = 0.4) +
  labs(
    x = "Visit (time)",
    y = "Model Residuals (mmHg)",
    title = "Residuals Across Study Visits"
  ) +
  theme_minimal()

#Shows some heteroscedasticity in week 6 as its wider there then at baseline or 12.
#This means lmer assumption of homoscedasticity might be violated, but its mild
# Can try and fix with glmmtmb which replaces single scalar variance with a sub-model
# that estimates a seprate variance parameter for each of the 3 weeks 
tapply(sbp_long$resid_9, sbp_long$time, sd)
(summary(model9.lme)$coefficients)
r2(model9.lme)
#Conditional R^2 might be inflated due to missing time_num in fixed effects
#Everything else should be valid. 
#Model 9 when time is non linear is much better then model 8 at predicting 
#Model 9 is knew best model due to non linear time variable. 

vc.9 <- as.data.frame(VarCorr(model9.lme))
var_within_person.9 <- vc.9$vcov[vc.9$grp == "Residual"]

var_between_people.9 <- vc.9$vcov[vc.9$grp == "id" & vc.9$var1 == "(Intercept)" & is.na(vc.9$var2)]

var_between_clinics.9 <- vc.9$vcov[vc.9$grp == "site_id" & vc.9$var1 == "(Intercept)" & is.na(vc.9$var2)]

total_variation = var_within_person.9 + var_between_people.9 + var_between_clinics.9
variation_individuals_percentage <- (var_between_people.9 / total_variation) * 100
variation_clinics_percentage <- (var_between_clinics.9 / total_variation) * 100
residual_variation_percentage <- (var_within_person.9 / total_variation) * 100

pander(cbind(
  "Residual Variation" = var_within_person.9,
  "Variation between People" = var_between_people.9,
  "Variation between Clinics" = var_between_clinics.9,
  "Total Variation in Model" = total_variation,
  "Residual Variation %" = residual_variation_percentage,
  "Variation between People %" = variation_individuals_percentage,
  "Variation between Clinics %" = variation_clinics_percentage
))


# 29.1% of variation explained by fixed variables variables
#94.0% of variation explained by fixed effects and random effects

re.9 <- ranef(model9.lme)$site_id
pander(re.9[order(re.9[, "(Intercept)"], decreasing = TRUE), ][1:10, ])
pander(re.9[order(re.9[, "(Intercept)"], decreasing = FALSE), ][1:10, ])
pander(re.9[order(re.9[, "age_c"], decreasing = TRUE), ][1:10, ])
pander(re.9[order(re.9[, "age_c"], decreasing = FALSE), ][1:10, ])

re.person.9 <- ranef(model9.lme)$id
pander(re.person.9[order(re.person.9[, "(Intercept)"], decreasing = TRUE), ][1:10, ])
pander(re.person.9[order(re.person.9[, "(Intercept)"], decreasing = FALSE), ][1:10, ])
pander(re.person.9[order(re.person.9[, "time_num"], decreasing = TRUE), ][1:10, ])
pander(re.person.9[order(re.person.9[, "time_num"], decreasing = FALSE), ][1:10, ])

check_model(model9.lme)


set.seed(3960)

train_ids.9 <- sample(unique(sbp_long$id), size = 0.8 * n_distinct(sbp_long$id))
#Trainging the model on this data - 24,000 observations from long version
#Of original data. 
training_data.9 <- sbp_long %>% filter(id %in% train_ids.9)
#Then testing the model on this data - 6000 observations from data set of long
#Where each participant recorded 3 times, for each sbp check at baseline, 6 and 12
test_data.9 <- sbp_long %>% filter(!id %in% train_ids.9)
nrow(training_data.9)
nrow(test_data.9)

model9_training <- lmer(
  sbp_value ~
    time +
    age_c +
    bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    lab_marker_03 +
    lab_marker_07 +
    lab_marker_12 +
    (1 + time_num | id) +
    (1 + age_c | site_id),
  data = training_data.9
)

test_data.9$predicted_sbp <- predict(
  model9_training,
  newdata = test_data.9,
  re.form = NULL,
  allow.new.levels = TRUE
)

RMSE.9 <- sqrt(mean((test_data.9$sbp_value - test_data.9$predicted_sbp)^2, na.rm = TRUE))
MAE.9 <- mean(abs(test_data.9$sbp_value - test_data.9$predicted_sbp), na.rm = TRUE)

#baseline check
mean_sbp.9 <- mean(training_data.9$sbp_value, na.rm = TRUE)
null_rmse.9 <- sqrt(mean((test_data.9$sbp_value - mean_sbp.9)^2, na.rm = TRUE))


pander(cbind(
  "Test MAE (mmHg):" = round(MAE.9, 2),
  "Test RMSE (mmHg):" = round(RMSE.9, 2),
  "Null Model RMSE (mmHg):" = round(null_rmse.9, 2),
  "Out of Sample R^2 (%):", (round(1 - (RMSE.9^2 / null_rmse.9^2), 5))*100
))

ggplot(test_data.9, aes(x = predicted_sbp, y = sbp_value)) +
  geom_point(alpha = 0.25, color = "black") + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "12-Week SBP: Observed vs. Full Mixed Model 9 Predictions",
    subtitle = "Includes both patient (id) and clinic (site_id) random intercepts & slopes",
    x = "Predicted SBP (mmHg)",
    y = "Observed SBP (mmHg)"
  ) +
  theme_minimal()

# Train on baseline and 6-week visits (plus all visits from training_data)
#Do this to then predict sbp at 12 weeks, and see the RMSE of prediction. 
train_longitudinal.9 <- sbp_long %>%
  filter(
    id %in% train_ids.9 |
      time != "sbp_wk12"
  )

test_month12.9 <- sbp_long %>%
  filter(
    !id %in% train_ids.9 &
      time == "sbp_wk12"
  )

model_long.9 <- update(model9_training, data = train_longitudinal.9)

# Include patient-level random effects (re.form = NULL)
test_month12.9$pred_m12 <- predict(model_long.9, newdata = test_month12.9, re.form = NULL)

rmse_m12.9 <- sqrt(mean((test_month12.9$sbp_value - test_month12.9$pred_m12)^2))
cat("12-Week Visit RMSE (Existing Patients):", round(rmse_m12.9, 2), "mmHg\n")

#Testing interaction models. 
#First must center numerical variables, then test its interaction with time
#One by one, then anova test it one by one against model9

sbp_long$bmi_c <- sbp_long$bmi - 
  mean(sbp_long$bmi)

sbp_long$alcohol_units_week_c <- sbp_long$alcohol_units_week - 
  mean(sbp_long$alcohol_units_week)

sbp_long$sleep_hours_c <- sbp_long$sleep_hours - 
  mean(sbp_long$sleep_hours)

sbp_long$exercise_hours_week_c <- sbp_long$exercise_hours_week - 
  mean(sbp_long$exercise_hours_week)

sbp_long$lab_marker_03_c <- sbp_long$lab_marker_03 - 
  mean(sbp_long$lab_marker_03)

sbp_long$lab_marker_07_c <- sbp_long$lab_marker_07 - 
  mean(sbp_long$lab_marker_07)

sbp_long$lab_marker_12_c <- sbp_long$lab_marker_12 - 
  mean(sbp_long$lab_marker_12)
view(sbp_long)

model9.lme.1 <- lmer(
  sbp_value ~
    time + 
    bmi_c +
    age_c +
    sex +
    smoker +
    alcohol_units_week_c +
    sleep_hours_c +
    exercise_hours_week_c +
    lab_marker_03_c +
    lab_marker_07_c +
    lab_marker_12_c +
    (1 + time_num | id) +
    (1 + age_c | site_id),
  data = sbp_long
)

model10.lme1 <- update(model9.lme.1, . ~ . + time_num:bmi_c)
anova(model9.lme.1, model10.lme1)#Very significant
model10.lme2 <- update(model9.lme.1, . ~ . + time_num:age_c)
anova(model9.lme.1, model10.lme2)#Very significant
model10.lme3 <- update(model9.lme.1, . ~ . + time_num:alcohol_units_week_c)
anova(model9.lme.1, model10.lme3)#Significant
model10.lme4 <- update(model9.lme.1, . ~ . + time_num:sleep_hours_c)
anova(model9.lme.1, model10.lme4) #Not Significant
model10.lme5 <- update(model9.lme.1, . ~ . + time_num:exercise_hours_week_c)
anova(model9.lme.1, model10.lme5) #Significant
model10.lme6 <- update(model9.lme.1, . ~ . + time_num:lab_marker_03_c)
anova(model9.lme.1, model10.lme6) #Very Significant
model10.lme7 <- update(model9.lme.1, . ~ . + time_num:lab_marker_07_c)
anova(model9.lme.1, model10.lme7) #Significant
model10.lme8 <- update(model9.lme.1, . ~ . + time_num:lab_marker_12_c)
anova(model9.lme.1, model10.lme8) #significant

#Sleep is a main effect only with time. 
#age, bmi, and labmarker 3 are clearly much better included in the model
#Next fit model11.lme with those 3. 

model11.lme <- update(
  model9.lme.1,
  . ~ . +
    time_num:age_c +
    time_num:bmi_c +
    time_num:lab_marker_03_c
)
fixef(model11.lme, add.dropped = TRUE)
X11 <- model.matrix(
  ~ time +
    age_c +
    bmi_c +
    sex +
    smoker +
    alcohol_units_week_c +
    sleep_hours_c +
    exercise_hours_week_c +
    lab_marker_03_c +
    lab_marker_07_c +
    lab_marker_12_c +
    time_num:age_c +
    time_num:bmi_c +
    time_num:lab_marker_03_c,
  data = sbp_long
)

qr(X11)$rank
ncol(X11)
summary(model11.lme)
anova(model9.lme, model11.lme)

#Ending at model11.lme
#Time to visualize
if(FALSE) {
bmi_c_predict <- ggpredict(
  model11.lme,
  terms = c("time_num [0,6,12]", "bmi_c [-10, 10]")
)

age_c_predict <- ggpredict(
  model11.lme,
  terms = c("time_num [0,6,12]", "age_c [-10, 10]")
)

lab3_c_predict <- ggpredict(
  model11.lme,
  terms = c("time_num [0,6,12]", "lab_marker_03_c [-10, 10]")
)
}
anova(model10.lme1, model10.lme2, model10.lme6, model11.lme)
AIC(model10.lme1, model10.lme2, model10.lme6, model11.lme)
BIC(model10.lme1, model10.lme2, model10.lme6, model11.lme)

set.seed(3960)

train_ids.11 <- sample(unique(sbp_long$id), size = 0.8 * n_distinct(sbp_long$id))
#Trainging the model on this data - 24,000 observations from long version
#Of original data. 
training_data.11 <- sbp_long %>% filter(id %in% train_ids.11)
#Then testing the model on this data - 6000 observations from data set of long
#Where each participant recorded 3 times, for each sbp check at baseline, 6 and 12
test_data.11 <- sbp_long %>% filter(!id %in% train_ids.11)
nrow(training_data.11)
nrow(test_data.11)

model11_training <- lmer(
  sbp_value ~
    time +
    age_c +
    bmi_c +
    sex +
    smoker +
    alcohol_units_week_c +
    sleep_hours_c +
    exercise_hours_week_c +
    lab_marker_03_c +
    lab_marker_07_c +
    lab_marker_12_c +
    time_num:age_c +
    time_num:bmi_c +
    time_num:lab_marker_03_c + 
    (1 + time_num | id) +
    (1 + age_c | site_id),
  data = training_data.11
)

test_data.11$predicted_sbp <- predict(
  model11_training,
  newdata = test_data.11,
  re.form = NULL,
  allow.new.levels = TRUE
)

RMSE.11 <- sqrt(mean((test_data.11$sbp_value - test_data.11$predicted_sbp)^2, na.rm = TRUE))
MAE.11 <- mean(abs(test_data.11$sbp_value - test_data.11$predicted_sbp), na.rm = TRUE)

#baseline check
mean_sbp.11 <- mean(training_data.11$sbp_value, na.rm = TRUE)
null_rmse.11 <- sqrt(mean((test_data.11$sbp_value - mean_sbp.11)^2, na.rm = TRUE))


pander(cbind(
  "Test MAE (mmHg):" = round(MAE.11, 2),
  "Test RMSE (mmHg):" = round(RMSE.11, 2),
  "Null Model RMSE (mmHg):" = round(null_rmse.11, 2),
  "Out of Sample R^2 (%):", (round(1 - (RMSE.11^2 / null_rmse.11^2), 5))*100
))

ggplot(test_data.11, aes(x = predicted_sbp, y = sbp_value)) +
  geom_point(alpha = 0.25, color = "black") + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "12-Week SBP: Observed vs. Full Mixed Model 9 Predictions",
    subtitle = "Includes both patient (id) and clinic (site_id) random intercepts & slopes",
    x = "Predicted SBP (mmHg)",
    y = "Observed SBP (mmHg)"
  ) +
  theme_minimal()
#Assumptions Check
check_model(model11.lme)
#Fixed Effects forest plot
plot_model(model11.lme, type = "est", sort.est = TRUE, vline.color = "gray")
# marginal trajectory plot for three interactions
p_bmi <- plot_predictions(model11.lme, condition = list("time_num", "bmi_c" = "threenum"))
p_age <- plot_predictions(model11.lme, condition = list("time_num", "age_c" = "threenum"))
p_lab <- plot_predictions(model11.lme, condition = list("time_num", "time_num" = "threenum"))
(p_bmi | p_age | p_lab) + plot_layout(guides = "collect")

re_plots <- plot_model(model11.lme, type = "re")
re_plots[[2]]


