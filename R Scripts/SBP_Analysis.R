source("Packages.R")
practice_data_clean <- read_csv("~/Multivariate_Practice/Data/practice_data.clean.csv")
View(practice_data_clean)

pander(summary(practice_data_clean$sbp))
hist(practice_data_clean$sbp, breaks = 40)

pander(summary(practice_data_clean[, 
                                   c("age", "bmi", "alcohol_units_week",
                                     "sleep_hours",
                                     "exercise_hours_week")]))
plot(practice_data_clean$age, practice_data_clean$sbp)
plot(practice_data_clean$bmi, practice_data_clean$sbp)
plot(practice_data_clean$alcohol_units_week, practice_data_clean$sbp)
plot(practice_data_clean$sleep_hours, practice_data_clean$sbp)
plot(practice_data_clean$exercise_hours_week, practice_data_clean$sbp)
boxplot(sbp ~ smoker, data = practice_data_clean)
boxplot(sbp ~ sex, data = practice_data_clean)

model1.lm <- lm(sbp ~ age +
                  bmi +
                  sex +
                  smoker +
                  alcohol_units_week +
                  sleep_hours +
                  exercise_hours_week,
                data = practice_data_clean)
summary(model1.lm)

par(mfrow=c(2,2))
plot(model1.lm)

model1.augment <- augment(model1.lm) %>% 
  mutate(index = 1:n()) %>% 
  select(index, everything())
pander(head(model1.augment), 50)

par(mfrow=c(1,1))
plot(model1.lm, 4, id.n = 5)
cooks_data <- model1.augment %>% 
  top_n(10, wt = .cooksd)

plot_data <- model1.augment %>% 
  mutate(index = row_number())
n <- nrow(plot_data)
p <- length(coef(model1.lm))
ggplot(plot_data, aes(x = index, y = .hat)) +
  geom_segment(aes(xend = index, yend = 0), color = "gray50") +
  geom_point(size = 2, color = "steelblue") +
  geom_hline(yintercept = c(2 * p / n, 3 * p / n), color = "red", linetype = "dashed") +
  labs(
    title = "Index Plot of Hat Values (Leverage)",
    x = "Observation Index",
    y = "Hat Value"
  ) +
  theme_minimal()
hat_data <- model1.augment %>% 
  top_n(10, wt = .hat)


#Common high cooks distance and hat values observations. 

common_ids <- cooks_data$index[cooks_data$index %in% hat_data$index]
print(common_ids)


pander(model1.augment[c(2711, 5859, 5979, 6002, 8262), ])

vif(model1.lm)

#No significant multicollinarity 

influencePlot(model1.lm)

# Different individuals for influence plot based on Cooks D, Std.residual, and Hat

pander(model1.augment[c(1758, 5859, 5979, 8999, 9378), ])

#Across both, 5859, 5979 are shown as influential in top 5. 
#See in plot they are large points

print("Influence diagnostics identified observations 5859 and 5979 as the most 
      potentially influential cases. Both observations exhibited high leverage, 
      large Cook's Distance values, and studentized residuals exceeding 2 in 
      absolute value. Cases 8999 and 9378 showed unusually large residuals but 
      low leverage, suggesting limited influence on model estimates despite 
      being response outliers. Observation 1758 demonstrated high leverage but 
      a relatively small residual and therefore had less influence on model 
      coefficients.")

model1.augment %>%
  filter(index %in% c(5859, 5979))
#See alcohol consumption for the 2 observations is over 160 units higher then
#The population average. 
#Standardize these 
Mean <- 13.25
SD <- 18.04
Standard_5859 <- (220 - Mean)/SD
Standard_5979 <- (179 - Mean)/SD
Standard_5859
Standard_5979
#Check alcohol against others
hist(practice_data_clean$alcohol_units_week)
boxplot(practice_data_clean$alcohol_units_week)
plot(practice_data_clean$alcohol_units_week,
     residuals(model1.lm))
#These are much larger then the rest of the observations. 
#Remove these observations to check again

#Test 1, Remove high influence points
model1_noinf <- lm(
  sbp ~ age + bmi + sex + smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week,
  data = practice_data_clean[-c(5859,5979), ]
)
par(mfrow=c(2,2))
plot(model1_noinf)
par(mfrow=c(1,1))
plot(model1_noinf, 4)
vif(model1_noinf)
influencePlot(model1_noinf)

summary(model1.lm)
anova(model1.lm)
summary(model1_noinf)
anova(model1_noinf)

coef(model1.lm)
coef(model1_noinf)
#Not much change
#We should not remove the units as they dont change much. 

#test 2, Log transform Alcohol Units Per Week
model1.lm.transformed <- lm(sbp ~ age +
                  bmi +
                  sex +
                  smoker +
                  log1p(alcohol_units_week) +
                  sleep_hours +
                  exercise_hours_week,
                data = practice_data_clean)
summary(model1.lm.transformed)
anova(model1.lm.transformed)

par(mfrow=c(2,2))
plot(model1.lm.transformed)
par(mfrow=c(1,1))
plot(model1.lm.transformed, 4)
vif(model1.lm.transformed)
influencePlot(model1.lm.transformed)

print("Two observations (5859 and 5979) were identified as influential through
      Cook's Distance and leverage diagnostics. Examination of the data 
      revealed extremely high alcohol consumption values (220 and 179 units 
      per week respectively), compared with a sample mean of 13.25 units 
      (SD = 18.04). A sensitivity analysis excluding these observations 
      produced only negligible changes to coefficient estimates and model 
      fit statistics, indicating that the substantive conclusions were robust 
      to their inclusion. A log-transformation of alcohol consumption was also 
      explored. Although the transformation reduced leverage associated with
      the extreme values, overall model fit decreased 
      (Adjusted R² = 0.266 versus 0.273). Consequently, the original 
      specification was retained for subsequent analyses.")


#Move to General Linear Model
model3.lm <- lm(
  sbp ~ age + bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    region +
    education +
    diet_type,
  data=practice_data_clean
)

summary(model3.lm)
anova(model3.lm)
anova(model1.lm, model3.lm)
AIC(model1.lm, model3.lm)
BIC(model1.lm, model3.lm)


print("Multiple linear regression was used to investigate factors associated 
      with systolic blood pressure. Higher age (β = 0.39, p < .001), 
      BMI (β = 0.77, p < .001), alcohol consumption (β = 0.12, p < .001), and 
      male sex (β = 4.43, p < .001) were associated with higher SBP, while 
      greater sleep duration (β = -1.11, p < .001) and exercise participation 
      (β = -0.38, p < .001) were associated with lower SBP. After adjustment 
      for these variables, region remained statistically significant 
      (p = .014), whereas education (p = .077) and diet type (p = .721) did not.
      Comparison of the baseline model and the general linear model indicated a 
      statistically significant improvement in fit, F(9, 9982) = 2.09, p = .027, 
      although the increase in explained variance was small 
      (R² = 0.274 vs. 0.275). Influential observations with extreme alcohol 
      consumption were investigated through sensitivity analysis; their removal 
      produced negligible changes in coefficient estimates, so they were 
      retained in the final model. Adding region, education, and diet type 
      produced a statistically significant improvement in model 
      fit (F(9, 9982) = 2.09, p = .027). However, the increase in explanatory 
      power was very small (R² increased from 0.274 to 0.275). Model comparison 
      using information criteria produced mixed results. AIC showed a negligible 
      preference for the expanded model (ΔAIC = 0.84), whereas BIC strongly 
      favoured the more parsimonious baseline model (ΔBIC = 64.06). Consequently, 
      although region contributed some additional explanatory value, 
      the practical improvement offered by the expanded model was limited.")
#Fit model 4, remove insignificant categorical variables. 
model4.lm <- lm(sbp ~ age + bmi +
                  sex +
                  smoker +
                  alcohol_units_week +
                  sleep_hours +
                  exercise_hours_week +
                  region,
                data=practice_data_clean
)
summary(model4.lm)
anova(model4.lm)
anova(model1.lm, model4.lm)
anova(model1.lm, model4.lm, model3.lm)
AIC(model1.lm, model4.lm, model3.lm)
BIC(model1.lm, model4.lm, model3.lm)

################ SBP lab markers

modellab1.lm <- lm(sbp ~ age + bmi + sex +
                     lab_marker_01 + lab_marker_02 + lab_marker_03 +
                     lab_marker_04 + lab_marker_05 +
                     lab_marker_06 + lab_marker_07 + lab_marker_08 +
                     lab_marker_09 + lab_marker_10 +
                     lab_marker_11 + lab_marker_12 + lab_marker_13 +
                     lab_marker_14 + lab_marker_15,
                   data = practice_data_clean)
vif(modellab1.lm)
lab_data <- practice_data_clean %>% 
  select(age, bmi,sex, lab_marker_01, lab_marker_02, lab_marker_03,
         lab_marker_04, lab_marker_05,
         lab_marker_06, lab_marker_07, lab_marker_08,
         lab_marker_09, lab_marker_10,
         lab_marker_11, lab_marker_12, lab_marker_13,
         lab_marker_14, lab_marker_15)
ggcorrplot(cor(lab_data[, -3]),
           method = "circle",
           hc.order = TRUE,
           type = "lower")
#Lasso
X <- model.matrix(sbp ~ age + bmi + sex +
                    lab_marker_01 + lab_marker_02 + lab_marker_03 +
                    lab_marker_04 + lab_marker_05 +
                    lab_marker_06 + lab_marker_07 + lab_marker_08 +
                    lab_marker_09 + lab_marker_10 +
                    lab_marker_11 + lab_marker_12 + lab_marker_13 +
                    lab_marker_14 + lab_marker_15,
                  data = practice_data_clean
)[, -1]
Y <- practice_data_clean$sbp

cv.lasso.lab <- cv.glmnet(
  X,
  Y,
  alpha = 1,
  family = "gaussian"
)
plot(cv.lasso.lab)
#Penalty for Lasso
cv.lasso.lab$lambda.min
coef(cv.lasso.lab,
     s = "lambda.min")
coef(cv.lasso.lab, 
     s = "lambda.1se")
#Markers 3, 7, and 12 all in their own clusters show strongest, with marker 1 surviving as well but quite small. 

#Ridge

cv.ridge.lab <- cv.glmnet(
  X,
  Y,
  alpha = 0)

coef(
  cv.ridge.lab,
  s = "lambda.min"
)
coef(
  cv.ridge.lab,
  s = "lambda.1se"
)
plot(cv.ridge.lab)
#Ridge kept all predictor lab markers in both methods, due to alpha = 0

#Elastic

cv.elastic.lab <- cv.glmnet(
  X,
  Y,
  alpha = 0.5)

coef(
  cv.elastic.lab,
  s = "lambda.min"
)
coef(
  cv.elastic.lab,
  s = "lambda.1se"
)
plot(cv.elastic.lab)
#Elastics main large lab markers were 3, 7, and 12 like Lasso
#This shows that each correlated block of markers, 3, 7, and 12 are the main focus point

min(cv.lasso.lab$cvm)
min(cv.ridge.lab$cvm)
min(cv.elastic.lab$cvm)
mse_lasso <- cv.lasso.lab$cvm[cv.lasso.lab$lambda == cv.lasso.lab$lambda.min]
mse_ridge <- cv.ridge.lab$cvm[cv.ridge.lab$lambda == cv.ridge.lab$lambda.min]
mse_elnet <- cv.elastic.lab$cvm[cv.elastic.lab$lambda == cv.elastic.lab$lambda.min]

model_comparison <- data.frame(
  Model = c("Lasso", "Ridge", "Elastic"),
  Lambda = c(cv.lasso.lab$lambda.min, cv.ridge.lab$lambda.min, cv.elastic.lab$lambda.min),
  MSE = c(mse_lasso, mse_ridge, mse_elnet)
)
pander(model_comparison)

print("Correlation analysis revealed three clusters of laboratory markers. 
      Although VIF values were modest (approximately 2.3), regularized 
      regression methods were used to account for the correlated structure 
      among predictors. Ridge regression retained all laboratory markers 
      while shrinking coefficient estimates toward zero. LASSO and Elastic 
      Net performed variable selection and consistently identified lab_marker_03, 
      lab_marker_07, and lab_marker_12 as the strongest laboratory predictors 
      of systolic blood pressure after adjustment for age, BMI, and sex. 
      Cross-validated predictive performance was very similar across methods 
      (CV-MSE ≈ 196.5-196.6). Given the negligible difference in predictive 
      accuracy, the Elastic Net 1-SE solution was preferred because it produced 
      the most parsimonious and interpretable model while retaining the key 
      laboratory markers.")

# Model Model based on Elastic 

model5.lm <- lm(
  sbp ~ age + bmi + sex + lab_marker_03 + lab_marker_07 + lab_marker_12,
  data = practice_data_clean)
summary(model5.lm)
summary(model4.lm)
par(mfrow=c(2,2))
plot(model5.lm)
par(mfrow=c(1,2))
influencePlot(model5.lm)
plot(model5.lm, 4)
removed_data <- practice_data_clean[-c(393, 8733, 3437, 5859, 8733, 8999, 9378),]

#Fit Model again removing high influence points
model5.lm2 <- lm(
  sbp ~ age + bmi + sex + lab_marker_03 + lab_marker_07 + lab_marker_12,
  data = removed_data)
summary(model5.lm2)
summary(model5.lm)

#Compare model5.lm to model modellab1.lm
AIC(model5.lm, modellab1.lm)
BIC(model5.lm, modellab1.lm)
anova(model5.lm, modellab1.lm)
#BIC and AIC prefer our new model5.lm, but anova thinks modellab1.lm fits better.

#Combine Model4 and model5 into model6, then compare. 
#Combining the lifestyle epidemological and biomarker models together
model6.lm <- lm(sbp ~ age + bmi + sex + 
                  smoker + alcohol_units_week + sleep_hours +
                  exercise_hours_week + region + lab_marker_03 + 
                  lab_marker_07 + lab_marker_12, 
                data = practice_data_clean)
summary(model6.lm)
par(mfrow=c(2,2))
plot(model6.lm)
par(mfrow=c(1,2))
influencePlot(model6.lm)
plot(model6.lm, 4)
vif(model6.lm)
anova(model5.lm, model6.lm)
anova(model4.lm, model6.lm)

AIC(model4.lm, model5.lm, model6.lm)
BIC(model4.lm, model5.lm, model6.lm)

#Model 6 is substantially better in all aspects compared to both model 4 and 5.
#no multicollinearity between bio markers, all significant except 2 regions,
#and the variation explained by model is higher compared to the previous models. 

#Linear Mixed Effect model of sbp and clinic clusters
practice_data_clean$age_c <-
  scale(
    practice_data_clean$age,
    center = TRUE,
    scale = FALSE
  )
mean(practice_data_clean$age_c)

nullmodel.lme <- lmer(
  sbp ~ 1 + (1 | site_id),
  data = practice_data_clean
)

(icc(nullmodel.lme))*100

# A null random-intercept model demonstrated clustering of SBP within clinics, 
#with an intraclass correlation coefficient (ICC) of 5.17%, indicating that 
#approximately 5% of SBP variation occurred between clinics. This justified 
#the use of multilevel modellin

#Random Intercept Model
model4.lme_RI <- lmer(sbp ~ age_c + bmi +
                 sex +
                 smoker +
                 alcohol_units_week +
                 sleep_hours +
                 exercise_hours_week +
                 region + (1 | site_id), 
               data=practice_data_clean)
#Random Intercept + Random SLope of Age_c
model4.lme_RIS <- mixed2 <- lmer(
  sbp ~ age_c +
    bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    region +
    (1 + age_c | site_id),
  data = practice_data_clean
)
anova(nullmodel.lme,
      model4.lme_RI,
      model4.lme_RIS)
AIC(nullmodel.lme,
    model4.lme_RI,
    model4.lme_RIS)
BIC(nullmodel.lme,
    model4.lme_RI,
    model4.lme_RIS)

ranef(model4.lme_RIS)$site_id
plot_model(
  model4.lme_RIS,
  type = "re",
  linewidth = 1
)
summary(model4.lme_RIS)


model6.lme_RIS <- mixed2 <- lmer(
  sbp ~ age_c +
    bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    region +
    lab_marker_03 + 
    lab_marker_07 + 
    lab_marker_12 +
    (1 + age_c | site_id),
  data = practice_data_clean
)
anova(model4.lme_RIS, model6.lme_RIS)
AIC(model4.lme_RIS, model6.lme_RIS)
BIC(model4.lme_RIS, model6.lme_RIS)
plot_model(
  model6.lme_RIS,
  type = "re",
  linewidth = 0
)
summary(model6.lme_RIS)

#SBP over time
sbp_long <- practice_data_clean %>%
  select(
    id,
    age,
    age_c,
    bmi,
    sex,
    smoker,
    alcohol_units_week,
    sleep_hours,
    exercise_hours_week,
    region,
    lab_marker_03,
    lab_marker_07,
    lab_marker_12,
    sbp,
    sbp_wk6,
    sbp_wk12,
    site_id
  ) %>%
  pivot_longer(
    cols = c(sbp, sbp_wk6, sbp_wk12),
    names_to = "time",
    values_to = "sbp_value"
  )
sbp_long$time_num <- case_when(
  sbp_long$time == "sbp" ~ 0,
  sbp_long$time == "sbp_wk6" ~ 6,
  sbp_long$time == "sbp_wk12" ~ 12)

sbp_long$time <- factor(sbp_long$time,
  levels = c("sbp", "sbp_wk6","sbp_wk12"))
sbp_long %>%
  group_by(time) %>%
  summarise(mean_sbp = mean(sbp_value))
#Examine change at population level overtime
ggplot(sbp_long,aes(time, sbp_value)) +
  stat_summary(fun = mean,
    geom = "line",
    group = 1) +
  stat_summary(fun = mean,geom = "point")

#Fitting Models
#Model 1 Random Intecept only
model.repeat.RI <- lmer(sbp_value ~ time +
                          (1 | id),
                        data = sbp_long)

summary(model.repeat.RI)
#Model 2 Random Intercept + Slope time_num
model.repeat.RS <- lmer(
  sbp_value ~ time_num +
    (1 + time_num | id),
  data = sbp_long)

summary(model.repeat.RS)

anova(model.repeat.RI,model.repeat.RS)
AIC(model.repeat.RI,model.repeat.RS)
BIC(model.repeat.RI,model.repeat.RS)
#Model 3 Random Intercept + Slope time_num and predictors
model.repeat.RS.1 <- lmer(
  sbp_value ~ time_num + age + bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    region +
    lab_marker_03 + 
    lab_marker_07 + 
    lab_marker_12 +
    (1 + time_num | id),
  data = sbp_long)
summary(model.repeat.RS.1)
anova(model.repeat.RI,model.repeat.RS, model.repeat.RS.1)
AIC(model.repeat.RI,model.repeat.RS, model.repeat.RS.1)
BIC(model.repeat.RI,model.repeat.RS, model.repeat.RS.1)

#COMBINE FOR MODEL 7 FINAL!

model7.lme <- lmer(
  sbp_value ~
    time_num +
    age_c +
    bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    region +
    lab_marker_03 +
    lab_marker_07 +
    lab_marker_12 +
    (1 + time_num | id) +
    (1 + age_c | site_id),
  data = sbp_long
)
summary(model7.lme)

anova(model.repeat.RS.1, model7.lme)
AIC(model.repeat.RS.1, model7.lme)
BIC(model.repeat.RS.1, model7.lme)

model8.lme <- lmer(
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
  data = sbp_long
)
summary(model8.lme)

anova(model7.lme, model8.lme)
AIC(model7.lme, model8.lme)
BIC(model7.lme, model8.lme)
#Region does nothing now and gets completely explained by repeated time measure and clinics