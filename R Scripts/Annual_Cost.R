source("Packages.R")

practice_data_clean <- read_csv("~/Multivariate_Practice/Data/practice_data.clean.csv")

pander(summary(practice_data_clean$annual_cost))
sd(practice_data_clean$annual_cost)

hist(practice_data_clean$annual_cost,
     breaks = 50,
     main = "Annual Health Care Cost Per Person",
     xlab = "Annual Cost")
boxplot(practice_data_clean$annual_cost,
        main = "Annual Health Care Cost Per Person")
#Heavily Right Skewed
practice_data_clean %>% 
  filter(annual_cost == 0) %>% 
  tally()
qqnorm(practice_data_clean$annual_cost)
qqline(practice_data_clean$annual_cost)
#Due to heavy skewness, qq-plot shows no support for normality

hist(log(practice_data_clean$annual_cost),
     breaks = 50,
     main = "Log Annual Cost",
     xlab = "log (annual_cost)")
qqnorm(log(practice_data_clean$annual_cost))
qqline(log(practice_data_clean$annual_cost))

# Fit baseline model

modelcost1.lm <- lm(
  annual_cost ~ 
    age +
    bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    primary_condition,
  data = practice_data_clean)

summary(modelcost1.lm)
anova(modelcost1.lm)

vif(modelcost1.lm)

par(mfrow = c(2,2))
plot(modelcost1.lm)

#heteroscedasticity 
u2 <- residuals(modelcost1.lm)^2
aux_model <- lm(u2 ~ 
    age +
    bmi +
    sex +
    smoker +
    alcohol_units_week +
    sleep_hours +
    exercise_hours_week +
    primary_condition,
  data = practice_data_clean)
n <- nobs(modelcost1.lm)
r2 <- summary(aux_model)$r.squared
lm_stat <- n * r2

df.cost <- length(coef(modelcost1.lm)) - 1
p_val <- 1 - pchisq(lm_stat, df = df.cost)
c("LM-Statistic" = lm_stat, "p-value" = p_val)

#double check as p-value = 0
bptest(modelcost1.lm)
# Model 2 removing not needed variables

modelcost2.lm <- lm(
  annual_cost ~ 
    age +
    bmi +
    sex +
    smoker +
    primary_condition,
  data = practice_data_clean)

summary(modelcost2.lm)
anova(modelcost2.lm)

vif(modelcost2.lm)

par(mfrow = c(2,2))
plot(modelcost2.lm)
bptest(modelcost2.lm)
#influential points
cost.augment <- augment(modelcost2.lm) %>%
  mutate(index = 1:n())

cooks_data <- cost.augment %>%
  arrange(desc(.cooksd)) %>%
  select(index, .cooksd) %>%
  head(10)
hat_data <- cost.augment %>%
  arrange(desc(.hat)) %>%
  select(index, .hat) %>%
  head(10)
stud_data <- cost.augment %>%
  arrange(desc(abs(.std.resid))) %>%
  select(index, .std.resid) %>%
  head(10)
common_ids <- Reduce(
  intersect,
  list(
    cooks_data$index,
    hat_data$index,
    stud_data$index
  )
)
common_ids

par(mfrow = c(1,2))
influencePlot(modelcost2.lm, 10)
plot(modelcost2.lm, 4)

practice_data_clean$annual_cost[c(393, 3056, 3607, 7139, 8733)]

#Remove these influential observations and fit same model

data_remove_cost <- practice_data_clean[-c(393, 3056, 3607, 7139, 8733),]
modelcost2.lm.2 <- lm(
  annual_cost ~ 
    age +
    bmi +
    sex +
    smoker +
    primary_condition,
  data = data_remove_cost)
summary(modelcost2.lm.2)
anova(modelcost2.lm.2)

par(mfrow = c(2,2))
plot(modelcost2.lm.2)
par(mfrow = c(1,2))
influencePlot(modelcost2.lm.2, 10)
plot(modelcost2.lm.2, 4)

#Removing the points doesnt change model much, so we will keep them
#Moving onto General Linear Model and comparing it to modelcost2.lm

modelcost2.gamma <- glm(
  annual_cost ~ 
    age + 
    bmi + 
    sex + 
    smoker + 
    primary_condition,
  family = Gamma(link = "log"),
  data = practice_data_clean
)

summary(modelcost2.gamma)
anova(modelcost2.gamma)
#When interpreting, exp(coefficient) and go from there. 
#This is done in next line of code with confidence intervals. 
exp(cbind(
  Estimate = coef(modelcost2.gamma),
  confint(modelcost2.gamma)
))

AIC(modelcost2.lm, modelcost2.gamma)

par(mfrow = c(2,2))
plot(modelcost2.gamma)
par(mfrow = c(1,2))
influencePlot(modelcost2.gamma, 10)
plot(modelcost2.gamma, 4)

#Removing sex as insignificant
modelcost3.gamma <- glm(
  annual_cost ~ 
    age + 
    bmi + 
    smoker + 
    primary_condition,
  family = Gamma(link = "log"),
  data = practice_data_clean
)
summary(modelcost3.gamma)
anova(modelcost3.gamma, modelcost2.gamma)
AIC(modelcost3.gamma, modelcost2.gamma)
BIC(modelcost3.gamma, modelcost2.gamma)

par(mfrow = c(2,2))
plot(modelcost3.gamma)
par(mfrow = c(1,2))
influencePlot(modelcost3.gamma, 10)
plot(modelcost3.gamma, 4)

#Choose modelcost3.gamma as final model for prediction of annual costs.
