source("Packages.R")

practice_data_clean <- read_csv("~/Multivariate_Practice/Data/practice_data.clean.csv")

pander(summary(practice_data_clean$gp_visits))
pander(c("Variance" = var(practice_data_clean$gp_visits), 
         "Mean" = mean(practice_data_clean$gp_visits)))
hist(practice_data_clean$gp_visits,
     main = "Count of GP visits Per Year",
     xlab = "GP Visits (Count)")
pander(summary(select(practice_data_clean,
               age,
               bmi,
               sex,
               primary_condition,
               exposure_years)))
model1.pois <- glm(
  gp_visits ~ age + bmi + sex + primary_condition + exposure_years,
  family = poisson,
  data = practice_data_clean
)
summary(model1.pois)
anova(model1.pois)

dispersion <- sum(residuals(model1.pois,
                            type = "pearson")^2) / df.residual(model1.pois)
dispersion

model2.pois <- glm(
  gp_visits ~ age + bmi + sex + primary_condition + 
    offset(log(exposure_years)),
  family = poisson,
  data = practice_data_clean
)
summary(model2.pois)

anova(model1.pois, model2.pois, test = "Chisq")
AIC(model1.pois, model2.pois)
BIC(model1.pois, model2.pois)


dispersion2 <- sum(residuals(model2.pois,
                             type = "pearson")^2) / df.residual(model2.pois)
dispersion2
dispersion - dispersion2

#IRR
cbind(
"IRR" = exp(coef(model2.pois)),
exp(confint(model2.pois))
)

pR2(model2.pois)
with(summary(model2.pois), 1 - deviance/null.deviance)

#Influential Points CHeck


influencePlot(model2.pois)
plot(model2.pois, 4)

influential_data <- practice_data_clean[-c(8248, 3056, 5823, 6383, 8733), ]

model2.pois.2 <- glm(
  gp_visits ~ age + bmi + sex + primary_condition + 
    offset(log(exposure_years)),
  family = poisson,
  data = influential_data
)
summary(model2.pois.2)
dispersion2.2 <- sum(residuals(model2.pois.2,
                            type = "pearson")^2) / df.residual(model2.pois.2)
dispersion2.2
dispersion2 - dispersion2.2

pR2(model2.pois.2)
with(summary(model2.pois.2), 1 - deviance/null.deviance)



cbind(
  "Model2 IRR" = exp(coef(model2.pois)),
  "Model2.2 IRR" = exp(coef(model2.pois.2)),
  "Relative Ratio" = exp(coef(model2.pois.2) - coef(model2.pois))
)

#negligable difference - choose model2.pois

