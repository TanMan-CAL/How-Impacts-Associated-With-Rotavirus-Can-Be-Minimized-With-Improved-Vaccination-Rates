install.packages("tidyverse", dependencies=T)
library(tidyverse)
library(ggplot2)
library(car)
setwd("D:\\Tanmay\\Coding Stuff\\RStudio\\projects")
master_csv = read.csv("finalDATASETS", header=TRUE)

vaccine_rates_below_80 = filter(master_csv, Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds.... <=80)
vaccine_rates_above_80 = filter(master_csv, Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds.... > 80)
vax_data_below = vaccine_rates_below_80$Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds....
vax_data_above = vaccine_rates_above_80$Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds....

couns_data_below = vaccine_rates_below_80$Entity
couns_data_above = vaccine_rates_above_80$Entity

countries_with_vaccs_below_80 = filter(master_csv, Entity %in% couns_data_below)
mortality_rates_below_80 = countries_with_vaccs_below_80$Child.mortality.from.rotavirus.per.100.000..IHME..2018.
health_care_expend_below_80 = vaccine_rates_below_80$Healthcare.spending
hdi_below_80 = vaccine_rates_below_80$hdix
gni_below_80 = vaccine_rates_below_80$gni

countries_with_vaccs_above_80 = filter(master_csv, Entity %in% couns_data_above)
mortality_rates_above_80 = countries_with_vaccs_above_80$Child.mortality.from.rotavirus.per.100.000..IHME..2018.
health_care_expend_above_80 = vaccine_rates_above_80$Healthcare.spending
hdi_above_80 = vaccine_rates_above_80$hdix
gni_above_80 = vaccine_rates_above_80$gni

master_csv$log_vacc_rates = log(master_csv$Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds....)



#MORTALITY RATES
t.test(mortality_rates_below_80, mortality_rates_above_80, alternative = "two.sided")
ggplot(master_csv, aes(x=Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds...., y = Child.mortality.from.rotavirus.per.100.000..IHME..2018.)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)
log_transform_mortality_rates = lm(formula = Child.mortality.from.rotavirus.per.100.000..IHME..2018.~log_vacc_rates, data = master_csv)
summary(log_transform_mortality_rates)  
# returns p-value of 0.001 with Pearson correlation
# minimal deviation with Q-Q plot with log_vac
plot(log_transform_mortality_rates)
residuals_mortality = residuals(object=log_transform_mortality_rates)
shapiro.test(x=residuals_mortality)
# Shapiro provides p-value greater than 0.05 so data does not deviate therefore supporting correlations



#HEALTH CARE EXPEND
t.test(health_care_expend_above_80, health_care_expend_below_80, alternative = "two.sided")
# p-value more than 0.05, thus ANOVA is conducted
ggplot(master_csv, aes(x=Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds...., y = Healthcare.spending)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)
log_transform_healthcare_expend = lm(formula = Healthcare.spending~log_vacc_rates, data = master_csv)
summary(log_transform_healthcare_expend)  
plot(log_transform_healthcare_expend)
residuals_healthcare_expend = residuals(object=log_transform_healthcare_expend)
shapiro.test(x=residuals_healthcare_expend)
# Shapiro test returns p-value < 0.05 (means data deviates), therefore  Spearman correlation
cor.test(master_csv$Healthcare.spending, master_csv$log_vacc_rates, method = "spearman")
# returns rho greater than 0.05, hence no correlation 
# ANOVA conducted as there is equal variance and normal distribution
anov_health_care = aov(health_care_expend_above_80~health_care_expend_below_80, master_csv)
# plot Q-Q graph shows significant non-normality, with ANOVA plot 
plot(anov_health_care)
summary(anov_health_care)
anova_health_care_resid = residuals(object=anov_health_care)
# not normal distribution
shapiro.test(x=anova_health_care_resid)
kruskal.test(health_care_expend_above_80~health_care_expend_below_80, master_csv)
# for health-care: Kruskal Wallis
# p-value greater than 0.05 indicating no correlation



#HDI
t.test(hdi_below_80, hdi_above_80, alternative = "two.sided")
ggplot(master_csv, aes(x=Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds...., y = hdix)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)
log_transform_hdi = lm(formula = hdix~log_vacc_rates, data = master_csv)
summary(log_transform_hdi)  
# returns p-value of 0.1 with Pearson correlation
# deviation with Q-Q plot with log_vac
plot(log_transform_hdi)
residuals_hdi = residuals(object=log_transform_hdi)
shapiro.test(x=residuals_hdi)
# shapiro retuns value of 0.015, so less than 0.05
cor.test(master_csv$hdi, master_csv$log_vacc_rates, method = "spearman")
# rho is too high, implying minimal correlation, but correlation is evident



#GNI
t.test(gni_above_80, gni_below_80, alternative = "two.sided")
ggplot(master_csv, aes(x=Indicator.Rotavirus.vaccines.completed.dose..RotaC..immunization.coverage.among.1.year.olds...., y = gni)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)
log_transform_gni = lm(formula = gni~log_vacc_rates, data = master_csv)
summary(log_transform_gni)  
plot(log_transform_gni)
residuals_gni = residuals(object=log_transform_gni)
shapiro.test(x=residuals_gni)
# Shapiro returns p-value of 1.022 x 10^-5
# do Spearman correlation
cor.test(master_csv$gni, master_csv$log_vacc_rates, method = "spearman")
# rho value suggests no correlations 
anov_gni = aov(gni_below_80~gni_above_80, master_csv)
summary(anov_gni)
# p-value of greater than 0.05
anov_gni_resid = residuals(object=anov_gni)
shapiro.test(x=anov_gni_resid)
plot(anov_gni)
# for GNI: Kruskal-Wallis, only ANOVA, as variance is normal





# EXTRA
games.howell <- function(grp, obs) {
  # Create combinations
  combs <- combn(unique(grp), 2)
  
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)
  
  statistics <- lapply(1:ncol(combs), function(x) {
    mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
    # t-values
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) + (std[combs[2,x]] / n[combs[2,x]]))
    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # numerator dof
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of denominator dof
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of denominator dof
    # p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    # sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 3)
  # Rename data frame columns
  colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit', 'lower limit')
  return(results)
}