##################################################################################################
# Course:            Data Science for Business Managers II 
# Lecturer:          Dr. Julian Runge
# Group 4:           Maximilian Drüschler, Ivana Grbus, Kim Hoffmann, Mika Sang, Wolfram Stahl
##################################################################################################
# Individual Assignment - Day 2 and Day 4
# Treatment effect and treatment effect heterogeneity

### set working environment
# add packages to the list as needed
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc", "Rmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest")

# install packages in list if needed
# lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# set wd
setwd("~/Desktop/PTMMA 25.nosync/Data Science for Business II/Day 2")

# load cleaned and prepared datasets
data <- read.csv("day2-data.csv", header=TRUE, sep=",")
data$X <- NULL
data$unique_id <- as.factor(data$unique_id)

# pull descriptive stats
summarySE(data, "d1_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

summarySE(data, "d7_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

# add winsorized revenue and repeat purchases for day 14
d <- .98
data$d14_rev_denoised <- data$d14_revenue
data$d14_rev_denoised[data$d14_revenue > quantile(filter(data,d14_revenue > 0)$d14_revenue, c(d))] <- quantile(filter(data,d14_revenue > 0)$d14_revenue, c(d))

data$d14_repeat_purchases <- data$d14_purchases-1
data$d14_repeat_purchases[data$d14_repeat_purchases<0] <- 0

summarySE(data, "d14_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

summarySE(data, "d30_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

### visualize treatment effects
# using test_bucket
plotmeans(d7_revenue ~ interaction(test_bucket, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          data=data,
          xlab="",
          ylab="Day 7 Gross Revenue",
          ylim=c(0.5,1.2),
          text.n.label="",
          main=c(""),
          p=0.95)

# using the prettier action2
data$action2 <- as.factor(data$action2)
data$treatment_group <- ordered(data$action2,
                        levels = c("2.99 USD", "4.99 USD", "29.99 USD", "Control"))

plotmeans(d7_revenue ~ interaction(treatment_group, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          data=data,
          xlab="",
          ylab="Day 7 Gross Revenue",
          ylim=c(0.5,1.2),
          text.n.label="",
          main=c(""),
          p=0.95)

### assess treatment effects using linear regression
# set the reference category for our independent variable / predictor
data$action2 <- relevel(data$action2, ref = "Control")

# estimate linear regression and show results
lm_treat = lm(d30_rev_denoised ~ action2, data = data)

summary(lm_treat) 



# find two heterogenity covariates through analysis and conceptual thinking

# conceptual thinking: RAM or device tier & country or country tier
# the more RAM, the more expensive the users phone is and therefore the user is wtp..
# same for device tier but phones already clustered into 5 tiers and therefore better for handling

# country tier 1 = "rich country" - country tier 5 = "poor country"
# wtp decreases with each country tier..

# analysis
lm_covar_ram <- lm(d30_rev_denoised ~ device_ram, data = data)
summary(lm_covar_ram)

lm_covar_country_tier <- lm(d30_rev_denoised ~ country_tier, data = data)
summary(lm_covar_country_tier)

lm_covar_device_tier <- lm(d30_rev_denoised ~ device_tier, data = data)
summary(lm_covar_device_tier)

# design a policy based on the two covariates
# code is only used to get scatter plots D30 Rev vs. covariate to decide about the policy shown in the ppt presentation
hist(data$device_ram)

plot(data$device_ram,data$d30_rev_denoised, ylab="D30 Rev Denoised", xlab="Device Ram", main="Ram vs Revenue")

data$device_tier <- as.factor(data$device_tier)
plot(data$device_tier,data$d30_rev_denoised, ylab="D30 Rev Denoised", xlab="# Device Tier", main="Device Tier vs Revenue")

data$country_tier <- as.factor(data$country_tier)
plot(data$country_tier,data$d30_rev_denoised, ylab="D30 Rev Denoised", xlab="Country Tier", main="Country Tier vs Revenue")

summary(data$device_ram)
summary(data$device_tier)
summary(data$country_tier)

# designing linear models for policy design and showing the results with summary()
lm_covar_policy <- lm(d30_rev_denoised ~ device_tier + country_tier, data = data)
summary(lm_covar_policy)

lm_covar_policy2 <- lm(d30_rev_denoised ~ device_tier * country_tier, data = data)
summary(lm_covar_policy2)

