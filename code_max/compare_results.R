###############################################################################
#                           Set Working Directory                             #
###############################################################################
# Adjust the path as needed
setwd("/Users/claudia.weileder/Desktop/mfcurve_dev/mfcurve")

###############################################################################
#                        Source Package & Function Files                      #
###############################################################################
# Ensure that the following files are located in the "R/" directory:
# - mfcurve_preprocessing.R
# - mfcurve_plotting.R
source("R/preprocessing.R")
source("R/plotting.R")

###############################################################################
#                               Load Libraries                                #
###############################################################################
library(dplyr)
library(tidyr)
library(plotly)

###############################################################################
#                               Simulate Data                                 #
###############################################################################
set.seed(123)
n <- 1000
df <- data.frame(
  wage = rnorm(n, mean = 15, sd = 5),
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),
  south = sample(c("Yes", "No"), n, replace = TRUE),
  union = sample(c("Yes", "No"), n, replace = TRUE)
)

outcome_var <- "wage"
factor_vars <- c("race", "south", "union")

###############################################################################
#                           TEST CASE 1: Base + Rounding                      #
###############################################################################
prep1 <- mfcurve_preprocessing(df, outcome = outcome_var, factors = factor_vars)
plottest <- mfcurve_plotting(
  group_stats_vis = prep1$group_stats_vis,
  lower_data = prep1$lower_data,
  grand_mean = prep1$grand_mean,
  outcome = outcome_var,
  factors = factor_vars,
  level = prep1$level,
  rounding = 1
)
group_stats <- prep1$group_stats  # mimic SaveProcessedData = TRUE

###############################################################################
#                       TEST CASE 2: Mode = 'collapsed'                       #
###############################################################################
prep2 <- mfcurve_preprocessing(df, outcome = outcome_var, factors = factor_vars)
plottest_collapsed <- mfcurve_plotting(
  group_stats_vis = prep2$group_stats_vis,
  lower_data = prep2$lower_data,
  grand_mean = prep2$grand_mean,
  outcome = outcome_var,
  factors = factor_vars,
  level = prep2$level,
  mode = "collapsed"
)

###############################################################################
#                       TEST CASE 3: Mode = 'expanded'                        #
###############################################################################
prep3 <- mfcurve_preprocessing(df, outcome = outcome_var, test = "zero", factors = factor_vars)
plottest_expanded <- mfcurve_plotting(
  group_stats_vis = prep3$group_stats_vis,
  lower_data = prep3$lower_data,
  grand_mean = prep3$grand_mean,
  outcome = outcome_var,
  factors = factor_vars,
  level = prep3$level,
  mode = "expanded"
)

###############################################################################
#                       TEST CASE 4: CI = FALSE                               #
###############################################################################
prep4 <- mfcurve_preprocessing(df, outcome = outcome_var, factors = factor_vars)
plottest_no_CI <- mfcurve_plotting(
  group_stats_vis = prep4$group_stats_vis,
  lower_data = prep4$lower_data,
  grand_mean = prep4$grand_mean,
  outcome = outcome_var,
  factors = factor_vars,
  level = prep4$level,
  CI = FALSE,
  plotOrigin = TRUE
)

###############################################################################
#                       TEST CASE 5: CI = TRUE                                #
###############################################################################
prep5 <- mfcurve_preprocessing(df, outcome = outcome_var, factors = factor_vars)
plottest_with_CI <- mfcurve_plotting(
  group_stats_vis = prep5$group_stats_vis,
  lower_data = prep5$lower_data,
  grand_mean = prep5$grand_mean,
  outcome = outcome_var,
  factors = factor_vars,
  level = prep5$level,
  CI = TRUE
)
