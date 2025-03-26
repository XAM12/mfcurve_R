###############################################################################
#                           Load Required Packages                            #
###############################################################################
library(magrittr)
library(dplyr)

###############################################################################
#                           Set Working Directory                             #
###############################################################################
# Adjust the path as needed
setwd("/Users/claudia.weileder/Desktop/mfcurve_dev/mfcurve")

###############################################################################
#                        Source Package & Function Files                      #
###############################################################################
# Ensure that the following files are located in the "R/" directory:
# - package.R
# - preprocessing.R
# - stat_testing.R
# - plotting.R
source("R/package.R")
source("R/preprocessing.R")
source("R/stat_testing.R")
source("R/plotting.R")

###############################################################################
#                              Minimal Example                                #
###############################################################################

## Simulated Dataset (Minimal Example)
# Generate a dataset with 1000 observations.
# 'wage' is normally distributed, and factors ('race', 'south', 'union') are randomly assigned.
set.seed(123)
n <- 1000
df <- data.frame(
  wage = rnorm(n, mean = 15, sd = 5),
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),
  south = sample(c("Yes", "No"), n, replace = TRUE),
  union = sample(c("Yes", "No"), n, replace = TRUE)
)

## Minimal Data Preprocessing
# Required parameters: data, outcome_var, and factors.
preprocessed_data <- mfcurve_preprocessing(
  data = df,
  outcome_var = "wage",
  factors = c("race", "south", "union")
)
# cat("Preprocessed Data:\n")
# print(head(preprocessed_data))

## Minimal Statistical Testing
# Only the required parameter 'data' is provided.
stat_test_results <- mfcurve_stat_testing(
  data = preprocessed_data
)
# cat("\nStatistical Test Results:\n")
# print(head(stat_test_results))

## Minimal Interactive Visualization
# Required parameters: stats, factors, and outcome.
plot_minimal <- mfcurve_plotting(
  stats = stat_test_results,
  factors = c("race", "south", "union"),
  outcome = "wage"
)
# Display the minimal plot
print(plot_minimal)

###############################################################################
#                           Extended Example                                  #
###############################################################################

## Simulated Dataset (Extended Example)
set.seed(123)
n <- 1000
df <- data.frame(
  wage = rnorm(n, mean = 15, sd = 5),
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),
  south = sample(c("Yes", "No"), n, replace = TRUE),
  union = sample(c("Yes", "No"), n, replace = TRUE)
)

## Data Preprocessing
# This function:
# - Removes missing values.
# - Creates a group identifier by combining the factor levels.
# - Calculates group-level summary statistics and ranks the groups.
preprocessed_data <- mfcurve_preprocessing(
  data = df,
  outcome_var = "wage",
  factors = c("race", "south", "union")
)
cat("Preprocessed Data:\n")
print(head(preprocessed_data))

## Statistical Testing
# This function performs a t-test for each group, comparing the group mean against the grand mean.
# It calculates t-values, p-values, and confidence intervals.
stat_test_results <- mfcurve_stat_testing(
  data = preprocessed_data,
  test = "mean",   # Compare group means with the grand mean
  alpha = 0.05     # Significance level of 5%
)
cat("\nStatistical Test Results:\n")
print(head(stat_test_results))

## Interactive Visualization Examples

### Example Plot 1: Collapsed Mode, Default Color Scheme
# - Lower panel shows only the factor names.
# - Upper panel's y-axis remains scrollable.
plot1 <- mfcurve_plotting(
  stats = stat_test_results,
  factors = c("race", "south", "union"),
  outcome = "wage",
  alpha = 0.05,
  showTitle = TRUE,
  mode = "collapsed",       # "collapsed": only factor names in the lower panel
  upper_fixed_range = FALSE, # Upper panel y-axis is scrollable
  color_scheme = "default",  # Default (colored) palette
  plotOrigin = FALSE         # Do not force (0,0) to be included
)

### Example Plot 2: Expanded Mode, Fixed Upper Y-Axis, Black-and-White Color Scheme
# - Lower panel displays both factor names and levels.
# - Upper panel's y-axis is fixed (no scrolling).
plot2 <- mfcurve_plotting(
  stats = stat_test_results,
  factors = c("race", "south", "union"),
  outcome = "wage",
  alpha = 0.05,
  showTitle = TRUE,
  mode = "expanded",         # "expanded": factor names with levels (e.g., "race White")
  upper_fixed_range = TRUE,  # Fix the y-axis in the upper panel
  color_scheme = "bw",       # Black-and-white (grayscale) palette
  plotOrigin = FALSE         # Do not force (0,0) to be included
)

### Example Plot 3: Collapsed Mode with Coordinate Origin Included
# - Forces the x-axis to start at 0 and extends the upper panel accordingly.
plot3 <- mfcurve_plotting(
  stats = stat_test_results,
  factors = c("race", "south", "union"),
  outcome = "wage",
  alpha = 0.05,
  showTitle = TRUE,
  mode = "collapsed",
  upper_fixed_range = FALSE,
  color_scheme = "default",
  plotOrigin = TRUE         # Include coordinate origin (0,0) in the plot
)

### Example Plot 4: Expanded Mode with Coordinate Origin Included, Default Color Scheme
# - Combines an expanded lower panel with (0,0) inclusion.
plot4 <- mfcurve_plotting(
  stats = stat_test_results,
  factors = c("race", "south", "union"),
  outcome = "wage",
  alpha = 0.05,
  showTitle = TRUE,
  mode = "expanded",
  upper_fixed_range = FALSE, # Upper panel remains scrollable
  color_scheme = "default",
  plotOrigin = TRUE          # Include coordinate origin (0,0) in the plot
)

## Display the Extended Example Plots
print(plot1)  # Collapsed mode, default colors, scrollable y-axis.
print(plot2)  # Expanded mode, black-and-white, fixed y-axis.
print(plot3)  # Collapsed mode with coordinate origin included.
print(plot4)  # Expanded mode with coordinate origin included.


# preprocessing und stat_testing in eine Funktion
# expanded mode hat kringel --> die sollen ausgefüllt
# line spacing um die faktoren weiter voneinander trennen
# bei plotOrigin true sind Achsen komisch dargestellt
# KI anzeigen ja/nein im plotting befehl und nicht interaktiv //Max
# Gruppengrößen mit in der hoverbox anzeigen + vllt als printoption für die, die png und nicht interaktiv//Max
# Signifikante sachen flaggen beim testen //Max
# signifikante sachen raute statt kreis //Max
