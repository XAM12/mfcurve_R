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
#                              Minimal Example                                #
###############################################################################

## Simulated Dataset (Minimal Example)
# Generate a dataset with 1000 observations.
# 'wage' is normally distributed, and factors ('race', 'south', 'union') are randomly assigned.
library(tidyverse)
set.seed(123)
n <- 1000
df <- data.frame(
  wage = rnorm(n, mean = 15, sd = 5),
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),
  south = sample(c("Yes", "No"), n, replace = TRUE),
  union = sample(c("Yes", "No"), n, replace = TRUE)
)

## Minimal Data Preprocessing and Statistical Testing
# This function removes missing values, creates a group identifier from the factors,
# calculates group-level summary statistics (including t-tests, confidence intervals, and significance),
# and prepares the data for plotting.
analysis_results_min <- mfcurve_preprocessing(
  data = df,
  outcome = "wage",
  factors = c("race", "south", "union")
)

## Minimal Interactive Visualization
# The plotting function creates an interactive Plotly plot that combines an upper panel (group means with
# confidence intervals and group sizes in the hover text) and a lower panel with factor level labels.
plot_minimal <- mfcurve_plotting(
  processed_data = analysis_results_min,
  outcome = "wage",
  showTitle = TRUE
)
print(plot_minimal)

###############################################################################
#                           Extended Example                                  #
###############################################################################

## For extended examples, we vary the preprocessing parameters 'mode' and 'plotOrigin'
## to see differences in the lower panel labels and axis settings.
## (Note: parameters such as 'upper_fixed_range' and 'color_scheme' have been removed,
##  as the current functions use default settings.)

# Example Plot 1: Collapsed Mode, default plotOrigin (FALSE)
analysis_results1 <- mfcurve_preprocessing(
  data = df,
  outcome = "wage",
  factors = c("race", "south", "union"),
  mode = "collapsed",   # Lower panel shows only factor names.
  plotOrigin = FALSE    # Let Plotly determine axis limits.
)
plot1 <- mfcurve_plotting(
  processed_data = analysis_results1,
  outcome = "wage",
  showTitle = TRUE
)

# Example Plot 2: Expanded Mode, default plotOrigin (FALSE)
analysis_results2 <- mfcurve_preprocessing(
  data = df,
  outcome = "wage",
  factors = c("race", "south", "union"),
  mode = "expanded",    # Lower panel shows factor names with levels (e.g., "race White").
  plotOrigin = FALSE
)
plot2 <- mfcurve_plotting(
  processed_data = analysis_results2,
  outcome = "wage",
  showTitle = TRUE
)

# Example Plot 3: Collapsed Mode with Coordinate Origin Included
analysis_results3 <- mfcurve_preprocessing(
  data = df,
  outcome = "wage",
  factors = c("race", "south", "union"),
  mode = "collapsed",
  plotOrigin = TRUE     # Force axes to include the origin.
)
plot3 <- mfcurve_plotting(
  processed_data = analysis_results3,
  outcome = "wage",
  showTitle = TRUE
)

# Example Plot 4: Expanded Mode with Coordinate Origin Included
analysis_results4 <- mfcurve_preprocessing(
  data = df,
  test = "zero",
  outcome = "wage",
  factors = c("race", "south", "union"),
  mode = "expanded",
  plotOrigin = TRUE
)
plot4 <- mfcurve_plotting(
  processed_data = analysis_results4,
  outcome = "wage",
  showTitle = TRUE
)

## Display the Extended Example Plots
print(plot1)  # Collapsed mode, default axis limits.
print(plot2)  # Expanded mode, default axis limits.
print(plot3)  # Collapsed mode with (0,0) included in axes.
print(plot4)  # Expanded mode with (0,0) included in axes.


# preprocessing und stat_testing in eine Funktion
# expanded mode hat kringel --> die sollen ausgefüllt
# line spacing um die faktoren weiter voneinander trennen
# bei plotOrigin true sind Achsen komisch dargestellt
# KI anzeigen ja/nein im plotting befehl und nicht interaktiv //Max
# Gruppengrößen mit in der hoverbox anzeigen + vllt als printoption für die, die png und nicht interaktiv//Max
# Signifikante sachen flaggen beim testen //Max
# signifikante sachen raute statt kreis //Max
