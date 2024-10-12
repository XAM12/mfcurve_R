# Load required libraries
library(dplyr)
library(mfcurve)


# Load the synthetic dataset from the 'comparison' folder
data <- read.csv("comparison/Simulated_Dataset.csv")

# Inspect the data to ensure it's loaded correctly
print(head(data))

# Convert 'race' to a numeric factor (like we did in Stata)
data$race_num <- as.numeric(as.factor(data$race))

# Run the mfcurve_preprocessing function with 'race_num' as a factor
preprocess_result <- mfcurve_preprocessing(data, outcome_var = "wage", factors = c("race_num", "south", "union"))

# Print the results to check the group means and standard deviations
print(preprocess_result)

# Save the preprocessing result to the 'comparison' folder for comparison with Stata
write.csv(preprocess_result, "comparison/r_group_stats.csv", row.names = FALSE)

# Notify the user that the file has been saved
cat("Preprocessing results saved to comparison/r_group_stats.csv\n")
