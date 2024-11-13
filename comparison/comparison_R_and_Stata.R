# Load required libraries # must be changed but probably not nec in CRAN anyway
library(dplyr)
library(stringr)

# Load the R output
r_output <- read.csv("comparison/r_group_stats.csv")

# Load the Stata output
stata_output <- read.csv("comparison/stata_output.csv")

# Process the R output by splitting the 'group' column into components
r_output <- r_output %>%
  mutate(race_num_r = case_when(
    grepl("^1", group) ~ "black",
    grepl("^2", group) ~ "other",
    grepl("^3", group) ~ "white"
  ),
  south_r = as.numeric(str_extract(group, "(?<=\\.)\\d")),
  union_r = as.numeric(str_extract(group, "(?<=\\.\\d\\.)\\d")))

# Merge the R and Stata outputs based on group components
comparison_df <- merge(r_output, stata_output,
                       by.x = c("race_num_r", "south_r", "union_r"),
                       by.y = c("race_num", "south", "union"),
                       suffixes = c("_r", "_stata"))

# Deduplicate to keep one row per group
comparison_df <- comparison_df %>%
  distinct(race_num_r, south_r, union_r, .keep_all = TRUE)

# Calculate differences between R and Stata outputs
comparison_df <- comparison_df %>%
  mutate(mean_diff = mean_outcome - group_mean,
         sd_diff = sd_outcome - group_sd)

# Add source information for R and Stata columns
comparison_df <- comparison_df %>%
  rename(mean_R = mean_outcome, sd_R = sd_outcome, mean_Stata = group_mean, sd_Stata = group_sd) %>%
  select(race_num_r, south_r, union_r, mean_R, mean_Stata, mean_diff, sd_R, sd_Stata, sd_diff)

# Summary: Check if the differences are all close to zero
all_means_match <- all(abs(comparison_df$mean_diff) < 1e-6)
all_sds_match <- all(abs(comparison_df$sd_diff) < 1e-6)

# Print the result of the comparison
if (all_means_match & all_sds_match) {
  cat("The group means and standard deviations align between R and Stata outputs.\n")
} else {
  cat("There are discrepancies between the R and Stata outputs.\n")
}

# Optionally, save the comparison results for review
write.csv(comparison_df, "comparison/r_vs_stata_comparison.csv", row.names = FALSE)

# Print a few rows of the comparison data for review
print(head(comparison_df))
