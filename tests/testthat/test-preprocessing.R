# Define the mfcurve_preprocessing function
mfcurve_preprocessing <- function(data, outcome_var, factors, groupvar = NULL) {

  # Ensure outcome_var and factors are in the data
  if (!outcome_var %in% colnames(data)) {
    stop("Outcome variable not found in the dataset.")
  }
  if (!all(factors %in% colnames(data))) {
    stop("One or more factor variables not found in the dataset.")
  }

  # Convert outcome_var to a symbol for dplyr operations
  outcome_var_sym <- rlang::sym(outcome_var)

  # Remove observations with missing values in outcome or factor variables
  data_clean <- data %>%
    dplyr::filter(!is.na(!!outcome_var_sym)) %>%
    dplyr::filter(!dplyr::if_any(dplyr::all_of(factors), is.na))

  # Create a group variable if not provided
  if (is.null(groupvar)) {
    # Group by interaction of factors, replicating Stata's logic
    data_clean <- data_clean %>%
      dplyr::mutate(group = interaction(!!!rlang::syms(factors), drop = TRUE)) %>%
      dplyr::mutate(group = as.numeric(factor(group))) # Convert to numeric for easier handling
  } else {
    # Ensure groupvar is present in the data
    if (!groupvar %in% colnames(data_clean)) {
      stop("Group variable not found in the dataset.")
    }
    # Use the provided group variable for grouping
    data_clean <- data_clean %>%
      dplyr::filter(!is.na(!!rlang::sym(groupvar))) %>%
      dplyr::mutate(group = !!rlang::sym(groupvar))
  }

  # Summarize data: mean, standard deviation, and count for each group
  data_summary <- data_clean %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(
      mean_outcome = mean(!!outcome_var_sym, na.rm = TRUE),
      sd_outcome = sd(!!outcome_var_sym, na.rm = TRUE),
      n = dplyr::n(),
      .groups = 'drop'
    )

  # Return the summarized data
  return(data_summary)
}

# Create a sample dataset similar to those used in Stata analysis
# Using the 'mtcars' dataset and modifying it to have some factors
set.seed(123)  # Set seed for reproducibility

# Creating a sample dataset
data <- mtcars %>%
  dplyr::mutate(
    cyl = as.factor(cyl),  # Convert 'cyl' to a factor variable
    gear = as.factor(gear), # Convert 'gear' to a factor variable
    random_factor = sample(c("A", "B", "C"), size = nrow(mtcars), replace = TRUE) # Add a random factor
  )

# Introducing some missing values to test the missing data handling
data$mpg[c(3, 7)] <- NA
data$cyl[c(2, 10)] <- NA

# Display the modified dataset
print("Sample Data for Testing:")
print(head(data))

# Test 1: Running the preprocessing function with outcome 'mpg' and factors 'cyl' and 'gear'
test_output_1 <- mfcurve_preprocessing(
  data = data,
  outcome_var = "mpg",
  factors = c("cyl", "gear")
)

# Display Test 1 Output
print("Test 1 Output (Grouping by cyl and gear):")
print(test_output_1)

# Test 2: Running the preprocessing function with a predefined group variable 'gear'
test_output_2 <- mfcurve_preprocessing(
  data = data,
  outcome_var = "mpg",
  factors = c("cyl"),
  groupvar = "gear"
)

# Display Test 2 Output
print("Test 2 Output (Grouping by predefined groupvar 'gear'):")
print(test_output_2)

# Test 3: Edge case with missing data in the factors
# Check that missing values are handled correctly
print("Test 3 Output (Handling missing values):")
print(mfcurve_preprocessing(
  data = data,
  outcome_var = "mpg",
  factors = c("cyl", "random_factor")
))
