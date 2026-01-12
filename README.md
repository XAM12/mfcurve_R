# mfcurve: Multi-Factor Curve Analysis in R

## Overview

`mfcurve` is an R package designed for  plotting results from multifactorial research designs (i.e. conjoint analyses, choice experiments, factorial survey experiments), replicating the functionality of the Stata `mfcurve` ado file. The package provides a streamlined workflow for:
- **Preprocessing data** to create grouped summaries.
- **Conducting statistical tests** to compare group means and assess significance.
- **Generating visualizations** to explore differences across groups, including confidence intervals and significance markers.

This package is ideal for researchers and analysts working with factorial survey experiments, conjoint analysis, or other multi-factor designs.

## Features
- **Data Preprocessing**: Easily group data by multiple factors or predefined group variables.
- **Statistical Testing**: Compare group means using t-tests, with flexible options for confidence levels.
- **Visualization**: Create publication-ready plots highlighting group differences and significant results.

## Installation

To install the package from GitHub, follow these steps:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install mfcurve package from GitHub
devtools::install_github("XAM12/mfcurve_R")
```

## Usage

Here’s a quick guide on how to use `mfcurve`:

### 1. Preprocess Data

```r
library(mfcurve)

# Example dataset
data <- data.frame(
  outcome = rnorm(100),
  factor1 = factor(sample(letters[1:3], 100, replace = TRUE)),
  factor2 = factor(sample(letters[4:5], 100, replace = TRUE))
)

# Preprocess data with two factors
preprocessed_data <- mfcurve_preprocessing(data, outcome_var = "outcome", factors = c("factor1", "factor2"))
```

### 2. Conduct Statistical Tests

```r
test_results <- mfcurve_stat_test(preprocessed_data, test = "mean", level = 0.95)
```

### 3. Visualize Results

```r
plot <- mfcurve_plot(test_results, title = "Group Differences in Outcome")
print(plot)
```

### Example Plot

## Documentation

Detailed documentation for each function is available within R:

```r
?mfcurve_preprocessing
?mfcurve_stat_test
?mfcurve_plot
```

## Vignette
For a comprehensive guide on using `mfcurve`, refer to the vignette:

```r
vignette("introduction-to-mfcurve")
```

## Contributing
Contributions are welcome! If you find bugs or have suggestions, feel free to open an issue or submit a pull request on [GitHub](https://github.com/XAM12/mfcurve_R).

## License
This package is licensed under GNU General Public License v3.0 or later. See the [LICENSE](LICENSE) file for details.

## Contact
For questions or suggestions, please contact:
- Maximilian Frank (Email: maximilian.frank@psy.lmu.de)