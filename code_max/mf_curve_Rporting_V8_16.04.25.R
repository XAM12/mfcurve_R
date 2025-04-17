###### Porting the Stata package "mfcurve" by Daniel Krähmer to R                     ######
###### Collaboration project by Daniel Krämer, Maximilian  Frank and Claudia Weileder ######
###### project funded by the DFG-project META-REP (SPP 2317)                          ######

####links and material collection
#mfcurve(Stata) GitLab-Page: https://gitlab.lrz.de/dkraehmer/mfcurve
#mfcurve(R) GitHub-Page:
#paper: https://journals.sagepub.com/doi/abs/10.1177/1536867X1201100409

####Set-up
library(dplyr)
library(tidyr)
library(plotly)

####data preparation and analysis 01
#simulated data similar to 'nlsw' dataset with 12 factor combinations
set.seed(123)
n <- 1000
df <- data.frame(
  wage = rnorm(n, mean = 15, sd = 5),
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),
  south = sample(c("Yes", "No"), n, replace = TRUE),
  union = sample(c("Yes", "No"), n, replace = TRUE)
)

####data preparation and analysis 02
#simulated data similar to 'nlsw' dataset with 12 factor combinations and significant differences

set.seed(123)
n <- 1000
df <- data.frame(
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),
  south = sample(c("Yes", "No"), n, replace = TRUE),
  union = sample(c("Yes", "No"), n, replace = TRUE)
)

# Base wage with random noise
df$wage <- rnorm(n, mean = 15, sd = 5)

# Introduce systematic differences:
# - White workers get a slight boost, Black workers get a penalty,
# - union membership gives a positive boost,
# - and the south variable also adds a small effect.
df$wage <- with(df, wage +
                  ifelse(race == "White", 2, ifelse(race == "Black", -2, 0)) +
                  ifelse(union == "Yes", 3, 0) +
                  ifelse(south == "Yes", -1, 1)
)

# You can inspect the group means to verify differences:
library(dplyr)
df %>% group_by(race, south, union) %>%
  summarize(mean_wage = mean(wage), n = n())

############ Function Definition ###################
mfcurve <- function(data, outcome, factors, test = "mean", alpha = 0.05, showTitle = TRUE,
                    SaveProcessedData = FALSE, mode = "collapsed", rounding = 2,
                    plotOrigin = FALSE, CI = TRUE) {
  # Ensure required packages are loaded
  require(dplyr)
  require(tidyr)
  require(plotly)

  # Validate rounding input
  if (!is.numeric(rounding) || rounding %% 1 != 0 || rounding < 0) {
    stop("The 'rounding' parameter must be a non-negative whole number.")
  }

  # Remove missing values
  vars <- c(outcome, factors)
  data <- data %>% drop_na(all_of(vars))

  # Create group variable
  data <- data %>%
    mutate(group = interaction(!!!syms(factors), sep = "_"))

  # Calculate grand mean
  grand_mean <- mean(data[[outcome]])

  # Group statistics
  group_stats <- data %>%
    group_by(group) %>%
    summarize(
      mean_outcome = mean(!!sym(outcome)),
      sd_outcome = sd(!!sym(outcome)),
      n = n()
    ) %>%
    ungroup()

  # ------------------------------------------------------------
  # SIGNIFICANCE TESTING
  # Two options:
  #  (1) test == "mean": compare each group to the grand mean (default)
  #  (2) test == "leave-one-out": compare each group to the mean of all other groups
  # ------------------------------------------------------------

  # Precompute sample sizes needed for leave-one-out test
  total_N <- nrow(data)
  total_sum <- sum(data[[outcome]])
  total_ss <- sum((data[[outcome]] - grand_mean)^2)

  # Perform significance testing based on selected option
  if (test == "mean") {
    # Testing against the grand mean
    group_stats <- group_stats %>%
      mutate(
        t_stat = (mean_outcome - grand_mean) / (sd_outcome / sqrt(n)),
        p_value = 2 * pt(-abs(t_stat), df = n - 1),
        sig = p_value < alpha
      )
  } else if (test == "leave-one-out") {
    # Testing each group against the mean of all other groups (leave-one-out)
    group_stats <- group_stats %>%
      rowwise() %>%  # Process each group individually
      mutate(
        others_n = total_N - n,
        group_sum = mean_outcome * n,
        others_mean = (total_sum - group_sum) / others_n,
        group_ss = (n - 1) * (sd_outcome^2),
        others_ss = total_ss - group_ss,
        others_sd = ifelse(others_n > 1, sqrt(others_ss / (others_n - 1)), NA_real_),
        se_diff = sqrt((sd_outcome^2 / n) + (others_sd^2 / others_n)),
        t_stat = (mean_outcome - others_mean) / se_diff,
        var1 = sd_outcome^2 / n,
        var2 = others_sd^2 / others_n,
        df_calc = (var1 + var2)^2 / ((var1^2 / (n - 1)) + (var2^2 / (others_n - 1))),
        p_value = 2 * pt(-abs(t_stat), df = df_calc),
        sig = p_value < alpha
      ) %>%
      ungroup()
  } else {
    stop("Unsupported test type. Allowed options are 'mean' (grand mean) and 'leave-one-out' (others mean).")
  }

  # Add a column to document which test type was chosen
  group_stats <- group_stats %>% mutate(test_type = test)

  # Rank groups with sequential numbers
  group_stats <- group_stats %>%
    arrange(mean_outcome) %>%
    mutate(rank = row_number())  # Ensure sequential numbering

  # Confidence intervals
  level <- 1 - alpha  # Confidence level
  group_stats <- group_stats %>%
    mutate(
      se = sd_outcome / sqrt(n),
      ci_lower = mean_outcome - qt(1 - alpha / 2, df = n - 1) * se,
      ci_upper = mean_outcome + qt(1 - alpha / 2, df = n - 1) * se,
      ci_width = (ci_upper - ci_lower) / 2
    )

  # Round numerical values based on user-defined rounding for visulation only
  # Does not affect the precision of the numbers in group statistics
  group_stats_vis <- group_stats %>%
    mutate(
      mean_outcome_vis = round(mean_outcome, rounding),
      sd_outcome_vis = round(sd_outcome, rounding),
      ci_lower_vis = round(ci_lower, rounding),
      ci_upper_vis = round(ci_upper, rounding),
      ci_width_vis = round(ci_width, rounding)  # Rounded CI width
    )

  # Separate group into factors
  group_stats <- group_stats %>%
    separate(group, into = factors, sep = "_")

  group_stats_vis <- group_stats_vis %>%
    separate(group, into = factors, sep = "_")

  # Determine axis limits based on data range and plotOrigin setting
  x_min <- min(group_stats_vis$rank)
  x_max <- max(group_stats_vis$rank) + 0.5  # Added padding to the right to show the full plot
  y_min <- min(group_stats_vis$ci_lower_vis)
  y_max <- max(group_stats_vis$ci_upper_vis)

  if (plotOrigin) {
    x_min <- min(x_min, 0)
    y_min <- min(y_min, 0)
  }

  # Prepare lower panel data
  lower_data <- group_stats %>%
    select(rank, all_of(factors)) %>%
    gather(key = "factor", value = "level", -rank)

  if (mode == "expanded") {
    # If expanded mode, append factor levels to names
    lower_data <- lower_data %>%
      mutate(factor = paste(factor, level, sep = " ")) %>%
      distinct()
  }

  # Assign numeric codes to levels
  lower_data <- lower_data %>%
    group_by(factor) %>%
    mutate(
      level_code = as.numeric(factor(level))
    ) %>%
    ungroup()

  # Assign positions to factors
  factor_levels <- unique(lower_data$factor)
  factor_positions <- data.frame(
    factor = factor_levels,
    y = seq(length(factor_levels), 1)
  )

  lower_data <- lower_data %>%
    left_join(factor_positions, by = "factor")


  # Upper panel plot with conditional CIs based on the CI argument
  upper_plot <- plot_ly(data = group_stats_vis)
    # (1) Add the Group Means trace (Confidence Intervals as a separate trace)
    if (CI) {
      upper_plot <- upper_plot %>%
        add_trace(
          x = ~rank,
          y = ~mean_outcome_vis,
          error_y = list(
            type = 'data',
            symmetric = FALSE,
            array = ~ci_upper_vis - mean_outcome_vis,
            arrayminus = ~mean_outcome_vis - ci_lower_vis
          ),
          type = 'scatter',
          mode = 'markers',
          marker = list(
            symbol = ~ifelse(sig, 'diamond', 'circle')
          ),
          text = ~paste0(
            "Mean: ", mean_outcome_vis, "<br>",
            "SD: ", sd_outcome_vis, "<br>",
            (level * 100), "%-CI: [", ci_lower_vis, ", ", ci_upper_vis, "]<br>",
            "Group size: ", n
          ),
          hoverinfo = 'text',
          name = 'Group Means'
        )
    } else {
      upper_plot <- upper_plot %>%
        add_trace(
          x = ~rank,
          y = ~mean_outcome_vis,
          type = 'scatter',
          mode = 'markers',
          marker = list(
            symbol = ~ifelse(sig, 'diamond', 'circle')
          ),
          text = ~paste0(
            "Mean: ", mean_outcome_vis, "<br>",
            "SD: ", sd_outcome_vis, "<br>",
            "Group size: ", n
          ),
          hoverinfo = 'text',
          name = 'Group Means'
        )
    }
    # (2) Add the Grand Mean trace next:
    upper_plot <- upper_plot %>%
      add_trace(
        x = c(x_min, x_max),
        y = c(grand_mean, grand_mean),
        type = 'scatter',
        mode = 'lines',
        line = list(dash = 'dash'),
        name = 'Grand Mean'
      ) %>%
      layout(
        xaxis = list(range = c(x_min, x_max)),
        yaxis = list(range = c(y_min, y_max))
      )

    # (3) Compute an offset and add the Significant Values trace using a star symbol,
    # which should appear in the legend with the specified name.
    offset <- 0.08 * (y_max - y_min)

    upper_plot <- upper_plot %>% add_trace(
      data = group_stats_vis[group_stats_vis$sig == TRUE, ],
      x = ~rank,
      y = ~ci_upper_vis + offset,
      mode = 'markers',
      type = 'scatter',
      marker = list(
        symbol = "star-dot",
        size = 8,
        color = "black"
      ),
      name = "Significant values",
      hoverinfo = 'skip',
      showlegend = TRUE
    )

  # Lower panel plot
  lower_plot <- plot_ly(
    data = lower_data,
    x = ~rank,
    y = ~y,
    text = ~paste("Factor:", factor, "<br>Level:", level),
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 10,
      color = ~level_code,
      colorscale = 'Viridis'
    ),
    showlegend = FALSE
  ) %>%
    layout(
      yaxis = list(
        tickvals = factor_positions$y,
        ticktext = factor_positions$factor,
        autorange = "reversed"
      ),
      xaxis = list(
        title = "Group Rank",
        tickvals = group_stats$rank,  # Ensure all ticks are shown
        ticktext = as.character(group_stats$rank)  # Ensure text matches ranks
      )
    )

  # Create a dynamic title based on outcome and factors
  title <- paste("Mean", outcome, "by the combination of", paste(factors, collapse = " / "))

  # Combine plots
  combined_plot <- subplot(
    upper_plot,
    lower_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.7, 0.3)
  )

  # Add title to the combined plot if showTitle is TRUE
  if (showTitle) {
    combined_plot <- combined_plot %>%
      layout(
        title = list(text = title, x = 0.5), # Add title to layout if showTitle is TRUE
        yaxis = list(title = outcome),
        yaxis2 = list(title = "Factors", showticklabels = TRUE)
      )
  } else {
    combined_plot <- combined_plot %>%
      layout(
        yaxis = list(title = outcome),
        yaxis2 = list(title = "Factors", showticklabels = TRUE)
      )
  }

  # Display the plot
  print(combined_plot)

  # Save processed data to global environment if SaveProcessedData is TRUE
  if (SaveProcessedData) {
    assign("group_stats", group_stats, envir = .GlobalEnv)
  }

  # Optionally return the plot object
  invisible(combined_plot)
}

###Testing
plottest <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), rounding = 1, SaveProcessedData = TRUE)
plottest_collapsed <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), SaveProcessedData = TRUE, mode = "collapsed")
plottest_expanded <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), SaveProcessedData = TRUE, mode = "expanded")

plottest_no_CI <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), CI = FALSE)
plottest_with_CI <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), CI = TRUE, test = "mean")
plottest_with_CI <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), CI = TRUE, test = "leave-one-out")
