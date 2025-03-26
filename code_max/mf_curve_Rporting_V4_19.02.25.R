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

####data preparation and analysis
#simulated data similar to 'nlsw' dataset with 12 factor combinations
set.seed(123)
n <- 1000
df <- data.frame(
  wage = rnorm(n, mean = 15, sd = 5),
  race = sample(c("White", "Black", "Other"), n, replace = TRUE),
  south = sample(c("Yes", "No"), n, replace = TRUE),
  union = sample(c("Yes", "No"), n, replace = TRUE)
)


############ Function Definition ###################
mfcurve <- function(data, outcome, factors, test = "mean", alpha = 0.05, showTitle = TRUE,
                    SaveProcessedData = FALSE, mode = "collapsed",  rounding = 2, plotOrigin = FALSE) {
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

  # Calculate grand meanome
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

  # Perform t-tests
  group_stats <- group_stats %>%
    mutate(
      t_stat = (mean_outcome - grand_mean) / (sd_outcome / sqrt(n)),
      p_value = 2 * pt(-abs(t_stat), df = n - 1),
      sig = p_value < alpha
    )

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
      ci_width = (ci_upper - ci_lower) / 2  # Calculate CI width
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
  if (plotOrigin) {
    x_min <- min(group_stats_vis$rank, 0)
    x_max <- max(group_stats_vis$rank) + 0.5  # Add padding for rightmost point
    y_min <- min(group_stats_vis$ci_lower_vis, 0)
    y_max <- max(group_stats_vis$ci_upper_vis)
  } else {
    x_min <- NULL  # Let plotly auto-determine
    x_max <- NULL
    y_min <- NULL
    y_max <- NULL
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


  # Upper panel plot
  upper_plot <- plot_ly(
    data = group_stats_vis,  # Use visualization-specific df
    x = ~rank,
    y = ~mean_outcome_vis,
    error_y = ~list(
      type = "data",
      symmetric = FALSE,
      array = ci_upper_vis - mean_outcome_vis,
      arrayminus = mean_outcome_vis - ci_lower_vis
    ),
    text = ~paste0(
      "Mean: ", mean_outcome_vis, "<br>",
      "SD: ", sd_outcome_vis, "<br>",
      "CI: [", ci_lower_vis, ", ", ci_upper_vis, "] / ±", ci_width_vis
    ),
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers',
    marker = list(
      color = ifelse(group_stats_vis$sig, 'red', 'blue')
    ),
    name = 'Group Means'
  ) %>%
    add_trace(
      x = c(min(group_stats_vis$rank), max(group_stats_vis$rank)),  # Use actual data range
      y = c(grand_mean, grand_mean),
      type = 'scatter',
      mode = 'lines',
      line = list(dash = 'dash'),
      name = 'Grand Mean',
      inherit = FALSE
    ) %>%
    layout(
      xaxis = list(
        range = if (!is.null(x_min)) c(x_min, x_max) else NULL,
        showgrid = FALSE  # Disable vertical grid lines in the upper panel
      ),
      yaxis = list(
        range = if (!is.null(y_min)) c(y_min, y_max) else NULL,
        showgrid = TRUE
        # Disable horizontal grid lines in the upper panel
      )
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
        autorange = "reversed",
        showgrid = TRUE
      ),
      xaxis = list(
        title = "Group Rank",
        tickvals = group_stats$rank,  # Ensure all ticks are shown
        ticktext = as.character(group_stats$rank),  # Ensure text matches ranks
        showgrid = FALSE
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
plottest <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), rounding = 3, SaveProcessedData = TRUE)
plottest <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), plotOrigin = TRUE)
plottest_collapsed <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), SaveProcessedData = TRUE, mode = "collapsed")
plottest_expanded <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), SaveProcessedData = TRUE, mode = "expanded")
