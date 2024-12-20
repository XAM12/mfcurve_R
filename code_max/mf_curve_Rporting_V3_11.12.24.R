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
mfcurve <- function(data, outcome, factors, test = "mean", alpha = 0.05, showTitle = TRUE, SaveProcessedData = FALSE, mode = "collapsed") {
  # Ensure required packages are loaded
  require(dplyr)
  require(tidyr)
  require(plotly)
  
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
      ci_upper = mean_outcome + qt(1 - alpha / 2, df = n - 1) * se
    )
  
  # Separate group into factors
  group_stats <- group_stats %>%
    separate(group, into = factors, sep = "_")
  
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
    data = group_stats,
    x = ~rank,
    y = ~mean_outcome,
    error_y = ~list(
      type = "data",
      symmetric = FALSE,
      array = ci_upper - mean_outcome,
      arrayminus = mean_outcome - ci_lower
    ),
    type = 'scatter',
    mode = 'markers',
    marker = list(
      color = ifelse(group_stats$sig, 'red', 'blue')
    ),
    name = 'Group Means'
  ) %>%
    add_trace(
      x = c(min(group_stats$rank), max(group_stats$rank)),
      y = c(grand_mean, grand_mean),
      type = 'scatter',
      mode = 'lines',
      line = list(dash = 'dash'),
      name = 'Grand Mean',
      inherit = FALSE  # Prevent inheriting error bars
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
plottest_collapsed <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), SaveProcessedData = TRUE, mode = "collapsed")
plottest_expanded <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union"), SaveProcessedData = TRUE, mode = "expanded")