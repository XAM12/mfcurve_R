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

#generate a grouping variable based on combinations of factors
df <- df %>%
  mutate(group = interaction(race, south, union, sep = "_"))

#calculate descriptive statistics
group_stats <- df %>%
  group_by(group) %>%
  summarize(
    mean_wage = mean(wage),
    sd_wage = sd(wage),
    n = n()
  ) %>%
  ungroup()

#calculate the grand mean
grand_mean <- mean(df$wage)


#conduct t-tests against the grand mean and flag significant differences
#should be an option later to user decide to test against which group
alpha <- 0.05
group_stats <- group_stats %>%
  mutate(
    t_stat = (mean_wage - grand_mean) / (sd_wage / sqrt(n)),
    p_value = 2 * pt(-abs(t_stat), df = n - 1),
    sig = p_value < alpha
  )

#rank groups by ordered means
group_stats <- group_stats %>%
  arrange(mean_wage) %>%
  mutate(rank = row_number())

#compute confidence intervals
group_stats <- group_stats %>%
  mutate(
    se = sd_wage / sqrt(n),
    ci_lower = mean_wage - qt(1 - alpha / 2, df = n - 1) * se,
    ci_upper = mean_wage + qt(1 - alpha / 2, df = n - 1) * se
  )

####data preparation for visualisation

#separate the group variable back into individual factors
group_stats <- group_stats %>%
  separate(group, into = c("race", "south", "union"), sep = "_") #maybe aviod this collapsing and reseperation in a future version of the code

#reshape data for plotting indicators (lower panel)
lower_data <- group_stats %>%
  select(rank, race, south, union) %>%
  gather(key = "factor", value = "level", -rank)

#assign numeric codes to levels and positions to factors
lower_data <- lower_data %>%
  group_by(factor) %>%
  mutate(level_code = as.numeric(factor(level))) %>%
  ungroup()

factor_levels <- unique(lower_data$factor)
factor_positions <- data.frame(
  factor = factor_levels,
  y = seq(length(factor_levels), 1)
)

lower_data <- lower_data %>%
  left_join(factor_positions, by = "factor")


####visualisation

#create the index plot (upper panel)
upper_plot <- plot_ly(
  data = group_stats,
  x = ~rank,
  y = ~mean_wage,
  error_y = ~list(
    type = "data",
    symmetric = FALSE,
    array = ci_upper - mean_wage,
    arrayminus = mean_wage - ci_lower
  ),
  type = 'scatter',
  mode = 'markers',
  marker = list(
    color = ifelse(group_stats$sig, 'red', 'blue')
  ),
  name = 'Mean Wage'
) %>%
  add_lines(
    x = ~rank,
    y = grand_mean,
    line = list(dash = 'dash'),
    name = 'Grand Mean'
  )

#create the indicator plot (lower panel)
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
    xaxis = list(title = "Group Rank")
  )


#combine the Upper and Lower Panels
combined_plot <- subplot(
  upper_plot,
  lower_plot,
  nrows = 2,
  shareX = TRUE,
  heights = c(0.7, 0.3)
) %>%
  layout(
    yaxis = list(title = "Mean Wage"),
    yaxis2 = list(title = "Factors", showticklabels = TRUE)
  )

########################## code as a custom function ########################

mfcurve <- function(data, outcome, factors, test = "mean", level = 0.95) {
  #ensure required packages are loaded
  require(dplyr)
  require(tidyr)
  require(plotly)
  
  #remove missing values
  vars <- c(outcome, factors)
  data <- data %>% drop_na(all_of(vars))
  
  #create group variable
  data <- data %>%
    mutate(group = interaction(!!!syms(factors), sep = "_"))
  
  #calculate grand mean
  grand_mean <- mean(data[[outcome]])
  
  #group statistics
  group_stats <- data %>%
    group_by(group) %>%
    summarize(
      mean_outcome = mean(!!sym(outcome)),
      sd_outcome = sd(!!sym(outcome)),
      n = n()
    ) %>%
    ungroup()
  
  #perform t-tests
  alpha <- 1 - level
  group_stats <- group_stats %>%
    mutate(
      t_stat = (mean_outcome - grand_mean) / (sd_outcome / sqrt(n)),
      p_value = 2 * pt(-abs(t_stat), df = n - 1),
      sig = p_value < alpha
    )
  
  #rank groups
  group_stats <- group_stats %>%
    arrange(mean_outcome) %>%
    mutate(rank = row_number())
  
  #confidence intervals
  group_stats <- group_stats %>%
    mutate(
      se = sd_outcome / sqrt(n),
      ci_lower = mean_outcome - qt(1 - alpha / 2, df = n - 1) * se,
      ci_upper = mean_outcome + qt(1 - alpha / 2, df = n - 1) * se
    )
  
  #separate group into factors
  group_stats <- group_stats %>%
    separate(group, into = factors, sep = "_")
  
  #prepare lower panel data
  lower_data <- group_stats %>%
    select(rank, all_of(factors)) %>%
    gather(key = "factor", value = "level", -rank)
  
  #assign numeric codes to levels
  lower_data <- lower_data %>%
    group_by(factor) %>%
    mutate(
      level_code = as.numeric(factor(level))
    ) %>%
    ungroup()
  
  #assign positions to factors
  factor_levels <- unique(lower_data$factor)
  factor_positions <- data.frame(
    factor = factor_levels,
    y = seq(length(factor_levels), 1)
  )
  
  lower_data <- lower_data %>%
    left_join(factor_positions, by = "factor")
  
  #upper panel plot
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
    name = 'Mean Outcome'
  ) %>%
    add_lines(
      x = ~rank,
      y = grand_mean,
      line = list(dash = 'dash'),
      name = 'Grand Mean'
    )
  
  #lower panel plot
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
      xaxis = list(title = "Group Rank")
    )
  
  #combine plots
  combined_plot <- subplot(
    upper_plot,
    lower_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.7, 0.3)
  ) %>%
    layout(
      yaxis = list(title = "Outcome"),
      yaxis2 = list(title = "Factors", showticklabels = TRUE)
    )
  
  return(combined_plot)
}

plot <- mfcurve(df, outcome = "wage", factors = c("race", "south", "union")) #discuss in the group which arguments should be passed to the user
plot


########################## testing with the sample dataset nlsw ########################
getwd()
setwd("C:\\Users\\Maximilian Frank\\Documents\\HoPo_Januar_2022\\LMU_Promotion\\mf_curve_Kooperationsprojekt\\Referenzgrafik")

df_nlsw <- read.csv(file = "nlsw.csv")

plot_nlsw <- mfcurve(df_nlsw, outcome = "wage", factors = c("race", "south", "union"))
plot_nlsw


######open questions
##which arguments pass to the user
##offer options to save values of the calculation
##autoprint plot?, option in the syntax and maybe also as a global option?



########################## code as a custom function V2 ########################
#####changes
#####pass alpha level to the user, default equals 0.05
#####preliminary autoprint option

mfcurve2 <- function(data, outcome, factors, test = "mean", alpha = 0.05) {
  # Ensure required packages are loaded
  require(dplyr)
  require(tidyr)
  require(plotly)
  
  #remove missing values
  vars <- c(outcome, factors)
  data <- data %>% drop_na(all_of(vars))
  
  #create group variable
  data <- data %>%
    mutate(group = interaction(!!!syms(factors), sep = "_"))
  
  #calculate grand mean
  grand_mean <- mean(data[[outcome]])
  
  #group statistics
  group_stats <- data %>%
    group_by(group) %>%
    summarize(
      mean_outcome = mean(!!sym(outcome)),
      sd_outcome = sd(!!sym(outcome)),
      n = n()
    ) %>%
    ungroup()
  
  #perform t-tests
  group_stats <- group_stats %>%
    mutate(
      t_stat = (mean_outcome - grand_mean) / (sd_outcome / sqrt(n)),
      p_value = 2 * pt(-abs(t_stat), df = n - 1),
      sig = p_value < alpha
    )
  
  #rank groups
  group_stats <- group_stats %>%
    arrange(mean_outcome) %>%
    mutate(rank = row_number())
  
  #confidence intervals
  level <- 1 - alpha
  group_stats <- group_stats %>%
    mutate(
      se = sd_outcome / sqrt(n),
      ci_lower = mean_outcome - qt(1 - alpha / 2, df = n - 1) * se,
      ci_upper = mean_outcome + qt(1 - alpha / 2, df = n - 1) * se
    )
  
  #separate group into factors
  group_stats <- group_stats %>%
    separate(group, into = factors, sep = "_")
  
  #prepare lower panel data
  lower_data <- group_stats %>%
    select(rank, all_of(factors)) %>%
    gather(key = "factor", value = "level", -rank)
  
  #assign numeric codes to levels
  lower_data <- lower_data %>%
    group_by(factor) %>%
    mutate(
      level_code = as.numeric(factor(level))
    ) %>%
    ungroup()
  
  #assign positions to factors
  factor_levels <- unique(lower_data$factor)
  factor_positions <- data.frame(
    factor = factor_levels,
    y = seq(length(factor_levels), 1)
  )
  
  lower_data <- lower_data %>%
    left_join(factor_positions, by = "factor")
  
  #upper panel plot
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
    name = 'Mean Outcome'
  ) %>%
    add_lines(
      x = ~rank,
      y = grand_mean,
      line = list(dash = 'dash'),
      name = 'Grand Mean'
    )
  
  #lower panel plot
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
      xaxis = list(title = "Group Rank")
    )
  
  #combine plots
  combined_plot <- subplot(
    upper_plot,
    lower_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.7, 0.3)
  ) %>%
    layout(
      yaxis = list(title = outcome),
      yaxis2 = list(title = "Factors", showticklabels = TRUE)
    )
  
  #display the plot
  print(combined_plot)
  
  #optionally return the plot object
  invisible(combined_plot)
}

########################## Testing ######################

plot_nlsw <- mfcurve2(df_nlsw, outcome = "wage", factors = c("race", "south", "union"))
