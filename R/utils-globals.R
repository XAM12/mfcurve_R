# quiet R CMD check for NSE column names created/used in dplyr pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(
  ".data",
  "factor_var","factor_lbl","factor_group","factor_level_combo",
  "mean_outcome","sd_outcome","ci_lower","ci_upper","ci_width","sig",
  "group","n","t_stat","p_value","group_sum","others_n","group_ss",
  "others_ss","others_sd","var1","var2","others_mean","se_diff",
  "df_calc","df","se"
))