library(targets)
library(tidyverse)

tar_load(any_of(c('outcome_df', 'hba1c_age_tests_p_adj', 'hba1c_above_goal_age_tests_p_adj')))

