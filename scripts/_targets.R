library(targets)
library(tarchetypes)
library(crew)

# Tell targets that the _targets.R file is in the scripts folder instead of the project root directory.
tar_config_set(
  script = './scripts/_targets.R',
  store = './scripts/_targets/'
)

# List the libraries.
libraries <- c(
  'tidyverse',
  'openxlsx2',
  'broom'
)

tar_option_set(
  packages = libraries,
  seed = 968548
)
# Tell targets that there is a folder of scripts defining functions, ./scripts/functions/ and that these scripts should be run.
tar_source('./scripts/functions/')

# Place targets here
tar_plan(
  wb_file = './raw-data/TPIAT_Peds_Data.xlsx',
  wb = wb_load(wb_file),
  outcome_df_raw = load_outcome_df(wb),
  outcome_a1c_df_raw = load_outcome_a1c_df(wb),
  outcome_df = prep_outcome_df(outcome_df_raw, outcome_a1c_df_raw),
  outcome_wilson_tests = calc_wilson_tests(outcome_df),
  demo_df_raw = load_demographics(wb),
  demo_df = prep_demographics(demo_df_raw),
  demo_chisq_tests = calc_demo_chisq_tests(demo_df),
  within_outcome_chisq_tests = calc_outcome_timepoint_chisq_tests(outcome_df),
  between_outcome_chisq_tests = calc_outcome_age_chisq_tests(outcome_df),
  hba1c_above_goal_age_tests = calc_a1c_bin_chisq_tests(outcome_df),
  hba1c_age_tests = calc_a1c_level_chisq_tests(outcome_df),
  completer_sensitivity_diffs = calc_sensitivity(outcome_df),
  between_outcome_logistic_regression = calc_outcome_age_logistic_tests(outcome_df),
  omnibus_variance_chisq_tests = calc_omnibus_variance_chisq_tests(outcome_df),
  adversarial_between_outcome_chisq_tests = calc_adversarial_outcome_age_chisq_tests(outcome_df),
  adversarial_outcome_comparisons = calc_adversarial_outcome_comparison(between_outcome_chisq_tests, adversarial_between_outcome_chisq_tests)
)
