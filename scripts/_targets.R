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
  'broom',
  'fastglm'
)

tar_option_set(
  packages = libraries,
  seed = 968548
)
# Tell targets that there is a folder of scripts defining functions, ./scripts/functions/ and that these scripts should be run.
tar_source('./scripts/functions/')

targets_load <- tar_plan(
  wb_file = './raw-data/TPIAT_Peds_Data.xlsx',
  wb = wb_load(wb_file),
  outcome_df_raw = load_outcome_df(wb),
  outcome_a1c_df_raw = load_outcome_a1c_df(wb),
  demo_df_raw = load_demographics(wb)
)

targets_prep <- tar_plan(
  outcome_df = prep_outcome_df(outcome_df_raw, outcome_a1c_df_raw),
  demo_df = prep_demographics(demo_df_raw)
)

targets_analyze <- tar_plan(
  outcome_wilson_tests = calc_wilson_tests(outcome_df),
  demo_chisq_tests = calc_demo_chisq_tests(demo_df),
  within_outcome_chisq_tests = calc_outcome_timepoint_chisq_tests(outcome_df),
  between_outcome_chisq_tests = calc_outcome_age_chisq_tests(outcome_df),
  between_outcome_retention_chisq_tests = calc_outcome_age_retention_chisq_tests(outcome_df),
  hba1c_above_goal_age_tests = calc_a1c_bin_chisq_tests(outcome_df),
  hba1c_age_tests = calc_a1c_level_chisq_tests(outcome_df),
  completer_sensitivity_diffs = calc_sensitivity(outcome_df),
  between_outcome_logistic_regression = calc_outcome_age_logistic_tests(outcome_df),
  omnibus_variance_itt_chisq_tests = calc_omnibus_variance_itt_chisq_tests(outcome_df),
  omnibus_variance_completer_chisq_tests = calc_omnibus_variance_completer_chisq_tests(outcome_df),
  adversarial_between_outcome_chisq_tests = calc_adversarial_outcome_age_chisq_tests(outcome_df),
  adversarial_outcome_comparisons = calc_adversarial_outcome_comparison(between_outcome_chisq_tests, adversarial_between_outcome_chisq_tests),
  adversarial_tipping_point_prop_tbl = calc_adversarial_tipping_point_proportions(outcome_df, between_outcome_chisq_tests),
  # adversarial_tipping_point_k_tbl = calc_adversarial_tipping_point_ks(outcome_df, between_outcome_chisq_tests),
  # adjusted_p_values = calc_adj_p_values(list(
  #   'outcome_wilson_tests' = outcome_wilson_tests,
  #   'demo_chisq_tests' = demo_chisq_tests,
  #   'within_outcome_chisq_tests' = within_outcome_chisq_tests,
  #   'between_outcome_retention_chisq_tests' = between_outcome_retention_chisq_tests,
  #   'hba1c_above_goal_age_tests' = hba1c_above_goal_age_tests,
  #   'hba1c_age_tests' = hba1c_age_tests,
  #   'between_outcome_logistic_regression' = between_outcome_logistic_regression,
  #   'omnibus_variance_itt_chisq_tests' = omnibus_variance_itt_chisq_tests,
  #   'omnibus_variance_completer_chisq_tests' = omnibus_variance_completer_chisq_tests,
  #   'adversarial_between_outcome_chisq_tests' = adversarial_between_outcome_chisq_tests,
  # ))
)

targets_report <- tar_plan(
  
  save_excel = save_results_excel(
    './results/tpiat-child-adult.xlsx',
    list(
      'Outcome Wilson Tests' = outcome_wilson_tests,
      'Demographic Chisq Tests' = demo_chisq_tests,
      'Within Outcome Chisq Tests' = within_outcome_chisq_tests,
      'Between Outcome Chisq Tests' = between_outcome_chisq_tests,
      'Between Outcome Ret Chisq Tests' = between_outcome_retention_chisq_tests,
      'HBA1C above goal age tests' = hba1c_above_goal_age_tests,
      'HBA1C age tests' = hba1c_age_tests,
      'Completer Sensitivity Diffs' = completer_sensitivity_diffs,
      'Between Outcome Logistic Reg' = between_outcome_logistic_regression,
      'Omni Chisq ITT Chisq Tests' = omnibus_variance_itt_chisq_tests,
      'Omni Chisq Comp Chisq Tests' = omnibus_variance_completer_chisq_tests,
      'Advers Btwn Outcome Chisq Tests' = adversarial_between_outcome_chisq_tests,
      'Advers Outcome Comparisons' = adversarial_outcome_comparisons,
      'Advers Prop Tipping Point' = adversarial_tipping_point_prop_tbl
    )
  )
  # save_paper_tables_excel = save_results_excel(
  #   './results/paper_tpiat-child-adult.xlsx',
  #   list(
  #     'Table S5' = tbl_s5
  #   )
  # )
)


# Place targets here
targets_all <- list(
  targets_load,
  targets_prep,
  targets_analyze,
  targets_report
)

targets_all