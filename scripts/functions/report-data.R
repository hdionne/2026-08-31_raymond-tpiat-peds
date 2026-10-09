num_fmt <- function(x) formatC(x, digits = 2) # Standard number format.

make_tbl1 <- function(demo_chisq_tests_p_adj) {
  demo_chisq_tests_p_adj %>% 
    mutate(
      'Characteristic' = characteristic, 
      'Odds Ratio' = formatC(estimate, digits = 2),
      'Adjusted P-Value' = p_adj,
      .keep = 'none'
    )
}

# Table 2 creation function
make_tbl2 <- function(outcome_wilson_tests_p_adj, within_outcome_chisq_tests_p_adj, between_outcome_chisq_tests_p_adj) {
  # Base: completer proportions with Wilson CIs.
  wilson_tbl <- outcome_wilson_tests_p_adj %>%
    filter(total_type == 'completer') %>%
    mutate(
      outcome,
      timepoint,
      age,
      `Estimate (95% CI)` = str_c(
        num_fmt(estimate * 100), '% (',
        num_fmt(conf.low * 100), '%, - ',
        num_fmt(conf.high * 100), '%)'
      ),
      .keep = 'none'
    )
  
  # Within-group p-values (vs baseline), joined on outcome/age/timepoint.
  within_tbl <- within_outcome_chisq_tests_p_adj %>%
    mutate(
      outcome,
      timepoint,
      age,
      `P Difference from Baseline (adj)` = p_adj,
      .keep = 'none'
    )
  
  # Between-group p-values (across age); no age column, so bind_rows.
  between_tbl <- between_outcome_chisq_tests_p_adj %>%
    mutate(
      outcome,
      timepoint,
      `P Variation Across Age Group (adj)` = p_adj,
      .keep = 'none'
    )
  
  wilson_tbl %>%
    left_join(within_tbl, by = join_by(outcome, timepoint, age)) %>%
    # Reshape to wide format: one column per age for estimate and p-value.
    pivot_wider(
      id_cols = c(outcome, timepoint),
      names_from = age,
      values_from = c(`Estimate (95% CI)`, `P Difference from Baseline (adj)`),
      names_sep = "_"
    ) %>%
    bind_rows(between_tbl) %>%
    mutate(timepoint = factor(timepoint, levels = c('Baseline', '6 Month', '1 Year', '3 Year'))) %>%
    arrange(outcome, is.na(timepoint), timepoint)
}

make_tbl3 <- function(outcome_df, hba1c_age_tests_p_adj, hba1c_above_goal_age_tests_p_adj) {
  # A1c_bin summary (original duplicated block).
  a1c_bin_tbl <- outcome_df %>%
    filter(outcome == 'a1c_bin') %>%
    pivot_wider(
      id_cols = c(timepoint, age),
      names_from = event,
      values_from = count
    ) %>%
    mutate(outcome = 'a1c_bin') %>%
    left_join(
      outcome_df %>%
        filter(outcome == 'encounter', event == 'yes') %>%
        select(timepoint, age, total_encounters = count),
      by = join_by(timepoint, age)
    ) %>%
    select(timepoint, outcome, age, total_encounters, `<5.7`, `>=7.0`, `>=9.0`)

  a1c_summary_tbl <- a1c_bin_tbl %>%
    left_join(
      select(hba1c_age_tests_p_adj, timepoint, 'Difference Across Age P-adj' = p_adj) %>%
        mutate(age = 'adult'),
      by = join_by(timepoint, age)
    ) %>%
    left_join(
      select(hba1c_above_goal_age_tests_p_adj, timepoint, 'Above-Goal Difference P-Aadj' = p_adj) %>%
        mutate(age = 'adult'),
      by = join_by(timepoint, age)
    ) %>%
    mutate(timepoint = factor(timepoint, levels = c('Baseline', '6 Month', '1 Year', '3 Year')))

  # Combine non‑a1c and a1c summary tables.
  a1c_summary_tbl %>%
    arrange(timepoint, is.na(outcome), outcome, age)
}






save_results_excel <- function(x, results) {
  
  wb <- wb_workbook()
  for(sheet in names(results)) {
    wb$add_worksheet(sheet = sheet)
    wb$add_data_table(sheet = sheet, x = results[[sheet]])
  }
  
  wb_save(wb, file = x)
}
