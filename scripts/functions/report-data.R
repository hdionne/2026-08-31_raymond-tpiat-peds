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

make_tbl3 <- function(outcome_df, hba1c_age_tests_p_adj, hba1c_above_goal_age_tests_p_adj) {
  
  # Counts of each a1c_bin event (one column per event), plus total encounters per timepoint/age.
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
  
  # Join the adjusted p-values onto the count table, then sort.
  a1c_summary_tbl <- a1c_bin_tbl %>%
    left_join(
      select(hba1c_age_tests_p_adj, timepoint, 'Difference Across Age P-adj' = p_adj) %>% 
        mutate(age = 'adult'),
      by = join_by(timepoint, age),
    ) %>%
    left_join(
      select(hba1c_above_goal_age_tests_p_adj, timepoint, 'Above-Goal Difference P-Aadj' = p_adj) %>% 
        mutate(age = 'adult'),
      by = join_by(timepoint, age),
    ) %>%
    mutate(timepoint = factor(timepoint, levels = c('Baseline', '6 Month', '1 Year', '3 Year'))) %>% 
    arrange(timepoint, is.na(outcome), outcome, age)
  
  a1c_summary_tbl

}






save_results_excel <- function(x, results) {
  
  wb <- wb_workbook()
  for(sheet in names(results)) {
    wb$add_worksheet(sheet = sheet)
    wb$add_data_table(sheet = sheet, x = results[[sheet]])
  }
  
  wb_save(wb, file = x)
}
