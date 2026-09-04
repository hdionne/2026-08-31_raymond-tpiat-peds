calc_wilson_tests <- function(outcome_df) {
  df <- outcome_df %>%
    group_by(outcome, timepoint, age) %>% 
    mutate(
      'calc_total' = sum(count),
    ) %>% 
    ungroup() %>% 
    mutate(
      map2(count, calc_total, \(x, y) {
        # if (x == 42) {browser()}
        if(!anyNA(c(x, y))) {
          tidy(prop.test(x, y, correct = FALSE))
        } else {
          data.frame('estimate' = NA)
        }
      }) %>% 
        bind_rows()
    ) %>% 
    ungroup()
  
  df <- df %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(df)
}

# This function will perform a chi-square test, but will switch to a fisher exact test if any case <5 counts.
# Expects a matrix.
chisq_with_fisher <- function(mat) {
  if (anyNA(mat)) {
    return(data.frame('p.value' = NA))
  } else {
    test <- chisq.test(mat, correct = FALSE)
    
    if (any(test$expected < 5)) { # If any expected counts are less than 5, replace with fisher exact test.
      test <- fisher.test(mat, conf.int = FALSE)
    }
    
    return(tidy(test))
  }
}

calc_demo_chisq_tests <- function(demo_df) {
  results <- demo_df %>% 
    mutate(
      'count_in' = count,
      'count_out' = age_total - count,
      .keep = 'unused'
    ) %>% 
    group_by(characteristic) %>% 
    group_modify(.f = \(df, group) {
      mat <- df %>% 
        column_to_rownames('age') %>% 
        select(count_in, count_out) %>% 
        as.matrix()
      estimate_or <- (mat['child', 'count_in'] / mat['child', 'count_out']) / (mat['adult', 'count_in'] / mat['adult', 'count_out'])
      result <- mat %>% 
        chisq_with_fisher() %>% 
        mutate('estimate' = estimate_or)
      return(result)
    }) %>% 
    ungroup()
  
  results <- results %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(results)
}

calc_outcome_timepoint_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression')) %>% 
    group_by(outcome, age) %>% 
    group_modify(\(df, group) {
      
      # Extract the baseline records, as all treated timepoints will be compared to the baseline.
      baseline_record <- df %>% 
        filter(timepoint == 'Baseline')
      treatment_records <- df %>% 
        filter(timepoint != 'Baseline')
      
      # For each timepoint, perform a chi-square test.
      results <- treatment_records %>% 
        group_by(timepoint) %>% 
        group_modify(\(df, group) {
          # Add the baseline record into the data, convert into a matrix, then run chi-square or fisher.
          mat <- df %>% 
            mutate(timepoint = group$timepoint) %>% 
            rbind(baseline_record) %>% 
            pivot_wider(
              id_cols = timepoint,
              names_from = event,
              values_from = count
            ) %>% 
            column_to_rownames('timepoint') %>% 
            as.matrix()
          
          estimate_or <- (mat[group$timepoint, 'yes'] / mat[group$timepoint, 'no']) / (mat['Baseline', 'yes'] / mat['Baseline', 'no'])
          
          results <- chisq_with_fisher(mat) %>% 
            mutate('estimate' = estimate_or)
          return(results)
        })
      
      return(results)
    }) %>% 
    ungroup()
  
  results <- results %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(results)
}

calc_outcome_age_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression')) %>% 
    group_by(outcome, timepoint) %>% 
    group_modify(\(df, group) {
      # Convert to matrix, then perform chi-square or fisher.
      mat <- df %>% 
        pivot_wider(
          id_cols = age,
          names_from = event,
          values_from = count
        ) %>% 
        column_to_rownames('age') %>% 
        as.matrix()
      
      estimate_or <- (mat['child', 'yes'] / mat['child', 'no']) / (mat['adult', 'yes'] / mat['adult', 'no'])
      
      results <- chisq_with_fisher(mat) %>% 
        mutate('estimate' = estimate_or)
      
      return(results)
    }) %>% 
    ungroup()
  
  results <- results %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(results)
}

calc_a1c_bin_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(outcome == 'a1c') %>% 
    mutate(
      'a1c_high' = event %in% c('7.0-7.9', '8.0-8.9', '>=9.0')
    ) %>% 
    group_by(timepoint, age, a1c_high) %>% 
    summarize('count' = sum(count)) %>% 
    group_by(timepoint) %>% 
    group_modify(.f = \(df, group) {
      mat <- df %>% 
        pivot_wider(id_cols = age, names_from = a1c_high, values_from = count) %>% 
        column_to_rownames('age') %>% 
        as.matrix()
      results <- chisq_with_fisher(mat)
      return(results)
    }) %>% 
    ungroup()
  
  results <- results %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(results)
}

calc_a1c_level_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(
      outcome == 'a1c',
      timepoint != 'Baseline'
    ) %>% 
    group_by(timepoint) %>% 
    group_modify(.f = \(df, group) {
      mat <- df %>% 
        pivot_wider(id_cols = age, names_from = event, values_from = count) %>% 
        column_to_rownames('age') %>% 
        as.matrix()
      results <- chisq_with_fisher(mat)
      return(results)
    }) %>% 
    ungroup()
  
  results <- results %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(results)
}

calc_sensitivity <- function(outcome_df) {
  results <- outcome_df %>% 
    group_by(outcome, timepoint, age) %>% 
    mutate(
      'prop_completer' = count / sum(count),
      'prop_itt' = count / ifelse(age == 'child', 97, 401),
      'prop_diff' = prop_completer - prop_itt
    )
  
  return(results)
}

calc_outcome_age_logistic_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression')) %>% 
    group_by(outcome, age) %>% 
    group_modify(\(df, group) {
      
      # Extract the baseline records, as all treated timepoints will be compared to the baseline.
      baseline_record <- df %>% 
        filter(timepoint == 'Baseline')
      treatment_records <- df %>% 
        filter(timepoint != 'Baseline')
      
      # For each timepoint, perform a chi-square test.
      results <- treatment_records %>% 
        group_by(timepoint) %>% 
        group_modify(\(df, group) {
          # browser()
          # Create a data frame using the baseline record and the current record, then run a logistic regression.
          df <- data.frame(
            response = c(
              rep('yes', filter(baseline_record, event == 'yes')$count),
              rep('no', filter(baseline_record, event == 'no')$count),
              rep('yes', filter(df, event == 'yes')$count),
              rep('no', filter(df, event == 'no')$count)
            ) %>% 
              factor(levels = c('no', 'yes')) %>% 
              as.integer() %>% 
              {.-1},
            timepoint = c(
              rep('baseline', filter(baseline_record, event == 'yes')$count),
              rep('baseline', filter(baseline_record, event == 'no')$count),
              rep(group$timepoint, filter(df, event == 'yes')$count),
              rep(group$timepoint, filter(df, event == 'no')$count)
            ) %>% 
              factor(levels = c('baseline', group$timepoint))
          )
          results <- tidy(glm(df, formula = response ~ timepoint, family = 'binomial'), conf.int = TRUE, exponentiate = TRUE)
          return(results)
        })
      
      return(results)
    }) %>% 
    ungroup()
  
  results <- results %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(results)
}

calc_omnibus_variance_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    group_by(outcome, age) %>% 
    group_modify(\(df, group) {
      # Convert to matrix, then perform chi-square or fisher.
      mat <- df %>% 
        pivot_wider(
          id_cols = timepoint,
          names_from = event,
          values_from = count
        ) %>% 
        column_to_rownames('timepoint') %>% 
        as.matrix()
      results <- chisq_with_fisher(mat)
      return(results)
    }) %>% 
    ungroup()
  
  results <- results %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(results)
}


calc_adversarial_outcome_age_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression')) %>% 
    group_by(outcome, timepoint) %>% 
    group_modify(\(df, group) {
      # Convert to matrix, then perform chi-square or fisher.
      # browser()
      mat <- df %>% 
        pivot_wider(
          id_cols = age,
          names_from = event,
          values_from = count
        ) %>% 
        column_to_rownames('age') %>% 
        as.matrix()
      
      child_greater <- (mat['child','yes'] / mat['child', 'no']) / (mat['adult','yes'] / mat['adult', 'no']) > 1
      if (child_greater | is.na(child_greater)) {
        mat['child', 'no'] <- mat['child', 'no'] + (97 - sum(mat['child',]))
        mat['adult', 'yes'] <- mat['adult', 'yes'] + (401 - sum(mat['adult',]))
      } else{
        mat['child', 'yes'] <- mat['child', 'yes'] + (97 - sum(mat['child',]))
        mat['adult', 'no'] <- mat['adult', 'no'] + (401 - sum(mat['adult',]))
      }
      
      estimate_or <- (mat['child', 'yes'] / mat['child', 'no']) / (mat['adult', 'yes'] / mat['adult', 'no'])
      
      results <- chisq_with_fisher(mat) %>% 
        mutate('estimate' = estimate_or)
      return(results)
    }) %>% 
    ungroup()
  
  results <- results %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
  
  return(results)
}

calc_adversarial_outcome_comparison <- function(chisq_tbl, adversarial_chisq_tbl) {
  results <- left_join(
    select(chisq_tbl, outcome, timepoint, estimate, p_adj, p_adj_signif),
    select(adversarial_chisq_tbl, outcome, timepoint, estimate, p_adj, p_adj_signif),
    by = join_by(outcome, timepoint)
  ) %>% 
    mutate(
      'adversarial_direction' = ifelse(sign(estimate.x) == sign(estimate.y), 'same', 'different'),
      'adversarial_significance' = ifelse(p_adj_signif.x == p_adj_signif.y, 'same', 'different'),
      'any_adversarial_effect' = (adversarial_direction == 'different' | adversarial_significance == 'different'),
      'adversarial_reversed' = (adversarial_direction == 'different' & p_adj_signif.x & p_adj_signif.y)
    )
  return(results)
}















