calc_wilson_tests <- function(outcome_df) {
  
  calc_p_adjust <- function(df) {
    df <- df %>% 
      mutate(
        'p_adj' = p.adjust(p.value, method = 'BH'),
        'p_signif' = p.value < 0.05,
        'p_adj_signif' = p_adj < 0.05,
        'signif_dropped' = p_signif & (!p_adj_signif)
      )
    return(df)
  }
  
  # Calculate itt yes proportion.
  df_itt <- outcome_df %>%
    filter(event %in% c('yes', 'no_itt')) %>% 
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
    ungroup() %>%
    filter(event == 'yes') %>% 
    calc_p_adjust() %>% 
    select(outcome, timepoint, age, event, count, calc_total, estimate, conf.low, conf.high, p.value, p_signif, p_adj, p_adj_signif, signif_dropped) %>% 
    rename_with(.cols = -c(outcome, timepoint, age, event, count), .fn = \(x) paste0(x, '_itt'))
  
  # Calculate completer yes proportion.
  df_completer <- outcome_df %>%
    filter(event %in% c('yes', 'no_completer')) %>% 
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
    ungroup() %>% 
    filter(event == 'yes') %>% 
    calc_p_adjust() %>% 
    select(outcome, timepoint, age, event, calc_total, estimate, conf.low, conf.high, p.value, p_signif, p_adj, p_adj_signif, signif_dropped) %>% 
    rename_with(.cols = -c(outcome, timepoint, age, event), .fn = \(x) paste0(x, '_completer'))
  
  # Merge the two tables.
  df_merge <- left_join(
    df_itt,
    df_completer,
    by = join_by(outcome, timepoint, age, event)
  )
  
  return(df_merge)
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
    filter(
      outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression'),
      event %in% c('yes', 'no_completer') # Need to make sure the comparisons are between "yes" and "no_completer". Don't want to look at "no_itt."
    ) %>% 
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
          
          estimate_or <- (mat[group$timepoint, 'yes'] / mat[group$timepoint, 'no_completer']) / (mat['Baseline', 'yes'] / mat['Baseline', 'no_completer'])
          
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
  df1 <- outcome_df %>% 
    filter(
      outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression'),
      event %in% c('yes', 'no_completer')
    ) %>% 
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
      
      estimate_or <- (mat['child', 'yes'] / mat['child', 'no_completer']) / (mat['adult', 'yes'] / mat['adult', 'no_completer'])
      
      results <- chisq_with_fisher(mat) %>% 
        mutate('estimate' = estimate_or)
      
      return(results)
    }) %>% 
    ungroup() %>% 
    mutate('event' = 'yes') %>% 
    mutate(
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
    
  
  # Perform a second set of tests for A1c. Needs to be calculated differently since the completer values are a little harder to calculate for this.
  df2 <- outcome_df %>% 
    filter(
      outcome == 'a1c' |
      (outcome == 'encounter' & event == 'yes')) %>% 
    group_by(timepoint) %>% 
    group_modify(\(df, group) {
      # browser()
      
      df_encounter <- df %>% 
        filter(outcome == 'encounter') %>% 
        select(-event) # Event column isn't needed
        
      
      df_a1c <- df %>% 
        filter(outcome == 'a1c') %>% 
        group_by(event)
      
      results <- df_a1c %>% 
        group_modify(\(df, group) {
          # browser()
          # Convert to matrix, then perform chi-square or fisher.
          mat <- df %>% 
            rbind(df_encounter) %>% 
            mutate(outcome = factor(outcome, levels = c('a1c', 'encounter'))) %>%  # Make a factor and arrange for consistent order when pivoted
            arrange(outcome) %>% 
            pivot_wider(
              id_cols = age,
              names_from = outcome,
              values_from = count
            ) %>% 
            column_to_rownames('age') %>% 
            as.matrix()
          
          estimate_or <- (mat['child', 1] / mat['child', 2]) / (mat['adult', 1] / mat['adult', 2])
          
          results <- chisq_with_fisher(mat) %>% 
            mutate('estimate' = estimate_or)
          
          return(results)
          
        }) %>% 
        ungroup() %>% 
        mutate('outcome' = 'a1c')
      
      return(results)
      
    }) %>% 
    ungroup() %>% 
      filter(!is.na(p.value)) # Remove any tests with NA p-values.
  
  results <- bind_rows(df1, df2)
  
  return(results)
}

calc_outcome_age_retention_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(
      outcome =='encounter',
      event %in% c('yes', 'no_itt')
    ) %>% 
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
      
      estimate_or <- (mat['child', 'yes'] / mat['child', 'no_itt']) / (mat['adult', 'yes'] / mat['adult', 'no_itt'])
      
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
  completer_df <- outcome_df %>% 
    filter(event %in% c('yes', 'no_completer')) %>% 
    pivot_wider(
      id_cols = c(outcome, timepoint, age),
      names_from = event,
      values_from = count
    ) %>% 
    mutate('prop_completer' = yes / (yes + no_completer))
  itt_df <- outcome_df %>% 
    filter(event %in% c('yes', 'no_itt')) %>% 
    pivot_wider(
      id_cols = c(outcome, timepoint, age),
      names_from = event,
      values_from = count
    ) %>% 
    mutate('prop_itt' = yes / (yes + no_itt)) %>% 
    select(-yes)
  
  results <- left_join(
    completer_df,
    itt_df,
    by = join_by(outcome, timepoint, age)
  ) %>% 
    mutate('prop_diff' = prop_completer - prop_itt)
  return(results)
}

calc_outcome_age_logistic_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(
      outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression'),
      event %in% c('yes', 'no_completer')
    ) %>% 
    group_by(outcome, timepoint) %>% 
    group_modify(\(df, group) {
      # browser()
      # Create a data frame using the baseline record and the current record, then run a logistic regression.
      df <- data.frame(
        response = c(
          rep('yes', filter(df, age == 'child', event == 'yes')$count),
          rep('no_completer', filter(df, age == 'child', event == 'no_completer')$count),
          rep('yes', filter(df, age == 'adult', event == 'yes')$count),
          rep('no_completer', filter(df, age == 'adult', event == 'no_completer')$count)
        ) %>% 
          factor(levels = c('no_completer', 'yes')) %>% 
          as.integer() %>% 
          {.-1},
        timepoint = c(
          rep('child', filter(df, age == 'child', event == 'yes')$count),
          rep('child', filter(df, age == 'child', event == 'no_completer')$count),
          rep('adult', filter(df, age == 'adult', event == 'yes')$count),
          rep('adult', filter(df, age == 'adult', event == 'no_completer')$count)
        ) %>% 
          factor(levels = c('adult', 'child'))
      )
      results <- tidy(glm(df, formula = response ~ timepoint, family = 'binomial'), conf.int = TRUE, exponentiate = TRUE) %>% 
        filter(term != '(Intercept)') %>% 
        select(-term) %>% 
        mutate('treatment' = 'child', .before = estimate)
      
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

calc_omnibus_variance_itt_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(event %in% c('yes', 'no_itt')) %>% 
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

calc_omnibus_variance_completer_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(event %in% c('yes', 'no_completer')) %>% 
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
        filter(event %in% c('yes', 'no_itt')) %>% 
        pivot_wider(
          id_cols = age,
          names_from = event,
          values_from = count
        ) %>% 
        column_to_rownames('age') %>% 
        as.matrix()
      
      child_greater <- (mat['child','yes'] / mat['child', 'no_itt']) / (mat['adult','yes'] / mat['adult', 'no_itt']) > 1
      if (child_greater | is.na(child_greater)) {
        mat['child', 'no_itt'] <- mat['child', 'no_itt'] + (97 - sum(mat['child',]))
        mat['adult', 'yes'] <- mat['adult', 'yes'] + (401 - sum(mat['adult',]))
      } else{
        mat['child', 'yes'] <- mat['child', 'yes'] + (97 - sum(mat['child',]))
        mat['adult', 'no_itt'] <- mat['adult', 'no_itt'] + (401 - sum(mat['adult',]))
      }
      
      estimate_or <- (mat['child', 'yes'] / mat['child', 'no_itt']) / (mat['adult', 'yes'] / mat['adult', 'no_itt'])
      
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
    select(chisq_tbl, outcome, timepoint, estimate, p.value, p_signif),
    select(adversarial_chisq_tbl, outcome, timepoint, estimate, p.value, p_signif),
    by = join_by(outcome, timepoint)
  ) %>% 
    mutate(
      'adversarial_direction' = ifelse(sign(estimate.x) == sign(estimate.y), 'same', 'different'),
      'adversarial_significance' = ifelse(p_signif.x == p_signif.y, 'same', 'different'),
      'any_adversarial_effect' = (adversarial_direction == 'different' | adversarial_significance == 'different'),
      'adversarial_reversed' = (adversarial_direction == 'different' & p_signif.x & p_signif.y)
    )
  return(results)
}

# This function performs a simple simulated dataset with the number of initial yesses, initial nos, number lost to followup, 
# and event probability within the followup group
simulate_ltf <- function(n_yes, n_no, n_ltf, p_ltf) {
  if(n_ltf > 10) {}
  c(rep(1, n_yes), rep(0, n_no), rbinom(n_ltf, 1, p_ltf))
}

# This function takes two sets of nos, yesses, lost-to-followup, and lost-to-followup probabilities and performs a logistic regression test,
# outputting whether or not the result is significant and whether it agrees with the estimated probabilities.
glm_comparison <- function(n_yes_tpiat, n_yes_tp, n_no_tpiat, n_no_tp, n_ltf_tpiat, n_ltf_tp, p_ltf_tpiat, p_ltf_tp) {
  # browser()
  # Take initial data and form simulation.
  df_tpiat <- data.frame(
    'cohort' = 'tpiat', 
    'event' = simulate_ltf(n_yes_tpiat, n_no_tpiat, n_ltf_tpiat, p_ltf_tpiat)
  )
  df_tp <- data.frame(
    'cohort' = 'tp', 
    'event' = simulate_ltf(n_yes_tp, n_no_tp, n_ltf_tp, p_ltf_tp)
  )
  df <- rbind(df_tpiat, df_tp)
  df$cohort <- factor(df$cohort, levels = c('tp', 'tpiat'))
  x <- model.matrix(event ~ cohort, df)
  glm_res <- fastglm(
    x = x,
    y = df$event,
    family = 'binomial',
    method = 3
  ) %>% 
    summary() %>% 
    coef()
  res <- list(
    'effect_agrees' = ((n_yes_tpiat / (n_yes_tpiat + n_no_tpiat)) > (n_yes_tp / (n_yes_tp + n_no_tp))) == 
      (glm_res[2,'Estimate', drop=TRUE] > 0),
    'is_signif' = glm_res[2,'Pr(>|z|)', drop=TRUE] < 0.05
  )
  
  return(res)
}


# This function calculates m glm_comparison results, and find the mean of effect agreement and significance across them.
mean_glms <- function(n_yes_tpiat, n_yes_tp, n_no_tpiat, n_no_tp, n_ltf_tpiat, n_ltf_tp, p_ltf_tpiat, p_ltf_tp, m) {
  # browser()
  sims <- as.list(rep(NA, m))
  for (i in seq(m)) {
    sims[[i]] <- glm_comparison(n_yes_tpiat, n_yes_tp, n_no_tpiat, n_no_tp, n_ltf_tpiat, n_ltf_tp, p_ltf_tpiat, p_ltf_tp)
  }
  sims <- list_transpose(sims)
  res <- list(
    'mean_effect_sign' = mean(c(-1,1)[sims$effect_agrees+1]*sims$is_signif),
    'mean_signif' = mean(sims$is_signif),
    'mean_tipping_point_reached' = mean(!(sims$effect_agrees) | !(sims$is_signif))
  )
  return(res)
}


# This function produces a grid of lost-to-followup probabilities, then iterates through that list and calculates the mean glm results at each
# level of lost-to-followup probability.
grid_comparisons <- function(df, tpiat_row, tp_row, p_ltf_grid, m) {
  # browser()
  p_ltf_df <- p_ltf_grid %>% 
    as_tibble() %>% 
    mutate(
      'mean_effect_sign' = as.list(rep(NA, nrow(p_ltf_grid))),
      'mean_signif' = as.list(rep(NA, nrow(p_ltf_grid))),
      'mean_tipping_point_reached' = as.list(rep(NA, nrow(p_ltf_grid)))
    )
  for (i in seq(nrow(p_ltf_df))) {
    # if (near(p_ltf_grid$p_ltf_tpiat[i], 0.0) & near(p_ltf_grid$p_ltf_tp[i], 1.0)) {
    #   browser()
    # }
    sim_res <- mean_glms(
      df$n_yes[tpiat_row], df$n_yes[tp_row], 
      df$n_no[tpiat_row], df$n_no[tp_row], 
      df$n_ltf[tpiat_row], df$n_ltf[tp_row],
      p_ltf_grid$p_ltf_tpiat[i], p_ltf_grid$p_ltf_tp[i],
      m
    )
    p_ltf_df$mean_effect_sign[[i]] <- sim_res$mean_effect_sign
    p_ltf_df$mean_signif[[i]] <- sim_res$mean_signif
    p_ltf_df$mean_tipping_point_reached[[i]] <- sim_res$mean_tipping_point_reached
  }
  
  p_ltf_df$mean_effect_sign <- unlist(p_ltf_df$mean_effect_sign)
  p_ltf_df$mean_signif<- unlist(p_ltf_df$mean_signif)
  p_ltf_df$mean_tipping_point_reached<- unlist(p_ltf_df$mean_tipping_point_reached)
  
  return(p_ltf_df)
}



calc_adversarial_tipping_points <- function(outcome_df, between_outcome_chisq_tests) {
  # Create data frame of simulations
  tmpdf <- outcome_df %>%
    filter(outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression')) %>% 
    pivot_wider(names_from = event, values_from = count) %>% 
    mutate(
      'n_yes' = yes,
      'n_no' = no_completer,
      # Apparently the numbers can vary by 1 or 2 due to independent suppression/rounding, so the cases where n_ltf goes negative are in error.
      'n_ltf' = pmax(no_itt, no_completer, 0),
      'p_hat' = n_yes / (n_yes + no_completer)
    ) %>% 
    select(-c(yes, no_itt, no_completer)) %>% 
    mutate('i' = row_number())
  
  tmpdf <- tmpdf %>% 
    arrange(desc(age == 'child')) %>% 
    group_by(outcome, timepoint) %>% 
    summarise(
      'ltf_grid' = list(grid_comparisons(
        tmpdf, 
        tpiat_row = i[1], 
        tp_row = i[2], 
        p_ltf_grid = expand.grid('p_ltf_tpiat' = seq(0,1,0.01), 'p_ltf_tp' = p_hat[2]), 
        m=30
      ))
    ) %>% 
    ungroup()
  
  # For each row, find the ltf probability nearest to the true probability for which either the effect disagrees from the result, 
  # or the glm is significant less than half the time. For samples that are already significant, they will be the closest p_ltf to p_hat
  tmpdf$tipping_point <- map2_dbl(
    .x = tmpdf$ltf_grid,
    .y = map(tmpdf$ltf_grid, \(x) x$p_ltf_tp[[1]]),
    .f = \(x, y) {
      x %>% 
        mutate('p_hat_dist' = abs(p_ltf_tpiat - y)) %>% 
        filter(mean_tipping_point_reached > 0.5) %>% 
        slice_min(p_hat_dist, n=1) %>% 
        {ifelse(nrow(.) == 1, .$p_ltf_tp, NA)}
      
    }
  )
  
  results <- between_outcome_chisq_tests %>% 
    select(outcome, timepoint, p.value) %>% 
    left_join(
      select(tmpdf, outcome, timepoint, tipping_point),
      by = join_by(outcome, timepoint)
    ) %>% 
    mutate(
      tipping_point = case_when(
        p.value >= 0.05 ~ 'not significant',
        is.na(tipping_point) ~ 'not reached',
        .default = as.character(formatC(tipping_point, digits = 3))
      )
    )
  
  return(results)
  
}


save_results_excel <- function(x, results) {
  
  wb <- wb_workbook()
  for(sheet in names(results)) {
    wb$add_worksheet(sheet = sheet)
    wb$add_data_table(sheet = sheet, x = results[[sheet]])
  }
  
  wb_save(wb, file = x)
}








