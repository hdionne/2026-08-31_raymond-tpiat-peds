calc_wilson_tests <- function(outcome_df) {
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
    filter(outcome == 'a1c_bin' | event == 'yes') %>% 
    select(outcome, timepoint, age, event, count, calc_total, estimate, conf.low, conf.high, p.value)
  
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
    select(outcome, timepoint, age, event, count, calc_total, estimate, conf.low, conf.high, p.value)
  
  # Merge the two tables.
  df_merge <- bind_rows('itt'  = df_itt, 'completer' = df_completer, .id = 'total_type') %>% 
    relocate(total_type, .before = calc_total)
  
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
        mutate('estimate_or' = estimate_or)
      return(result)
    }) %>% 
    ungroup()
  
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
        mutate('estimate_or' = estimate_or)
      
      return(results)
    }) %>% 
    ungroup() %>% 
    mutate('event' = 'yes')

    
  
  # Perform a second set of tests for A1c. Needs to be calculated differently since the completer values are a little harder to calculate for this.
  df2 <- outcome_df %>% 
    filter(
      outcome == 'a1c_bin' |
      (outcome == 'encounter' & event == 'yes')) %>% 
    group_by(timepoint) %>% 
    group_modify(\(df, group) {
      # browser()
      
      df_encounter <- df %>% 
        filter(outcome == 'encounter') %>% 
        select(-event) # Event column isn't needed
        
      
      df_a1c <- df %>% 
        filter(outcome == 'a1c_bin') %>% 
        group_by(event)
      
      results <- df_a1c %>% 
        group_modify(\(df, group) {
          # browser()
          # Convert to matrix, then perform chi-square or fisher.
          mat <- df %>% 
            rbind(df_encounter) %>% 
            mutate(outcome = factor(outcome, levels = c('a1c_bin', 'encounter'))) %>%  # Make a factor and arrange for consistent order when pivoted
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
            mutate('estimate_or' = estimate_or)
          
          return(results)
          
        }) %>% 
        ungroup() %>% 
        mutate('outcome' = 'a1c_bin')
      
      return(results)
      
    }) %>% 
    ungroup() %>% 
      filter(!is.na(p.value)) # Remove any tests with NA p-values.
  
  results <- bind_rows(df1, df2)
  
  return(results)
}

calc_outcome_age_retention_rates <- function(outcome_df) {
  # browser()
  # Get the retention rates, then calculate the wilson proportion and confidence interval. 
  results <- outcome_df %>% 
    filter(outcome == 'encounter', event == 'yes') %>% 
    select(timepoint, age, total = count) %>% 
    left_join(
      .,
      filter(., timepoint == 'Baseline'),
      by = join_by(age),
      suffix = c('_current', '_baseline')
    ) %>% 
    mutate(
      'n_ltf' = total_baseline - total_current,
      'prop_retained' = total_current / total_baseline
    ) %>% 
    rowwise() %>% 
    mutate(
      map2(n_ltf, total_baseline, \(x, y) {
        # if (x == 42) {browser()}
        if(!anyNA(c(x, y))) {
          tidy(prop.test(x, y, correct = FALSE))
        } else {
          data.frame('estimate' = NA)
        }
      }) %>% 
        bind_rows()
    ) %>% 
    rename('timepoint' = timepoint_current) %>% 
    select(-timepoint_baseline)
  
  return(results)
}

calc_outcome_age_retention_chisq_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(
      outcome =='encounter',
      event %in% c('yes', 'no_itt')
    ) %>% 
    mutate(event = factor(event, c('yes', 'no_itt'))) %>% 
    group_by(outcome, timepoint) %>% 
    group_modify(\(df, group) {
      # Convert to matrix, then perform chi-square or fisher.
      mat <- df %>% 
        pivot_wider(
          id_cols = age,
          names_expand = TRUE,
          names_from = event,
          values_from = count,
          values_fill = 0
        ) %>% 
        column_to_rownames('age') %>% 
        as.matrix()
      
      estimate_or <- (mat['child', 'yes'] / mat['child', 'no_itt']) / (mat['adult', 'yes'] / mat['adult', 'no_itt'])
      
      results <- chisq_with_fisher(mat) %>% 
        mutate('estimate' = estimate_or)
      
      return(results)
    }) %>% 
    ungroup()
  

  
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
  

  
  return(results)
}

calc_outcome_age_logistic_tests <- function(outcome_df) {
  results <- outcome_df %>% 
    filter(
      outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression'),
      event %in% c('yes', 'no_completer')
    ) %>% 
    group_by(outcome) %>% 
    group_modify(\(df, group) {
      # browser()
      # Create a data frame using the baseline record and the current record, then run a logistic regression.
      
      df_new <- df %>%  # Convert the data frame into a longer version by grouping by all the variables and creating duplicates for each count.
        group_by(age, timepoint, event) %>% 
        group_modify(\(df, group) {
          df <- data.frame(row.names = seq(df$count))
        }) %>% 
        mutate(event = factor(event, levels = c('no_completer', 'yes')) %>% as.integer() %>% {.-1})
      
      # Calculate the expanded logistic regression
      mod_full <- glm(df_new, formula = event ~ timepoint + age:timepoint, family = 'binomial')
      # Calculate the reduced logistic regression
      mod_reduced <- glm(df_new, formula = event ~ timepoint, family = 'binomial')
      # Calculate the Anova.
      mod_aov <- anova(mod_reduced, mod_full)
      # Convert the models into results tables.
      results_full <- tidy(mod_full, conf.int = TRUE, exponentiate = TRUE) %>%
        filter(grepl(x = term, pattern = 'agechild$')) %>%  # Only care about the interaction terms
        mutate(
          'timepoint' = str_extract(term, pattern = '(?<=timepoint)[[:print:]]+(?=\\:)'),
          'age' = str_extract(term, pattern = '(?<=age)[[:print:]]+'),
        ) %>% 
        select(timepoint, age,estimate, conf.low, conf.high, p.value)
      results_aov <- tidy(mod_aov)[2,] %>% 
        select(p.value)
      # Merge results tables.
      results <- bind_rows(
        'logistic' = results_full,
        'anova' = results_aov,
        .id = 'test'
      )
      
      return(results)
    }) %>% 
    ungroup()
  

  
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
  

  
  return(results)
}

calc_adversarial_outcome_comparison <- function(chisq_tbl, adversarial_chisq_tbl) {
  results <- left_join(
    select(chisq_tbl, outcome, timepoint, estimate, p.value),
    select(adversarial_chisq_tbl, outcome, timepoint, estimate, p.value),
    by = join_by(outcome, timepoint)
  ) %>% 
    mutate(
      'adversarial_direction' = ifelse(sign(estimate.x) == sign(estimate.y), 'same', 'different'),
      'adversarial_significance' = ifelse((p.value.x < 0.05) == (p.value.y < 0.05), 'same', 'different'),
      'any_adversarial_effect' = (adversarial_direction == 'different' | adversarial_significance == 'different'),
      'adversarial_reversed' = (adversarial_direction == 'different' & p.value.x < 0.05 & p.value.y < 0.05)
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


# This type of analysis can probably be converted into a tidymodels grid, where the additional simulated data is handled via preprocessing. 
# This would be a good thing to look into, as it could make a decent tidymodels extension package. 
calc_adversarial_tipping_point_proportions <- function(outcome_df, outcome_retention_rates, between_outcome_chisq_tests) {
  # Create data frame of simulations
  # browser()
  had_encounter_df <- select(outcome_retention_rates, timepoint, age, n_ltf)
  
  tmpdf <- outcome_df %>%
    filter(outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression')) %>% 
    pivot_wider(names_from = event, values_from = count) %>% 
    mutate(
      'n_yes' = yes,
      'n_no' = no_completer,
      # Apparently the numbers can vary by 1 or 2 due to independent suppression/rounding, so the cases where n_ltf goes negative are in error.
      'p_hat' = n_yes / (n_yes + no_completer)
    ) %>% 
    select(-c(yes, no_itt, no_completer)) %>% 
    mutate('i' = row_number()) %>% 
    left_join(had_encounter_df, join_by(timepoint, age))
  
  tmpdf <- tmpdf %>% 
    arrange(desc(age == 'child')) %>% 
    group_by(outcome, timepoint) %>% 
    reframe(
      'ltf_grid' = list(
        grid_comparisons(
          tmpdf, 
          tpiat_row = i[1], 
          tp_row = i[2], 
          p_ltf_grid = expand.grid('p_ltf_tpiat' = seq(0,1,0.01), 'p_ltf_tp' = p_hat[2]), 
          m=30
        ),
        grid_comparisons(
          tmpdf, 
          tpiat_row = i[1], 
          tp_row = i[2], 
          p_ltf_grid = expand.grid('p_ltf_tpiat' = p_hat[1], 'p_ltf_tp' = seq(0,1,0.01)), 
          m=30
        )
      ),
      'shift_group' = c('child', 'adult')
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
    inner_join(
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


# This function performs a simple simulated dataset with the number of initial yesses, initial nos, lost-to-followup ks, 
# whether to apply the k the comp or ref group, and the direction of the k-shift, and event probability within the 
# followup group
simulate_ltf_k <- function(n_yes, n_no, k, k_direction) {
  c(rep(1, n_yes), rep(0, n_no), rep(k_direction, k))
}


# This function takes two sets of nos, yesses, lost-to-followup ks, whether to apply the k the comp or ref group, and the direction of the k-shift, 
# and performs a logistic regression test, outputting whether or not the result is significant and whether it agrees with the estimated probabilities.
glm_comparison_k <- function(
  n_yes_comp, n_yes_ref, n_no_comp, n_no_ref, 
  k_comp, k_ref, 
  k_comp_dir, k_ref_dir
) {
  # browser()
  # Take initial data and form simulation.
  df_comp <- data.frame(
    'cohort' = 'comp', 
    'event' = simulate_ltf_k(n_yes_comp, n_no_comp, k_comp, k_comp_dir)
  )
  df_ref <- data.frame(
    'cohort' = 'ref', 
    'event' = simulate_ltf_k(n_yes_ref, n_no_ref, k_ref, k_ref_dir)
  )
  df <- rbind(df_comp, df_ref)
  df$cohort <- factor(df$cohort, levels = c('ref', 'comp'))
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
    'effect_agrees' = ((n_yes_comp / (n_yes_comp + n_no_comp)) > (n_yes_ref / (n_yes_ref + n_no_ref))) == 
      (glm_res[2,'Estimate', drop=TRUE] > 0),
    'is_signif' = glm_res[2,'Pr(>|z|)', drop=TRUE] < 0.05
  )
  res$tipping_point_reached <- !(res$effect_agrees) | !(res$is_signif)
  
  return(res)
}


# This function produces a grid of lost-to-followup probabilities, then iterates through that list and calculates the glm result at each
# level of lost-to-followup probability.
# Since this is a constant change, there is no need for simulations
seq_comparisons_k <- function(df, comp_row, ref_row, k_ltf_seq, which_k) {
  # if (
  #   df$outcome[comp_row] == 'anxiety' &
  #   df$timepoint[comp_row] == '3 Year' &
  #   df$age[comp_row] == 'child'
  # ) {
  #   browser()
  # }
  comp_higher <- (df$n_yes[comp_row] / df$n_no[comp_row]) > (df$n_yes[ref_row] / df$n_no[ref_row])
  
  k_ltf_df <- k_ltf_seq %>% 
    as_tibble_col(column_name = 'k') %>% 
    mutate(
      'effect_sign' = rep(NA, nrow(.)),
      'signif' = rep(NA, nrow(.)),
      'tipping_point_reached' = rep(NA, nrow(.))
    )
  for (i in seq(nrow(k_ltf_df))) {
    # if (near(p_ltf_grid$p_ltf_comp[i], 0.0) & near(p_ltf_grid$p_ltf_tp[i], 1.0)) {
    #   browser()
    # }
    
    # For each value of k, perform a glm comparison
    sim_res <- glm_comparison_k(
      df$n_yes[comp_row], df$n_yes[ref_row], df$n_no[comp_row], df$n_no[ref_row], 
      ifelse(which_k == 'comp', k_ltf_df$k[i], 0), ifelse(which_k == 'ref', k_ltf_df$k[i], 0), 
      1-comp_higher, as.integer(comp_higher)
    )

    # Extract the results of the glm comparison and add it into the table.
    k_ltf_df$effect_sign[[i]] <- sim_res$effect_agrees
    k_ltf_df$signif[[i]] <- sim_res$is_signif
    k_ltf_df$tipping_point_reached[[i]] <- sim_res$tipping_point_reached
  }

  return(k_ltf_df)
}


# This type of analysis can probably be converted into a tidymodels grid, where the additional simulated data is handled via preprocessing. 
# This would be a good thing to look into, as it could make a decent tidymodels extension package. 
calc_adversarial_tipping_point_ks <- function(outcome_df, outcome_retention_rates, between_outcome_chisq_tests) {
  # Create data frame of simulations
  
  had_encounter_df <- select(outcome_retention_rates, timepoint, age, n_ltf)
  
  
  tmpdf <- outcome_df %>%
    filter(outcome %in% c('opioid_use', 'hospitalization', 'anxiety', 'depression')) %>% 
    pivot_wider(names_from = event, values_from = count) %>% 
    mutate(
      'n_yes' = yes,
      'n_no' = no_completer,
      # Apparently the numbers can vary by 1 or 2 due to independent suppression/rounding, so the cases where n_ltf goes negative are in error.
      'p_hat' = n_yes / (n_yes + no_completer)
    ) %>% 
    select(-c(yes, no_itt, no_completer)) %>% 
    mutate('i' = row_number()) %>% 
    left_join(had_encounter_df, by = join_by(timepoint, age))
  
  tmpdf2 <- tmpdf %>% 
    arrange(desc(age == 'child')) %>% 
    group_by(outcome, timepoint) %>% 
    reframe(
      'shift_group' = c('child', 'adult'),
      'ltf_grid' = list(
        seq_comparisons_k(tmpdf, i[1], i[2], c(0,seq(n_ltf[1])), 'comp'),
        seq_comparisons_k(tmpdf, i[1], i[2], c(0,seq(n_ltf[2])), 'ref')
      )
    ) %>% 
    ungroup()
  
  # For each row, find the ltf probability nearest to the true probability for which either the effect disagrees from the result, 
  # or the glm is significant less than half the time. For samples that are already significant, they will be the closest p_ltf to p_hat
  tmpdf2$tipping_point <- map_dbl(
    .x = tmpdf2$ltf_grid,
    .f = \(x) {
      x %>% 
        filter(tipping_point_reached > 0.5) %>% 
        slice_min(k, n=1) %>% 
        {ifelse(nrow(.) == 1, .$k, NA)}
      
    }
  )
  
  results <- between_outcome_chisq_tests %>% 
    select(outcome, timepoint, p.value) %>% 
    inner_join(
      select(tmpdf2, outcome, timepoint, shift_group, tipping_point),
      by = join_by(outcome, timepoint)
    ) %>% 
    mutate(
      tipping_point = case_when(
        p.value >= 0.05 ~ 'not significant',
        is.na(tipping_point) ~ 'not reached',
        .default = as.character(tipping_point)
      )
    )
  
  return(results)
  
}


calc_adj_p_values <- function(...) {
  #  This function takes a bunch of tables, defines a list of table subsets from these tables to get corrected p-values 
  # from them, and calculates the adjusted p-values across these tables. 
  # Get the names of the targets that I want to calculate p-values from. 
  target_names <- map_chr(rlang::exprs(...), as.character)
  targets <- setNames(rlang::dots_list(...), target_names)
  # en_table_list <- sym(list(...))
  
  # This function extracts the table name and row name, adds them to the table, applies optional filtering 
  # (or any additional function) and returns the table name, index, and p-values as a table.
  collect_p_values <- function(tbl, .f = NULL, ...) {
    table_name <- as.character(rlang::ensym(tbl))
    tbl <- mutate(targets[[table_name]], 'i' = row_number(), 'table_name' = table_name)
    if(is.function(.f)) tbl <- .f(tbl, ...)
    tbl <- select(tbl, table_name, i, p.value)
    return(tbl)
  }
  
  # This is going to be a table containing four columns: a group variable, a sheet variable, a row number variable, and 
  # a p-value variable. The full table will be constructed using a bind_rows call. Each group will be user defined, and 
  # will probably be made up of a second bind_rows call if merging multiple tables. 
  # Maybe in the future I can make this a list of formulas like gtsummary, so I don't need to wrap the same groups in
  # `bind_rows`.
  p_values <- bind_rows(
    # For the `between-cohort` group, calculate adjusted p-values for all between-outcome chi-square and retention tests, 
    # including binned a1c but excluding non-binned a1c.
    'baseline' = bind_rows(
      collect_p_values(demo_chisq_tests),
      collect_p_values(between_outcome_chisq_tests), 
      collect_p_values(between_outcome_retention_chisq_tests)
    ),
    'within-cohort' = bind_rows(
      collect_p_values(outcome_wilson_tests),
      collect_p_values(within_outcome_chisq_tests),
      collect_p_values(hba1c_age_tests)
    ),
    'confirmatory-logistic' = collect_p_values(between_outcome_logistic_regression, filter, test == 'logistic'),
    'between-cohort-anova' = collect_p_values(between_outcome_logistic_regression, filter, test == 'anova'),
    'within-group-omnibus-itt' = collect_p_values(omnibus_variance_itt_chisq_tests),
    'within-group-omnibus-completer' = collect_p_values(omnibus_variance_completer_chisq_tests),
    'hba1c-above-goal' = collect_p_values(hba1c_above_goal_age_tests),
    'adversarial_between_outcome' = collect_p_values(adversarial_between_outcome_chisq_tests),
    .id = 'p_adjust_group'
  )
  
  adjust_p_values <- function(df) {
    df_new <- mutate(
      df,
      'p_adj' = p.adjust(p.value, method = 'BH'),
      'p_signif' = p.value < 0.05,
      'p_adj_signif' = p_adj < 0.05,
      'signif_dropped' = p_signif & (!p_adj_signif)
    )
    return(df_new)
  }
  
  adj_p_values <- p_values %>%
    group_by(p_adjust_group) %>% 
    group_modify(.f = \(df, group) adjust_p_values(df))
  
  return(adj_p_values)
}

add_adjusted_p <- function(data, p_adj_df) {
  target_name = as.character(rlang::enexpr(data))
  
  data <- data %>% 
    mutate('i' = row_number())
  
  p_adj_df <- p_adj_df %>% 
    filter(table_name == target_name) %>% 
    select(-c(table_name))
  
  left_join(data, p_adj_df, join_by(i, p.value))
  
}







