prep_outcome_df <- function(outcome_df_raw, outcome_a1c_df_raw) {
  # Load the standard outcome dfs that are all in the same format.
  outcome_df <- outcome_df_raw %>% 
    select(outcome, timepoint, child_yes, adult_yes, child_no, adult_no) %>% 
    pivot_longer(
      cols = c(child_yes, adult_yes, child_no, adult_no),
      names_to = c('age', 'event'),
      values_to = 'count',
      names_sep='_'
    ) %>% 
    mutate(timepoint, outcome, event, age, count, .keep = 'none')
  
  # The outcome A1C is in a different format and so needs to be handled differently.
  outcome_a1c_df <- outcome_a1c_df_raw %>% 
    select(outcome, timepoint, event, child_event, adult_event) %>% 
    pivot_longer(
      cols = c(child_event, adult_event),
      names_to = c('age'),
      values_to = 'count',
      names_pattern='(^[[:print:]]+(?=_))'
    ) %>% 
    mutate(timepoint, outcome, event, age, count, .keep = 'none')
  
  # Thomas wants the A1c data binned, so I will create a new set of outcomes which bin the a1c samples.
  outcome_a1c_bin_df <- outcome_a1c_df %>% 
    group_by(timepoint, outcome, age) %>% 
    group_modify(.f = \(df, group) {
      df_5 <- filter(df, event == '<5.7')
      df_7 <- filter(df, event %in% c('7.0-7.9', '8.0-8.9', '>=9.0')) %>% 
        summarize('event' = '>=7.0', count = sum(count))
      df_9 <- filter(df, event == '>=9.0')
      
      df_new <- bind_rows(df_5, df_7, df_9)
      
      return(df_new)
    }) %>% 
    mutate(outcome = 'a1c_bin')
  
  # Merge the outcome dfs
  df <- bind_rows(outcome_df, outcome_a1c_df, outcome_a1c_bin_df)
  
  # Previous calculations are for ITT only. So the "no" events should actually be "no_itt".
  # Need to calculate the the completer values, using the individuals who had at least one event. Then I can calculate "no_completer" as the difference between
  # each event yes, and the "had encounter" event yes.
  yes_df <- df %>% 
    filter(
      outcome != 'encounter',
      event == 'yes'
    ) %>% 
    mutate('n_yes' = count) %>% 
    select(-c(event, count)) 
  completer_df <- df %>% 
    filter(
      outcome == 'encounter',
      event == 'yes'
    ) %>% 
    mutate('n_completer' = count) %>% 
    select(-c(outcome, event, count))
    
  # Merge the n_completer table with the "yes" values to calculate no_completer
  no_completer_df <- left_join(
    yes_df,
    completer_df,
    by = join_by(timepoint, age)
  ) %>% 
    mutate(
      event = 'no_completer',
      count = n_completer - n_yes,
      .keep = 'unused'
    )
  
  # Add the "no_completer" values to the final table, and rename the current "no" to "no_itt".
  df2 <- df %>% 
    mutate(event = ifelse(event == 'no', 'no_itt', event)) %>% 
    rbind(no_completer_df)
  
  return(df2)
}

prep_demographics <- function(demo_df) {
  
  colnames(demo_df) <- c('characteristic', 'child', 'adult', 'total')
  
  for(x in c('child', 'adult', 'total')) {
    demo_df[,x] <- as.integer(demo_df[,x])
  }
  
  # Extract the count record
  total <- demo_df %>% filter(characteristic == 'Count')
  
  df <- demo_df %>% 
    filter(characteristic != 'Count') %>% 
    select(characteristic, child, adult) %>% 
    pivot_longer(
      cols = c(child, adult),
      names_to = 'age',
      values_to = 'count'
    ) %>% 
      mutate(
        count = as.integer(count),
        age_total = ifelse(age == 'child', total$child, total$adult),
        total = total$total
      )
    
  return(df)
}