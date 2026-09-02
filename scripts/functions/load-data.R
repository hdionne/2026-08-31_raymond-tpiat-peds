load_outcome_sheet <- function(wb, sheet) {
  df <- wb_read(
    wb,
    sheet,
    rows = 3:7,
    cols = col2int('A'):col2int('L')
  )
  
  colnames(df) <- c('timepoint', 'description', 'child_yes', 'adult_yes', 'yes_calc-total', 'yes_input-total', 'child_no', 'adult_no', 'no_calc-total', 'no_input-total', 'calc-total', 'input-total')
  
  for(x in colnames(df)[-c(1,2)]) {
    df[,x] <- as.integer(df[,x])
  }
  
  return(df)
}


load_outcome_df <- function(wb) {
  sheets_to_load <- c(
    'opioid_use' = 'TPIAT Opioid Pop Method',
    'hospitalization' = 'TPIAT Hospitalization PM',
    'anxiety' = 'TPIAT Anxiety PM',
    'depression' = 'TPIAT Depression PM',
    'encounter' = 'TPIAT Had Encounter PM'
  )
  
  dfs <- map(as.list(sheets_to_load), \(sheet) load_outcome_sheet(wb, sheet))
  df <- bind_rows(dfs, .id = 'outcome')
  
  return(df)
}

load_outcome_a1c_sheet <- function(wb, start_col) {
  df_raw <- wb_read(
    wb,
    'TPIAT A1C PM',
    rows = 3:7,
    cols = c(col2int(c('A', 'B')), seq(col2int(start_col), length.out = 4), col2int('AA'):col2int('AF'))
  )
  
  colnames(df_raw) <- c('timepoint', 'description', 'child_event', 'adult_event', 'event_calc_total', 'event_input_total', 'child_calc_total', 'adult_calc_total', 'calc_total', 'child_input_total', 'adult_input_total', 'input_total')
  
  for(x in colnames(df_raw)[-c(1,2)]) {
    df_raw[,x] <- as.integer(df_raw[,x])
  }
  
  return(df_raw)
}

load_outcome_a1c_df <- function(wb) {
  start_cols <- as.list(col2int(c('C', 'G', 'K', 'O', 'S', 'W'))) %>% 
    setNames(c('<5.7', '5.7-6.4', '6.5-6.9', '7.0-7.9', '8.0-8.9', '>=9.0'))
  dfs <- map(start_cols, \(x) load_outcome_a1c_sheet(wb, x))
  
  df <- bind_rows(dfs, .id = 'event')
  
  df$outcome <- 'a1c'
  
  return(df)
}

load_demographics <- function(wb) {
  df <- wb_read(
    wb,
    sheet = 'TPIAT Demographics',
    rows = c(14, 16:18, 20:25, 27:28, 30:37, 39:47),
    cols = 1:4
  )
  
  return(df)
}
