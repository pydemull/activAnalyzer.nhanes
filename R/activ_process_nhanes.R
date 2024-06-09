
activ_process_nhanes <- function(
    data, 
    config,
    demo, 
    bmx
    ){
  
  # Analyse dataset only if it is OK for both reliability and calibration
  if(!(data[1, "PAXSTAT"][[1]] == 1 && data[1, "PAXCAL"][[1]] == 1)) {
    
    all_metrics <- 
      tibble::tibble(
        SEQN              = data[1, "SEQN"][[1]],
        PAXSTAT           = data[1, "PAXSTAT"][[1]],
        PAXCAL            = data[1, "PAXCAL"][[1]],
        valid_days        = 0,
        wear_time         = NA,
        total_counts_axis1= NA,
        total_counts_vm   = NA,
        axis1_per_min     = NA,
        vm_per_min        = NA,
        minutes_SED       = NA,
        minutes_LPA       = NA,
        minutes_MPA       = NA,
        minutes_VPA       = NA,
        minutes_MVPA      = NA,
        percent_SED       = NA,
        percent_LPA       = NA,
        percent_MPA       = NA,
        percent_VPA       = NA,
        percent_MVPA      = NA,
        ratio_mvpa_sed    = NA,
        mets_hours_mvpa   = NA,
        total_kcal        = NA,
        pal               = NA,
        total_steps       = NA,
        max_steps_60min   = NA,
        max_steps_30min   = NA,
        max_steps_20min   = NA,
        max_steps_5min    = NA,
        max_steps_1min    = NA,
        peak_steps_60min  = NA,
        peak_steps_30min  = NA,
        peak_steps_20min  = NA,
        peak_steps_5min   = NA,
        peak_steps_1min   = NA,
        ig                = NA,
        "M1/3"            = NA,
        M120              = NA,
        M60               = NA,
        M30               = NA,
        M15               = NA,
        M5                = NA,
        mean_breaks       = NA,
        alpha             = NA,
        MBD               = NA,
        UBD               = NA,
        gini              = NA
      )
  } else {
    
  # Prepare dataset for analysis
  data <- as.data.frame(data)
  
  # Extract id from dataset
  id <- as.character(data$SEQN[[1]])
  
  # Link the names of the variables set by the user (cf. config.csv file) to the 
  # global names used in the code
  ID         <- config |> dplyr::filter(CODE_NAME == "ID") |> dplyr::pull(USER_VALUE)
  AGE        <- config |> dplyr::filter(CODE_NAME == "AGE") |> dplyr::pull(USER_VALUE)
  SEX        <- config |> dplyr::filter(CODE_NAME == "SEX") |> dplyr::pull(USER_VALUE)
  SEX_MALE   <- config |> dplyr::filter(CODE_NAME == "SEX_MALE") |> dplyr::pull(USER_VALUE)
  SEX_FEMALE <- config |> dplyr::filter(CODE_NAME == "SEX_FEMALE") |> dplyr::pull(USER_VALUE)
  WEIGHT     <- config |> dplyr::filter(CODE_NAME == "WEIGHT") |> dplyr::pull(USER_VALUE)

  # Reconfigure SEX variable to ensure sex levels names can be used by the subsequent functions
  demo <- 
    demo |> 
    dplyr::mutate({{ SEX }} := forcats::fct_recode(demo |> dplyr::pull(SEX) |> as.character(), "male" = SEX_MALE, "female" = SEX_FEMALE))
  
  # Set parameters
  age    <- demo[demo[ID] == id, AGE][[1]]
  sex    <- as.character(demo[demo[ID] == id, SEX][[1]])
  weight <- bmx[bmx[ID] == id, WEIGHT][[1]]
  
     ## Manage problematic parameters
     if (is.na(age)) {age <- demo[demo[ID] == id, "RIDAGEYR"][[1]]}
     if (age == 0) {age <- 1}
     if (length(weight) == 0) {weight <- NA}

  axis_preschooler <- config |> dplyr::filter(CODE_NAME == "AXIS_PRESCHOOLER") |> dplyr::pull(USER_VALUE)
  axis_child       <- config |> dplyr::filter(CODE_NAME == "AXIS_CHILD") |> dplyr::pull(USER_VALUE)
  axis_adolescent  <- config |> dplyr::filter(CODE_NAME == "AXIS_ADOLESCENT") |> dplyr::pull(USER_VALUE)
  axis_adult       <- config |> dplyr::filter(CODE_NAME == "AXIS_ADULT") |> dplyr::pull(USER_VALUE)
  axis_older       <- config |> dplyr::filter(CODE_NAME == "AXIS_OLDER") |> dplyr::pull(USER_VALUE)
  
  sed_cutpoint_preschooler <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  sed_cutpoint_child       <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  sed_cutpoint_adolescent  <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  sed_cutpoint_adult       <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  sed_cutpoint_older       <- config |> dplyr::filter(CODE_NAME == "SED_CUTPOINT_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  mpa_cutpoint_preschooler <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  mpa_cutpoint_child       <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  mpa_cutpoint_adolescent  <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  mpa_cutpoint_adult       <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  mpa_cutpoint_older       <- config |> dplyr::filter(CODE_NAME == "MPA_CUTPOINT_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  vpa_cutpoint_preschooler <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  vpa_cutpoint_child       <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  vpa_cutpoint_adolescent  <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  vpa_cutpoint_adult       <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  vpa_cutpoint_older       <- config |> dplyr::filter(CODE_NAME == "VPA_CUTPOINT_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  equation_ee_preschooler <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_PRESCHOOLER") |> dplyr::pull(USER_VALUE)
  equation_ee_child       <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_CHILD") |> dplyr::pull(USER_VALUE)
  equation_ee_adolescent  <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_ADOLESCENT") |> dplyr::pull(USER_VALUE)
  equation_ee_adult       <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_ADULT") |> dplyr::pull(USER_VALUE)
  equation_ee_older       <- config |> dplyr::filter(CODE_NAME == "EQUATION_EE_OLDER") |> dplyr::pull(USER_VALUE)
  
  epoch_target_preschooler <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  epoch_target_child       <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  epoch_target_adolescent  <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  epoch_target_adult       <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  epoch_target_older       <- config |> dplyr::filter(CODE_NAME == "EPOCH_TARGET_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  frame_preschooler <- config |> dplyr::filter(CODE_NAME == "FRAME_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  frame_child       <- config |> dplyr::filter(CODE_NAME == "FRAME_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  frame_adolescent  <- config |> dplyr::filter(CODE_NAME == "FRAME_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  frame_adult       <- config |> dplyr::filter(CODE_NAME == "FRAME_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  frame_older       <- config |> dplyr::filter(CODE_NAME == "FRAME_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  allowanceFrame_preschooler <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  allowanceFrame_child       <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  allowanceFrame_adolescent  <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  allowanceFrame_adult       <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  allowanceFrame_older       <- config |> dplyr::filter(CODE_NAME == "ALLOWANCE_FRAME_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  streamFrame_preschooler <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_PRESCHOOLER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  streamFrame_child       <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_CHILD") |> dplyr::pull(USER_VALUE) |> as.numeric()
  streamFrame_adolescent  <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_ADOLESCENT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  streamFrame_adult       <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_ADULT") |> dplyr::pull(USER_VALUE) |> as.numeric()
  streamFrame_older       <- config |> dplyr::filter(CODE_NAME == "STREAM_FRAME_OLDER") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  valid_wear_time_start <- config |> dplyr::filter(CODE_NAME == "VALID_WEAR_TIME_START") |> dplyr::pull(USER_VALUE)
  valid_wear_time_end <- config |> dplyr::filter(CODE_NAME == "VALID_WEAR_TIME_END") |> dplyr::pull(USER_VALUE)
  minimum_wear_time <- config |> dplyr::filter(CODE_NAME == "MINIMUM_WEAR_TIME") |> dplyr::pull(USER_VALUE) |> as.numeric()
  
  # Set function parameters
  
     # Axis
     if (age < 6)                {axis <- axis_preschooler}
     if (age >= 6 && age < 13)   {axis <- axis_child}
     if (age >= 13 && age < 18)  {axis <- axis_adolescent}
     if (age >= 18 && age < 65)  {axis <- axis_adult}
     if (age >= 65)              {axis <- axis_older}
  
     # SED cut-point
     if (age < 6)                {sed_cutpoint <- sed_cutpoint_preschooler}
     if (age >= 6 && age < 13)   {sed_cutpoint <- sed_cutpoint_child}
     if (age >= 13 && age < 18)  {sed_cutpoint <- sed_cutpoint_adolescent}
     if (age >= 18 && age < 65)  {sed_cutpoint <- sed_cutpoint_adult}
     if (age >= 65)              {sed_cutpoint <- sed_cutpoint_older}

     # MPA cut-point (the settings initially provided in the config.csv file 
     # (that was not specifically built for NHANES analysis) are overridden to keep
     # consistency with the Troiano et al. paper (2008; DOI: 10.1249/mss.0b013e31815a51b3). 
     # The following settings are from ActiGraph website (https://actigraphcorp.my.site.com/support/s/article/What-s-the-difference-among-the-Cut-Points-available-in-ActiLife):
     if (age < 6)               {mpa_cutpoint <- 1400}
     if (age >= 6 && age < 7)   {mpa_cutpoint <- 1400}
     if (age >= 7 && age < 8)   {mpa_cutpoint <- 1515}
     if (age >= 8 && age < 9)   {mpa_cutpoint <- 1638}
     if (age >= 9 && age < 10)  {mpa_cutpoint <- 1770}
     if (age >= 10 && age < 11) {mpa_cutpoint <- 1910}
     if (age >= 11 && age < 12) {mpa_cutpoint <- 2059}
     if (age >= 12 && age < 13) {mpa_cutpoint <- 2220}
     if (age >= 13 && age < 14) {mpa_cutpoint <- 2393}
     if (age >= 14 && age < 15) {mpa_cutpoint <- 2580}
     if (age >= 15 && age < 16) {mpa_cutpoint <- 2781}
     if (age >= 16 && age < 17) {mpa_cutpoint <- 3000}
     if (age >= 17 && age < 18) {mpa_cutpoint <- 3239}
     if (age >= 18)             {mpa_cutpoint <- 2020}
      
     # VPA cut-point (the settings initially provided in the config.csv file 
     # (that was not specifically built for NHANES analysis) are overridden to keep
     # consistency with the Troiano et al. paper (2008; DOI: 10.1249/mss.0b013e31815a51b3). 
     # The following settings are from ActiGraph website (https://actigraphcorp.my.site.com/support/s/article/What-s-the-difference-among-the-Cut-Points-available-in-ActiLife):
     if (age < 6)               {vpa_cutpoint <- 3758}
     if (age >= 6 && age < 7)   {vpa_cutpoint <- 3758}
     if (age >= 7 && age < 8)   {vpa_cutpoint <- 3947}
     if (age >= 8 && age < 9)   {vpa_cutpoint <- 4147}
     if (age >= 9 && age < 10)  {vpa_cutpoint <- 4360}
     if (age >= 10 && age < 11) {vpa_cutpoint <- 4588}
     if (age >= 11 && age < 12) {vpa_cutpoint <- 4832}
     if (age >= 12 && age < 13) {vpa_cutpoint <- 5094}
     if (age >= 13 && age < 14) {vpa_cutpoint <- 5375}
     if (age >= 14 && age < 15) {vpa_cutpoint <- 5679}
     if (age >= 15 && age < 16) {vpa_cutpoint <- 6007}
     if (age >= 16 && age < 17) {vpa_cutpoint <- 6363}
     if (age >= 17 && age < 18) {vpa_cutpoint <- 6751}
     if (age >= 18)             {vpa_cutpoint <- 5999}

     # Equation for estimating energy expenditure
     if (age < 6)                {equation <- equation_ee_preschooler}
     if (age >= 6 && age < 13)   {equation <- equation_ee_child}
     if (age >= 13 && age < 18)  {equation <- equation_ee_adolescent}
     if (age >= 18 && age < 65)  {equation <- equation_ee_adult}
     if (age >= 65)              {equation <- equation_ee_older}

     # Epoch target
     if (age < 6)                {epoch_target <- epoch_target_preschooler}
     if (age >= 6 && age < 13)   {epoch_target <- epoch_target_child}
     if (age >= 13 && age < 18)  {epoch_target <- epoch_target_adolescent}
     if (age >= 18 && age < 65)  {epoch_target <- epoch_target_adult}
     if (age >= 65)              {epoch_target <- epoch_target_older}
  
     # Frame (wear time)
     if (age < 6)                {frame <- frame_preschooler}
     if (age >= 6 && age < 13)   {frame <- frame_child}
     if (age >= 13 && age < 18)  {frame <- frame_adolescent}
     if (age >= 18 && age < 65)  {frame <- frame_adult}
     if (age >= 65)              {frame <- frame_older}
  
     # Allowance Frame (wear time)
     if (age < 6)                {allowanceFrame <- allowanceFrame_preschooler}
     if (age >= 6 && age < 13)   {allowanceFrame <- allowanceFrame_child}
     if (age >= 13 && age < 18)  {allowanceFrame <- allowanceFrame_adolescent}
     if (age >= 18 && age < 65)  {allowanceFrame <- allowanceFrame_adult}
     if (age >= 65)              {allowanceFrame <- allowanceFrame_older}
  
     # Stream Frame (wear time)
     if (age < 6)                {streamFrame <- streamFrame_preschooler}
     if (age >= 6 && age < 13)   {streamFrame <- streamFrame_child}
     if (age >= 13 && age < 18)  {streamFrame <- streamFrame_adolescent}
     if (age >= 18 && age < 65)  {streamFrame <- streamFrame_adult}
     if (age >= 65)              {streamFrame <- streamFrame_older}
    
  
  # Set column names for wave 2003-2004
  if (as.numeric(id) <= 31125) {
    names(data)[8] <- "axis1"
    data$steps <- NA # no step data is provided for the wave 2003-2004
  }
  
  # Set columns names for wave 2005-2006
  if (as.numeric(id) > 31125) {
    names(data)[8] <- "axis1"
    names(data)[9] <- "steps"
  }
 
  # Add timestamp information and dummy vm data
  DATES <- dplyr::if_else(data$PAXDAY < 10, paste0("2005-01-0", data$PAXDAY), paste0("2005-01-", data$PAXDAY))
  TIMES <- hms::as_hms(as.numeric(3600 * data$PAXHOUR + 60 * data$PAXMINUT))
  TS <- paste(DATES, TIMES, sep = " ")
  data$vm <- data$axis1 # set dummy vm variable so that subsequent functions can work
  data$TimeStamp <- TS

  # Add nonwear time and intensity marks to the dataset
  data_with_intensity_marks <-
    data |> 
    activAnalyzer::mark_wear_time(
      TS = "TimeStamp",
      to_epoch = epoch_target,
      cts = axis,
      frame = frame,
      allowanceFrame = allowanceFrame,
      streamFrame = streamFrame
    ) |> 
    activAnalyzer::mark_intensity(
      col_axis = axis,
      sed_cutpoint = sed_cutpoint,
      mpa_cutpoint = mpa_cutpoint,
      vpa_cutpoint = vpa_cutpoint,
      equation = equation,
      age = age,
      weight = weight,
      sex = sex
      )
  
  # Get results by day
  results_by_day <-
    activAnalyzer::recap_by_day(
      data = data_with_intensity_marks,
      col_axis = axis,
      valid_wear_time_start = valid_wear_time_start,
      valid_wear_time_end = valid_wear_time_end,
      age = age,
      weight = weight,
      sex = sex,
      start_first_bin = 0,
      start_last_bin = 10000,
      bin_width = 500
    )
  
  # Get mean results
  main_metrics <-
    results_by_day$df_all_metrics  |> 
    activAnalyzer::average_results(minimum_wear_time = minimum_wear_time, fun = "mean")
  if(as.numeric(id) <= 31125) {main_metrics$total_steps <- NA}
  
  # Compute accumulation metrics related to sedentary behaviour if the measurement
  # is valid
  valid_dates <- 
    results_by_day$df_all_metrics |> 
    dplyr::filter(wear_time >= minimum_wear_time * 60) |> 
    dplyr::pull(date)
  
  if(length(valid_dates) > 0) {
    accum_metrics_sed <- 
      activAnalyzer::compute_accumulation_metrics(
        data = data_with_intensity_marks, 
        behaviour = "sed",
        dates = valid_dates
      )$metrics
  }
  
  if(length(valid_dates) == 0) {
    accum_metrics_sed <- 
      data.frame(
        mean_breaks = NA,
        alpha = NA,
        MBD = NA,
        UBD = NA,
        gini = NA
      )
    
  }
  
  # Get all metrics
  all_metrics <- cbind(dplyr::select(data, SEQN, PAXSTAT, PAXCAL)[1, ], main_metrics, accum_metrics_sed)

  }
  
  # Return  results
  return(all_metrics)

}

