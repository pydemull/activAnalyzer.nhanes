# Load packages
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c(
    "activAnalyzer",
    "dplyr",
    "forcats",
    "hms",
    "haven",
    "mirai",
    "parallel",
    "readr",
    "survey",
    "tibble"
    ),
  memory = "transient", 
  garbage_collection = TRUE
)

# Run the R scripts in the R/ folder
tar_source()

# Define pipeline
list(
  # Create data directory
  tar_target(
    name = data_dir,
    command = dir.create("./data")
  ),
  # Download demographic data for wave 2003-2004
  tar_target(
    name = DEMO_C,
    command = get_nhanes_data(code = "DEMO", wave = "C"),
    format = "file"
  ),
  # Download demographic data for wave 2005-2006
  tar_target(
    name = DEMO_D,
    command = get_nhanes_data(code = "DEMO", wave = "D"),
    format = "file",
  ),
  # Download weight data for wave 2003-2004
  tar_target(
    name = WHQ_C,
    command = get_nhanes_data(code = "WHQ", wave = "C"),
    format = "file"
  ),
  # Download weight data for wave 2005-2006
  tar_target(
    name = WHQ_D,
    command = get_nhanes_data(code = "WHQ", wave = "D"),
    format = "file"
  ),
  # Download accelerometer data for wave 2003-2004 (takes several minutes)
  tar_target(
    name = PAXRAW_C,
    command = get_nhanes_data(code = "PAXRAW", wave = "C"),
    format = "file"
  ),
  # Download accelerometer data for wave 2005-2006 (takes several minutes)
  tar_target(
    name = PAXRAW_D,
    command = get_nhanes_data(code = "PAXRAW", wave = "D"),
    format = "file"
  ),
  # Import demographic data for wave 2003-2004 
  #   Convert age from months to years
  #   Recode Gender variable
  tar_target(
    name = df_demo_c,
    command = readRDS(DEMO_C) |> 
      dplyr::mutate(
        RIDAGEEX = RIDAGEEX / 12,
        RIAGENDR = fct_recode(as.factor(RIAGENDR), "male" = "1", "female" = "2")
        )
  ),
  # Import demographic data for wave 2005-2006  
  #   Convert age from months to years
  #   Recode Gender variable
  tar_target(
    name = df_demo_d,
    command = readRDS(DEMO_D) |> 
      dplyr::mutate(
        RIDAGEEX = RIDAGEEX / 12,
        RIAGENDR = fct_recode(as.factor(RIAGENDR), "male" = "1", "female" = "2")
      )
  ),
  # Import weight data for wave 2003-2004 | Convert weight from pounds to kg
  tar_target(
    name = df_whq_c,
    command = readRDS(WHQ_C) |> 
      dplyr::mutate(WHD020 = dplyr::if_else(WHD020 %in% c(7777, 9999), NA, WHD020 / 2.2046))
  ),
  # Import weight data for wave 2005-2006 | Convert weight from pounds to kg
  tar_target(
    name = df_whq_d,
    command = readRDS(WHQ_D) |> 
      dplyr::mutate(WHD020 = dplyr::if_else(WHD020 %in% c(7777, 9999), NA, WHD020 / 2.2046))
  ),
  # Import accelerometer data for wave 2003-2004 (takes minutes)
  tar_target(
    name = df_paxraw_c,
    command = readRDS(PAXRAW_C),
  ),
  # Import accelerometer data for wave 2005-2006 (takes minutes)
  tar_target(
    name = df_paxraw_d,
    command = readRDS(PAXRAW_D)
  ),
  # Nest accelerometer datasets
  tar_target(
    name = df_paxraw_c_nest,
    command = df_paxraw_c |> 
      dplyr::mutate(ID = SEQN) |> 
      dplyr::group_by(ID) |> 
      tidyr::nest()
  ),
  tar_target(
    name = df_paxraw_d_nest,
    command = df_paxraw_d |> 
      dplyr::mutate(ID = SEQN) |> 
      dplyr::group_by(ID) |> 
      tidyr::nest()
  ),
  # Import data analysis configuration
  tar_target(
    name = config,
    command = readr::read_csv2("config.csv")
  ),
  # Process accelerometer datasets relating to wave 2003-2004
  tar_group_size(
    name = df_paxraw_c_all_metrics,
    command = activ_process_all_nhanes(
      datasets = df_paxraw_c_nest,
      config = config, 
      demo = df_demo_c, 
      whq = df_whq_c
    ),
    size = 100
  ),
  # Process accelerometer datasets relating to wave 2005-2006
  tar_group_size(
    name = df_paxraw_d_all_metrics,
    command = activ_process_all_nhanes(
      datasets = df_paxraw_d_nest,
      config = config, 
      demo = df_demo_d, 
      whq = df_whq_d
    ),
    size = 100
  ),
  # Bind demographic data from both waves
  tar_target(
    name = df_demo_all_waves,
    command = dplyr::bind_rows(df_demo_c, df_demo_d)
    ),
  # Bind weight data from both waves
  tar_target(
    name = df_whq_all_waves,
    command = dplyr::bind_rows(df_whq_c, df_whq_d)
  ),
  # Bind accelerometer results from both waves
  tar_target(
    name = df_paxmetrics_all_waves,
    command = dplyr::bind_rows(df_paxraw_c_all_metrics, df_paxraw_d_all_metrics)
  ),
  # Join demographic, weight, and accelerometer metrics datasets
  tar_target(
    name = df_final,
    command = list(df_demo_all_waves, df_whq_all_waves, df_paxmetrics_all_waves) |> 
      purrr::reduce(dplyr::full_join, by = "SEQN")
  )
)

