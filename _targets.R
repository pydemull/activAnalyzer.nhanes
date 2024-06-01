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

# Run the R scripts from the R/ folder
tar_source()

# Define pipeline
list(
  # Get demographic data for wave 2003-2004
  tar_target(
    name = df_demo_c,
    command = haven::read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.xpt") |> 
      dplyr::mutate(
        RIDAGEEX = RIDAGEEX / 12,
        RIAGENDR = fct_recode(as.factor(RIAGENDR), "male" = "1", "female" = "2")
      )
  ),
  # Get demographic data for wave 2005-2006
  tar_target(
    name = df_demo_d,
    command = haven::read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.xpt") |> 
      dplyr::mutate(
        RIDAGEEX = RIDAGEEX / 12,
        RIAGENDR = fct_recode(as.factor(RIAGENDR), "male" = "1", "female" = "2")
      )
  ),
  # Get weight data for wave 2003-2004 | Convert weight from pounds to kg
  tar_target(
    name = df_whq_c,
    command = haven::read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/WHQ_C.xpt") |> 
      dplyr::mutate(WHD020 = dplyr::if_else(WHD020 %in% c(7777, 9999), NA, WHD020 / 2.2046))
  ),
  # Get weight data for wave 2005-2006 | Convert weight from pounds to kg
  tar_target(
    name = df_whq_d,
    command = haven::read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/WHQ_D.xpt") |> 
      dplyr::mutate(WHD020 = dplyr::if_else(WHD020 %in% c(7777, 9999), NA, WHD020 / 2.2046))
  ),
  # Get accelerometer data for wave 2003-2004 (takes several minutes)
  tar_target(
    name = df_paxraw_c,
    command = 
     { 
       options(timeout = max(2000, getOption("timeout")))
       temp <- tempfile()
       download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004//PAXRAW_C.zip", temp)
       data <- haven::read_xpt(unz(temp, "paxraw_c.xpt"))
       unlink(temp)
       return(data)
    }
  ),
  # Get accelerometer data for wave 2005-2006 (takes several minutes)
  tar_target(
    name = df_paxraw_d,
    { 
      options(timeout = max(2000, getOption("timeout")))
      temp <- tempfile()
      download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006//PAXRAW_D.zip", temp)
      data <- haven::read_xpt(unz(temp, "paxraw_d.xpt"))
      unlink(temp)
      return(data)
    }
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
  # Join demographic, weight, and accelerometer metrics datasets, and add
  # age categories
  tar_target(
    name = df_final,
    command = list(df_demo_all_waves, df_whq_all_waves, df_paxmetrics_all_waves) |> 
      purrr::reduce(dplyr::full_join, by = "SEQN") |> 
      dplyr::mutate(
        RIDAGEEX_UPDATED = if_else(is.na(RIDAGEEX), RIDAGEYR, RIDAGEEX),
        CAT_AGE = case_when(
          RIDAGEEX_UPDATED < 6                           ~ "Preschoolers",
          RIDAGEEX_UPDATED >= 6 & RIDAGEEX_UPDATED < 13  ~ "Children",
          RIDAGEEX_UPDATED >= 13 & RIDAGEEX_UPDATED < 18 ~ "Adolescents",
          RIDAGEEX_UPDATED >= 18 & RIDAGEEX_UPDATED < 65 ~ "Adults",
          RIDAGEEX_UPDATED >= 65                         ~ "Older adults"
        ) |> 
          as.factor() |> 
          fct_relevel("Preschoolers", "Children", "Adolescents", "Adults", "Older adults")
      )
  ),
  # Building the report
  tar_render(report, "analysis.Rmd", output_dir = "out/")
)

