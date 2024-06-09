# Load packages
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c(
    "activAnalyzer",
    "dplyr",
    "flextable",
    "forcats",
    "ggdist",
    "ggplot2",
    "haven",
    "hms",
    "janitor",
    "mirai",
    "patchwork",
    "parallel",
    "purrr",
    "readr",
    "rnhanesdata",
    "survey",
    "tibble",
    "tidyr"
    ),
  memory = "transient", 
  garbage_collection = TRUE
)

# Run the R scripts in the R/ folder
tar_source()

# Define pipeline
list(
  # Download demographic data for wave 2003-2004 & Convert age (examination stage)
  # from months to years and recode gender
  tar_target(
    name = df_demo_c,
    command = read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.xpt") |> 
      mutate(
        RIDAGEEX = RIDAGEEX / 12,
        RIAGENDR = fct_recode(as.factor(RIAGENDR), "male" = "1", "female" = "2")
      )
  ),
  # Download demographic data for wave 2005-2006 & Convert age from months to 
  # years and recode gender
  tar_target(
    name = df_demo_d,
    command = read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.xpt") |> 
      mutate(
        RIDAGEEX = RIDAGEEX / 12,
        RIAGENDR = fct_recode(as.factor(RIAGENDR), "male" = "1", "female" = "2")
      )
  ),
  # Download anthropometric data for wave 2003-2004
  tar_target(
    name = df_bmx_c,
    command = read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.xpt")
  ),
  # Download anthropometric data for wave 2005-2006
  tar_target(
    name = df_bmx_d,
    command = read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.xpt")
  ),
  # Download accelerometer data for wave 2003-2004 (takes several minutes)
  tar_target(
    name = df_paxraw_c,
    command = 
     { 
       options(timeout = max(2000, getOption("timeout")))
       temp <- tempfile()
       download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004//PAXRAW_C.zip", temp)
       data <- read_xpt(unz(temp, "paxraw_c.xpt"))
       unlink(temp)
       return(data)
    }
  ),
  # Download accelerometer data for wave 2005-2006 (takes several minutes)
  tar_target(
    name = df_paxraw_d,
    { 
      options(timeout = max(2000, getOption("timeout")))
      temp <- tempfile()
      download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006//PAXRAW_D.zip", temp)
      data <- read_xpt(unz(temp, "paxraw_d.xpt"))
      unlink(temp)
      return(data)
    }
  ),
  # Nest accelerometer datasets
  ## Wave 2003-2004
  tar_target(
    name = df_paxraw_c_nest,
    command = df_paxraw_c |> 
      mutate(ID = SEQN) |> 
      group_by(ID) |> 
      nest()
  ),
  ## Wave 2005-2006
  tar_target(
    name = df_paxraw_d_nest,
    command = df_paxraw_d |> 
      mutate(ID = SEQN) |> 
      group_by(ID) |> 
      nest()
  ),
  # Import data analysis configuration
  tar_target(
    name = config,
    command = read_csv2("config.csv")
  ),
  # Process accelerometer data relating to wave 2003-2004 (takes several hours)
  tar_group_size(
    name = df_paxraw_c_all_metrics,
    command = activ_process_all_nhanes(
      datasets = df_paxraw_c_nest,
      config = config, 
      demo = df_demo_c, 
      bmx = df_bmx_c
    ),
    size = 100
  ),
  # Process accelerometer data relating to wave 2005-2006 (takes several hours)
  tar_group_size(
    name = df_paxraw_d_all_metrics,
    command = activ_process_all_nhanes(
      datasets = df_paxraw_d_nest,
      config = config, 
      demo = df_demo_d, 
      bmx = df_bmx_d
    ),
    size = 100
  ),
  # Bind demographic data from both waves
  tar_target(
    name = df_demo_all_waves,
    command = bind_rows(df_demo_c, df_demo_d)
    ),
  # Bind anthropometric data from both waves
  tar_target(
    name = df_bmx_all_waves,
    command = bind_rows(df_bmx_c, df_bmx_d)
  ),
  # Bind accelerometer results from both waves
  tar_target(
    name = df_paxmetrics_all_waves,
    command = bind_rows(df_paxraw_c_all_metrics, df_paxraw_d_all_metrics) |> 
      # remove metrics computed with the (here non-appropriate) default algorithms
      # from the {activAnalyzer} package:
      select(-c(total_counts_vm, vm_per_min, mets_hours_mvpa:pal)) |> 
      # remove step-based metrics beacause they cannot be used when considering the 4-yr cycle
      # (these metrics were computed by default with the {activAnalyzer} package):
      select(-c(total_steps, max_steps_60min:peak_steps_1min))
  ),
  # Join demographic, anthropometric, and accelerometer metrics datasets, and add
  # age categories
  tar_target(
    name = df_final,
    command = list(df_demo_all_waves, df_bmx_all_waves, df_paxmetrics_all_waves) |> 
      reduce(full_join, by = "SEQN") |> 
      rename(M1_3 = "M1/3") |>  # change name because it is problematic for the svyby() function
      mutate(
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
  # Get a dataset with the participants having >=4 valid days
  tar_target(
    name = df_final_4_valid_days,
    command = df_final |> filter(valid_days >= 4)
  ),
  # Count the number of participants for NHANES 2003-2004,
  tar_target(
    name = n_nhanes_c,
    command = length(unique(df_demo_c$SEQN))
  ),
  # Count the number of participants for NHANES 2005-2006,
  tar_target(
    name = n_nhanes_d,
    command = length(unique(df_demo_d$SEQN))
  ),
  # Count the number of participants with accelerometer data for NHANES 2003-2004,
  tar_target(
    name = n_nhanes_paxraw_c,
    command = length(unique(df_paxraw_c$SEQN))
  ),
  # Count the number of participants with accelerometer data for NHANES 2005-2006,
  tar_target(
    name = n_nhanes_paxraw_d,
    command = length(unique(df_paxraw_d$SEQN))
  ),
  # Count the number of participants with valid accelerometer data for NHANES 2003-2006
  # (i.e., with 4 valid days or more)
  tar_target(
    name = n_paxraw_valid,
    command = nrow(df_final_4_valid_days)
  ),
  # Reweight participants for the 4-yr cycle with updated age categories
  tar_target(
    name = df_final_rw, 
    command =
      reweight_accel(
        df_final,
        age_bks = c(0, 6, 13, 18, 65, Inf),
        right = FALSE
      )
  ),
  # Define survey design
  tar_target(
    name = design,
    command = 
      svydesign(
        ids = ~ SDMVPSU,
        strata = ~ SDMVSTRA,
        weights = ~ wtmec4yr_adj,
        data = df_final_rw,
        nest = TRUE
      )
  ),
  # Subset general design to target people with at least 4 valid days
  tar_target(
    name = design_valid_pam,
    command =
      subset(
        x = design, 
        subset = valid_days >= 4
      )
  ),
  # Get the number of degrees of freedom for the survey design
  tar_target(
    name = degf_design_valid_pam,
    command = degf(design_valid_pam)
  ),
  # Set the list of the metrics of interest
  tar_target(
    name = metrics_list,
    command = df_final_rw |> select(total_counts_axis1:gini) |> names()
  ),
  # Compute the estimates of the quantiles for the activity metrics of interest
  tar_target(
    name = metrics_quantiles,
    command =
      lapply(
        metrics_list,
        get_adj_quantiles_by,
        by = "CAT_AGE",
        design = design_valid_pam
      ) |>
      bind_rows() |>
      mutate(
        metric = as.factor(metric) |>
          fct_relevel(metrics_list) |>
          fct_recode(
            "Total vertical counts" = "total_counts_axis1",
            "Vertical axis CPM" = "axis1_per_min",
            "Sedentary time (min)" = "minutes_SED",
            "LPA time (min)" = "minutes_LPA",
            "MPA time (min)" = "minutes_MPA",
            "VPA time (min)" = "minutes_VPA",
            "MVPA time (min)" = "minutes_MVPA",
            "% Wear time sedentary" = "percent_SED",
            "% Wear time LPA" = "percent_LPA",
            "% Wear time MPA" = "percent_MPA",
            "% Wear time VPA" = "percent_VPA",
            "% Wear time MVPA" = "percent_MVPA",
            "MVPA / Sedentary time ratio" = "ratio_mvpa_sed",
            "Intensity gradient" = "ig",
            "M1/3" = "M1_3",               
            "Sedentary breaks" = "mean_breaks",
            "Median bout duration (min)" = "MBD",
            "Usual bout duration (min)" = "UBD",
            "Power-law exponent alpha" = "alpha",
            "Gini index" = "gini"
          )
      )
  ), 
  # Build table for physical activity metrics
  tar_target(
    name = tab_pa_metrics,
    command = 
      metrics_quantiles |>
      filter(
        metric %in% c(
          "Total vertical counts",
          "Vertical axis CPM",
          "LPA time (min)",
          "MPA time (min)",
          "VPA time (min)",
          "MVPA time (min)",
          "% Wear time LPA",
          "% Wear time MPA",
          "% Wear time VPA",
          "% Wear time MVPA",
          "MVPA / Sedentary time ratio",
          "M1/3",
          "M60",
          "M30",
          "M15",
          "M5"
        )
      ) |> 
      format_tab_quantiles()
  ),
  # Build table for sedentary behaviour metrics
  tar_target(
    name = tab_sed_metrics,
    command = 
      metrics_quantiles |>
      filter(
        metric %in% c(
          "Sedentary time (min)",
          "Sedentary breaks",
          "Median bout duration (min)",
          "Usual bout duration (min)",
          "Power-law exponent alpha",
          "Gini index"
        )
      ) |> 
      format_tab_quantiles()
  ),
  # Build plots showing individual data points along with the quantile estimates
  tar_target(
    name = plots_quantiles, 
    command =
        {
          # Keep participants with >= 4 valid days and rename variables
          df_final_4_valid_days_renamed <-
            df_final_4_valid_days |>
            rename(
                "Total vertical counts" = "total_counts_axis1",
                "Vertical axis CPM" = "axis1_per_min",
                "Sedentary time (min)" = "minutes_SED",
                "LPA time (min)" = "minutes_LPA",
                "MPA time (min)" = "minutes_MPA",
                "VPA time (min)" = "minutes_VPA",
                "MVPA time (min)" = "minutes_MVPA",
                "% Wear time sedentary" = "percent_SED",
                "% Wear time LPA" = "percent_LPA",
                "% Wear time MPA" = "percent_MPA",
                "% Wear time VPA" = "percent_VPA",
                "% Wear time MVPA" = "percent_MVPA",
                "MVPA / Sedentary time ratio" = "ratio_mvpa_sed",
                "Intensity gradient" = "ig",
                "M1/3" = "M1_3",
                "Sedentary breaks" = "mean_breaks",
                "Median bout duration (min)" = "MBD",
                "Usual bout duration (min)" = "UBD",
                "Power-law exponent alpha" = "alpha",
                "Gini index" = "gini"
              )
          
          # Get list of metrics names
          metrics_list <- df_final_4_valid_days_renamed |> select(`Total vertical counts`:`Gini index`) |> names()
          
          # Get plots
          lapply(metrics_list, plot_adj_quantiles, data = df_final_4_valid_days_renamed, metrics_quantiles = metrics_quantiles)
        }
    ), 
  # Build figure for physical activity metrics
  tar_target(
    name = fig_pa,
    command = 
      wrap_plots(plots_quantiles[-c(3, 21:25)], ncol = 3) +
      plot_layout(axes = 'collect') + plot_annotation(tag_levels = 'A')
  ),
  # Save figure for physical activity metrics
  tar_target(
    name = fig_pa_tiff,
    command = ggsave(
      "paper/fig_pa.tiff",
      fig_pa,
      scaling = 0.5,
      height = 10,
      width = 5
    ),
    format = "file"
  ),  
  # Build figure for sedentary behaviour metrics
  tar_target(
    name = fig_sed,
    command = 
      wrap_plots(plots_quantiles[c(3, 21:25)], ncol = 3) +
      plot_layout(axes = 'collect') + plot_annotation(tag_levels = 'A')
  ),
  # Save figure for sedentary behaviour metrics
  tar_target(
    name = fig_sed_tiff,
    command = ggsave(
      "paper/fig_sed.tiff",
      fig_sed,
      scaling = 0.6,
      height = 5,
      width = 7
    ),
    format = "file"
  ),
  # Build the report for ICAMPAM absract
  tar_render(report, "icampam/analysis.Rmd", output_dir = "icampam/"),
  
  # Build the paper
  tar_quarto(paper, "paper/manuscript.qmd")
)

