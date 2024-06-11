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
    "skimr",
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
  # Process accelerometer data related to wave 2003-2004 (takes several hours)
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
  # Process accelerometer data related to wave 2005-2006 (takes several hours)
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
      rename(M1_3 = "M1/3") |>  # change name because it is problematic for the 
                                # survey::svyby() function
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
  # Compute the proportion of participants with valid accelerometer data for NHANES 2003-2006
  # (i.e., with 4 valid days or more)
  tar_target(
    name = prop_paxraw_valid,
    command = round_half_up(n_paxraw_valid / (n_nhanes_paxraw_c + n_nhanes_paxraw_c) * 100, digits = 0)
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
  # Set the list of the population characteristics of interest
  tar_target(
    name = part_char_list,
    command = c("RIDAGEEX_UPDATED", "BMXHT", "BMXWT", "BMXBMI")
  ),
  # Set the list of the metrics of interest
  tar_target(
    name = metrics_list,
    command = df_final_rw |> select(total_counts_axis1:gini) |> names()
  ),
  # Compute the estimates of the quantiles for the population characteristics 
  # of interest (from participants with 4 valid days or more)
  tar_target(
    name = part_char_quantiles,
    command = 
      lapply(
        part_char_list,
        get_adj_quantiles_by,
        by = "CAT_AGE",
        design = design_valid_pam
      ) |>
      bind_rows()  |>
      mutate(
        var = as.factor(var) |> # var is a variable built when using the get_adj_quantiles_by()
                                # function
          fct_relevel(part_char_list) |>
          fct_recode(
            "Age (yr)" = "RIDAGEEX_UPDATED",
            "Height (cm)" = "BMXHT",
            "Weight (kg)" = "BMXWT",
            "BMI (km/m²)" = "BMXBMI",
          )
      )
  ),
  # Compute the estimates of the quantiles for the activity metrics of interest
  # (from participants with 4 valid days or more)
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
        var = as.factor(var) |> # var is a variable built when using the get_adj_quantiles_by()
                                # function
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
  # Build table for participant characteristics (weighted results) using 
  # participants with 4 valid days or more
  tar_target(
    name = tab_part_char,
    command = 
      {
      # Get the number of participants by age categories  
        df_n <-
          df_final_4_valid_days |>
          group_by(CAT_AGE) |>
          summarise(N = formatC(as.character(n()), big.mark = ",")) |> 
          pivot_wider(names_from = CAT_AGE, values_from = N) |> 
          mutate(var = "N") |> 
          select(var, everything())
    
      # Get the final table
        part_char_quantiles |>
          format_tab_quantiles2(nb_part = df_n, col_var_name = "Variable")
      }
  ),
  # Build table for response rates for the number of valid days wearing the accelerometer
  # and for wear time for participants with 4+ valid days
  tar_target(
    name = tab_response_rate_accel,
    command = 
      df_final |>
      filter(!is.na(PAXSTAT) & !is.na(PAXCAL)) |>
      count(CAT_AGE, valid_days) |>
      group_by(CAT_AGE) |>
      mutate(prop = round_half_up(n / sum(n) * 100, digits = 1)) |>
      select(-n) |>
      pivot_wider(names_from = valid_days, values_from = prop) |>
      left_join(
        
        # Add wear time results for participants with >= 4 valid days
        df_final_4_valid_days |>
          group_by(CAT_AGE) |>
          select(wear_time) |>
          skim() |>
          yank("numeric") |>
          mutate(
            across(c(mean, sd), ~ round_half_up(.x, digits = 2)), 
            `Mean (SD) wear time for participants with 4+ valid days (min)` = paste0(mean, " (", sd, ")")
            ) |>
          select(CAT_AGE, "Mean (SD) wear time for participants with 4+ valid days (min)")
        
      ) |> 
      rename("Age category" = CAT_AGE) |>
      flextable() |>
      bold(part = "header") |>
      add_header_row(
        values = c("Age category", "Number of valid days of accelerometer wear (%)", "Mean (SD) wear time for participants with 4+ valid days (min)"),
        colwidths = c(1, 8, 1),
        top = TRUE
      ) |> 
      merge_v(j = 1, part = "header") |> 
      merge_v(j = 10, part = "header") |> 
      align(i = 1, j = 2:9, align = "center", part = "header") |> 
      valign(i = c(1, 2), j = c(1, 10), valign = "top", part = "header") |> 
      width(j = c(2:9), width = 0.5) |> 
      width(j = 10, width = 2)
  ),
  
  # Build table for physical activity volume metrics (weighted results) using 
  # participants with 4 valid days or more
  tar_target(
    name = tab_pa_vol_metrics,
    command = 
      metrics_quantiles |>
      filter(
        var %in% c(
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
          "MVPA / Sedentary time ratio"
        )
      ) |> 
      format_tab_quantiles(col_var_name = "Metric")
  ),
  # Build table for physical activity intensity distribution metrics (weighted results)  
  # using participants with 4 valid days or more
  tar_target(
    name = tab_pa_int_distri_metrics,
    command = 
      metrics_quantiles |>
      filter(
        var %in% c(
          "Intensity gradient",
          "M1/3",
          "M120",
          "M60",
          "M30",
          "M15",
          "M5"
        )
      ) |> 
      format_tab_quantiles(col_var_name = "Metric")
  ),
  # Build table for sedentary behaviour metrics (weighted results) using 
  # participants with 4 valid days or more
  tar_target(
    name = tab_sed_metrics,
    command = 
      metrics_quantiles |>
      filter(
        var %in% c(
          "Sedentary time (min)",
          "% Wear time sedentary",
          "Sedentary breaks",
          "Median bout duration (min)",
          "Usual bout duration (min)",
          "Power-law exponent alpha",
          "Gini index"
        )
      ) |> 
      format_tab_quantiles(col_var_name = "Metric")
  ),
  # Build plots showing individual data points along with the quantile estimates  using 
  # participants with 4 valid days or more
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
          lapply(metrics_list, plot_adj_quantiles, data = df_final_4_valid_days_renamed, vars_quantiles = metrics_quantiles)
        }
    ), 
  # Build figure for physical activity volume metrics
  tar_target(
    name = fig_pa_vol,
    command = 
      wrap_plots(plots_quantiles[c(1:2, 4:7, 9:13)], ncol = 3) +
      plot_layout(axes = 'collect') + plot_annotation(tag_levels = 'A')
  ),
  # Save figure for physical activity volume metrics
  tar_target(
    name = fig_pa_vol_tiff,
    command = ggsave(
      "paper/fig_pa_vol.tiff",
      fig_pa_vol,
      scaling = 0.5,
      height = 10,
      width = 5
    ),
    format = "file"
  ),
  # Build figure for physical activity intensity distribution metrics
  tar_target(
    name = fig_pa_int_distri,
    command = 
      wrap_plots(plots_quantiles[c(14:20)], ncol = 3) +
      plot_layout(axes = 'collect') + plot_annotation(tag_levels = 'A')
  ),
  # Save figure for physical activity intensity distribution metrics
  tar_target(
    name = fig_pa_int_distri_tiff,
    command = ggsave(
      "paper/fig_pa_int_distri.tiff",
      fig_pa_int_distri,
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
      wrap_plots(plots_quantiles[c(3, 8, 21:25)], ncol = 3) +
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
  # Build the report for ICAMPAM abstract
  tar_render(icampam_abstract, "icampam/analysis.Rmd", output_dir = "icampam/"),
  
  # Build the paper including all analysis
  tar_quarto(paper, "paper/manuscript.qmd")
)

