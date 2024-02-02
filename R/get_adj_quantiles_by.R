# Get adjusted quantiles
get_adj_quantiles_by <- function(x, by, design) {
  
  # Compute quantiles
  df <-
    survey::svyby(
      formula = as.formula(paste0( "~" , x)),
      by = as.formula(paste0( "~" , by)),
      design = design,
      FUN = svyquantile,
      quantile = c(0, 0.25, 0.5, 0.75, 1),
      vartype = "ci",
      level = 0.95
    )
  
  # Remove row names
  rownames(df) <- NULL
  
  # Update columns names
  names(df) <- c(
    by, 
    "q0", "q25", "q50", "q75", "q100", 
    "q0_ci_l", "q25_ci_l", "q50_ci_l", "q75_ci_l", "q100_ci_l",
    "q0_ci_u", "q25_ci_u", "q50_ci_u", "q75_ci_u", "q100_ci_u"
  )
  
  # Add a column containing the considered metric
  df$metric <- x
  
  # Reorder columns
  df <- df |> dplyr::select(metric, everything())
  
  return(df)
}
