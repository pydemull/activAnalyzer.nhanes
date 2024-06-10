# Get adjusted quantiles
get_adj_quantiles_by <- function(x, by, design) {
  
  # Compute quantiles
  df <-
    survey::svyby(
      formula = as.formula(paste0( "~" , x)),
      by = as.formula(paste0( "~" , by)),
      design = design,
      FUN = survey::svyquantile,
      quantile = c(0.25, 0.5, 0.75),
      vartype = "ci",
      level = 0.95
    )
  
  # Remove row names
  rownames(df) <- NULL
  
  # Update columns names
  names(df) <- c(
    by, 
    "q25", "q50", "q75", 
    "q25_ci_l", "q50_ci_l", "q75_ci_l",
    "q25_ci_u", "q50_ci_u", "q75_ci_u"
  )
  
  # Add a column containing the considered variable
  df$var <- x
  
  # Reorder columns
  df <- df |> dplyr::select(var, everything())
  
  return(df)
}
