# Get adjusted quantiles
get_adj_quantiles_by <- function(x, by, design) {
  df <-
    svyby(as.formula(paste0( "~" , x)), 
          by = as.formula(paste0( "~" , by)), 
          design = design,
          FUN = svyquantile,
          quantile = c(0, 0.25, 0.5, 0.75, 1),
          vartype = "ci",
          level = 0.95
    )
  
  rownames(df) <- NULL
  
  names(df) <- c(
    by, 
    "q0", "q25", "q50", "q75", "q100", 
    "q0_ci_l", "q25_ci_l", "q50_ci_l", "q75_ci_l", "q100_ci_l",
    "q0_ci_u", "q25_ci_u", "q50_ci_u", "q75_ci_u", "q100_ci_u"
  )
  
  df$metric <- x
  
  df <- df |> select(metric, everything())
  
  return(df)
}
