plot_adj_quantiles <- function(var, data, vars_quantiles) {
  
  p <-
    data |>
    filter(CAT_AGE != "Preschoolers") |> # There is no data for preschoolers
    ggplot(aes(x = CAT_AGE)) +
    geom_dots(aes(y = .data[[var]]), side = "right", color = "grey80") +
    geom_boxplot(
      data = vars_quantiles |> filter(var == {{ var }}),
      aes(
        ymin = q25,
        lower = q25, 
        middle = q50, 
        upper = q75, 
        ymax = q75
      ),
      stat = "identity",
      color = "red",
      width = 0.1,
      linewidth = 0.3
    ) +
    geom_errorbar(
      data = vars_quantiles |> filter(var == {{ var }}),
      aes(
        x = stage(start = CAT_AGE, after_scale(x - 0.15)), 
        ymin = q50_ci_l, 
        ymax = q50_ci_u
      ),
      width = 0.1,
      linewidth = 0.1,
      color = "red"
    ) +
    labs( 
      x = "", 
    ) +
    scale_x_discrete(limits = rev) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0)
    ) +
    coord_flip()
  
  return(p)
}
