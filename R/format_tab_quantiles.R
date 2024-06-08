format_tab_quantiles <- function(data) {
  
  data |> 
  mutate(
    across(q25:q75_ci_u, ~round_half_up(.x, digits = 2)),
    across(q25:q75_ci_u, ~formatC(.x, format = "f", digits = 2, big.mark = ",")),
    `25th` = paste0(q25, "\n", "[", q25_ci_l, "; ", "\n", q25_ci_u, "]"),
    `50th` = paste0(q50, "\n", "[", q50_ci_l, "; ", "\n", q50_ci_u, "]"),
    `75th` = paste0(q75, "\n", "[", q75_ci_l, "; ", "\n", q75_ci_u, "]")
  ) |> 
    select(metric, CAT_AGE, `25th`:`75th`) |> 
    pivot_longer(cols = c(`25th`:`75th`), names_to = "Quantile", values_to = "Estimate [95% CI]") |> 
    pivot_wider(names_from = CAT_AGE, values_from = `Estimate [95% CI]`) |> 
    rename(Metric = metric) |> 
    flextable() |> 
    width(j = 1, 1) |> 
    width(j = 2:6, rep(1.5)) |> 
    merge_v(j = "Metric") |> 
    valign(j = c(1:6), valign = "top", part = "header") |> 
    valign(j = c(1:6), valign = "top", part = "body") |> 
    bold(part = "header")
}
