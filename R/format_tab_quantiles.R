format_tab_quantiles <- function(data, col_var_name) {
  
  data |> 
    dplyr::mutate(
    dplyr::across(q25:q75_ci_u, ~janitor::round_half_up(.x, digits = 2)),
    dplyr::across(q25:q75_ci_u, ~formatC(.x, format = "f", digits = 2, big.mark = ",")),
    `25th` = paste0(q25, "\n", "[", q25_ci_l, "; ", "\n", q25_ci_u, "]"),
    `50th` = paste0(q50, "\n", "[", q50_ci_l, "; ", "\n", q50_ci_u, "]"),
    `75th` = paste0(q75, "\n", "[", q75_ci_l, "; ", "\n", q75_ci_u, "]")
  ) |> 
    dplyr::select(var, CAT_AGE, `25th`:`75th`) |> 
    tidyr::pivot_longer(cols = c(`25th`:`75th`), names_to = "Quantile", values_to = "Estimate [95% CI]") |> 
    tidyr::pivot_wider(names_from = CAT_AGE, values_from = `Estimate [95% CI]`) |> 
    dplyr::rename({{col_var_name}} := var) |> 
    flextable::flextable() |> 
    flextable::width(j = 1, 1) |> 
    flextable::width(j = 2:6, rep(1.5)) |> 
    flextable::merge_v(j = col_var_name) |> 
    flextable::valign(j = c(1:6), valign = "top", part = "header") |> 
    flextable::valign(j = c(1:6), valign = "top", part = "body") |> 
    flextable::bold(part = "header")
}
