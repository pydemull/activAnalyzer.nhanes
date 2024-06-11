format_tab_quantiles2 <- function(data, nb_part, col_var_name) {
  
  bind_rows(
    nb_part,
    data |> 
    dplyr::mutate(
      dplyr::across(q25:q75_ci_u, ~janitor::round_half_up(.x, digits = 2)),
      dplyr::across(q25:q75_ci_u, ~formatC(.x, format = "f", digits = 2, big.mark = ",")),
      `Median (IQR)` = paste0(q50, "\n",  " (", q25, "-", q75, ")" )
    ) |> 
    dplyr::select(var, CAT_AGE, `Median (IQR)`) |> 
    tidyr::pivot_wider(names_from = CAT_AGE, values_from = `Median (IQR)`) 
    )|> 
    dplyr::rename({{col_var_name}} := var) |> 
    flextable::flextable() |> 
    flextable::width(j = 2:5, rep(2.5)) |> 
    flextable::valign(j = c(1:5), valign = "top", part = "header") |> 
    flextable::valign(j = c(1:5), valign = "top", part = "body") |> 
    flextable::bold(part = "header") |> 
    flextable::italic(i = 1, j = 1, italic = TRUE, part = "body")
}
