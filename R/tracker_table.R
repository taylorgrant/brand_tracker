tracker_table <- function(dat, brand, filters){
  
  dataset_type <- attr(dat, "dataset_type")
  subtitle <- paste("Results for", dataset_typ)
  
  tmp <- data.table::rbindlist(dat, idcol = "Category") |> 
    dplyr::tibble() |> 
    dplyr::select(-c(prop_test, p_value, statistic)) |> 
    dplyr::select(Category:svy_q, `%-C` = proportion_control, 
                  `%-E` = proportion_test, Lift = lift, `Sample-Control` = total_control,
                  `Sample-Exposed` = total_test, `Sig. Level` = sig_level)
  
  if (length(filters) == 2) {
    
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% filters[1] & 
                      group_2 %in% filters[2]) |> 
      dplyr::select(-c(group_1, group_2))
  } else if (length(filters) == 1) {
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% filters[1]) |> 
      dplyr::select(-group_1)
  } else {
  }
  
  tmp |> 
    gt::gt() |>
    gt::tab_header(
      title = glue::glue("Make: {brand}"),
      subtitle = gt::md("glue::glue({subtitle}<br>Second Subtitle")
    ) |> 
    gt::tab_footnote(
      footnote = "Second Subtitle",
      locations = gt::cells_title(groups = "title")
    ) |> 
    gt::fmt_percent(
      columns = contains("%"),
      decimals = 0
    ) |> 
    gt::fmt_number(
      columns = contains("Sample"),
      decimals = 0
    )
}