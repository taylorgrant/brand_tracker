raw_tables <- function(data, filters){
  if (length(filters) == 3) {
    sub3 <- glue::glue("{filters[1]} & {filters[2] & filters[3]}")
  } else if (length(filters) == 2) {
    sub3 <- glue::glue("{filters[1]} & {filters[2]}")
  } else if (length(filters) == 1) {
    sub3 <- glue::glue("{filters[1]}")
  } else {
    filters <- glue::glue("Overall")
  }
  brand <- unique(data$brand_vars_result$`Unaided Awareness`$svy_q)
  path <- main_path <- here::here("processed",glue::glue("{brand}-{stringr::str_replace(filters, '/', '-')}_{Sys.Date()}"))
  
  tmpout <- lapply(data, function(sublist) {
    tmpdf <- data.table::rbindlist(sublist, idcol = "category") |> 
      dplyr::select(-prop_test) |> 
      dplyr::mutate(#dplyr::across(dplyr::contains("proportion"), ~scales::percent(., accuracy = 1)),
                    dplyr::across(dplyr::starts_with("n_"), round),
                    dplyr::across(dplyr::starts_with("total_"), round),
                    dplyr::across(dplyr::matches("p_value|statistic"), ~round(., digits = 4)))
  })
  brand_vars_avg <- tmpout[[1]] |> 
    dplyr::group_by(dplyr::across(dplyr::contains("group"))) |> 
    dplyr::summarise(dplyr::across(contains("proportion"), mean)) |> 
    dplyr::mutate(lift = proportion_test - proportion_control)
  
  key_attrs_avg <- tmpout[[2]] |> 
    dplyr::group_by(dplyr::across(dplyr::contains("group"))) |> 
    dplyr::summarise(dplyr::across(contains("proportion"), mean)) |> 
    dplyr::mutate(lift = proportion_test - proportion_control)
  
  brand_attrs_avg <- tmpout[[3]] |> 
    dplyr::group_by(dplyr::across(dplyr::contains("group"))) |> 
    dplyr::summarise(dplyr::across(contains("proportion"), mean)) |> 
    dplyr::mutate(lift = proportion_test - proportion_control)
  
  tmpout <- c(tmpout, 
               list(brand_vars_avg = brand_vars_avg,
              key_attrs_avg = key_attrs_avg,
              brand_attrs_avg = brand_attrs_avg))
  openxlsx::write.xlsx(tmpout, file.path(path, "raw_data", "campaign_results.xlsx"))
}