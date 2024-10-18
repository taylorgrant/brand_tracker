tracker_table <- function(dat, brand, filters, dataset_type){
  
  library(gt)
  sub1 <- glue::glue("Make: {brand}")
  sub2 <- glue::glue("Channel: {dataset_type}")
  
  # set up the data for the tables 
  if (names(dat)[1] == "Unaided Awareness") {
    tmp <- data.table::rbindlist(dat, idcol = "Category") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::select(Category:svy_q, `%-C` = proportion_control, 
                    `%-E` = proportion_test, Lift = lift, `Sample-Control` = total_control,
                    `Sample-Exposed` = total_test, `Sig. Level` = sig_level) |> 
      dplyr::rename(Selection = svy_q)
  } else if (names(dat)[1] == "Stands for joy") {
    tmp <- data.table::rbindlist(dat, idcol = "Key Attributes") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::select(`Key Attributes`:svy_q, `%-C` = proportion_control, 
                    `%-E` = proportion_test, Lift = lift, `Sample-Control` = total_control,
                    `Sample-Exposed` = total_test, `Sig. Level` = sig_level) |> 
      dplyr::rename(Selection = svy_q)
  } else {
    tmp <- data.table::rbindlist(dat, idcol = "Brand Attributes") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::select(`Brand Attributes`:svy_q, `%-C` = proportion_control, 
                    `%-E` = proportion_test, Lift = lift, `Sample-Control` = total_control,
                    `Sample-Exposed` = total_test, `Sig. Level` = sig_level) |> 
      dplyr::rename(Selection = svy_q)
  }
  
  
  if (length(filters) == 3) {
    sub3 <- glue::glue("{filters[1]} & {filters[2] & filters[3]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% filters[1] & 
                      group_2 %in% filters[2] & 
                      group_3 %in% filters[3]) |> 
      dplyr::select(-c(group_1, group_2, group_3))
    
  } else if (length(filters) == 2) {
    sub3 <- glue::glue("{filters[1]} & {filters[2]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% filters[1] & 
                      group_2 %in% filters[2]) |> 
      dplyr::select(-c(group_1, group_2))
    
  } else if (length(filters) == 1) {
    
    sub3 <- glue::glue("{filters[1]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% filters[1]) |> 
      dplyr::select(-group_1)
  } else {
    sub3 <- glue::glue("No filters")
  }

  # create directories to save tables 
  path <- create_directory(brand, filters = gsub(" & ", "-", sub3))
  file_name <- switch(names(tmp)[1], 
                      "Category" = paste0(tolower(dataset_type),"-brand_vars.png"),
                      "Key Attributes" = paste0(tolower(dataset_type),"-key_attrs.png"),
                      "Brand Attributes" = paste0(tolower(dataset_type),"-brand_attrs.png")
  )
    
    
  tmp |> 
    gt::gt() |>
    gt::tab_header(
      title = glue::glue("Brand Tracker Results"),
      subtitle = gt::md(glue::glue("{sub1}<br>{sub2}<br>Filter: {sub3}"))
    ) |> 
    gt::tab_footnote(
      footnote = "Footnote if needed",
      locations = gt::cells_title(groups = "title")
    ) |> 
    gt::fmt_percent(
      columns = dplyr::matches("%|Sig"),
      decimals = 0
    ) |> 
    gt::fmt_number(
      columns = dplyr::contains("Sample"),
      decimals = 0
    ) |> 
    gt::opt_table_font(
      font = list(
        gt::google_font("Gothic A1")
      )
    ) |> 
    # Remove NAs and replace with empty string
    gt::sub_missing(
      columns = dplyr::contains("Level"),
      missing_text = ""
    ) |> 
    # aligning the title and subtitle left
    gt::tab_style(
      style = gt::cell_text(align = 'left',
                        weight = 'bold',
                        size = gt::px(16)),
      locations = gt::cells_title(c("title"))
    ) |> 
    gt::tab_style(
      style = gt::cell_text(align = 'left',
                        weight = 'bold',
                        size = gt::px(14)),
      locations = gt::cells_title(c("subtitle")) 
    ) |> 
    gt::tab_style(
      style = gt::cell_text(weight = 'bold',
                        size = px(13)),
      locations = list(
        gt::cells_column_labels(
          columns = dplyr::everything()))
    ) |> 
    gt::tab_style(
      style = gt::cell_text(align = 'center'),
      locations = gt::cells_column_labels(
        columns = -1  # Exclude the first column
      )
    ) |>
    # font size in table
    gt::tab_style(
      style = gt::cell_text(size = px(12)),
      locations = cells_body(
        columns = dplyr::everything())
    ) |> 
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = -1)
    ) |> 
    # Conditional formatting for Lift and Sig. Level
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        columns = -c(1,2),
        rows = Lift > 0 & `Sig. Level` == 0.8
      )
    ) |> 
    gt::tab_style(
      style = gt::cell_text(weight = "bold", color = "darkgreen"),
      locations = gt::cells_body(
        columns = -c(1,2),
        rows = Lift > 0 & `Sig. Level` >= 0.9
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold", color = "red"),
      locations = gt::cells_body(
        columns = -c(1:2),
        rows = Lift < 0 & `Sig. Level` >= 0.9
      )
    ) |> 
    # final options
    gt::tab_options(
      data_row.padding = gt::px(6),
      row_group.padding = gt::px(6),
      source_notes.font.size = gt::px(10),
      footnotes.font.size = gt::px(10),
      footnotes.marks = "LETTERS",
      table.font.names = "Gothic A1"
    )  |> 
    gt::gtsave(file.path(path, "tables", dataset_type, file_name), expand = 10)
}
