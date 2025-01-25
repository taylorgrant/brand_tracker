ev_tracker_table <- function(dat, brand, filters, dataset_type){
  
  library(gt)
  sub1 <- glue::glue("{brand}")
  sub2 <- glue::glue("Channel: {dataset_type}")
  
  # set up the data for the tables 
  if ((brand == "BMW" & names(dat)[1] == "Unaided Awareness") | (brand != 'BMW' & names(dat)[1] == "Aided Awareness")) {
    tmp <- data.table::rbindlist(dat, idcol = "Category") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::select(Category:svy_q, `%-C` = proportion_control, 
                    `%-E` = proportion_test, Lift = lift, `Sample-Control` = total_control,
                    `Sample-Exposed` = total_test, `Sig. Level` = sig_level) |> 
      dplyr::rename(Selection = svy_q) |> 
      dplyr::mutate(Category = ifelse(Category == "Brand Momentum" & Selection == "Ascending - Top 2 Box", paste(Category, "- Top 2 Box"), Category)) |> 
      dplyr::select(-Selection) |> 
      dplyr::filter(Category != "Brand Momentum")
    sample <- glue::glue("Sample Overall: Control = {round(tmp$`Sample-Control`[1])}, Exposed = {round(tmp$`Sample-Exposed`[1])};<br>Sample BMW EV Aware: Control = {round(tmp$`Sample-Control`[3])}, Exposed = {round(tmp$`Sample-Exposed`[3])}")
  } else { #if (names(dat)[1] == "Offers EVs with competitive ranges") {
    tmp <- data.table::rbindlist(dat, idcol = "Key Attributes") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::select(`Key Attributes`:svy_q, `%-C` = proportion_control, 
                    `%-E` = proportion_test, Lift = lift, `Sample-Control` = total_control,
                    `Sample-Exposed` = total_test, `Sig. Level` = sig_level) |> 
      dplyr::rename(Selection = svy_q) |> 
      dplyr::mutate(`Key Attributes` = trimws(gsub("\\(.*", "", `Key Attributes`))) |> 
      dplyr::select(-Selection) 
    sample <- glue::glue("Sample BMW EV Aware: Control = {round(tmp$`Sample-Control`[1])}, Exposed = {round(tmp$`Sample-Exposed`[1])}")
  } 
  
  # check length of filters to figure out subtitles
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
    sub3 <- glue::glue("Overall")
  }
  
  # create directories to save tables 
  path <- ev_create_directory(brand, filters = gsub(" & ", "-", sub3))
  file_name <- switch(names(tmp)[1], 
                      "Category" = paste0(tolower(dataset_type),"-brand_vars.png"),
                      "Key Attributes" = paste0(tolower(dataset_type),"-key_attrs.png"),
                      "Brand Attributes" = paste0(tolower(dataset_type),"-brand_attrs.png")
  )
  
  footnote_text <- glue::glue(
    "<span>Rows in <b style='color:darkgreen;'>green</b>/<b style='color:red;'>green</b> are significant with confidence level of 95%</span><br>{sample}"
  )
  
  tmp |> 
    gt() |>
    tab_header(
      title = glue::glue("Brand Tracker Results - {sub1}"),
      subtitle = gt::md(glue::glue("{sub2}"))
    ) |> 
    tab_footnote(
      footnote = html(footnote_text),
      locations = gt::cells_title(groups = "title")
    ) |> 
    # formatting percentage
    gt::fmt_percent(
      columns = dplyr::matches("%|Sig"),
      decimals = 0
    ) |> 
    # formatting numbers
    gt::fmt_number(
      columns = dplyr::contains("Sample"),
      decimals = 0
    ) |> 
    # setting font to BMW which is already loaded
    opt_table_font(
      font = c(
        "BMW"
      )
    ) |> 
    # Remove NAs and replace with empty string
    gt::sub_missing(
      columns = dplyr::contains("Level"),
      missing_text = ""
    ) |> 
    # aligning the title left
    gt::tab_style(
      style = gt::cell_text(align = 'left',
                            weight = 'bold',
                            size = gt::px(16)),
      locations = gt::cells_title(c("title"))
    ) |> 
    # aligning the subtitle left
    gt::tab_style(
      style = gt::cell_text(align = 'left',
                            weight = 'bold',
                            size = gt::px(14)),
      locations = gt::cells_title(c("subtitle")) 
    ) |> 
    # bolding the column headers
    gt::tab_style(
      style = gt::cell_text(weight = 'bold',
                            size = px(13)),
      locations = list(
        gt::cells_column_labels(
          columns = dplyr::everything()))
    ) |> 
    # centering column labels (not first column)
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
    # centering all variables in table (not first column)
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = -1)
    ) |> 
    gt::tab_style(
      style = gt::cell_text(weight = "bold", color = "darkgreen"),
      locations = gt::cells_body(
        columns = -1,
        rows = Lift > 0 & `Sig. Level` >= 0.95
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold", color = "red"),
      locations = gt::cells_body(
        columns = -c(1:2),
        rows = Lift < 0 & `Sig. Level` >= 0.95
      )
    ) |> 
    # Format the Lift column and add "+" for positive values
    fmt(
      columns = c(Lift),
      fns = function(x) {
        ifelse(x > 0, paste0("+", x), as.character(x))
      }
    ) |> 
    # hide columns from the tables 
    cols_hide(
      columns = c(`Sample-Control`, `Sample-Exposed`, `Sig. Level`)
    ) |> 
    # final options
    gt::tab_options(
      data_row.padding = gt::px(4),
      row_group.padding = gt::px(4),
      source_notes.font.size = gt::px(10),
      footnotes.font.size = gt::px(10),
      footnotes.marks = "", # empty footnote mark
    )  |> 
    gt::gtsave(file.path(path, "tables", dataset_type, file_name), expand = 10)
}
