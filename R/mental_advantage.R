# Function to calculate the matrix and apply the custom formula
mental_advantage <- function(dat, N, filters = NULL, note) {
  
  if (!is.null(filters)) {
    tmpdat <- dat |> 
      dplyr::filter(group_1 == filters) |>
      dplyr::select(Category, svy_q, n) |> 
      tidyr::pivot_wider(names_from = svy_q, values_from = n) |> 
      janitor::adorn_totals(where = c("row", "col"))
  } else {
    tmpdat <- dat |> 
      dplyr::select(Category, svy_q, n) |> 
      tidyr::pivot_wider(names_from = svy_q, values_from = n) |> 
      janitor::adorn_totals(where = c("row", "col"))
  }
  
  title <- glue::glue("Mental Advantage - {unique(dat$cat)}")
  
  # Extract the Row Totals and Column Totals
  row_totals <- tmpdat$Total[1:nrow(tmpdat)-1]  # Row totals, excluding the last row ("Total")
  column_totals <- tmpdat[nrow(tmpdat), 2:(ncol(tmpdat)-1)]  # Column totals, excluding the "Attribute" and "Total" column
  total_total <- tmpdat$Total[nrow(tmpdat)]    # Total total 
  
  # Step 1: Calculate the matrix (Row Total * Column Total) / Total Total
  calculate_values <- function(row_totals, column_totals, total_total) {
    return((row_totals %*% t(as.numeric(column_totals))) / total_total)
  }
  
  # Apply the matrix calculation
  calculated_matrix <- calculate_values(row_totals, column_totals, total_total)
  
  # Convert the matrix back to a tibble
  calculated_df <- dplyr::as_tibble(calculated_matrix)
  colnames(calculated_df) <- colnames(tmpdat)[2:(ncol(tmpdat)-1)]  # Use same column names as df
  calculated_df <- dplyr::bind_cols(Attribute = tmpdat$Category[1:(nrow(tmpdat)-1)], calculated_df)
  
  # Step 2: Apply the formula (calculated_df / df) / N for each cell
  result_df <- tmpdat[1:(nrow(tmpdat)-1), 2:(ncol(tmpdat)-1)]  # Exclude last row (Total) and first column (Attribute) for calculation
  
  # Ensure both calculated_df and result_df are numeric
  calculated_numeric <- dplyr::as_tibble(lapply(calculated_df[, 2:(ncol(tmpdat)-1)], as.numeric))
  result_numeric <- dplyr::as_tibble(lapply(result_df, as.numeric))
  
  # Apply the formula (df - calculated_df) / N
  result_df <- dplyr::as_tibble(round(((result_numeric - calculated_numeric) / N)*100))
  colnames(result_df) <- colnames(tmpdat)[2:(ncol(tmpdat)-1)]  # Keep the same column names
  
  # Add the Attribute column back
  final_result <- dplyr::bind_cols(Attribute = tmpdat$Category[1:(nrow(tmpdat)-1)], result_df)
  
  library(gt)
  final_result |> 
    gt() |> 
    tab_header(
      title = title,
      # subtitle = glue::glue("{sub3}")
    ) |> 
    # aligning the title left
    gt::tab_style(
      style = gt::cell_text(align = 'left',
                            weight = 'bold',
                            size = gt::px(16)),
      locations = gt::cells_title(c("title"))
    ) |> 
    gt::tab_footnote(
      footnote = note,
      locations = gt::cells_title(groups = "title")
    ) |> 
    data_color(
      columns = 2:ncol(final_result),
      # columns = c(BMW, Audi, Lexus, `Mercedes Benz`, Tesla),
      fn = scales::col_bin(
        bins = c(-Inf, -4, 5, Inf),
        palette = c("maroon", "white", "navyblue")
      )
    ) |> 
    tab_style(
      style = cell_text(weight = 'bold',
                        size = px(13)),
      locations = list(
        cells_column_labels(
          columns = everything()),
        cells_row_groups(groups = TRUE))
    ) |> 
    # centering column labels (not first column)
    gt::tab_style(
      style = gt::cell_text(align = 'center'),
      locations = gt::cells_column_labels(
        columns = -1  # Exclude the first column
      )
    ) |> 
    gt::tab_style(
      style = gt::cell_text(align = 'left'),
      locations = gt::cells_column_labels(
        columns = 1  # only the first column
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
      style = gt::cell_text(align = "left"),
      locations = gt::cells_body(columns = 1)
    ) |>
    # final options
    gt::tab_options(
      data_row.padding = gt::px(4),
      row_group.padding = gt::px(4),
      source_notes.font.size = gt::px(10),
      footnotes.font.size = gt::px(10),
      footnotes.marks = "", # empty footnote mark
    ) 
}
