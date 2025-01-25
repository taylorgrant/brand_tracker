# Function to process list of dataframes
ev_process_list <- function(list_of_dataframes) {
  # Map over each tibble directly in the list
  results <- list_of_dataframes |> 
    purrr::map(ev_proptest_dataframe)  # Apply process_dataframe to each tibble
  
  # Combine all results into a single tibble using data.table::rbindlist()
  combined_result <- results |> 
    data.table::rbindlist(fill = TRUE) |> 
    tibble::as_tibble() |> 
    dplyr::mutate(Category = as.character(Category))
  
  return(combined_result)
}
