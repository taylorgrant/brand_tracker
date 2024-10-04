#' Significance test across control / exposed
#' 
#' Take the tidy dataframe and spread it for easy prop.test
#'
#' @param res Tidy dataframe returned by the `question_summary()` function
#'
#' @return Wide dataframe with response proportions, n, total for each group, p.value and chi-square statistic
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming `question_summary()` returned a list of multiple questions
#' # run across all dataframes in my list 
#' result_list <- purrr::map(results, result_prop_test)
#' }
result_prop_test <- function(res) {
  res |> 
    tidyr::pivot_wider(
      names_from = !!rlang::sym(names(df)[1]), 
      values_from = c(proportion, n, total)
    ) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      prop_test = list(prop.test(c(n_control, n_test), c(total_control, total_test))),
      p_value = prop_test$p.value,  # Extract the p-value
      statistic = prop_test$statistic # Extract the chi-squared statistic
    ) |> 
    dplyr::ungroup()
}