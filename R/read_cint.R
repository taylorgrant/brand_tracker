#' Read in tracker data from Cint
#'
#' This function reads in the raw data and weights the data using the weights provided by Cint in the file. All are then put into the global environment
#' 
#' @param file_loc Give the location of the tracker file
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' file_loc <- "/folder/director/file.csv"
#' read_cint(file_loc)
#' }
read_cint <- function(file_loc) {
  
  df <- readr::read_csv(file_loc) |> 
    janitor::clean_names() |> 
    dplyr::distinct(response_id, .keep_all = TRUE)
  
  # apply weights (via srvyr package) ---------------------------------------
  # put all into memory
  digital <<- df |> 
    srvyr::as_survey_design(ids = 1, weight = weights_digital) # digital
  
  social <<- df |> 
    srvyr::as_survey_design(ids = 1, weight = weights_social) # social
  
  campaign <<- df |> 
    srvyr::as_survey_design(ids = 1, weight = weights_xmedia) # overall campaign
  
  assign("df", df, envir = globalenv())
}