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
  
  gen_labels = c("Gen Alpha", "Gen Z", "Millennials", "Gen X",
                 "Boomers", "Silent", "Greatest")
  
  df <- readr::read_csv(file_loc) |> 
    janitor::clean_names() |> 
    dplyr::distinct(response_id, .keep_all = TRUE) |> 
    dplyr::mutate(yob = lubridate::year(Sys.Date()) - age) |>
    dplyr::mutate(generations = dplyr::case_when(yob < 2013 & yob > 1996 ~ "Gen Z",
                                                 yob < 1997 & yob > 1980 ~ "Millennials",
                                                 yob < 1981 & yob > 1964 ~ "Gen X",
                                                 yob < 1965 & yob > 1945 ~ "Boomers",
                                                 yob < 1946 & yob > 1927 ~ "Silent",
                                                 yob < 1928 ~ "Greatest",
                                                 yob > 2012 ~ "Gen Alpha"),
                  genz_millen = ifelse(str_detect(generations, "Z|Mill"), "Gen Z/Millennial", "Gen X/Boomer"),
                  generations = factor(generations, levels = gen_labels))
  
  # apply weights (via srvyr package) ---------------------------------------
  # put all into memory
  
  assign("digital", df |> srvyr::as_survey_design(ids = 1, weight = weights_digital), envir = .GlobalEnv)
  
  assign("social", df |> srvyr::as_survey_design(ids = 1, weight = weights_social), envir = .GlobalEnv)
  
  assign("campaign", df |> srvyr::as_survey_design(ids = 1, weight = weights_xmedia), envir = .GlobalEnv)
  
  assign("unweighted", df |> srvyr::as_survey_design(ids = NULL), envir = .GlobalEnv)
  
  assign("df", df, envir = globalenv())
}