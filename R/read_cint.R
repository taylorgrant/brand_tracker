#' Read in tracker data from Cint
#'
#' This function reads in the raw data and weights the data using the weights provided by Cint in the file. All are then put into the global environment
#' 
#' @param file_loc Give the location of the tracker file
#'
#' @return No object is returned. Side effect: multiple survey objects are created in the global environment.
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
  premium_makes <- c("car_make_29", "car_make_30","car_make_37", "car_make_40", 
                     "car_make_41","car_make_43", "car_make_47", "car_make_48", "car_make_51")
  
  df <- readr::read_csv(file_loc) |> 
    janitor::clean_names() |> 
    dplyr::distinct(response_id, .keep_all = TRUE) |> 
    dplyr::mutate(
      yob = lubridate::year(Sys.Date()) - age,
      generations = dplyr::case_when(
        yob < 2013 & yob > 1996 ~ "Gen Z",
        yob < 1997 & yob > 1980 ~ "Millennials",
        yob < 1981 & yob > 1964 ~ "Gen X",
        yob < 1965 & yob > 1945 ~ "Boomers",
        yob < 1946 & yob > 1927 ~ "Silent",
        yob < 1928 ~ "Greatest",
        yob > 2012 ~ "Gen Alpha"
      ),
      demo_region = dplyr::case_when(
        demo_state %in% c("ALABAMA", "ARKANSAS", "DISTRICT OF COLUMBIA", 
                          "FLORIDA", "GEORGIA", "KENTUCKY", "LOUISIANA", "MISSISSIPPI", 
                          "NORTH CAROLINA", "OKLAHOMA", "SOUTH CAROLINA", "TENNESSEE", 
                          "TEXAS", "VIRGINIA", "WEST VIRGINIA") ~ "South",
        demo_state %in% c("ILLINOIS", "INDIANA", "IOWA", "KANSAS", "MICHIGAN",
                          "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "OHIO", 
                          "SOUTH DAKOTA", "WISCONSIN") ~ "Midwest",
        demo_state %in% c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "MARYLAND", 
                          "NEW HAMPSHIRE", "NEW JERSEY", "NEW YORK", "PENNSYLVANIA", 
                          "RHODE ISLAND", "VERMONT") ~ "Northeast",
        TRUE ~ "West"
      ),
      genz_millen = dplyr::if_else(stringr::str_detect(generations, "Z|Mill"), "Gen Z/Millennial", "Gen X/Boomer"),
      generations = factor(generations, levels = gen_labels), 
      demo_premium = dplyr::case_when(
        demo_income != "$100k-149k" & 
          rowSums(dplyr::across(dplyr::any_of(premium_makes), ~ as.integer(. != "0"))) > 0 ~ 'Premium',
        TRUE ~ "Non-Premium"
      ),
      date = as.Date(end_date, format = "%m/%d/%Y"),
      month = lubridate::month(date, label = TRUE),
      weights_tv = dplyr::if_else(is.na(weights_tv), 0, weights_tv)
    )
  
  assign("digital", df |> srvyr::as_survey_design(ids = 1, weight = weights_digital), envir = .GlobalEnv)
  assign("social", df |> srvyr::as_survey_design(ids = 1, weight = weights_social), envir = .GlobalEnv)
  assign("campaign", df |> srvyr::as_survey_design(ids = 1, weight = weights_xmedia), envir = .GlobalEnv)
  assign("tv", df |> srvyr::as_survey_design(ids = 1, weight = weights_tv), envir = .GlobalEnv)
  assign("unweighted", df |> srvyr::as_survey_design(ids = NULL), envir = .GlobalEnv)
  assign("df", df, envir = .GlobalEnv)
}
