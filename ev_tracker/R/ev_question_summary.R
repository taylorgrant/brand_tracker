#' Summarize a question in the survey
#' 
#' Summarizes a given question; if the data is cut by a group, specify groups 
#'
#' @param data Survey dataset, weighted using the `srvyr` package
#' @param groups Crosstab group or groups, names from columns in survey data
#' @param qq Question to summarize
#'
#' @return Tidy dataframe with control/exposed, any groups, and questions along with results
#' @export
#'
#' @examples
#' \dontrun{
#' ## single question 
#' 
#' brand_vars <- dplyr::tibble(var = c("unaided_awareness_coded","awr_a_1", "awr_aad_1", 
#' "con_br_5", "opn_br_x1"), 
#' q = c("Unaided Awareness","Aided Awareness", "Aided Ad Awareness", 
#' "Purchase Consideration", "Brand Momentum"))
#' 
#' question_summary(data = social, qq = brand_vars$var[2]) 
#' 
#' question_summary(data = social, groups = "demo_gender", qq = brand_vars$var[2])
#' 
#' ## multiple questions 
#' 
#' results <- purrr::map(brand_vars$var, ~question_summary(data = social, groups = NULL, qq = .x)) 
#' names(results) <- brand_vars$q
#' }
ev_question_summary <- function(data, groups = NULL, qq) {
  
  # proper control/exposed by ad source
  match_control <- switch(names(data$allprob), "weights_xmedia" = "matched_control_xmedia", 
                          "weights_digital" = "matched_control_digital", 
                          "weights_social" = 'matched_control_social')
  
  # renaming for variable as 'svy_q'
  tmp <- data |> dplyr::rename(svy_q = !!rlang::sym(qq))
  
  # if brand momentum, convert to top 2 / bottom 2 box
  if (stringr::str_detect(qq, "momentum_br")) {
    tmp <- tmp |> 
      dplyr::mutate(svy_q = dplyr::case_when(
        svy_q %in%  c("Strongly Ascending - The brand is very much on the rise and has a lot of positive momentum", 
                      "Somewhat Ascending - The brand appears to be gaining ground and heading in a positive direction") ~ "Ascending - Top 2 Box",
        svy_q %in%  c("Somewhat Descending - The brand appears to be losing some ground and heading in a negative direction", 
                      "Strongly Descending - The brand is very much on the decline and lacks positive momentum") ~ "Descending - Bottom 2 Box",
        svy_q == "Neutral - The brand's position seems stable, neither ascending nor descending" ~ "Neutral"
      ), 
      svy_q = factor(svy_q, levels = c("Ascending - Top 2 Box", "Neutral", "Descending - Bottom 2 Box"))) 
  }
  
  # if not unaided awareness, drop NULL (NULL are unaware of BMW; don't want them in our denominator)
  if (qq != "unaided_awareness_coded") {
    tmp <- tmp |>
      dplyr::filter(svy_q != "NULL") 
  }
  
  if (qq == "awr_a_ev_1") {
    tmp <- tmp |> 
      dplyr::mutate(svy_q = dplyr::case_when(
        svy_q == "Offers one or more fully electric vehicles" ~ "BMW",
        TRUE ~ "0"
      ))
  } else if (qq == "awr_a_ev_2") {
    tmp <- tmp |> 
      dplyr::mutate(svy_q = dplyr::case_when(
        svy_q == "Offers one or more fully electric vehicles" ~ "Mercedes Benz",
        TRUE ~ "0"
      ))
  } else if (qq == "awr_a_ev_3") {
    tmp <- tmp |> 
      dplyr::mutate(svy_q = dplyr::case_when(
        svy_q == "Offers one or more fully electric vehicles" ~ "Tesla",
        TRUE ~ "0"
      ))
  } else if (qq == "awr_a_ev_4") {
    tmp <- tmp |> 
      dplyr::mutate(svy_q = dplyr::case_when(
        svy_q == "Offers one or more fully electric vehicles" ~ "Lucid",
        TRUE ~ "0"
      ))
  } else if (qq == "awr_a_ev_5") {
    tmp <- tmp |> 
      dplyr::mutate(svy_q = dplyr::case_when(
        svy_q == "Offers one or more fully electric vehicles" ~ "Rivian",
        TRUE ~ "0"
      ))
  }
  
  # set up groupings for the data (if groups are there)
  if (!is.null(groups)) {
    tmp <- tmp |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), 
                      dplyr::across(dplyr::all_of(groups)), svy_q)
    
  } else {
    tmp <- tmp |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), svy_q)
    
  }
  
  # process for proportion and n count
  tmp <- tmp |> 
    srvyr::summarise(proportion = srvyr::survey_mean(),
                     n = srvyr::survey_total()) |> 
    dplyr::filter(!!rlang::sym(match_control) != "unclassified") |> 
    dplyr::filter(!!rlang::sym(match_control) != "NULL") # TV is `NULL` rather than `unclassified`
  
  # adding totals for easier testing later on
  if (!is.null(groups)) {
    tmp |>
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), 
                      dplyr::across(dplyr::all_of(groups))) |>
      dplyr::mutate(total = sum(n)) |> 
      dplyr::filter(svy_q != "NULL") |> #dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
    
  } else {
    
    tmp |> dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), svy_q) |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control))) |> 
      dplyr::mutate(total = sum(n)) |> 
      dplyr::filter(svy_q != "NULL") |> # dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
  }
}
