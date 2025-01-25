ev_question_summary_all_brands <- function(data, groups = NULL, qq) {
  
  # no control/exposed when looking at all brands 
  total <- data |> dplyr::summarise(srvyr::survey_total()) |> dplyr::pull(coef)
  
  # renaming for variable as 'svy_q'
  tmp <- data |> dplyr::rename(svy_q = !!rlang::sym(qq))
  
  # setting EV awareness to dichotomous
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
  
  # if brand momentum, convert to top 2 / bottom 2 box
  if (stringr::str_detect(qq, "momentum_br")) {
    tmp <- tmp |> 
      dplyr::mutate(svy_q = dplyr::case_when(
        svy_q %in%  c("Strongly Ascending - The brand is very much on the rise and has a lot of positive momentum", "Somewhat Ascending - The brand appears to be gaining ground and heading in a positive direction") ~ "Ascending - Top 2 Box",
        svy_q %in%  c("Somewhat Descending - The brand appears to be losing some ground and heading in a negative direction", "Strongly Descending - The brand is very much on the decline and lacks positive momentum") ~ "Descending - Bottom 2 Box",
        svy_q == "Neutral - The brand's position seems stable, neither ascending nor descending" ~ "Neutral"
      ), 
      svy_q = factor(svy_q, levels = c("Ascending - Top 2 Box", "Neutral", "Descending - Bottom 2 Box"))) 
  }
  
  # if not unaided awareness, drop NULL (NULL are unaware of BMW; don't want them in our denominator)
  if (qq != "unaided_awareness_coded") {
    tmp <- tmp |>
      dplyr::filter(svy_q != "NULL") 
  }
  # set up groupings for the data (if groups are there)
  if (!is.null(groups)) {
    tmp <- tmp |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(groups)), svy_q)
    
  } else {
    tmp <- tmp |> 
      dplyr::group_by(dplyr::across(svy_q))
    
  }
  
  # process for proportion and n count
  tmp <- tmp |> 
    srvyr::summarise(proportion = srvyr::survey_mean(),
                     n = srvyr::survey_total())
  
  # adding totals for easier testing later on
  if (!is.null(groups)) {
    tmp |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::mutate(total = sum(n)) |> 
      dplyr::filter(svy_q != "NULL") |> #dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
    
  } else {
    
    tmp |> dplyr::group_by(svy_q) |> 
      dplyr::mutate(total = total) |> 
      dplyr::filter(svy_q != "NULL") |> # dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
  }
}
