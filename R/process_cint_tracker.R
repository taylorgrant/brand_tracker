# BMW Brand Tracker from Cint # 

pacman::p_load(tidyverse, janitor, here, glue, srvyr)

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

file_loc <- here::here("cint_data", "CX95768 BMW Raw Data File July August 2024.csv")
read_cint(file_loc)



 
question_summary <- function(data, groups = NULL, qq) {

  # proper control/exposed by ad source
  match_control <- switch(names(data$allprob), "weights_xmedia" = "matched_control_xmedia", 
                          "weights_digital" = "matched_control_digital", 
                          "weights_social" = 'matched_control_social')
  
  # renaming for variable as 'svy_q'
  data <- data |> dplyr::rename(svy_q = !!rlang::sym(qq))
  
  # if not unaided awareness, drop NULL (NULL are unaware of BMW; don't want them in our denominator)
  if (qq != "unaided_awareness_coded") {
    data <- data |>
      dplyr::filter(svy_q != "NULL") 
  }
  # set up groupings for the data
  if (!is.null(groups)) {
    data <- data |> dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), 
                                    dplyr::across(dplyr::all_of(groups)), svy_q)
  } else {
    data <- data |> dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), svy_q)
  }
  # process for proportion and n count
  data <- data |> 
    srvyr::summarise(proportion = srvyr::survey_mean(),
              n = srvyr::survey_total()) |> 
    dplyr::filter(!!rlang::sym(match_control) != "unclassified")
  
  # adding totals for easier testing later on
  if (!is.null(groups)) {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), 
                      dplyr::across(dplyr::all_of(groups))) |>
      dplyr::mutate(total = sum(n)) |> 
      dplyr::filter(svy_q != "NULL") |> #dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
  } else {
    data |> dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), svy_q) |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control))) |> 
      dplyr::mutate(total = sum(n)) |> 
      dplyr::filter(svy_q != "NULL") |> #dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
  }
}

mapping <- read_csv(here("cint_data", "survey_mapping.csv"), n_max = 347) |> 
  clean_names() |> 
  mutate(question = make_clean_names(question))

mapping |> 
  filter(str_detect(question_text, "BMW")) |> 
  select(question, selection) |> tp()

# INDIVIDUAL QUESTION -----------------------------------------------------
# run without groups 
question_summary(data = social, qq = brand_vars$var[2])
# run with groups 
question_summary(data = camp, groups = "demo_race_ethnicity", qq = "awr_a_1")


# MULTIPLE QUESTIONS ------------------------------------------------------
# map across multiple survey questions 
brand_vars <- dplyr::tibble(var = c("unaided_awareness_coded","awr_a_1", "awr_aad_1", 
                             "con_br_5", "opn_br_x1"),
                  q = c("Unaided Awareness","Aided Awareness", 
                        "Aided Ad Awareness", "Purchase Consideration",
                        "Brand Momentum"))

key_attrs <- tibble(var = c("att_br_1_10", "att_br_18_10", "att_br_19_10", 
                            "att_br_20_10", "att_br_21_10", "att_br_22_10", 
                            "att_br_23_10", "att_br_24_10","att_br_25_10",
                            "att_br_26_10", "att_br_27_10", "att_br_28_10", 
                            "att_br_29_10"),
                    q = c("Stands for joy", "Creates joy for future generations",
                          "Continuously surprises and delights with innovative offers and products",
                          "Creates positive lasting memories", 
                          "Is committed to implement sustainability (e.g. to recycle and reuse materials, to avoid waste)",
                          "Offers desirable products and services", 
                          "Puts customers first", "Makes technology exciting",
                          "Offers engaging and interactive technology",
                          "Fits into my lifestyle",
                          "Allows me to focus on myself", 
                          "Stands for luxury", 
                          "Is a reward for my accomplishments"))

brand_attrs <- tibble(var = c("att_br2_1_10", "att_br2_2_10", "att_br2_3_10", "att_br2_4_10", 
                              "att_br2_5_10", "att_br2_6_10", "att_br2_7_10", "att_br2_8_10", 
                              "att_br2_9_10"),
                      q = c("I fully trust", "I can fully identify with", 
                            "I really like", "I would like to own", 
                            "I would be willing to pay more for than for other premium brands", 
                            "Is leading in electric drive", 
                            "Is leading in digitalization (i.e. digital user experience, apps, connectivity, voice assistant, 5G, over", 
                            "Is leading in sustainability efforts (e.g. to recycle and reuse materials to avoid waste)", 
                            "Is a brand that will still be relevant in 50 years"))

my_groups <- c("demo_gender")
my_groups <- NULL

# run using map() - groups are either "my_groups" or NULL
results <- purrr::map(brand_vars$var, ~question_summary(data = social, groups = NULL, qq = .x))
names(results) <- brand_vars$q # name each list component



# SPREAD DATA AND RUN PROP.TEST -------------------------------------------

# FUNCTION spread and run prop.test
process_df <- function(df) {
  df |> 
    pivot_wider(
      names_from = matched_control_social, 
      values_from = c(proportion, n, total)
    ) |> 
    rowwise() |> 
    mutate(
      prop_test = list(prop.test(c(n_control, n_test), c(total_control, total_test))),
      p_value = prop_test$p.value,  # Extract the p-value
      statistic = prop_test$statistic # Extract the chi-squared statistic
    ) |> 
    ungroup()
}

# run across all dataframes in my list 
result_list <- map(results, process_df)

result_list[[5]]



# this is a single table 
df_wide <- results[[5]] |> 
  pivot_wider(
    names_from = matched_control_social, 
    values_from = c(proportion, n, total)
  ) |> 
  rowwise() |> # prop.test on a rowwise basis
  mutate(
    prop_test = list(prop.test(c(n_control, n_test), c(total_control, total_test))),
    p_value = prop_test$p.value,  # Extract the p-value
    statistic = prop_test$statistic # Extract the chi-squared statistic
  ) |> 
  ungroup()

# should i pivot back to tidy, or just use extra lines for plotting?
