# have to process all brands at the same time # 

process_all_brands <- function(){
  
  # get all of the questions with proper ID variables
  tracker_qs <- brand_choice_all()
  
  # process three levels of data across all brands 
  process_tracker <- function(tracker_section_data, tracker_qs) {
    brand_vars <- purrr::map(tracker_qs$brand_vars$var, ~question_summary_all_brands(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$brand_vars$q)
    key_attrs <- purrr::map(tracker_qs$key_attrs$var, ~question_summary_all_brands(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$key_attrs$q)
    brand_attrs <- purrr::map(tracker_qs$brand_attrs$var, ~question_summary_all_brands(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$brand_attrs$q)
    
    list(
      brand_vars = brand_vars,
      key_attrs = key_attrs,
      brand_attrs = brand_attrs
    )
  }
  
  # run using the unweighted srvry survey 
  total_results <- process_tracker(unweighted, tracker_qs)
  
  # rename any grouping variables for eventual filter
  rename_groups <- function(data) {
    # Find the position of the 'svy_q' column
    svy_pos <- which(names(data) == "svy_q")
    # If 'svy_q' is not the first column, rename the previous columns
    if (svy_pos > 1) {
      colnames(data)[1:(svy_pos - 1)] <- paste0("group_", seq_len(svy_pos - 1))
    }
    return(data)  # Return the modified tibble
  }
  
  renamed_total_results <- map(total_results, ~ map(.x, rename_groups))
  
  # apply table_prep to allow user to select filters 
  group_filter <- table_prep(renamed_total_results$brand_vars[[1]])
  
  # get each list and put into tibble for easy filtering if necessary
  brand_vars <- renamed_total_results$brand_vars
  key_attrs <- renamed_total_results$key_attrs
  brand_attrs <- renamed_total_results$brand_attrs
    
  brand_vars_tbl <- data.table::rbindlist(brand_vars, idcol = "Category") |> 
    dplyr::tibble() |> 
    dplyr::mutate(Category = ifelse(Category == "Brand Momentum" & svy_q == "On its way up - Top 2 Box", paste(Category, "- Top 2 Box"), Category)) |> 
    dplyr::mutate(svy_q = as.character(svy_q)) |> 
    dplyr::filter(Category != "Brand Momentum") |>
    dplyr::mutate(svy_q = ifelse(svy_q == "On its way up - Top 2 Box", NA, svy_q)) |> 
    tidyr::fill(svy_q, .direction = "down") |> 
    dplyr::filter(Category != "Unaided Awareness") |> 
    mutate(cat = "Brand Metrics") |> 
    mutate(Category = factor(Category, levels = c("Aided Awareness", "Aided Ad Awareness",
                                                  "Purchase Consideration", "Brand Momentum - Top 2 Box"))) |> 
    mutate(svy_q = factor(svy_q, levels = c("BMW", "Audi", "Lexus", "Mercedes Benz", "Tesla"))) |> 
    arrange(Category, svy_q)
  
  key_attrs_tbl <- data.table::rbindlist(key_attrs, idcol = "Category") |> 
    dplyr::tibble() |> 
    arrange(Category) |> 
    mutate(cat = "Key Model Attributes") |> 
    dplyr::mutate(Category = trimws(gsub("\\(.*", "", Category)),
                  Category = dplyr::case_when(stringr::str_detect(Category, "^Allows") ~ "Allows me to focus on myself",
                                              stringr::str_detect(Category, "^Continuously") ~ "Innovative offers and products",
                                              stringr::str_detect(Category, "^Creates joy") ~ "Creates joy for future generations",
                                              stringr::str_detect(Category, "^Creates posit") ~ "Creates positive lasting memories",
                                              stringr::str_detect(Category, "^Fits") ~ "Fits into my lifestyle",
                                              stringr::str_detect(Category, "accomplishments") ~ "Is a reward for my accomplishments",
                                              stringr::str_detect(Category, "committed") ~ "Committed to sustainability",
                                              stringr::str_detect(Category, "^Makes") ~ "Makes technology exciting",
                                              stringr::str_detect(Category, "desirable") ~ "Offers desirable products and services",
                                              stringr::str_detect(Category, "engaging") ~ "Offers engaging and interactive technology",
                                              stringr::str_detect(Category, "^Puts") ~ "Puts customers first",
                                              Category == "Stands for joy" ~ "Stands for joy",
                                              Category == "Stands for luxury" ~ "Stands for luxury"),
                  Category = factor(Category, levels = c(
                    "Stands for joy", "Creates joy for future generations", "Innovative offers and products", 
                    "Creates positive lasting memories", "Committed to sustainability", 
                    "Offers desirable products and services", "Puts customers first", 
                    "Makes technology exciting", "Offers engaging and interactive technology", 
                    "Fits into my lifestyle", "Allows me to focus on myself", "Stands for luxury", 
                    "Is a reward for my accomplishments"
                  ))) |> 
    dplyr::mutate(svy_q = factor(svy_q, levels = c("BMW", "Audi", "Lexus", "Mercedes Benz", "Tesla"))) |> 
    dplyr::arrange(Category, svy_q)
  
  brand_attrs_tbl <- data.table::rbindlist(brand_attrs, idcol = "Category") |> 
    dplyr::tibble() |> 
    mutate(cat = "Brand Attributes") |> 
    dplyr::mutate(Category = trimws(gsub("\\(.*", "", Category))) |> 
    dplyr::mutate(Category = dplyr::case_when(stringr::str_detect(Category, "relevant") ~ "Will still be relevant in 50 years",
                                              stringr::str_detect(Category, "premium") ~ "Pay more than for other premium brands",
                                              TRUE ~ Category),
      Category = factor(Category, levels = c(
      "I fully trust", "I can fully identify with", "I really like", 
      "I would like to own", "Pay more than for other premium brands", 
      "Is leading in electric drive", "Is leading in digitalization", 
      "Is leading in sustainability efforts", "Will still be relevant in 50 years")),
      svy_q = factor(svy_q, levels = c("BMW", "Audi", "Lexus", "Mercedes Benz", "Tesla"))) |> 
    dplyr::arrange(Category, svy_q)

  # put into single tibble
  tmp <- rbind(brand_vars_tbl, key_attrs_tbl, brand_attrs_tbl)
  
  # check length of filters to figure out subtitles
  if (length(group_filter) == 3) {
    sub3 <- glue::glue("{group_filter[1]} & {group_filter[2] & group_filter[3]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% group_filter[1] & 
                      group_2 %in% group_filter[2] & 
                      group_3 %in% group_filter[3]) |>  
      dplyr::select(-c(group_1, group_2, group_3))
    
  } else if (length(group_filter) == 2) {
    sub3 <- glue::glue("{group_filter[1]} & {group_filter[2]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% group_filter[1] & 
                      group_2 %in% group_filter[2])  |> 
      dplyr::select(-c(group_1, group_2))
    
  } else if (length(group_filter) == 1) {
    
    sub3 <- glue::glue("{group_filter[1]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% group_filter[1]) |>  
      dplyr::select(-group_1)
  } else {
    sub3 <- glue::glue("No filters")
  }
  
  # now split the dataframe into a series of lists by question category 
  all_results <- split(tmp, tmp$Category)
  
  if (sub3 == "No filters") {
    footnote <- glue::glue("Total Sample; N: {scales::comma(unique(all_results[[1]]$total))}; (A,B,C,D,E) indicate significant difference at 90% confidence interval")
  } else {
    footnote <- glue::glue("{sub3} subsample; N: {scales::comma(unique(all_results[[1]]$total))}; (A,B,C,D,E) indicate significant difference at 90% confidence interval")
  }
  # run everything through prop.test and formatting for gt
  combined_results <- process_list(all_results)
  
  # to save - need the path 
  path <- create_directory(brand, filters = gsub(" & ", "-", sub3))
  # build gt table 
  sig_table(combined_results, footnote) |> 
    gt::gtsave(file.path(path, "competitive", "competitive_table.png"), expand = 10)
  
  }
  









