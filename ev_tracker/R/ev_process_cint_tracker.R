# BMW Brand Tracker from Cint # 

pacman::p_load(tidyverse, janitor, here, glue, srvyr)

ev_cint_wrapper <- function(file_location, brand, my_groups = NULL) {
  source(here::here("R", "ev_read_cint.R"))
  source(here::here("R", "ev_brand_choice.R"))
  source(here::here("R", "ev_question_summary.R"))
  source(here::here("R", "ev_result_prop_test.R"))
  source(here::here("R", "ev_table_prep.R"))
  source(here::here("R", "ev_create_directory.R"))
  source(here::here("R", "ev_tracker_table.R"))
  source(here::here("R", "ev_tracker_dumbell.R"))
  source(here::here("R", "ev_brand_choice_all.R"))
  source(here::here("R", "ev_question_summary_all_brands.R"))
  source(here::here("R", "ev_proptest_dataframe.R"))
  source(here::here("R", "ev_process_list.R"))
  source(here::here("R", "ev_sig_table.R"))
  # source(here::here("R", "mental_advantage.R"))
  source(here::here("R", "ev_process_all_brands.R"))
  source(here::here("R", "ev_raw_tables.R"))
  
  # 1. READ IN THE SURVEY DATA FROM CINT ------------------------------------
  ev_read_cint(file_location)
  
  # 2. LOAD TRACKER QUESTIONS FOR BMW ---------------------------------------
  ev_tracker_qs <- ev_brand_choice(brand)
  
  # Helper function to apply ev_question_summary and result_prop_test
  ev_process_tracker <- function(tracker_section_data, ev_tracker_qs) {
    brand_vars <- purrr::map(ev_tracker_qs$brand_vars$var, ~ev_question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = ev_tracker_qs$brand_vars$q)
    key_attrs <- purrr::map(ev_tracker_qs$key_attrs$var, ~ev_question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = ev_tracker_qs$key_attrs$q)
    # brand_attrs <- purrr::map(ev_tracker_qs$brand_attrs$var, ~ev_question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
    #   purrr::set_names(nm = tracker_qs$brand_attrs$q)
    
    # Apply result_prop_test
    brand_vars_result <- purrr::map(brand_vars, ev_result_prop_test)
    key_attrs_result <- purrr::map(key_attrs, ev_result_prop_test)
    # brand_attrs_result <- purrr::map(brand_attrs, ev_result_prop_test)

    list(
      brand_vars_result = brand_vars_result,
      key_attrs_result = key_attrs_result
      # brand_attrs_result = brand_attrs_result
    )
  }
  
  # 3. PROCESS EACH SECTION (CAMPAIGN, SOCIAL, DIGITAL) -----------------------
  campaign_results <- ev_process_tracker(campaign, ev_tracker_qs)
  social_results <- ev_process_tracker(social, ev_tracker_qs)
  digital_results <- ev_process_tracker(digital, ev_tracker_qs)
  # tv_results <- process_tracker(tv, tracker_qs)

  # 4. APPLY TABLE PREP TO ALLOW USER TO SELECT FILTERS ---------------------
  group_filter <- ev_table_prep(campaign_results$brand_vars_result[[1]])
  f <- group_filter
  
  # 5. PREPARE THE TABLE USING TRACKER_TABLE FUNCTION ------------------------
  # Combine results for tracker_table 
  # Need to pass the relevant result (e.g., brand_vars_result, key_attrs_result, brand_attrs_result)
  
  purrr::map(campaign_results, ~ev_tracker_table(.x, brand, group_filter, "Campaign"))
  purrr::map(social_results, ~ev_tracker_table(.x, brand, group_filter, "Social"))
  purrr::map(digital_results, ~ev_tracker_table(.x, brand, group_filter, "Digital"))
  # purrr::map(tv_results, ~tracker_table(.x, brand, group_filter, "TV"))
  # 
  purrr::map(campaign_results, ~ev_tracker_figure(.x, brand, group_filter, "Campaign"))
  purrr::map(social_results, ~ev_tracker_figure(.x, brand, group_filter, "Social"))
  purrr::map(digital_results, ~ev_tracker_figure(.x, brand, group_filter, "Digital"))
  # purrr::map(tv_results, ~tracker_figure(.x, brand, group_filter, "TV"))
  # 
  # # run full competitive to get entire table
  ev_process_all_brands(f)
  ev_raw_tables(campaign_results, filters = f, name = "campaign")
  ev_raw_tables(digital_results, filters = f, name = "digital")
  ev_raw_tables(social_results, filters = f, name = "social")
}









  


