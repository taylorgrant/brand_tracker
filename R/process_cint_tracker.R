# BMW Brand Tracker from Cint # 

pacman::p_load(tidyverse, janitor, here, glue, srvyr)

cint_wrapper <- function(file_location, brand, my_groups = NULL) {
  source(here::here("R", "read_cint.R"))
  source(here::here("R", "brand_choice.R"))
  source(here::here("R", "question_summary.R"))
  source(here::here("R", "result_prop_test.R"))
  source(here::here("R", "table_prep.R"))
  source(here::here("R", "create_directory.R"))
  source(here::here("R", "tracker_table.R"))
  source(here::here("R", "tracker_dumbell.R"))
  source(here::here("R", "brand_choice_all.R"))
  source(here::here("R", "question_summary_all_brands.R"))
  source(here::here("R", "proptest_dataframe.R"))
  source(here::here("R", "process_list.R"))
  source(here::here("R", "sig_table.R"))
  
  # 1. READ IN THE SURVEY DATA FROM CINT ------------------------------------
  read_cint(file_location)
  
  # 2. LOAD TRACKER QUESTIONS FOR BMW ---------------------------------------
  tracker_qs <- brand_choice(brand)
  
  # Helper function to apply question_summary and result_prop_test
  process_tracker <- function(tracker_section_data, tracker_qs) {
    brand_vars <- purrr::map(tracker_qs$brand_vars$var, ~question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$brand_vars$q)
    key_attrs <- purrr::map(tracker_qs$key_attrs$var, ~question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$key_attrs$q)
    brand_attrs <- purrr::map(tracker_qs$brand_attrs$var, ~question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$brand_attrs$q)
    
    # Apply result_prop_test
    brand_vars_result <- purrr::map(brand_vars, result_prop_test)
    key_attrs_result <- purrr::map(key_attrs, result_prop_test)
    brand_attrs_result <- purrr::map(brand_attrs, result_prop_test)
    
    list(
      brand_vars_result = brand_vars_result,
      key_attrs_result = key_attrs_result,
      brand_attrs_result = brand_attrs_result
    )
  }
  
  # 3. PROCESS EACH SECTION (CAMPAIGN, SOCIAL, DIGITAL) -----------------------
  campaign_results <- process_tracker(campaign, tracker_qs)
  social_results <- process_tracker(social, tracker_qs)
  digital_results <- process_tracker(digital, tracker_qs)
  
  # 4. APPLY TABLE PREP TO ALLOW USER TO SELECT FILTERS ---------------------
  group_filter <- table_prep(campaign_results$brand_vars_result[[1]])
  
  # 5. PREPARE THE TABLE USING TRACKER_TABLE FUNCTION ------------------------
  # Combine results for tracker_table 
  # Need to pass the relevant result (e.g., brand_vars_result, key_attrs_result, brand_attrs_result)
  
  purrr::map(campaign_results, ~tracker_table(.x, brand, group_filter, "Campaign"))
  purrr::map(social_results, ~tracker_table(.x, brand, group_filter, "Social"))
  purrr::map(digital_results, ~tracker_table(.x, brand, group_filter, "Digital"))
  # 
  purrr::map(campaign_results, ~tracker_figure(.x, brand, group_filter, "Campaign"))
  purrr::map(social_results, ~tracker_figure(.x, brand, group_filter, "Social"))
  purrr::map(digital_results, ~tracker_figure(.x, brand, group_filter, "Digital"))
  
}

# old <- "~/R/bmw/brand_tracker/cint_data/CX95768 BMW Raw Data File July August 2024.csv"
file_location <- "~/R/bmw/brand_tracker/cint_data/BMW Deliverable File Sep 2024.csv"
brand <- "BMW"
my_groups <- NULL
my_groups = c("demo_gender")

cint_wrapper(file_location, brand = 'BMW', my_groups = NULL)

res <- cint_wrapper(file_location, brand = 'BMW', my_groups = c("demo_gender"))
 

# MANUALLY PULLING QUESTIONS ----------------------------------------------

# running through srvyr with weights (campaign, social, digital)
campaign |> 
  group_by(matched_control_xmedia, unaided_awareness_coded) |> 
  srvyr::summarise(proportion = srvyr::survey_mean(),
                   n = srvyr::survey_total()) 

# or running on the raw data 
df |> 
  count(matched_control_xmedia, unaided_awareness_coded) |> 
  group_by(matched_control_xmedia) |> 
  mutate(total = sum(n))
 
  
df |> 
  count(x_media_banner, class_campaign)



f# mapping and questions ---------------------------------------------------

# i can maybe make this so that it's a function - call the brand and it will populate with 
# proper variables
mapping <- read_csv(here("cint_data", "survey_mapping.csv"), n_max = 347) |> 
  clean_names() |> 
  mutate(question = make_clean_names(question))

mapping |> 
  filter(str_detect(question_text, "BMW")) |> 
  select(question, selection) |> tp()


unweighted |> 
  summarise(total = survey_total())



  


