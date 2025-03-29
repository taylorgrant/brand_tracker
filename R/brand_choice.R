#' Retrieve tracker variable names for a specific brand
#'
#' Based on the input brand name, this function returns the survey variable names used
#' to measure brand awareness, key attributes, and brand perceptions in the tracker data.
#' 
#' Supported brands include: Audi, BMW, Lexus, Mercedes Benz, Tesla, and None of the Above.
#' This function is case-insensitive and includes alias handling (e.g., "merc" → "Mercedes Benz").
#'
#' @param brand Character. Name of the brand of interest (e.g., "BMW").
#'
#' @return A named list of three tibbles:
#' \describe{
#'   \item{brand_vars}{Variables and question labels for awareness and momentum.}
#'   \item{key_attrs}{Variables and question labels for key model attributes.}
#'   \item{brand_attrs}{Variables and question labels for brand perceptions.}
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' brand_choice("BMW")
#' brand_choice("merc")
#' }
brand_choice <- function(brand) {
  brand_clean <- tolower(brand)
  
  if (!stringr::str_detect(brand_clean, "mercedes|tesla|bmw|audi|lexus|none")) {
    stop("Select a make included in the tracker")
  }
  
  if (stringr::str_detect(brand_clean, "merc")) {
    brand <- "Mercedes Benz"
  } else if (stringr::str_detect(brand_clean, "none")) {
    brand <- "None of the Above"
  }
  
  brand_clean <- tolower(brand)
  
  bv_n <- switch(brand_clean, 
                 "none of the above" = c("6", "7", "99", "x6"),
                 "audi" = c("4", "4", "4", "x4"),
                 "bmw" = c("1", "1", "1", "x1"),
                 "lexus" = c("7", "31", "5", "x30"),
                 "mercedes benz" = c("2", "2", "2", "x2"),
                 "tesla" = c("3", "3", "3", "x3"))
  
  ka_n <- switch(brand_clean, 
                 "none of these" = rep("99", 13),
                 "audi" = rep("4", 13),
                 "bmw" = rep("1", 13),
                 "lexus" = rep("5", 13),
                 "mercedes benz" = rep("2", 13),
                 "tesla" = rep("3", 13))
  
  ba_n <- switch(brand_clean, 
                 "none of the above" = rep("99", 9),
                 "audi" = rep("4", 9),
                 "bmw" = rep("1", 9),
                 "lexus" = rep("5", 9),
                 "mercedes benz" = rep("2", 9),
                 "tesla" = rep("3", 9))
  
  bv_vec <- c("awr_a_", "awr_aad_", "con_br_", "opn_br_")
  ka_vec <- c("att_br_17_", "att_br_18_", "att_br_19_", 
              "att_br_20_", "att_br_21_", "att_br_22_", 
              "att_br_23_", "att_br_24_","att_br_25_",
              "att_br_26_", "att_br_27_", "att_br_28_", 
              "att_br_29_")
  ba_vec <- c("att_br2_1_", "att_br2_2_", "att_br2_3_", "att_br2_4_", 
              "att_br2_5_", "att_br2_6_", "att_br2_7_", "att_br2_8_", 
              "att_br2_9_")
  
  if (brand_clean == "bmw") {
    brand_vars <- dplyr::tibble(
      var = c("unaided_awareness_coded", glue::glue("{bv_vec}{bv_n}")),
      q = c("Unaided Awareness", "Aided Awareness", "Aided Ad Awareness", 
            "Purchase Consideration", "Brand Momentum")
    )
  } else {
    brand_vars <- dplyr::tibble(
      var = glue::glue("{bv_vec}{bv_n}"),
      q = c("Aided Awareness", "Aided Ad Awareness", 
            "Purchase Consideration", "Brand Momentum")
    )
  }
  
  key_attrs <- dplyr::tibble(
    var = glue::glue("{ka_vec}{ka_n}"),
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
          "Is a reward for my accomplishments")
  )
  
  brand_attrs <- dplyr::tibble(
    var = glue::glue("{ba_vec}{ba_n}"),
    q = c("I fully trust", "I can fully identify with", 
          "I really like", "I would like to own", 
          "I would be willing to pay more for than for other premium brands", 
          "Is leading in electric drive", 
          "Is leading in digitalization (i.e. digital user experience, apps, connectivity, voice assistant, 5G, over", 
          "Is leading in sustainability efforts (e.g. to recycle and reuse materials to avoid waste)", 
          "Is a brand that will still be relevant in 50 years")
  )
  
  list(brand_vars = brand_vars, key_attrs = key_attrs, brand_attrs = brand_attrs)
}
