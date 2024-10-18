#' Choose the brand of interest
#' 
#' Brands in the tracker include Audi, BMW, Lexus, Mercedes Benz, Tesla, and None of the Above
#'
#' @param brand String, name of brand
#'
#' @return List of atributes and column names for each
#' @export
#'
#' @examples
#' \dontrun{
#' choice <- brand_choice("BMW")
#' }
brand_choice <- function(brand) {
  if (!stringr::str_detect(tolower(brand), "mercedes|tesla|bmw|audi|lexus|none")) {
    stop("Select a make included in the tracker")
  }
  if (stringr::str_detect(tolower(brand), "merc")) {
    brand <- "Mercedes Benz"
  } else if (stringr::str_detect(tolower(brand), "none")) {
    brand <- "None of the Above"
  }
  bv_n <- switch(tolower(brand), 
                 "none of the above" = c("6", "7", "1", "x6"),
                 "audi" = c("4", "4", "28", "x4"),
                 "bmw" = c("1", "1", "5", "x1"),
                 "lexus" = c("7", "31", "31", "x30"),
                 "mercedes benz" = c("2", "2", "26", "x2"),
                 "tesla" = c("3", "3", "27", "x3"))
  
  ka_n <- switch(tolower(brand), 
                 "none of the above" = rep("6", 13),
                 "audi" = rep("13", 13),
                 "bmw" = rep("10", 13),
                 "lexus" = rep("36", 13),
                 "mercedes benz" = rep("31", 13),
                 "tesla" = rep("32", 13))
  
  ba_n <- switch(tolower(brand), 
                 "none of the above" = rep("6", 9),
                 "audi" = rep("13", 9),
                 "bmw" = rep("10", 9),
                 "lexus" = rep("36", 9),
                 "mercedes benz" = rep("31", 9),
                 "tesla" = rep("32", 9))
  
  bv_vec <- c("awr_a_", "awr_aad_", "con_br_", "opn_br_")
  ka_vec <- c("att_br_1_", "att_br_18_", "att_br_19_", 
              "att_br_20_", "att_br_21_", "att_br_22_", 
              "att_br_23_", "att_br_24_","att_br_25_",
              "att_br_26_", "att_br_27_", "att_br_28_", 
              "att_br_29_")
  ba_vec <- c("att_br2_1_", "att_br2_2_", "att_br2_3_", "att_br2_4_", 
              "att_br2_5_", "att_br2_6_", "att_br2_7_", "att_br2_8_", 
              "att_br2_9_")
  
  if (tolower(brand) == "bmw") {
    brand_vars <- dplyr::tibble(var = c("unaided_awareness_coded", glue::glue("{bv_vec}{bv_n}")),
                                q = c("Unaided Awareness","Aided Awareness",
                                      "Aided Ad Awareness", "Purchase Consideration",
                                      "Brand Momentum"))
  } else {
    brand_vars <- dplyr::tibble(var = c(glue::glue("{bv_vec}{bv_n}")),
                                q = c("Aided Awareness",
                                      "Aided Ad Awareness", "Purchase Consideration",
                                      "Brand Momentum"))
  }
  key_attrs <- dplyr::tibble(var = c(glue::glue("{ka_vec}{ka_n}")),
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
  brand_attrs <- dplyr::tibble(var = c(glue::glue("{ba_vec}{ba_n}")),
                               q = c("I fully trust", "I can fully identify with", 
                                     "I really like", "I would like to own", 
                                     "I would be willing to pay more for than for other premium brands", 
                                     "Is leading in electric drive", 
                                     "Is leading in digitalization (i.e. digital user experience, apps, connectivity, voice assistant, 5G, over", 
                                     "Is leading in sustainability efforts (e.g. to recycle and reuse materials to avoid waste)", 
                                     "Is a brand that will still be relevant in 50 years"))
  
  list(brand_vars = brand_vars, key_attrs = key_attrs, brand_attrs = brand_attrs)
}