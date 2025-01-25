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
ev_brand_choice <- function(brand) {
  if (!stringr::str_detect(tolower(brand), "bmw|tesla|mercedes|lucid|rivian")) {
    stop("Select a make included in the tracker")
  }
  if (stringr::str_detect(tolower(brand), "merc")) {
    brand <- "Mercedes Benz"
  } else if (stringr::str_detect(tolower(brand), "none")) {
    brand <- "None of the Above"
  }
  bv_n <- switch(tolower(brand), 
                 "none of the above" = c("99", "7", "99", "x6"),
                 "bmw" = c("1", "2", "2", "x1"),
                 "mercedes benz" = c("2", "3", "3", "x2"),
                 "tesla" = c("3", "4", "4", "x3"),
                 "lucid" = c("4", "5", "5", "x4"),
                 "rivian" = c("5", "6", "6", "x5"))
  
  ka_n <- switch(tolower(brand), 
                 "none of these" = rep("99", 14),
                 "bmw" = rep("7", 14),
                 "mercedes benz" = rep("8", 14),
                 "tesla" = rep("9", 14),
                 "lucid" = rep("10", 14),
                 "rivian" = rep("11", 14))
  
  # ba_n <- switch(tolower(brand), 
  #                "none of the above" = rep("99", 9),
  #                "audi" = rep("4", 9),
  #                "bmw" = rep("1", 9),
  #                "lexus" = rep("5", 9),
  #                "mercedes benz" = rep("2", 9),
  #                "tesla" = rep("3", 9))
  
  bv_vec <- c("awr_a_ev_", "awr_aad_", "con_br_", "momentum_br_")
  ka_vec <- c("att_br_17_", "att_br_18_", "att_br_19_", 
              "att_br_20_", "att_br_21_", "att_br_22_", 
              "att_br_24_", "att_br_25_","att_br_26_",
              "att_br_27_", "att_br_28_", "att_br_29_", 
              "att_br_30_", "att_br_31_")
  # ba_vec <- c("att_br2_1_", "att_br2_2_", "att_br2_3_", "att_br2_4_", 
  #             "att_br2_5_", "att_br2_6_", "att_br2_7_", "att_br2_8_", 
  #             "att_br2_9_")
  
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
                             q = c("EVs with competitive ranges", "Makes stylish EVs", 
                                   "Superior build quality and craftsmanship",
                                   "Easy and convenient access to public charging", 
                                   "Superior driving performance",
                                   "Leading tech features in their EVs", 
                                   "Sustainably manufactures their EVs",
                                   "Boasts a luxurious interior", "Superior driving comfort",
                                   "High priority on safety", 
                                   "Excellent support for maintenance and repairs", 
                                   "Willing to pay more for this brand’s EVs",
                                   "Proud to own and drive this brand",
                                   "EVs that provide a confident and exciting drive"))
  # brand_attrs <- dplyr::tibble(var = c(glue::glue("{ba_vec}{ba_n}")),
  #                              q = c("I fully trust", "I can fully identify with", 
  #                                    "I really like", "I would like to own", 
  #                                    "I would be willing to pay more for than for other premium brands", 
  #                                    "Is leading in electric drive", 
  #                                    "Is leading in digitalization (i.e. digital user experience, apps, connectivity, voice assistant, 5G, over", 
  #                                    "Is leading in sustainability efforts (e.g. to recycle and reuse materials to avoid waste)", 
  #                                    "Is a brand that will still be relevant in 50 years"))
  
  list(brand_vars = brand_vars, key_attrs = key_attrs)#, brand_attrs = brand_attrs)
}
