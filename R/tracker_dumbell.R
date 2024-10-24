tracker_figure <- function(dat, brand, filters, dataset_type){
  
  # Function to create points along a segment
  create_gradient_segment <- function(x, y1, y2, n_points = 100) {
    tibble::tibble(
      x = x,
      y = seq(y1, y2, length.out = n_points),
      yend = dplyr::lead(y),
      value = seq(0, 1, length.out = n_points)  # For color gradient
    ) |> 
      na.omit()  # Remove the last NA row from lead()
  }
  
  # sub1 <- glue::glue("Make: {brand}")
  plot_title <- glue::glue("Channel: {dataset_type}")
  
  # set up the data for the plots 
  if ((brand == "BMW" & names(dat)[1] == "Unaided Awareness") | (brand != 'BMW' & names(dat)[1] == "Aided Awareness")) {
    tmp <- data.table::rbindlist(dat, idcol = "Category") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::mutate(Category = ifelse(Category == "Brand Momentum" & svy_q == "On its way up - Top 2 Box", paste0(Category, "\nTop 2 Box"), Category)) |>
      dplyr::mutate(Category = dplyr::case_when(Category == "Unaided Awareness" ~ "Unaided\nAwareness",
                                                Category == "Aided Awareness" ~ "Aided\nAwareness",
                                                Category == "Aided Ad Awareness" ~ "Aided Ad\nAwareness",
                                                Category == "Purchase Consideration" ~ "Purchase\nConsideration",
                                                TRUE ~ Category)) |> 
      dplyr::mutate(txtcolor = dplyr::case_when(lift > 0 & sig_level >= .90 ~ 'darkgreen',
                                                lift < 0 & sig_level >= .90 ~ "red",
                                                TRUE ~ 'black'),
                    control_shift = dplyr::case_when(lift > 2 ~ -.03,
                                                     lift > 0 & lift <= 2 ~ -.04,
                                                     lift < -2 ~ .03,
                                                     lift < 0 & lift >= -2 ~ .04),
                    exposed_shift = dplyr::case_when(lift > 2 ~ .03,
                                                     lift > 0 & lift <= 2 ~ .04,
                                                     lift < -2 ~ -.03,
                                                     lift < 0 & lift >= -2 ~ -.04),
                    lift = ifelse(lift > 0, paste0("+", lift), lift),
                    sig_level = ifelse(is.na(sig_level), 0, sig_level),
                    lift = ifelse(sig_level >= .9, paste0(lift, "*"), lift),
                    nudgex = .25,
                    point_size = 3,
                    text_size = 3,
                    axis_text_size = 10,
                    lift_size = 4) |> 
      dplyr::select(-svy_q) |> 
      dplyr::filter(Category != "Brand Momentum") |> 
      dplyr::mutate(Category = 
                      factor(Category, 
                             levels = c("Unaided\nAwareness", "Aided\nAwareness", 
                                        "Aided Ad\nAwareness", "Purchase\nConsideration", 
                                        "Brand Momentum\nTop 2 Box")),
      Category = forcats::fct_rev(Category)) 
    sample <- glue::glue("* Statistically significant lift at 90% confidence interval\nSample Overall:", 
                         "Control = {round(tmp$`total_control`[1])}, Exposed = {round(tmp$`total_test`[1])};",
                         "\nSample BMW Aware: Control = {round(tmp$`total_control`[3])}, Exposed = {round(tmp$`total_test`[3])}")
  
    } else if (names(dat)[1] == "Stands for joy") {
    tmp <- data.table::rbindlist(dat, idcol = "Category") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::mutate(Category = trimws(gsub("\\(.*", "", Category)),
                    Category = c("Stands for joy", "Creates joy for\nfuture generations", "Innovative offers\nand products", 
                                 "Creates positive\nlasting memories", "Committed to\nsustainability", 
                                 "Offers desirable\nproducts and services", "Puts customers first", 
                                 "Makes technology exciting", "Offers engaging and\ninteractive technology", 
                                 "Fits into my lifestyle", "Allows me to focus\non myself", "Stands for luxury", 
                                 "Is a reward for\nmy accomplishments"),
                    Category = factor(Category, 
                                      levels = c("Stands for joy", "Creates joy for\nfuture generations", "Innovative offers\nand products", 
                                                 "Creates positive\nlasting memories", "Committed to\nsustainability", 
                                                 "Offers desirable\nproducts and services", "Puts customers first", 
                                                 "Makes technology exciting", "Offers engaging and\ninteractive technology", 
                                                 "Fits into my lifestyle", "Allows me to focus\non myself", "Stands for luxury", 
                                                 "Is a reward for\nmy accomplishments")),
                    Category = forcats::fct_rev(Category)) |>
      dplyr::mutate(txtcolor = dplyr::case_when(lift > 0 & sig_level >= .90 ~ 'darkgreen',
                                                lift < 0 & sig_level >= .90 ~ "red",
                                                TRUE ~ 'black'),
                    control_shift = dplyr::case_when(lift > 2 ~ -.03,
                                                     lift > 0 & lift <= 2 ~ -.04,
                                                     lift < -2 ~ .03,
                                                     lift == 0 ~ -.03,
                                                     lift < 0 & lift >= -2 ~ .04),
                    exposed_shift = dplyr::case_when(lift > 2 ~ .03,
                                                     lift > 0 & lift <= 2 ~ .04,
                                                     lift < -2 ~ -.03,
                                                     lift == 0 ~ .03,
                                                     lift < 0 & lift >= -2 ~ -.04),
                    lift = ifelse(lift > 0, paste0("+", lift), lift),
                    sig_level = ifelse(is.na(sig_level), 0, sig_level),
                    lift = ifelse(sig_level >= .9, paste0(lift, "*"), lift),
                    nudgex = .4,
                    point_size = 2,
                    text_size = 2.5,
                    axis_text_size = 8,
                    lift_size = 3) |> 
      dplyr::select(-svy_q)
  } else {
    tmp <- data.table::rbindlist(dat, idcol = "Category") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::mutate(Category = trimws(gsub("\\(.*", "", Category)),
                    Category = c("I fully trust", "I can fully\nidentify with", "I really like", 
                                 "I would like to own", "I would be willing to\npay more for than for\nother premium brands", 
                                 "Is leading in\nelectric drive", "Is leading in\ndigitalization", 
                                 "Is leading in\nsustainability efforts", "Will still be relevant\nin 50 years"),
                    Category = factor(Category, 
                                      levels = c("I fully trust", "I can fully\nidentify with", "I really like", 
                                                 "I would like to own", "I would be willing to\npay more for than for\nother premium brands", 
                                                 "Is leading in\nelectric drive", "Is leading in\ndigitalization", 
                                                 "Is leading in\nsustainability efforts", "Will still be relevant\nin 50 years")),
                    Category = forcats::fct_rev(Category)) |>
      dplyr::mutate(txtcolor = dplyr::case_when(lift > 0 & sig_level >= .90 ~ 'darkgreen',
                                                lift < 0 & sig_level >= .90 ~ "red",
                                                TRUE ~ 'black'),
                    control_shift = dplyr::case_when(lift > 2 ~ -.03,
                                                     lift > 0 & lift <= 2 ~ -.04,
                                                     lift < -2 ~ .03,
                                                     lift == 0 ~ -.03,
                                                     lift < 0 & lift >= -2 ~ .04),
                    exposed_shift = dplyr::case_when(lift > 2 ~ .03,
                                                     lift > 0 & lift <= 2 ~ .04,
                                                     lift < -2 ~ -.03,
                                                     lift == 0 ~ .03,
                                                     lift < 0 & lift >= -2 ~ -.04),
                    lift = ifelse(lift > 0, paste0("+", lift), lift),
                    sig_level = ifelse(is.na(sig_level), 0, sig_level),
                    lift = ifelse(sig_level >= .9, paste0(lift, "*"), lift),
                    nudgex = .4,
                    point_size = 2,
                    text_size = 2.5,
                    axis_text_size = 8,
                    lift_size = 3)
  }
  
  
  if (length(filters) == 3) {
    sub3 <- glue::glue("{filters[1]} & {filters[2] & filters[3]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% filters[1] & 
                      group_2 %in% filters[2] & 
                      group_3 %in% filters[3]) |> 
      dplyr::select(-c(group_1, group_2, group_3))
    
  } else if (length(filters) == 2) {
    sub3 <- glue::glue("{filters[1]} & {filters[2]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% filters[1] & 
                      group_2 %in% filters[2]) |> 
      dplyr::select(-c(group_1, group_2))
    
  } else if (length(filters) == 1) {
    
    sub3 <- glue::glue("{filters[1]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% filters[1]) |> 
      dplyr::select(-group_1)
  } else {
    sub3 <- glue::glue("No filters")
  }
  
  # PLOT --------------------------------------------------------------------
  

  # run data to get gradient
  segment_data <- tmp |> 
    dplyr::group_by(Category) |> 
    dplyr::do(create_gradient_segment(.$Category, .$proportion_control, .$proportion_test)) |> 
    dplyr::ungroup()
  
  # axis limits for the plot 
  low <- floor(min(tmp$proportion_control)*10)/10
  hi <- ceiling(max(tmp$proportion_test)*10)/10
  
  # plotting
  ggplot2::ggplot() + 
    ggplot2::geom_segment(data = segment_data, 
                          ggplot2::aes(x = x, xend = x, y = y, yend = yend, color = value), size = 1) +
    ggplot2::scale_color_gradient(low = "#6f6f6f", high = "#0166B1", guide = "none") +
    ggplot2::geom_point(data = tmp, 
                        ggplot2::aes(x = Category, y = proportion_control), 
               color = "#6f6f6f", size = tmp$point_size) +
    ggplot2::geom_text(data = tmp, 
                       ggplot2::aes(x = Category, y = proportion_control, label = scales::percent(proportion_control, accuracy = 1)), 
              color = "#6f6f6f", size = tmp$text_size, nudge_x = tmp$nudgex, nudge_y = tmp$control_shift) +
    ggplot2::geom_point(data = tmp, 
                        ggplot2::aes(x = Category, y = proportion_test), 
               color = "#0166B1", size = tmp$point_size) +
    ggplot2::geom_text(data = tmp, 
                       ggplot2::aes(x = Category, y = proportion_test, label = scales::percent(proportion_test, accuracy = 1)), 
               color = "#0166B1", size = tmp$text_size, nudge_x = tmp$nudgex, nudge_y = tmp$exposed_shift) +
    ggplot2::scale_y_continuous(labels = scales::percent, breaks = seq(low, hi, .2)) + 
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    theme_bmw(grid = "Y") +
    ggplot2::coord_flip() +
    ggplot2::annotate("rect", ymin = hi+.1, ymax = hi+.2, xmin = -Inf, xmax = Inf, fill = "#efefe3") +
    ggplot2::geom_text(data=tmp,
                       ggplot2::aes(label = lift, x = Category, y = hi+.15),
              fontface="bold", size = tmp$lift_size, family = "BMW",
              color = tmp$txtcolor) +
    ggplot2::geom_text(data= tmp[1,],
                       ggplot2::aes(x=Category, y = hi+.15, label="LIFT"),
              color="#7a7d7e", family = "BMW",
              size=3.1, vjust=-2, fontface="bold") +
    ggplot2::labs(x = NULL, y = NULL,
         title = plot_title,
         subtitle = "Comparing 
         <span style = 'color:#6f6f6f;'>*Control*</span> and 
         <span style = 'color:#0166B1;'>*Exposed*</span>",
         caption = glue::glue(sample)) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = unique(tmp$axis_text_size), lineheight = .7),
                   plot.caption = ggplot2::element_text(size = 7),
                   plot.title.position = "plot",
                   plot.subtitle = ggtext::element_markdown())
  # sizing for Key Vars
  ggplot2::ggsave("~/Desktop/tmp_ka1.png", width = 4.45, height = 4.2)
  ggplot2::ggsave("~/Desktop/tmp_ka2.png", width = 4.45, height = 4.5)
  ggplot2::ggsave("~/Desktop/tmp_ba3.png", width = 4.45, height = 4.5)
}
