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
  channel_text <- glue::glue("Channel: {dataset_type}")
  
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
      dplyr::mutate(txtcolor = dplyr::case_when(lift > 0 & sig_level >= .95 ~ 'darkgreen',
                                                lift < 0 & sig_level >= .95 ~ "red",
                                                TRUE ~ 'black'),
                    control_shift = dplyr::case_when(lift > 2 ~ -.03,
                                                     lift > 0 & lift <= 2 ~ -.04,
                                                     lift < -2 ~ .03,
                                                     lift == 0 ~ -.04,
                                                     lift <= 0 & lift >= -2 ~ .04),
                    exposed_shift = dplyr::case_when(lift > 2 ~ .03,
                                                     lift > 0 & lift <= 2 ~ .04,
                                                     lift < -2 ~ -.03,
                                                     lift == 0 ~ .04,
                                                     lift < 0 & lift >= -2 ~ -.04),
                    lift = ifelse(lift > 0, paste0("+", lift), lift),
                    sig_level = ifelse(is.na(sig_level), 0, sig_level),
                    lift = ifelse(sig_level >= .95, paste0(lift, "*"), lift),
                    nudgex = .25,
                    point_size = 3,
                    text_size = 3,
                    axis_text_size = 10,
                    lift_size = 3,
                    fig_height = 4.2) |> 
      dplyr::select(-svy_q) |> 
      dplyr::filter(Category != "Brand Momentum") |> 
      dplyr::mutate(Category = 
                      factor(Category, 
                             levels = c("Unaided\nAwareness", "Aided\nAwareness", 
                                        "Aided Ad\nAwareness", "Purchase\nConsideration", 
                                        "Brand Momentum\nTop 2 Box")),
      Category = forcats::fct_rev(Category)) 
    # sample <- glue::glue("* Statistically significant lift at 90% confidence interval\n{sub3} Sample Overall: ", 
    #                      "Control = {round(tmp$`total_control`[1])}, Exposed = {round(tmp$`total_test`[1])}; BMW Aware: Control = {round(tmp$`total_control`[3])}, Exposed = {round(tmp$`total_test`[3])}")
  
    } else if (names(dat)[1] == "Stands for joy") {
    tmp <- data.table::rbindlist(dat, idcol = "Key Attributes") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::mutate(txtcolor = dplyr::case_when(lift > 0 & sig_level >= .95 ~ 'darkgreen',
                                                lift < 0 & sig_level >= .95 ~ "red",
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
                    lift = ifelse(sig_level >= .95, paste0(lift, "*"), lift),
                    nudgex = .4,
                    point_size = 2,
                    text_size = 2.5,
                    axis_text_size = 8,
                    lift_size = 3,
                    fig_height = 4.8) |> 
      dplyr::select(-svy_q)
    
    # sample <- glue::glue("* Statistically significant lift at 90% confidence interval\n{sub3} Sample ", 
    #                      "BMW Aware: Control = {round(tmp$`total_control`[3])}, Exposed = {round(tmp$`total_test`[3])}")
  } else {
    tmp <- data.table::rbindlist(dat, idcol = "Brand Attributes") |> 
      dplyr::tibble() |> 
      dplyr::select(-c(prop_test, p_value, statistic)) |> 
      dplyr::mutate(txtcolor = dplyr::case_when(lift > 0 & sig_level >= .95 ~ 'darkgreen',
                                                lift < 0 & sig_level >= .95 ~ "red",
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
                    lift = ifelse(sig_level >= .95, paste0(lift, "*"), lift),
                    nudgex = .4,
                    point_size = 2,
                    text_size = 2.5,
                    axis_text_size = 8,
                    lift_size = 3,
                    fig_height = 4.8)
    
    # sample <- glue::glue("* Statistically significant lift at 90% confidence interval\n{sub3} Sample ", 
    #                      "BMW Aware: Control = {round(tmp$`total_control`[3])}, Exposed = {round(tmp$`total_test`[3])}")
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
    sub3 <- glue::glue("Overall")
  }
  
  # captions and titles 
  if (length(filters) >= 1) {
    plot_title <- glue::glue("Group: {sub3}")
  } else {
    plot_title <- glue::glue("{sub3}")
  }
  sample <- glue::glue("* Statistically significant lift at 95% confidence interval\n{channel_text}; {sub3} Sample ", 
                       "BMW Aware: Control = {round(tmp$`total_control`[3])}, Exposed = {round(tmp$`total_test`[3])}")
  
  # convert attributes to factors for consistent plotting 
  if (names(tmp)[1] == "Key Attributes") {
    tmp <- tmp |> 
      dplyr::mutate(`Key Attributes` = trimws(gsub("\\(.*", "", `Key Attributes`)),
                    `Key Attributes` = c("Stands for joy", "Creates joy for\nfuture generations", "Innovative offers\nand products",
                                 "Creates positive\nlasting memories", "Committed to\nsustainability",
                                 "Offers desirable\nproducts and services", "Puts customers first",
                                 "Makes technology exciting", "Offers engaging and\ninteractive technology",
                                 "Fits into my lifestyle", "Allows me to focus\non myself", "Stands for luxury",
                                 "Is a reward for\nmy accomplishments"),
                    `Key Attributes` = factor(`Key Attributes`,
                                      levels = c("Stands for joy", "Creates joy for\nfuture generations", "Innovative offers\nand products",
                                                 "Creates positive\nlasting memories", "Committed to\nsustainability",
                                                 "Offers desirable\nproducts and services", "Puts customers first",
                                                 "Makes technology exciting", "Offers engaging and\ninteractive technology",
                                                 "Fits into my lifestyle", "Allows me to focus\non myself", "Stands for luxury",
                                                 "Is a reward for\nmy accomplishments")),
                    `Key Attributes` = forcats::fct_rev(`Key Attributes`))
  } else if (names(tmp)[1] == "Brand Attributes") {
    tmp <- tmp |> 
      dplyr::mutate(`Brand Attributes` = trimws(gsub("\\(.*", "", `Brand Attributes`)),
                    `Brand Attributes` = c("I fully trust", "I can fully\nidentify with", "I really like",
                                 "I would like to own", "I would be willing to\npay more for than for\nother premium brands",
                                 "Is leading in\nelectric drive", "Is leading in\ndigitalization",
                                 "Is leading in\nsustainability efforts", "Will still be relevant\nin 50 years"),
                    `Brand Attributes` = factor(`Brand Attributes`,
                                      levels = c("I fully trust", "I can fully\nidentify with", "I really like",
                                                 "I would like to own", "I would be willing to\npay more for than for\nother premium brands",
                                                 "Is leading in\nelectric drive", "Is leading in\ndigitalization",
                                                 "Is leading in\nsustainability efforts", "Will still be relevant\nin 50 years")),
                    `Brand Attributes` = forcats::fct_rev(`Brand Attributes`))
  }
  
  # PLOT --------------------------------------------------------------------
  
  # create directories to save figures
  path <- create_directory(brand, filters = gsub(" & ", "-", sub3))
  file_name <- switch(names(tmp)[1], 
                      "Category" = paste0(tolower(dataset_type),"-brand_vars_plot.png"),
                      "Key Attributes" = paste0(tolower(dataset_type),"-key_attrs_plot.png"),
                      "Brand Attributes" = paste0(tolower(dataset_type),"-brand_attrs_plot.png")
  )

  # rename the first column 
  tmp <- tmp |> dplyr::rename(Category = 1)
  
  # run data to get gradient
  segment_data <- tmp |> 
    dplyr::group_by(Category) |> 
    dplyr::do(create_gradient_segment(.$Category, .$proportion_control, .$proportion_test)) |> 
    dplyr::ungroup()
  
  # axis limits for the plot 
  low <- floor(min(tmp$proportion_control)*10)/10
  hi <- ceiling(max(tmp$proportion_test)*10)/10
  
  # plotting
  plot <- ggplot2::ggplot() + 
    ggplot2::geom_segment(data = segment_data, 
                          ggplot2::aes(x = x, xend = x, y = y, yend = yend, color = value), linewidth = 1) +
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
    ggplot2::theme(axis.text = ggplot2::element_text(size = unique(tmp$axis_text_size), lineheight = .8),
                   plot.caption = ggplot2::element_text(size = 7),
                   plot.title.position = "plot",
                   plot.subtitle = ggtext::element_markdown(),
                   # plot.background = element_rect(colour = "#0166B1", fill = NA, linewidth = .15)
                   )
  # sizing for Key Vars
  ggplot2::ggsave(filename = file.path(path, "figures", dataset_type, file_name), 
                  plot = plot, 
                  width = 4.45, height = tmp$fig_height)
}
