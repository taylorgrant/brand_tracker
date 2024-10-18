create_directory <- function(brand, filters, main_subfolders = c("tables", "figures"), subfolders = c("Campaign", "Social", "Digital")) {
  if (is.null(filters)) {
    # Create the main folder based on brand and date
    main_path <- here::here(glue::glue("{brand}_{Sys.Date()}"))
    if (!dir.exists(main_path)) {
      dir.create(main_path, recursive = TRUE)
    } 
  } else {
    main_path <- here::here(glue::glue("{brand}-{filters}_{Sys.Date()}"))
    if (!dir.exists(main_path)) {
      dir.create(main_path, recursive = TRUE)
    } 
  }
    
  # Create the main subfolders ("tables" and "figures")
  purrr::walk(main_subfolders, function(main_subfolder) {
    main_subfolder_path <- file.path(main_path, main_subfolder)
    if (!dir.exists(main_subfolder_path)) {
      dir.create(main_subfolder_path)
    } 
    
    # Create the subfolders ("Campaign", "Social", "Digital") inside each main subfolder
    purrr::walk(subfolders, function(subfolder) {
      subfolder_path <- file.path(main_subfolder_path, subfolder)
      if (!dir.exists(subfolder_path)) {
        dir.create(subfolder_path)
      } 
    })
  })
  return(main_path)
  }



