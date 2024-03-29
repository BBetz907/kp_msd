ou_file <- glamr::return_latest(folderpath =  "Data/", pattern = "OU_IM_FY2")
ou_file_archive <- glamr::return_latest(folderpath =  "Data/", pattern = "OU_IM_FY1")

ou <- gophr::read_psd(ou_file, save_rds = TRUE, remove_txt = FALSE)
ou_archived <- gophr::read_psd(ou_file_archive, save_rds = TRUE, remove_txt = FALSE)

ou_archived |> filter(country=="Nepal", 
                     str_detect(indicator, "TB")) |> count(fiscal_year, indicator)


