path <- "Data/site-level/"
file_mli <- glamr::return_latest(folderpath =  "Data/site-level/", pattern = "Malawi") |> str_c(p)
file_cdi <- glamr::return_latest(folderpath =  "Data/site-level/", pattern = "Cote")

files <- c(file_mli, file_cdi)

# filenames <- map(files, ~str_c(path, .x))

df <- map_dfr(files, ~read_psd(file = .x))

#list of prime partners of KP mechanisms

mwi_kp_dic <- df |> filter(country=="Malawi", 
             mech_code %in% c("81759", "81764", "70190"),
             otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  filter(!str_detect(facility, "Health|Hospital|Dispensary|Maternity|Clinic|Project|Dream|District|Facility")) |> 
            group_by(facilityuid) |> summarise(.groups = "drop") |> pull()



  
mli <- df |> filter(country=="Malawi",
             otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID"),
             ) |> 
  mutate(level = case_when(facilityuid == "~" ~ "above site", 
                           facilityuid %in% kp_dic ~ "DIC",
                           .default = "facility"),
         mech_type = if_else(mech_code %in% c("81759", "81764", "70190"), "KP mech", "Other mech")) |> 
  select(-contains("qtr")) |> 
  group_by(across(-c("cumulative", "targets")))|> 
  summarize(across(c("cumulative","targets"), ~sum(., na.rm = TRUE)), .groups= "drop") |> 
  glimpse()


cdi_empower <- df |> filter(
             mech_code %in% c("81612", "81611"),
             otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID"),
             cumulative > 0, 
             fiscal_year == 2024) |> 
  group_by(prime_partner_name, snu1, psnu, community) |> summarise()

cdi_empower |> writexl::write_xlsx(path = "Documents/cdi_mechanisms.xlsx")

??openxlsx
library(openxlsx)
