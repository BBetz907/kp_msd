library(tidyverse)
library(janitor)
library(gagglr)
library(grabr)
library(htmltools)

# set_pano("bbetz@usaid.gov")

load_secrets()

# set_paths(folderpath_msd = "Data",
#                     folderpath_datim =  "Data",
#                     folderpath_downloads =  "Data")
## comment in/out the above after setting initially


#create active session

sess <- grabr::pano_session(username = pano_user(), password = pano_pwd())

# Extract data items details
# url <- "https://pepfar-panorama.org/forms/downloads/"

mer_items <- pano_items(page_url = dir_mer_path, 
                        username = pano_user(),
                        password = pano_pwd()) 

mer_items

dest_path <- paste0(si_path(),"/Temp/")



# pull latest psnuXim MSD ---------------------------------------------------------
url_psnu_im <- mer_items %>%
  filter(type == "file zip_file",
         str_detect(item, ".*_PSNU_IM_FY2.*.zip$")) %>%
  pull(path) %>%
  first() |> print()

 # quick fix to filepaths --------------------------------------------------
grabr::pano_download(item_url = url_psnu_im, session = sess)



 
# obtain OUxIM MSD  ------------------------------------------------
url_ou_im <- mer_items %>%
   filter(type == "file zip_file",
          str_detect(item, ".*_OU_IM_FY.*.zip$")) %>%
   pull(path) %>%
   first() 

 # quick fix to filepaths --------------------------------------------------
 grabr::pano_download(item_url = url_ou_im, session = sess)
 
 
 

# optional additional data
# Load packages
 library(rvest)
 library(httr)
 library(zip)

# pull latest NAT_subnat MSD ---------------------------------------------------------
url_nat_subnat <- mer_items %>%
  filter(type == "file zip_file",
         str_detect(item, ".*_NAT_SUBNAT_FY.*.zip$")) %>%
  pull(path) %>% 
  first() |> print()

grabr::pano_download(item_url = url_nat_subnat, session = sess)


# Download archived PSNUxIM MSD ------------------------------------------------
url_archived_msd <- items %>%
  filter(type == "file zip_file",
         str_detect(parent, "FY1"),
         str_detect(item, ".*_PSNU_IM_FY1.*.zip$")) %>% pull(path) |> print()
  

grabr::pano_download(item_url = url_archived_msd, session = sess)



# obtain mer calculations across time  ------------------------------------------------
url_calc_by_time_items <- items %>%
  filter(type == "file zip_file",
         str_detect(parent, "MER Calculations Across Time$")
  ) %>%
  pull(path) |> print()

dest_path_site <- paste0(si_path(),"/site-level calculations/")

map(url_calc_by_time_items, ~grabr::pano_download(item_url = .x, session = sess, dest = dest_path_site))


# obtain site-level data for select countries -----------------------------
#read list of OUs
epic_ou_data <- read_csv(file = "data/central_mechanism_mech_codes.csv")

#create list for filtering
epic_ous <- epic_ou_data |> 
  pull(operating_unit) |> print()
class(epic_ous)

epic_ou_data |> filter(operating_unit %in% epic_ous[1:2]) #test


#replace characters and spaces for str_detect. Then concatenate
epic_ous_list <- epic_ous |> str_replace_all("\\s", "\\\\s") |> str_replace("\\'", "\\\\'") |> 
  paste(collapse = "|") |> print()


url_site <- items |> 
  filter(type == "file zip_file",
         str_detect(parent, "n\\/Site\\sLevel"),
         str_detect(item, epic_ous_list)
         ) |>
  pull(path) |>
  print()

dest_path_site2 <- paste0(si_path(),"/site-level/")

map(url_site, ~grabr::pano_download(item_url = .x, session = sess, dest = dest_path_site2))
