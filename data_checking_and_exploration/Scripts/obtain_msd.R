remotes::install_github("USAID-OHA-SI/gagglr", build_vignettes = TRUE)
library(gagglr)
library(glamr)
library(keyring)
library(tidyverse)

load_secrets()


 glamr::set_paths(folderpath_msd = "Data",
                   folderpath_datim =  "Data",
                   folderpath_downloads =  "Data")
## comment in/out the above after setting initially


#create active session

sess <- pano_session(username = pano_user(), password = pano_pwd())

# Extract data items details
url <- "https://pepfar-panorama.org/forms/downloads/"

cont <- pano_content(page_url = url, session = sess)


# Download most recent PSNUxIM MSD ------------------------------------------------
# Extract data items details
dirs <- pano_elements(page_html = cont)

dir_mer_path <- dirs %>%
  filter(str_detect(item, "^MER")) %>%
  pull(path)

mer_items <- pano_content(page_url = dir_mer_path, session = sess) %>%
  pano_elements(page_url = dir_mer_path)
# Extract MER data items details from HTML CODE
 dest_path <- paste0(si_path(),"/Temp/")
 

# pull latest psnuXim MSD ---------------------------------------------------------
url_psnu_im <- mer_items %>%
  filter(type == "file zip_file",
         str_detect(item, ".*_PSNU_IM_FY2.*.zip$")) %>%
  pull(path) %>%
  first()

# quick fix to filepaths --------------------------------------------------
pano_download(item_url = url_psnu_im, session = sess)


######################################################################################
######################################################################################
######################################################################################

# optional additional data
################################################################################
# Load packages
 library(rvest)
 library(httr)
 library(zip)

# pull latest NAT_subnat MSD ---------------------------------------------------------
url_nat_subnat <- mer_items %>%
  filter(type == "file zip_file",
         str_detect(item, ".*_NAT_SUBNAT_FY.*.zip$")) %>%
  pull(path) %>%
  first()

pano_download(item_url = url_nat_subnat, session = sess)


# Download archived PSNUxIM MSD ------------------------------------------------
url_archived_msd <- mer_items[1,4] %>% pull(path) #enter directory for FY15 - ... from mer_items

mer_items2 <- pano_content(page_url = url_archived_msd, session = sess) %>%
  pano_elements(page_url = url_archived_msd)


url_psnu_im_archive <- mer_items2 %>%
  filter(type == "file zip_file",
         str_detect(item, ".*_PSNU_IM_FY1.*.zip$")) %>%
  pull(path) %>%
  first()


pano_download(item_url = url_psnu_im_archive, session = sess)


# Download archived regional program site-level MSDs ------------------------------------------------
url_site_msd <- mer_items[4,4] %>% pull(path) #enter directory for FY15 - ... from mer_items

mer_items3 <- pano_content(page_url = url_site_msd, session = sess) %>%
  pano_elements(page_url = url_site_msd)

# mer_items3_1 <- mer_items3[2,4] %>% pull(path)

mer_site_region <- mer_items3 %>%
  filter(type == "file zip_file",
         str_detect(item, "Region")) %>%
  pull(path) 

mer_site_region[1]

pano_download(item_url = mer_site_region[1], session = sess)
pano_download(item_url = mer_site_region[3], session = sess)

#learn how to iterate this step