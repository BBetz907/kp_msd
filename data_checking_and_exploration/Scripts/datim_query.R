
# call packages -----------------------------------------------------------



library(tidyverse)
library(janitor)
library(gagglr)
library(grabr)
library(htmltools)
library(glue)

# set_pano("bbetz@usaid.gov")

load_secrets()



# datim query -------------------------------------------------------------

ou_rp <- "Asia Regional Program"

ou_id <- get_ouuid(operatingunit = ou) 



# query PLHIV number
datim_pops(ou = ou_rp)

#identify technical areas
glamr::datim_dim_items("Technical Area")

# More detailed requests with dimension names (converted into uid)
test <- datim_query(
  ou = "Asia Region",                    # Operating unit
  level = "prioritization",             # org level
  pe = "2022Oct",                 # periods
  ta = "TX_CURR",                  # From dimension: Technical Area
  value = c("MER Targets", "MER Results"),         # From dimension: Targets / Results
  # disaggs = "Age/Sex/HIVStatus", # From dimension: Disaggregation Type
  # dimensions = c("Sex"),         # Additional dimension: Sex
  baseurl = "https://datim.org/",
  verbose = TRUE
) |> 
  clean_names() |> glimpse()

test |> pull(targets_results)
