cascades <- c("KP_PREV", "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_NEW", "PrEP_CT", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag2")


zim <- mer_df %>%  filter(country == "Zimbabwe", 
                          #the request was only for 1 OU. Thus, the fastest way to filter the giant data set is to start with that filter.
                          fiscal_year == 2022,   
                          #also they only want one year, the current year so you should filter here
                          # str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE) %>% 
                          #only KP data are requested so total should be removed 
                          str_detect(standardizeddisaggregate, "KeyPop") == TRUE,
                          indicator %in% cascades) %>% 
  # filter(indicator %in% indicator_list) %>% 
  #this works fine only because we included all cascade indicators but you could have filtered further down to the indicators in the visuals they requested. See cascades list above
  mutate(indicator = recode(as.character(indicator), "TX_PVLS" = paste0(indicator,"_",numeratordenom)),
        indicator = factor(indicator, levels = cascades),
         funding_agency = recode(funding_agency, "HHS/CDC" = "CDC")) %>% 
  arrange(indicator) %>%
  glimpse()


class(zim$indicator)
table(zim$snu1) #note Harare, the Capitol, is an SNU and PSNU. Fairly safe here to aggregate to SNU level after exploration. This reduces data set complexity 
#also note that mech code, mech name, and partner (and TX_ML reason) are also beyond the scope of the data request so they can be removed from the select statement

zimbabwe <- zim %>% filter(disaggregate != "KeyPop/Status") %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         fy = fiscal_year,
         # partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),
         # tx_ml_reason = case_when(indicator=="TX_ML" ~ str_extract(otherdisaggregate, "(?<=Outcome\\s-\\s).+")),
         # keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         # keypop = recode(keypop, "People in prisons" = "Prisoners")
         #not importan for this analysis
         ) %>%
  # select(operatingunit, country, snu1, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, tx_ml_reason, keypop, fy, targets, cumulative) %>%
  #also note that mech code, mech name, and partner (and TX_ML reason) are also beyond the scope of the data request so they can be removed from the select statement
  #of course psnu can also be removed, along with keypop and disaggregate. We only need one of disagg and disaggregate--choose the simplest.
  select( country, snu1, indicator, funding_agency, numeratordenom, disagg, fy, targets, cumulative) %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>% 
  group_by(country, snu1, indicator, funding_agency, numeratordenom, disagg, fy) %>%
  summarise(cumulative = sum(cumulative), targets = sum(targets), .groups = "drop") %>%
  #the lines above are new. Note we have shrunk the data even further.
  glimpse()


#for visualization in R, the order of indicator matters. For visualization in r or Tableau it's just simpler to summarize to the important levels of the data

# zim <- check %>% filter(country == "Zimbabwe")
# remove this line above
write_csv(zim, "Dataout/Zimbabwe_FY22_KP_PEPFAR_data.csv")
