cascades <- c("KP_PREV", "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_NEW", "PrEP_CT", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag2")


zim <- mer_df %>%  filter(country == "Zimbabwe", 
                          fiscal_year == 2022,   
                          str_detect(standardizeddisaggregate, "KeyPop") == TRUE,
                          indicator %in% cascades) %>% 
  mutate(indicator = recode(as.character(indicator), "TX_PVLS" = paste0(indicator,"_",numeratordenom)),
        indicator = factor(indicator, levels = cascades),
         funding_agency = recode(funding_agency, "HHS/CDC" = "CDC")) %>% 
  arrange(indicator) %>%
  glimpse()


class(zim$indicator)
table(zim$snu1)

zimbabwe <- zim %>% filter(disaggregate != "KeyPop/Status") %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         fy = fiscal_year,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),) %>%
  select( country, snu1, psnu, indicator, funding_agency, numeratordenom, disagg, fy, targets, cumulative) %>%
  mutate(indicator = factor(indicator, levels = cascade)) %>% arrange(indicator) %>% 
  group_by(country, snu1, psnu, indicator, funding_agency, numeratordenom, disagg, fy) %>%
  summarise(cumulative = sum(cumulative), targets = sum(targets), .groups = "drop") %>%
  glimpse()

write_csv(zim, "Data/Zimbabwe_FY22_KP_PEPFAR_data.csv")
