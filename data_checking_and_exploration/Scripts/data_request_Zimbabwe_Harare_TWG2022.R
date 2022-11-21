df <- mer_df %>%  filter(
  str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE) %>%
  mutate(indicator = recode(indicator, "TX_PVLS" = paste0(indicator,"_",numeratordenom)),
         funding_agency = recode(funding_agency, "HHS/CDC" = "CDC")) %>%
  filter(indicator %in% indicator_list) %>% mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>% glimpse()

check <- df %>% filter(disaggregate != "KeyPop/Status") %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         fy = fiscal_year,
         partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),
         tx_ml_reason = case_when(indicator=="TX_ML" ~ str_extract(otherdisaggregate, "(?<=Outcome\\s-\\s).+")),
         keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         keypop = recode(keypop, "People in prisons" = "Prisoners")) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, tx_ml_reason, keypop, fy, targets, cumulative) %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) 

zim <- check %>% filter(country == "Zimbabwe")

write_csv(zim, "Data/Zimbabwe_KP_PEPFAR_data.csv")
