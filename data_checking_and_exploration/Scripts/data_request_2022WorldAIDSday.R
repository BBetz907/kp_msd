library(janitor)
WAD_KP <- mer_df %>% filter(fiscal_year == 2022,
                         str_detect(disaggregate, "^Total\\sNumerator$|^KeyPop(?!\\/Status)"),      #keep only KP data, eliminating duplicate disagg results for KP_PREV /status
                         indicator %in% c("PrEP_NEW", "TX_CURR", "KP_PREV"),
                         funding_agency == "USAID") %>% 
  select(fiscal_year, disaggregate, indicator, cumulative) %>% 
  mutate(cumulative = coalesce(cumulative, 0)) %>%
  group_by(fiscal_year, disaggregate, indicator) %>%
  summarise(cumulative = sum(cumulative),  .groups = 'drop') %>%
  pivot_wider(names_from = indicator, values_from = cumulative) 



WAD_KP %>% select(fiscal_year, disaggregate, PrEP_NEW) %>%
  pivot_wider(names_from = disaggregate, values_from = PrEP_NEW) %>% 
  clean_names() %>% 
  mutate(percentkpprep = key_pop/total_numerator) %>% 
  select(-key_pop_hiv_status) %>%
  print()

WAD_KP %>% filter(str_detect(disaggregate, "^KeyPop(?!\\/Status)")) %>%
  select(-disaggregate) %>% 
  group_by(fiscal_year) %>% 
  pivot_longer(KP_PREV:TX_CURR, 
               names_to = "indicator"
                 ) %>%
  filter(!is.na(value)) %>%
  print()
