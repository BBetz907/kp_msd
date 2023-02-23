tx_indicators <- c("TX_CURR", "TX_PVLS", "TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag2")

tx <- mer_df %>%  filter(indicator %in% tx_indicators,
                         fiscal_year == "2022",
                         str_detect(standardizeddisaggregate, "Total") == TRUE) %>%
  mutate(indicator = recode(indicator, "TX_PVLS" = paste0(indicator,"_",numeratordenom))) %>% 
  pivot_longer(targets:cumulative, names_to = "value_type", values_to = "value") %>%
  mutate(value = replace_na(value, 0),
         disagg = "Total",
         funding_agency = "PEPFAR") %>%
  group_by(operatingunit, country, indicator, disagg, funding_agency, value_type) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = value_type, values_from = value) %>% ungroup() %>%
  print()

write_csv(tx, file = "Dataout/PEPFAR_FY22Q3_Global_TX_VL.csv")

tx %>% filter(operatingunit == "Asia Region") %>% 
  select(country, indicator, qtr3) %>% 
  pivot_wider(names_from = indicator, values_from = qtr3) %>% 
  mutate(vlc = TX_PVLS_D/TX_CURR_Lag2,
         vls = TX_PVLS_N/TX_PVLS_D) %>%
  print()
