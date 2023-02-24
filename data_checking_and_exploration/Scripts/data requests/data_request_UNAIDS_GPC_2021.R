

gpc_country <- c("Angola", "Botswana", "Brazil", "Cameroon", "China", "Cote d'Ivoire", "Democratic Republic of the Congo", "DRC",
"Eswatini", "Ethiopia", "Ghana", "India", "Indonesia", "Iran", "Kenya", "Lesotho", "Malawi", "Mexico",
"Mozambique", "Myanmar", "Burma","Namibia", "Nigeria", "Pakistan", "South Africa", "Uganda", "Ukraine", "Tanzania","Zambia", "Zimbabwe")

glimpse(check)
table(check$indicator)
gpc <- check %>% filter(fy == 2021,
                 indicator %in% c("KP_PREV", "TX_CURR"),
                 disagg == "KP",
                 # keypop != "Prisoners",
                 country %in% gpc_country) %>% 
  select(country, fy,  keypop, indicator, cumulative) %>% 
  mutate(cumulative = coalesce(cumulative, 0)) %>%
  group_by(country, fy,  keypop, indicator) %>%
  summarise(cumulative = sum(cumulative),  .groups = 'drop') %>%
  pivot_wider(names_from = indicator, values_from = cumulative) %>%
  mutate(KP_PREV  = coalesce(KP_PREV , 0),
         TX_CURR  = coalesce(TX_CURR , 0)) %>%
  print(n=100)

nrow(as.data.frame(unique(gpc$country)))

# table(gpc$country)
# table(gpc$disaggregate, gpc$indicator)

write_csv(gpc, "Dataout/FY21_GPC_country_PEPFAR_data.csv")


###########
gpc_coverage <- check %>% filter(fy == 2021,
                                     indicator %in% c("KP_PREV", "TX_CURR"),
                                     disagg == "KP",
                                     # keypop != "Prisoners",
                                     country == "Cameroon",
                                     country %in% gpc_country) %>% 
  select(country, psnu, fy,  keypop, indicator, cumulative) %>% 
  mutate(cumulative = coalesce(cumulative, 0),
         cumulative = if_else(cumulative>0,1,0)) %>%
  group_by(country, psnu, fy,  keypop, indicator) %>%
  summarise(cumulative = max(cumulative),  .groups = 'drop') %>%
  pivot_wider(names_from = indicator, values_from = cumulative) %>%
  mutate(KP_PREV  = coalesce(KP_PREV , 0),
         TX_CURR  = coalesce(TX_CURR , 0)) %>%
  print(n=20)

write_csv(gpc_coverage, "Dataout/FY21_GPC_country_PEPFAR_coverage.csv")

