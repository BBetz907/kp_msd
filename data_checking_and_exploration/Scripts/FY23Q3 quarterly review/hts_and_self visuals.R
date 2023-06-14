#self_test by kp

semi %>%
  mutate(fy = str_extract(fyq, "^.{4}")) %>% 
  filter(funding_agency=="USAID", disagg=="KP", 
         indicator %in% c("HTS_TST_POS", "HTS_TST", "HTS_SELF"),
         !results == 0,
         fy == "FY23") %>% 
  group_by(keypop, disagg, indicator) %>% 
  summarise(results=sum(results), .groups = "drop") %>% 
  pivot_wider(names_from = c("indicator", "disagg"), values_from = results, names_glue = "{disagg}_{indicator}") %>%
  mutate(across(KP_HTS_SELF:KP_HTS_TST_POS, ~replace_na(.x, 0)),
         KP_TST = KP_HTS_SELF+KP_HTS_TST,
         KP_HTS_SELF_p = round(KP_HTS_SELF/KP_TST, digits = 2),
         KP_TST_p = round(KP_HTS_TST/KP_TST, digits = 2),
         # country = fct_reorder(country, desc(KP_HTS_SELF_p)),
         # ou2 = case_when(str_detect(operatingunit, "Region") ~ operatingunit)
  ) %>% 
  arrange((KP_HTS_SELF )) %>% print()


self_test <- semi %>%
  mutate(fy = str_extract(fyq, "^.{4}")) %>% 
  filter(funding_agency=="USAID", disagg=="KP", 
                                         indicator %in% c("HTS_TST_POS", "HTS_TST", "HTS_SELF"),
                                         !results == 0,
                  fy == "FY23") %>% 
  group_by(indicator, fy, funding_agency, country, operatingunit, mech_code, disagg) %>% 
  summarise(results=sum(results), .groups = "drop") %>% 
  pivot_wider(names_from = c("indicator", "disagg"), values_from = results, names_glue = "{disagg}_{indicator}") %>% 
  mutate(across(KP_HTS_SELF:KP_HTS_TST_POS, ~replace_na(.x, 0)),
         KP_TST = KP_HTS_SELF+KP_HTS_TST,
         KP_HTS_SELF_p = round(KP_HTS_SELF/KP_TST, digits = 2),
         KP_TST_p = round(KP_HTS_TST/KP_TST, digits = 2),
         country = fct_reorder(country, desc(KP_HTS_SELF_p)),
         ou2 = case_when(str_detect(operatingunit, "Region") ~ operatingunit)
         ) %>% 
  arrange((KP_HTS_SELF )) %>% print()


self_test %>% ggplot(aes(x=HTS_SELF_p)) + geom_boxplot() + geom_jitter(aes(y=1, size=TST, color=ou2))
#25% of of country/mechanisms are doing 45:55 ratio of self_testing
quantile(self_test$HTS_SELF_p, prob=0.75)
summary(self_test$HTS_SELF_p)
self_test


#sns


#summarize to global level, by indicator, FY, modality, and testing_focus 
usaid_modalities_kpfocus_by_country <- usaid_modalities_kpfocus %>%    #includes all modalities for summary
  filter(fy == "FY23", indicator != "HTS_TST_NEG") %>% 
  group_by(fy, indicator, modality_group, age, funding_agency, testing_focus, country, operatingunit, mech_code) %>% 
  summarise(results = sum(results), .groups = "drop") %>% glimpse()


#create totals, excluding modality then merge back
sns <- usaid_modalities_kpfocus_by_country %>%
  group_by(fy, indicator, testing_focus, country, mech_code) %>% 
  summarise(totals = sum(results), .groups = "drop") %>% 
  right_join(usaid_modalities_kpfocus_by_country, by=c("fy", "indicator", "testing_focus", "country", "mech_code"), 
             # multiple = "any"
  ) %>% 
  filter(modality_group=="SNS") %>% 
  relocate(totals, .after = results) %>%
  mutate(proportion = round(results/totals,2)) %>% 
  pivot_wider(names_from = c("indicator", "modality_group"), values_from = results:proportion, names_glue = "{modality_group}_{indicator}_{.value}") %>%
  clean_names() %>% 
  glimpse()

sns_self <- sns %>% right_join(self_test, by = c("mech_code", "country", "fy", "funding_agency", "operatingunit")) %>% 
  select(-age, -ou2) %>% glimpse()

sns_self %>% arrange(desc(sns_hts_tst_pos_proportion)) %>% gt()
glimpse(sns_self)
summary(sns_self$sns_hts_tst_pos_proportion) 

trenders_sns_self <- sns_self %>% filter(KP_HTS_SELF_p >= quantile(KP_HTS_SELF_p, probs = 0.75, na.rm	 = TRUE) | 
                      sns_hts_tst_pos_proportion >= quantile(sns_hts_tst_pos_proportion, probs = 0.75, na.rm	 = TRUE)) %>% glimpse()

trenders_sns_self %>% select(-7, -9:-11, -15, -16, -17) %>% arrange(desc(sns_hts_tst_pos_proportion)) %>%
  gt()

trenders_sns_self %>%  ggplot(aes(x=sns_hts_tst_pos_proportion, y = KP_HTS_SELF_p)) + 
  geom_text(aes(label=country)) + si_style_xyline()
