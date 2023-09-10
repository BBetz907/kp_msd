
semi_pre <- df %>% filter(str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE,
                          indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "HTS_SELF", "KP_PREV")) %>%
  mutate(fy = fiscal_year,
         partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),
         keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         keypop = recode(keypop, "People in prisons" = "Prisoners"),
         kp_prev_status = case_when(disaggregate == "KeyPop/Status" ~ 
                                      str_extract(categoryoptioncomboname, 
                                                  "(?<=\\,\\s).+$")),
         country = recode(country,
                          "Democratic Republic of the Congo" = "DRC",
                          "Papua New Guinea" = "PNG")) %>% 
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, otherdisaggregate, keypop, kp_prev_status, fy, qtr1, qtr2, qtr3, qtr4) %>%
  pivot_longer(qtr1:qtr4, names_to = "qtr", values_to = "results" ) %>% 
  mutate(results = coalesce(results, 0),
         qtr = str_replace(qtr, "qtr","Q"),
         qtr = recode(qtr, "Q1" = "Q2",
                      "Q3" = "Q4"),
         fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr)) %>% glimpse()


#stop here to investigate completeness of KP_PREV
semi_pre %>% filter(indicator == "KP_PREV", funding_agency == "USAID", disagg=="KP",
                    !(disaggregate=="KeyPop/Status" & (fy==2019 | fyq =="FY20 Q2"))) %>% 
  group_by(fyq, funding_agency, disaggregate, disagg) %>% summarise(results=sum(results)) %>% 
  pivot_wider(names_from = disaggregate, values_from = results) %>% 
  janitor::clean_names() %>% 
  mutate(
    kp_prev_completeness = scales::percent(key_pop_status/key_pop, accuracy =1),
    kp_prev_missing = key_pop - key_pop_status
  ) #globally it appears that it doesn't get fully reported until FY20 Q4

#continue but filter out FYQs with incomplete Key Pop Status disaggs
semi <- semi_pre %>%  filter(!(indicator == "KP_PREV" & kp_prev_status %in% c("Declined Testing Or Testing Referral", "Known Positives") ), #focus is testing so these are excluded
                             !(disaggregate=="KeyPop/Status" & (fy==2019 | fyq =="FY20 Q2")) #disaggs were largely incomplete for KP_PREV before FY20Q4 (<80% as compared to >95 thereafet)
) %>%
  mutate(indicator=if_else(is.na(kp_prev_status), indicator, "KP_PREV_TESTED")) %>% 
  select(-kp_prev_status, -otherdisaggregate) 

rm(semi_pre) #delete precursor data set




# indicator_list <- append(indicator_list,'KP_PREV_TESTED') #add newly coded variable to ordered indicator list
# indicator_list 

usaid_semi_kp_tst_pre <- semi %>% filter(funding_agency=="USAID", disagg=="KP", 
                                         indicator %in% c("KP_PREV_TESTED", "KP_PREV", "HTS_TST", "HTS_SELF"),
                                         !results == 0) %>% 
  group_by(indicator, fyq, funding_agency, disagg) %>% summarise(results=sum(results), .groups = "drop") %>% 
  pivot_wider(names_from = indicator, values_from = results) %>%
  mutate(TST = HTS_SELF+HTS_TST,
         HTS_SELF_p = scales::percent(HTS_SELF/TST, accuracy = 1),
         TST_p = scales::percent(HTS_TST/TST, accuracy = 1),
         TST_prev_r = KP_PREV/TST,
         HTS_TST_prev_tst_r = KP_PREV_TESTED/HTS_TST) %>% print()


# create long format for geom_col -----------------------------------------
usaid_semi_kp_tst_self <- usaid_semi_tst_pre %>% 
  select(fyq, funding_agency, disagg, HTS_TST, HTS_SELF) %>% pivot_longer(cols = c("HTS_TST", "HTS_SELF"), names_to = "indicator", values_to = "testing") %>%
  glimpse()


# remove testing indicators and merge by fyq to combine the wide ( --------


usaid_semi_kp_tst <- usaid_semi_kp_tst_pre %>% right_join(usaid_semi_kp_tst_self, by = c("fyq", "funding_agency", "disagg"),
                                                       multiple = "any") %>% 
  mutate(
    # kp_prev_test = case_when(indicator == "KP_PREV_TESTED" ~ results),
    #        kp_prev = case_when(indicator == "KP_PREV" ~ results),
    #        testing = case_when(str_detect(indicator,"HTS") ~ results),
    #        self_test = case_when(indicator == "HTS_SELF" ~ results),
    #        summarise(self_test_percent = scales::percent(self_test/testing, accuracy=1))
    indicator  = factor(indicator, levels = rev((indicator_list)))) %>% #convert the indicator factor to sort
  # group_by(fyq) %>% summarise(self_test_percent = scales::percent(self_test/testing, accuracy=1)) %>% ungroup() %>%
  print(n=24)




# save basic chart of stacked HIVST and HTS_TST -------
simple_usaid_semi_kp_tst <- usaid_semi_kp_tst %>% ggplot(mapping=aes(x=fyq, )) +
  geom_col(aes(y=testing, fill=indicator), alpha=1, na.rm = TRUE, 
           width = 0.8,
           position = position_stack()) +
  annotate("label",x= 8.5, y= 5e5, fill = "#BFDDFF",
           label="HTS_TST", label.size = 0, size = 5, label.padding = unit(1, "lines")) +
  annotate("label", x= 8.5, y= 4.2e4, fill = "#C43D4D", color = "white",
           label="HTS_SELF", label.size = 0, size = 5, label.padding = unit(1, "lines")) +
  scale_fill_manual(values=c("#BFDDFF", "#C43D4D")) +
  glitr::si_style_nolines() +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3),
                     breaks = c(200000, 400000, 600000, 800000, 1000000),
                     limits = c(0, 1200000),
                     expand = c(0,0) #removes space between axes and bars
  ) 

#add labels to basic chart and export
simple_usaid_semi_kp_tst + 
  labs(title="<span style = 'color:#C43D4D;'>HIVST growth</span> contributes to increased KP prevention & testing",
       subtitle = glue::glue("Semiannual trends showing {usaid_semi_kp_tst$disagg} HTS_SELF and HTS_TST, {usaid_semi_kp_tst$funding_agency}"),
       x = "",
       caption = glue::glue("Data sources: {file_name[1]}
                            {file_name2}")) +   
  theme(plot.title = element_markdown(hjust = 0, size = 24),
                                                      plot.subtitle = element_text(hjust = 0, size = 15),
                                                      panel.grid.major.y = element_line(),
                                                      axis.title.y  = element_blank(),
                                                      legend.position = "none", 
                                                      legend.justification = "left",
                                                      legend.title = element_blank(),
                                                      plot.caption = element_text(size=12),
                                                      axis.text = element_text(size=14))

ggsave(filename = "Images/self-testing-growth-usaid_3.png", plot=last_plot(), width = 12, height = 9)





#add KP_PREV lines 
#then add labels to basic chart and export

simple_usaid_semi_kp_tst +
  geom_line(aes(y=KP_PREV_TESTED, group=indicator), linewidth = 0.8, 
            color = grey70k, 
            na.rm = TRUE) +
  annotate("text", x="FY22 Q4", y=7.9e5, label = "KP_PREV Newly Tested", color=grey70k) + 
  # geom_text(aes(y=KP_PREV_TESTED-40000, 
  #               label = scales::label_number(scale=1e-3, suffix='K', accuracy = 1)(KP_PREV_TESTED)),
  #           color = old_rose) +
  geom_line(aes(y=KP_PREV, group=indicator), linewidth = 0.5,
            color = grey40k, na.rm = TRUE) + 
  annotate("text", x="FY19 Q4", y=6.85e5, label = "KP_PREV\nTotal", color=grey60k) + 
  annotate("text", x=7, y=1.09e6, label = ">1 testing option per KP reached with prevention services", size = 5, color = "#C43D4D", fontface="bold") +
  glitr::si_style_nolines() +
  # <span style = 'color:#1e87a5;'>SNS</span>
  labs(title="<span style = 'color:#C43D4D;'>HIVST growth</span> contributes to increased KP prevention & testing",
       subtitle = glue::glue("Semiannual trends showing {usaid_semi_kp_tst$disagg} HTS_SELF and HTS_TST, compared to KP_PREV, {usaid_semi_kp_tst$funding_agency}"),
       # subtitle = str_wrap("The sum of HTS_TST and HTS_SELF exceeds the count of newly tested KP_PREV,  \nsuggesting some KP may receive both.", 60),
       x = "",
       caption = glue::glue("Data sources: {file_name[1]}
                            {file_name2}")) +
  theme(plot.title = element_markdown(hjust = 0, size = 24),
        plot.subtitle = element_text(hjust = 0, size = 15),
        panel.grid.major.y = element_line(),
        axis.title.y  = element_blank(),
        legend.position = "none", 
        legend.justification = "left",
        legend.title = element_blank(),
        plot.caption = element_text(size=12),
        axis.text = element_text(size=14))


ggsave(filename = "Images/self-testing-growth-usaid.png", plot=last_plot(), width = 12, height = 9)




# simpLIFY visual to show only HTS_SELF -------
usaid_semi_kp_tst %>% filter(indicator=="HTS_SELF") %>%
  ggplot(mapping=aes(x=fyq, )) +
  geom_col(aes(y=testing, fill=indicator), alpha=1, na.rm = TRUE, 
           width = 0.8,
           position = position_stack()) +
  geom_text(aes(label=scales::comma(testing, accuracy = 1, suffix = " K", scale = 1e-3), y = testing), 
            vjust = -1, color = "#C43D4D", size=6) + 
  annotate("label", x= 8.5, y= 4.2e4, fill = "#C43D4D",
           label="HTS_SELF", label.size = 0, size = 6, label.padding = unit(1, "lines"), 
           color= "white") +
  # geom_text(aes(y=HTS_SELF, label = HTS_SELF_p), 
  #           vjust = -1.01,
  #           hjust = 0.47,
  #           color="#d56d4b",
  #           fontface = "bold",
  #           na.rm=TRUE
  # ) +
  scale_fill_manual(values=c("#C43D4D")) +
  glitr::si_style_nolines() +
  theme(plot.title = element_markdown(hjust = 0, size = 24),
        plot.subtitle = element_text(hjust = 0, size = 15),
        panel.grid = element_blank(),
        axis.title.y  = element_blank(),
        legend.position = "none", 
        legend.justification = "left",
        legend.title = element_blank(),
        axis.text.y = element_text(color="white"),
        plot.caption = element_text(size=12),
        axis.text.x = element_text(size=14)
  ) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3),
                     breaks = c(200000, 400000, 600000, 800000, 1000000),
                     limits = c(0, 1200000),
                     expand = c(0,0) #removes space between axes and bars
  ) + 
  labs(title="<span style = 'color:#C43D4D;'>HIVST growth among Key Populations</span>",
       subtitle = glue::glue("Semiannual trends, {usaid_semi_kp_tst$funding_agency}"),
       x = "",
       caption = glue::glue("Data sources: {file_name[1]}
                            {file_name2}")) 

ggsave(filename = "Images/self-testing-growth-usaid_2.png", plot=last_plot(), width = 12, height = 9)












###################################################
###################################################






usaid_semi_kp_tst %>% count(funding_agency)

# explore by country ------------------------------------------------------

semi_ratios <- semi %>% filter(disagg=="KP", fy == 2023, funding_agency=="USAID",
                indicator %in% c("HTS_TST", "HTS_SELF")) %>%
  #add mech information ref table
  group_by(operatingunit, country, indicator, keypop) %>%
  summarise(results = sum(results), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = results) %>%
  clean_names() %>%
  mutate(self_ratio = replace_na(round(hts_self/hts_tst, digits = 2),0)) %>% 
  arrange(desc(self_ratio)) %>% 
  group_by(keypop) %>% 
  mutate(self_ratio_kp = round(sum(hts_self, na.rm = TRUE)/sum(hts_tst, na.rm = TRUE), digits = 2),
         self_ratio_kp_75 = quantile(self_ratio, probs = 0.75),
         self_ratio_kp_med = median(self_ratio)) %>%
  ungroup() %>% print()

write_csv(semi_ratios, "Dataout/self_test_leaders.csv")


# semi_ratios %>% count(country)
semi_ratios %>% count(self_ratio_kp_med, keypop)
semi_ratios %>% count(self_ratio_kp, keypop)

self_innovators <- semi_ratios %>% filter(hts_self + hts_tst > 10 &
        # self_ratio > self_ratio_kp_med & keypop %in% c("MSM", "FSW", "TG") | 
                         self_ratio > self_ratio_kp_75 | self_ratio == "I") %>% 
  select(-contains("hts"), -contains("kp")) %>%
  pivot_wider(names_from = keypop, values_from = self_ratio ) %>% 
  # across(hts_tst_FY22:positivity_FY23, ~replace_na(.x, 0) 
  arrange(operatingunit) 

self_innovators_totals <- semi_ratios %>% filter(hts_self + hts_tst > 10 &
                                            # self_ratio > self_ratio_kp_med & keypop %in% c("MSM", "FSW", "TG") | 
                                            self_ratio > self_ratio_kp_75 ) %>% 
  select(-contains("rat")) %>% group_by(country) %>% summarise(hts_self = sum(hts_self))


self_innovators %>% inner_join(self_innovators_totals) %>%
  filter(!country %in% c("Uganda", "Kenya")) %>%
  arrange(desc(hts_self)) %>% select(-operatingunit, hts_self) %>%
  mutate(across(2:5, ~round(.x, digits = 0)),
         across(6:7, ~round(.x, digits = 1))) %>%
  gt() %>% 
  sub_missing(
    columns = 2:7,
    missing_text = ""
  ) 
self_innovators_totals

#iterate using PURRR
self_innovators %>% drop_na(Prisoners) %>% select(operatingunit, country, Prisoners) %>% arrange(desc(Prisoners))
