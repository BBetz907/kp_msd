# Testing by modality -----------------------------------------------------
modality_annual_usaid <- modality %>%
        rename(partner = prime_partner_name) %>%
        mutate(fy = str_c("FY", str_extract(as.character(fy), ".{2}$")),
               country = recode(country,
                            "Democratic Republic of the Congo" = "DRC",
                            "Papua New Guinea" = "PNG"),
               modality_group = case_when(
                 str_detect(modality, "Index") ~ "Index",
                 str_detect(modality, "SNS") ~ "SNS", 
                 # str_detect(modality, "TB") ~ "TB", 
                 # str_detect(modality, "PITC") ~ "PITC", 
                 # modality == "VCT" ~ "VCT",
                 TRUE ~ "Other"),
               # modality_group = if_else(modality_group == "Other" & str_detect(modality, "Mod"), "Community", modality_group)
               ) %>%     
    filter(funding_agency == "USAID",
           str_detect(indicator, "HTS"),
           age == "15+",
           # mech_code != "17422"
           ) %>% 
    # semi_join(sns_mechs) %>% #filter by mechanisms who report SNS
    group_by(across(operatingunit:fy), modality_group) %>% 
    summarize(results = sum(results), .groups = "drop") %>%
    glimpse()

modality_annual_usaid

#explore modality
# modality_annual_usaid %>% gt()
modality_annual_usaid %>% count(modality) 
modality_annual_usaid %>% count(modality_group)  


modality_group_list <- c("SNS", "Index", "VCT", "Community", "TB")
testing_focus_list <- c("KP-focused (>75% KP)", 
                        "Integrated (25-75% KP)", 
                        "Integrated (<25% KP)") #define order  

#read in mech info, identifying whether mechanisms primarily serve KP, many KP, or onlya few
  #then merge
usaid_modalities_kpfocus <- read_csv(file = "Data/kp_focused_testing_fy21-23_by_mechcode_country.csv", 
                    col_types = cols(.default = "c")) %>% 
    clean_names() %>% select(-mechanism_id_country) %>% 
    rename("mech_code" = "mechanism_id",
           "testing_focus" = "kp_vs_integrated_testing") %>% 
    mutate(testing_focus = str_remove(testing_focus, "[T,t]esting\\s")) %>%
    right_join(modality_annual_usaid, by=c("mech_code", "country")) %>%
    mutate(
           modality_group = factor(modality_group, levels = rev(modality_group_list)),
           testing_focus = if_else(is.na(testing_focus), "Integrated (<25% KP)", testing_focus), 
                #recode missing as integrated, <25%KP
           testing_focus = factor(testing_focus, levels = (testing_focus_list)),
                #recode as factor for sorting
           age = "15+") 

#summarize to global level, by indicator, FY, modality, and testing_focus
usaid_modalities_kpfocus_global <- usaid_modalities_kpfocus %>%   
  # filter(!country=="Zambia") %>% #TEST to see what happens if Zambia is removed
  group_by(fy, indicator, modality_group, age, funding_agency, testing_focus) %>% 
  summarise(results = sum(results), .groups = "drop") %>% glimpse()

#create totals, excluding modality then merge back
usaid_modalities_kpfocus_global_with_totals <- usaid_modalities_kpfocus_global %>%
  group_by(fy, indicator, testing_focus) %>% 
  summarise(totals = sum(results), .groups = "drop") %>% 
  right_join(usaid_modalities_kpfocus_global, by=c("fy", "indicator", "testing_focus"), 
             # multiple = "any"
             ) %>%
  mutate(modality_proportion = round(results/totals,3),
         modality_percent = if_else(testing_focus == "KP-focused (>75% KP)" |
                                      modality_group == "SNS" & testing_focus != "Integrated (<25% KP)"  | 
                                      modality_group =="Index", 
                                    scales::percent(results/totals, accuracy = 1), "")) %>%
  glimpse()

usaid_modalities_kpfocus_global_with_totals %>% count(modality_group,)



# visual for combining visuals tests -----------------------------------------------

tst_and_pos <- usaid_modalities_kpfocus_global_with_totals %>% filter(indicator=="HTS_TST_POS" | indicator=="HTS_TST",
                                                                  modality_group != "Other") %>%
  mutate(modality_percent = 
           # if_else(testing_focus == "KP-focused (>75% KP)" & modality_group %in% c("SNS", "Index") |
                   # modality_group == "SNS" & testing_focus != "Integrated (<25% KP)", 
                 scales::percent(results/totals, accuracy = 1), 
         # "")
)


tst_and_pos %>% filter(testing_focus!= "Integrated (<25% KP)") %>%
  ggplot(mapping = aes(x=fy, y=modality_proportion, fill = modality_group)) +
  geom_col() + guides(fill = guide_legend(
    reverse = TRUE
  )) +   # reverses color fill legend order, for horizontal view
  scale_fill_manual(values=c(grey40k, "#1e87a5")) +
  geom_text(aes(label=modality_percent),
            position=position_stack(vjust=0.5),
            fontface = "bold", color=grey10k) +
  facet_grid(cols = vars(indicator), rows=vars(testing_focus), 
             # switch = "y", #move right to left
             labeller = label_wrap_gen(width = 12, multi_line = TRUE)
             ) +
  si_style_ygrid() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(size =14, hjust = 0.5, vjust=1.1, face="bold",colour = grey90k), # facet label position
        strip.text.y.right = element_text(size =14, angle = 0, face="bold",colour = grey90k),
        # axis.title.y = element_text(margin = 1),
        legend.direction = "horizontal",
        legend.position = c(x=.25, y=.425),
        legend.justification = "center",
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", color = "white"),
        plot.title = element_markdown(hjust = 0, size =24),
        plot.subtitle = element_text(hjust = 0))+ 
  scale_y_continuous(labels = label_number(suffix = "%", scale = 1e2),
                     # breaks = c(200000, 400000, 600000, 800000, 1000000),
                     # limits = c(0,0.38),
                     expand = c(0,0)) + #removes space between axes and bars 
  labs(title = glue::glue("<span style = 'color:#1e87a5;'>SNS</span> are an important alternative to <span style = 'color:#a7a9ac;'>Index</span> testing<br>for Key Populations, despite lower positivity"),
       subtitle = glue::glue("\nProportion of {tst_and_pos$indicator[1]} and {tst_and_pos$indicator[2]} from selected modalities, {tst$funding_agency}, ages {tst$age}\n"),
       y = glue::glue("Proportion of tests"),
       x = "",
       caption = glue::glue("Data source: {file_name}")
  )

ggsave(filename = "Images/testing_and_pos_kp_focus_modality_trends.png", plot = last_plot(), width = 8, height = 6.74)

#Zambia mechanism transition and small declines in positivity from SNS are responsible for declining proportion of SNS in FY23


tst_and_pos %>% filter(indicator == "HTS_TST_POS", modality_group != "Index") %>%
  mutate(results = if_else(fy=="FY23", results/2, results/4)) %>%
  ggplot(mapping = aes(x=fy, y=results, fill = modality_group)) +
  geom_col() + guides(fill = guide_legend(
    reverse = TRUE
  )) +   # reverses color fill legend order, for horizontal view
  scale_fill_manual(values=c( denim, "#1e87a5")) +
  geom_text(aes(label=modality_percent),
            position=position_stack(vjust=0.5),
            fontface = "bold", color=grey10k) +
  facet_grid(cols = vars(indicator), rows=vars(testing_focus), 
             # switch = "y", #move right to left
             labeller = label_wrap_gen(width = 12, multi_line = TRUE)
  ) +
  si_style_ygrid() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(hjust = 0.5, vjust=1.1, face="bold",colour = grey90k), # facet label position
        strip.text.y.right = element_text(angle = 0, face="bold",colour = grey90k),
        # axis.title.y = element_text(margin = 1),
        legend.direction = "horizontal",
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", color = "white"),
        plot.title = element_text(hjust = 0.45, size =24),
        plot.subtitle = element_text(hjust = 0.45))+ 
  scale_y_continuous(
                     # labels = label_number(suffix = "%", scale = 1e2),
                     # breaks = c(200000, 400000, 600000, 800000, 1000000),
                     # limits = c(0,0.38),
                     expand = c(0,0)) + #removes space between axes and bars 
  labs(title = glue::glue("SNS and Index testing"),
       subtitle = glue::glue("Proportion of {tst_and_pos$indicator[1]} and {tst_and_pos$indicator[2]} from selected modalities, {tst$funding_agency}, ages {tst$age}"),
       y = glue::glue("Proportion of tests"),
       x = "",
       caption = glue::glue("Data source: {file_name}")
  )

ggsave(filename = "Images/testing_pos_kp_focus_modality_trends.png", plot = last_plot(), width = 4, height = 8.1)


# country level modality analysis -----------------------------------------

#explore countries contributing to declines in SNS pos proportion
#summarize to global level, by indicator, FY, modality, and testing_focus
usaid_modalities_kpfocus_country <- usaid_modalities_kpfocus %>%
  filter(!fy %in% c("FY21")) %>%
  group_by(fy, indicator, modality_group, age, funding_agency, country, testing_focus, mech_code, mech_name) %>% 
  summarise(results = sum(results), .groups = "drop") %>% 
  mutate(results = round(if_else(fy == "FY23", results/2, results/4))) %>% #calculate quarterly average
  pivot_wider(names_from = indicator, values_from = results) %>% clean_names() %>%
  select(-hts_tst_neg) %>%
  mutate(positivity = round(hts_tst_pos/hts_tst, 2)) %>% glimpse()

fy_tst_shifts <-
  usaid_modalities_kpfocus_country %>% pivot_wider(names_from = fy, values_from = hts_tst:positivity) %>%
  mutate(across(hts_tst_FY22:positivity_FY23, ~replace_na(.x, 0)),
         hts_tst_pos_diff = hts_tst_pos_FY23 - hts_tst_pos_FY22,
         positivity_diff = positivity_FY23 - positivity_FY22,
         country = fct_reorder(country, positivity_diff)) %>%
  filter(testing_focus == "KP-focused (>75% KP)", modality_group == "SNS") %>% arrange(positivity_diff) 
  
fy_tst_shifts %>% filter(country=="Zambia") %>% gt() #view results

usaid_modalities_kpfocus_country %>% filter(testing_focus == "KP-focused (>75% KP)", modality_group == "SNS", 
                                            # !country=="Zambia",
                                            !(hts_tst_pos == hts_tst & hts_tst_pos ==1)) %>% 
  group_by(fy) %>%
  summarise(across(hts_tst:hts_tst_pos, list(sum))) %>%
  mutate(positivity = hts_tst_pos_1/hts_tst_1 )
  
  ggplot2::ggplot(aes(x=fy, y=positivity)) + geom_boxplot() + geom_jitter(aes(color=country))

#create totals, excluding modality then merge back
scatter <- usaid_modalities_kpfocus_country %>%
  group_by(fy, indicator, country, mech_code, testing_focus) %>% 
  summarise(totals = sum(results), .groups = "drop") %>%  
  right_join(usaid_modalities_kpfocus_country, by=c("fy", "indicator", "testing_focus", "country", "mech_code"), 
             # multiple = "any"
  ) %>% 
  mutate(proportion = round(results/totals,3),
         percent = scales::percent(results/totals, accuracy = 1)) %>%
  pivot_wider(names_from = modality_group, values_from = c(results, proportion, percent)) %>% 
  clean_names()
  
scattered <- scatter %>% filter(fy %in% c("FY22","FY23")) 

#create list of IMs who did SNS testing
sns_mechs <- scatter %>% drop_na(results_sns) %>% 
  group_by(mech_code) %>% 
  summarise(results_sns=sum(results_sns)) %>% 
  filter(results_sns>0) %>% 
  select(mech_code)


scattered %>% 
  ggplot(aes(x=proportion_sns, y=testing_focus)) + 
  geom_jitter(aes(group=mech_code)) + geom_boxplot(aes(fill=testing_focus, alpha = 0.4)) +
  si_style_xyline() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  labs(title = glue::glue("{scattered$funding_agency} {scattered$indicator} scatter plot comparing SNS proportions, by country, mech type"),
       subtitle = glue::glue("age: {scattered$age}"))

scattered %>% 
  ggplot(aes(x=results_sns, y=testing_focus)) + 
  geom_jitter(aes(group=mech_code)) + geom_boxplot(aes(fill=testing_focus, alpha=0.4)) +
  si_style_xyline() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  labs(title = glue::glue("{scattered$funding_agency} {scattered$indicator} scatter plot comparing sns volumes, by country, mech type"),
       subtitle = glue::glue("age: {scattered$age}"))

high_p_sns <- scattered %>% 
  filter(fy == "FY23") %>%
  filter(proportion_sns > .25) %>% 
  group_by(country) %>% summarize()

high_p_sns



outlier_zambia <- scattered %>%
  filter(country=="Zambia", indicator != "HTS_TST_NEG") %>%
  # pivot_wider(names_from = indicator, values_from = c("results_sns", "totals")) %>%
  # arrange(desc(proportion_sns)) %>%
  select(-contains("index"), -contains("vct"), -contains("na"), -contains("proportion"), -contains("comm"))


write_csv(outlier_zambia, "Dataout/outlier_zambia.csv")

export_sns <- scattered %>%
  filter(fy == "FY23", indicator != "HTS_TST_NEG") %>%
  semi_join(high_p_sns) %>%
  # pivot_wider(names_from = indicator, values_from = c("results_sns", "totals")) %>%
  # arrange(desc(proportion_sns)) %>%
  select(-contains("index"), -contains("vct"), -contains("na"), -contains("proportion"), -contains("comm"))

write_csv(export_sns, "Dataout/sns_leaders.csv")



export_sns2 <- scattered %>%
  filter(fy == "FY23", indicator != "HTS_TST_NEG") %>%
  # pivot_wider(names_from = indicator, values_from = c("results_sns", "totals")) %>%
  # arrange(desc(proportion_sns)) %>%
  select(-contains("index"), -contains("vct"), -contains("na"), -contains("proportion"), -contains("comm"))

write_csv(export_sns2, "Dataout/sns_leaders2.csv")


#SNS led by Regional Programs, TZ, DR, SSudan, Cameroon, Ethiopia, RSA
