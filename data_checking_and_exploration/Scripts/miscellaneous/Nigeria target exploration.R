# targets_and_msd  <- read_csv("Data Out/targets_and_msd.csv")

indicator_order <-  c("KP_PREV", "HTS_TST", "TX_NEW", "TX_CURR")

nigeria_targets <- mer_df |> filter(country == "Nigeria",
                                    fiscal_year >= 2022,
                                             # standardized_disaggregate != "KeyPop/Status",
                                    indicator %in% indicator_order) |> 
                             mutate(keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
                                             keypop = recode(keypop, "People in prisons" = "Prisoners"),
                                    msd_version = str_extract(file, "(?<=Data\\/).+(?=\\.rds)"))  |> 
                          filter(!is.na(keypop)) |> 
                            rename(fy = fiscal_year,
                                   key_pops = keypop,
                                   country_name = country,
                                   standardized_disaggregate = standardizeddisaggregate,
                                   other_disaggregate = otherdisaggregate) |> glimpse()

nigeria_targets_long <- nigeria_targets |>  
  mutate(indicator = factor(indicator, levels = indicator_order),
         indicator = if_else(standardized_disaggregate=="KeyPop/Status", str_c(indicator, 
                                                                               str_extract(other_disaggregate, "Known\\sat\\sEntry|(?<=\\,\\s).+"),
                                                                               sep="-"), indicator)) |> 
  group_by(psnu, fy, indicator, key_pops, msd_version) |> 
  summarise(targets = sum(targets, na.rm=TRUE),
            cumulative = sum(cumulative, na.rm=TRUE),
  ) |>   
    arrange(key_pops, psnu, indicator, fy) |> print()
  
# targets_and_msd |> filter(country_name == "Nigeria",
#                           indicator %in% c("KP_PREV", "HTS_TST", "TX_CURR"),
#                           !is.na(key_pops),
#                           fiscal_year==2023) |> glimpse()
# count(agency, psnu)
#   
# nigeria_targets_wide <- nigeria_targets_long |> 
#   pivot_wider(names_from = c(fy), 
#               values_from = c(targets, cumulative), 
#               names_glue = "{.value}_{fy}"
#               ) |> 
#   relocate(targets_2022, targets_2023, .before = targets_2024) |> 
#   select(-cumulative_2024)|> 
#   print()

nigeria_results_targets <- nigeria_targets_long |> 
  pivot_longer(cols = targets:cumulative, names_to = c("results_or_targets")) |> 
  filter(!is.na(value)) 

nigeria_kp_prev_by_pop <- nigeria_results_targets |> 
  select(psnu,indicator,key_pops,msd_version,results_or_targets,fy,value) |>  glimpse()
# |> filter(indicator=="KP_PREV")
write_csv(nigeria_kp_prev_by_pop, "./Dataout/nigeria_kp_prev_by_pop.csv")


kpse_states_2023 <- c("Adamawa", "Bauchi", "Bayelsa", "Borno", "Delta", "Ebonyi", "Ekiti", "Jigawa", "Katsina", "Kebbi", "Kogi", "Kwara", "Niger", "Ogun", "Ondo", "Osun", "Plateau", "Sokoto", "Yobe", "Zamfara")

  
state_by_agency <- nigeria_targets |> filter(fiscal_year==2023, 
                          values >0,
                          indicator=="KP_PREV"
                          ) |> count(psnu, funding_agency)

usaid_states <- state_by_agency |> filter(funding_agency == "USAID") |> select(psnu) |> knitr::combine_words(psnu, before = "`", after = "'")
cdc_states <- state_by_agency |> filter(funding_agency == "CDC") |> select(psnu) |> knitr::combine_words(psnu, before = "`", after = "'")
# rm(targets_and_msd)
# gc()
nigeria_targets |> count(indicator, standardized_disaggregate)
