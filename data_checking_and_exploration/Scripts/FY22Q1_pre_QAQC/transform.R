

table(mer_df$indicator)

indicator_list <- c("KP_PREV",  "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_NEW", "PrEP_CT", "PrEP_CURR", "HTS_TST_POS",
                    "TX_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", "TX_NET_NEW")

df <- mer_df %>%  filter(
                    str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE) %>%
                  mutate(indicator = recode(indicator, "TX_PVLS" = paste0(indicator,"_",numeratordenom)),
                         funding_agency = recode(funding_agency, "HHS/CDC" = "CDC")) %>%
                  filter(indicator %in% indicator_list) %>% mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>% glimpse()


check <- df %>% filter(disaggregate != "KeyPop/Status") %>%
  mutate(
        cumulative = coalesce(cumulative, 0),
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

qcheck <- df %>% filter(fiscal_year >= 2022, #cumulative and targets
                       str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE,
                       disaggregate != "KeyPop/Status") %>%
  mutate(fy = fiscal_year,
         partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),
         keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         keypop = recode(keypop, "People in prisons" = "Prisoners")) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, keypop, fy, qtr1, qtr2, qtr3, qtr4) %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>%
  pivot_longer(qtr1:qtr4, names_to = "qtr", values_to = "results" ) %>%
  mutate(results = coalesce(results, 0),
         qtr = str_replace(qtr, "qtr","Q"),
         fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr))

rm(df)

modality <- mer_df %>% filter(str_detect(standardizeddisaggregate, "KeyPop|Total") == FALSE,
                             fiscal_year >= 2022,
                          str_detect(indicator, "HTS_TST") == TRUE) %>%
  pivot_longer(qtr1:qtr4, names_to = "qtr", values_to = "results" ) %>%
  mutate(results = coalesce(results, 0),
         qtr = str_replace(qtr, "qtr","Q"),
         fy = fiscal_year,
         fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr),
         age = trendscoarse) %>%
  select(operatingunit, country, snu1, psnu, prime_partner_name, mech_code, mech_name, indicator, funding_agency, numeratordenom, disaggregate, modality, fy, fyq, results, age, ageasentered, sex) %>%
  glimpse()

mmd <- mer_df %>% filter(fiscal_year >= 2022,
                         str_detect(indicator,"TX_CURR(?!_Lag)"),
                     str_detect(disaggregate, "ARVDispense|Total")) %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         partner = prime_partner_name,
         fy = fiscal_year,
         arv = str_extract(otherdisaggregate, "(?<=-\\s).+")) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disaggregate, arv, otherdisaggregate, fy, targets, cumulative) %>%
  glimpse()



# obtain latest Q and FYQ -------------------------------------------------


current_fyq_df <- modality %>% filter(!is.na(results), results>0) %>% select(fyq)
current_fyq <- c(max(current_fyq_df$fyq))

current_fy <- check %>% filter(!is.na(cumulative), cumulative>0) %>% select(fy)
current_fy <- c(max(current_fy$fy))

current <- current_fyq_df %>% 
  filter(fyq == current_fyq) %>% group_by_all() %>% summarise(fyq=max(fyq)) %>% 
  mutate(fyy = as.double(str_extract(fyq, "(?<=FY)[1-9]{2}")),
         fy = as.double(str_extract(fyq, "(?<=FY)[1-9]{2}"))+2000,
         q = as.double(str_extract(fyq, "[1-4]$")),
         vlc_d_q = if_else(q > 2, q-2, q+2),
    vlc_d_fy = if_else(str_detect(fyq, "Q1$|Q2$"), fy-1, fy),
   vlc_d_fyy = if_else(str_detect(fyq, "Q1$|Q2$"), fyy-1, fyy),
   vlc_d_fyq = str_c("FY", as.character(vlc_d_fyy), " Q", as.character(vlc_d_q)),
   vlc_d_q = str_c("qtr", as.character(vlc_d_q)),
      ) %>% 
  select(-q, -fyq) %>% print()

current$vlc_d_q
#eventually turn this into var name to use in coalesce of current lagged FY BELOW

# Munge VLC denominator from 2q previous for fiscal year ------------------
young <- c("15-19", "20-24", "24-29")
older <- c("30-34", "35-39", "40-44")

glimpse(df)

lagged_curr_fy <- df %>% 
  filter(indicator=="TX_CURR",
         fiscal_year == current$vlc_d_fy) %>%
  mutate(cumulative = coalesce(qtr3 ,0), ############improve code here by using current$vlc_d_q as parameter to specify the qtr name to keep, could pivot and do it that way
         fiscal_year = current$fy,
         indicator = "TX_CURR_Lag2") %>% 
  select(-contains("qtr"), -targets) %>%
  glimpse()         

lagged_prev_fy <- df %>% 
  filter(indicator=="TX_CURR",
         fiscal_year < current_fy) %>% 
  mutate(cumulative = coalesce(qtr2,0),
         indicator = "TX_CURR_Lag2") %>% select(-contains("qtr"), -targets)

         
vlc_d <- bind_rows(lagged_prev_fy, lagged_curr_fy) %>% mutate(
         fy = fiscal_year,
         partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop|Age"),
         disagg = recode(disagg, "KeyPop" = "KP",
                         "Age" = "Age/Sex"),
         keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         keypop = recode(keypop, "People in prisons" = "Prisoners")) %>% 
  mutate(agesex = if_else(sex=="Female" & ageasentered %in% young, "AGYW", 
                          if_else(sex=="Male" & ageasentered %in% young, "ABYM", 
                                  case_when(sex=="Female" & ageasentered %in% older ~ "adult women")))) %>% 
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, keypop, agesex, fy, cumulative) %>%
  filter(!(is.na(agesex) & disagg == "Age/Sex")) %>%
  group_by(across(-c(cumulative))) %>% summarise(cumulative = sum(cumulative), .groups = "drop") %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator)
  




# delete full MSD ---------------------------------------------------------


rm(mer_df)

table(check$funding_agency)

