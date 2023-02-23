library(tidyverse)
library(janitor)


df_target_change <- mer_df %>%  filter(fiscal_year >= 2020, #cumulative and targets
                                 str_detect(standardizeddisaggregate, "KeyPop") == TRUE,
                                 indicator %in% c("HTS_TST_POS", "TX_NEW")) %>% 
  mutate(funding_agency = recode(funding_agency, "HHS/CDC" = "CDC")) %>% 
  filter(funding_agency=="USAID") %>%
  group_by(country, indicator, fiscal_year) %>% 
  mutate(targets = if_else(is.na(targets), 0, targets),
         cumulative = if_else(is.na(cumulative), 0, cumulative)) %>%
  summarise(targets=sum(targets), results=sum(cumulative)) %>% glimpse()

  targets <- df_target_change %>% 
    select(-results) %>% 
    pivot_wider(names_from = fiscal_year, values_from = targets) %>% clean_names() %>% 
  mutate(x2020 = if_else(is.na(x2020), 0, x2020),
         x2021 = if_else(is.na(x2021), 0, x2021),
         x2022 = if_else(is.na(x2022), 0, x2022),
         x2023 = if_else(is.na(x2023), 0, x2023),
         change = x2022-x2020) %>% 
  filter(change < 0) %>%
    arrange()
glimpse()

print(targets, n=80)
countries <- as.list(targets$country)

results_targets <-  df_target_change %>%
  filter(fiscal_year == "2020",
         country %in% countries) %>% 
  mutate(ach = results/targets) %>%
  glimpse()

print(results_targets, n=58)


#14 of 25 countries with declining targets from 2020 to 2022 had low performance. Others had programmatic shifts
#or changes is parnters and geographies, ceding programs to CDC, etc. Additionally KPIF is gone.
#Global high performance on casefinding and linkage does not mean that all countries were achieving.
#countries with high achievement increased targets, countries with low achievement decreased targets.
#Additionally, some KPIF targets were present for FY20
#Finally, we should remember that positive HIV-tests are negative events. KP programs
#emphasize prevention and not just casefinding. Additionally, they rely on significant community
#work and continuity. It takes investment to expand to other geographies to find positives, especially.
#As the program works, you would expect fewer cases of HIV. Positives are found by increasing s
#screening and targeted testing, and other innovations, and geographic expansion (i.e. applying
#a community model in more places, or increasing staff). To ask for more ambitious global KP test
#and treat targets ignores the realities of KP community programming, diversity of achievement
#by country, and a flat line funding environment. So why would you ask for
#"more ambition"? without advocating for new funds? Overemphasis on high Target setting versus
#strengthening progrrams, community, environment, treatment, and prevention, is not a key PEPFAR
#priority any longer. This mindset is largely a relic of the Birx era. This relentless emphasis
#on case finding led to index testing expansion and even human rights abuses, faced 
#disprorportionaately by Key Populations and AGYW. This resulted in massive index test stoppage
#and introduction of community-led monitoring to prevent future abuses. You should familiarize 
#yourself with these realities, understand outcomes of past mindsets, and explore implications for 
#data interpretation and use. Members of the KP team and experienced leaders like Maria can be very
#valuable if you wish to learn, and are happy to contribute to the talking points for future presentations. 

#KP programming does require expansion, to new geographies, clients of sex workers, etc. But 
#this will require increased investment and targeting of funding, not just "past overachievement
#showing that it is possible"

