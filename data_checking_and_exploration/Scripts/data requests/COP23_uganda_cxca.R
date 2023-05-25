uganda_cxca <- mer_df %>% filter(country == "Uganda", str_detect(indicator, "CXCA"), 
                                 disaggregate== "Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
                                 sex == "Female", 
                                 ageasentered %in% c("25-29", "30-34", "35-39", "40-44", "45-49")
                                 ) %>% 
  mutate(age = recode(ageasentered,
                      "25-29" = "25-34",
                      "30-34" = "25-34",
                      "35-39" = "35-49", 
                      "40-44" = "35-49", 
                      "45-49" = "35-49"),
         # psnuuid = str_to_upper(psnuuid)
         ) %>% 
  select(fiscal_year, snuprioritization, psnu, psnuuid, age, sex, indicator, cumulative) %>% 
  group_by(across(-c(cumulative))) %>% summarise(results = sum(cumulative)) %>%
  pivot_wider(names_from = c("fiscal_year", "indicator"), values_from = results) %>%
glimpse()

library(readxl)
merge_data <- readxl::read_xlsx(path = "Data/CXCA DATA.xlsx") %>% 
  mutate(psnu = str_extract(PSNU, ".+(?=\\s\\[)"),
         psnuuid = str_extract(PSNU, "(?<=\\s\\[).+(?=\\])")) %>% 
  rename(age = Age) %>% select(-PSNU, -Prioritization) %>% print()

cxca_target_vars <- uganda_cxca %>% right_join(merge_data, by = c("age", "psnu", "psnuuid")) %>% 
 relocate(ID, `FY23 TX_CURR`, `2021_CXCA_SCRN`, `-FY22 CXCA_SCRN`, `-FY23 CXCA_SCRN`, `2021_CXCA_SCRN_POS`, `FY24 TX_NEW`, `2022_CXCA_SCRN_POS`, .after = sex) %>% 
 rename(`-FY21_CXCA_SCRN` = `2021_CXCA_SCRN` ) %>%
    select(-Sex,  -`2022_CXCA_SCRN`, -`-FY21 CXCA_SCRN`, -`FY21 CXCA_SCRN POS`, -`FY22 CXCA_SCRN POS  (1/2)`) %>%
  glimpse()

library(writexl)
writexl::write_xlsx(cxca_target_vars, path = "Dataout/cxca_target_vars.xlsx")


