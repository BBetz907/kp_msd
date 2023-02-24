library(tidyverse)
library(patchwork)
library(dplyr)
library(glitr)
library(scales)

pwid_indicators <- c("HTS_TST_POS","TX_NEW", "TX_CURR", "TX_PVLS_N","TX_PVLS_D", "KP_PREV") #add KP_PREV later

pwid_cascade_fy22 <- check %>% filter(fy == 2022, disagg == "KP", keypop == "PWID", indicator %in% pwid_indicators) %>%
  group_by(fy, indicator, country) %>%
  summarise(sum.cum=sum(cumulative), sum.targets=sum(targets), .groups = 'drop') %>%
  mutate(ach = sum.cum/sum.targets) %>% arrange(country)
