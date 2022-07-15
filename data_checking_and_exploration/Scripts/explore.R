library(scales)
library(gridExtra)
library(grid)
library(glitr)
library(ggplot2)
library(stats)
library(ggrepel)
library(scales)



pos <-  c("HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS_N")
neg <-  c("KP_PREV", "HTS_TST", "HTS_TST_NEG", "HTS_SELF", "PrEP_NEW", "PrEP_CT", "PrEP_CURR")

cascade <- check %>% filter(country == "Botswana", disagg == "KP", indicator %in% neg) %>%
  group_by(fy, indicator) %>%
  summarise(sum.cum=sum(cumulative)) %>%
  ggplot2::ggplot(aes(x = indicator, y = sum.cum, fill = factor(fy)))

cascade + geom_col() + geom_label(color = "white", position = position_stack(vjust = 0.9), aes(label = sum.cum)) + facet_wrap(~ fy) + theme(legend.position = "none")

#positive cascade
cascade <- check %>% filter(country == "Botswana", disagg == "KP",
                            indicator %in% pos, str_detect(mech_name, "EpiC")) %>%
  group_by(fy, indicator) %>%
  summarise(sum.cum=sum(cumulative)) %>%
  ggplot2::ggplot(aes(x = indicator, y = sum.cum, fill = factor(fy)))

cascade + geom_col() + geom_label(color = "white", position = position_stack(vjust = 0.9), aes(label = sum.cum)) + facet_wrap(~ fy) + theme(legend.position = "none")




#target achievement
ach_fsw <- check %>% filter(country == "Tanzania", disagg == "KP", fy == 2022) %>%
  group_by(keypop, indicator) %>%
  summarise(sum.cum=sum(cumulative), sum.targets=sum(targets)) %>%
  mutate(ach = sum.cum/sum.targets) %>% glimpse() %>%
  filter(sum.cum!=0, keypop=="FSW") %>%
  ggplot2::ggplot(aes(x = indicator, y = sum.cum, group=keypop))

ach_fsw + geom_col() + facet_grid(~ keypop) + geom_label(position = position_stack(vjust = 0.9), aes(label = percent(ach, accuracy = 0.1))) +
  geom_label(position = position_stack(vjust = 0.1), aes(label = sum.cum))

### snapshot -

# 1a ----------------------------------------------------------------------
prep_ach <- check %>% filter(country == "Kenya", disagg == "KP", fy == 2022) %>%
  group_by(keypop, indicator) %>%
  summarise(sum.cum=sum(cumulative), sum.targets=sum(targets)) %>%
  mutate(ach = sum.cum/sum.targets) %>% glimpse() %>%
  filter(sum.cum!=0, indicator=="PrEP_NEW") %>%
  ggplot2::ggplot(aes(x = " ", y = sum.cum, group=keypop))

prep_ach + geom_col() + facet_grid(~ keypop) + geom_label(position = position_stack(vjust = 0.9), aes(label = percent(ach, accuracy = 0.1))) +
  geom_label(position = position_stack(vjust = 0.1), aes(label = sum.cum))

# 1b ----------------------------------------------------------------------

linkage <- c("HTS_TST_POS", "TX_NEW", "PrEP_NEW")

tst_pos_trends <- qcheck %>% filter(country == "Cameroon", disagg == "KP",results!=0, indicator %in% linkage) %>%
  group_by(fyq, indicator) %>%
  summarise(sum.results=sum(results)) %>%
  ggplot2::ggplot(aes(x = fyq, y = sum.results, group=indicator))

tst_pos_trends + geom_line(aes(color = indicator)) +
  geom_label(aes(label = sum.results))

# 1c ----------------------------------------------------------------------
table(qcheck$indicator)
sa <- c("Zambia", "Malawi", "Botswana")
pvls_trends_country <- qcheck %>% filter(country %in% sa, disagg == "KP",
                                         results!=0, indicator == "TX_PVLS_D") %>%
  group_by(fyq, country, indicator) %>%
  summarise(sum.results=sum(results))

pvls_trends_country +
  ggplot2::ggplot(aes(x = fyq, y = sum.results, group=country)) +
  geom_line(aes(color = country)) +
  geom_label(aes(label = sum.results))

# 1d ----------------------------------------------------------------------
measure_indicators <- c("TX_PVLS_N", "TX_PVLS_D", "TX_CURR_Lag1", "TX_CURR_Lag2", "HTS_TST_POS", "TX_NEW", "HTS_TST_NEG", "PrEP_NEW")

measure_trends_country <- qcheck %>% filter(country %in% sa, disagg == "KP", results!=0, indicator %in% measure_indicators) %>%
  group_by(fyq, country, indicator) %>%
  summarise(sum.results=sum(results)) %>%
  pivot_wider(values_from = sum.results, names_from = indicator) %>%
  mutate(vls = TX_PVLS_N/TX_PVLS_D,
         vlc = TX_PVLS_D/TX_CURR_Lag2,
         linkage = TX_NEW/HTS_TST_POS) %>% glimpse()

#note: no dedup data here, but some dedup data in dashboard


measure_trends_country %>% ggplot2::ggplot(aes(x = fyq, y = vlc, group=country)) +
  geom_line(aes(color = country)) +
  geom_label(aes(label = percent(vlc, accuracy = 1)))



# 2a ----------------------------------------------------------------------
table(qcheck$indicator)
link <- c("HTS_TST_POS", "TX_NEW")

linkage_inds <- qcheck %>% filter(funding_agency == "USAID", country == "Vietnam", disagg == "KP", results!=0,
                                  indicator %in% linkage, indicator != "PrEP_NEW") %>%
  group_by(fyq, country, indicator) %>%
  summarise(sum.results=sum(results)) %>%
  ggplot2::ggplot(aes(x = fyq, y = sum.results, group=indicator))

linkage_inds + geom_line(aes(color = indicator)) +
  geom_label(aes(label = sum.results))

# 2b ----------------------------------------------------------------------
tx <- c("TX_CURR", "TX_NET_NEW")
curr <- qcheck %>% filter(funding_agency == "USAID", country == "Vietnam", disagg == "KP", results != 0,
                                 indicator %in% tx ) %>%
  group_by(fyq, country, indicator) %>%
  summarise(sum.results=sum(results))

curr %>% ggplot2::ggplot(aes(x = fyq, y = sum.results, group=indicator)) + geom_line(aes(color = indicator)) +
  geom_label(aes(label = sum.results))

table(curr$indicator)

# 2c ----------------------------------------------------------------------
measure_indicators <- c("TX_PVLS_N", "TX_PVLS_D", "TX_CURR_Lag1", "TX_CURR_Lag2", "HTS_TST_POS", "TX_NEW", "HTS_TST_NEG", "PrEP_NEW")

measure_trends_country <- qcheck %>% filter(funding_agency == "USAID", country %in% sa, disagg == "KP",results!=0, indicator %in% measure_indicators) %>%
  group_by(fyq, country, indicator) %>%
  summarise(sum.results=sum(results)) %>%
  pivot_wider(values_from = sum.results, names_from = indicator) %>%
  mutate(vls = TX_PVLS_N/TX_PVLS_D,
         vlc = TX_PVLS_D/TX_CURR_Lag2,
         linkage = TX_NEW/HTS_TST_POS,
         prep_init = PrEP_NEW/HTS_TST_NEG
  ) %>% glimpse()

measure_trends_country %>% filter(country=="Malawi") %>% ggplot2::ggplot(aes(x = fyq, y = vlc, group=country)) +
  geom_line(aes(color = country)) +
  geom_label(aes(label = percent(vlc, accuracy = 1)))

measure_trends_country %>% filter(country=="Malawi") %>% ggplot2::ggplot(aes(x = fyq, y = vls, group=country)) +
  geom_line(aes(color = country)) +
  geom_label(aes(label = percent(vls, accuracy = 1)))


# 2d ----------------------------------------------------------------------
tst <- c("HTS_TST", "HTS_TST_POS")
tst_by_kp <- qcheck %>% filter(funding_agency == "USAID", country == "Malawi", disagg == "KP",results!=0, indicator %in% tst) %>%
  group_by(fyq, keypop, indicator) %>%
  summarise(sum.results=sum(results)) %>%
  pivot_wider(values_from = sum.results, names_from = indicator) %>%
  mutate(link = HTS_TST_POS/HTS_TST) %>% glimpse()

tst_by_kp %>% ggplot2::ggplot(aes(x = fyq, y = link, group=keypop)) +
  geom_line(aes(color = keypop)) +
  geom_label(aes(label = percent(link, accuracy = 1)))


# 2f ----------------------------------------------------------------------
prep <- qcheck %>% filter(funding_agency == "USAID", country == "Malawi", disagg == "KP",results!=0, indicator == "PrEP_NEW") %>%
  group_by(fyq, indicator) %>%
  summarise(sum.results=sum(results)) %>% glimpse()


prep %>% ggplot2::ggplot(aes(x = fyq, y = sum.results)) + geom_line() +
  geom_label(aes(label = sum.results))


# 3 achievement ----------------------------------------------------------------------
ach_country <- check %>% filter(fy == 2022, funding_agency %in% usaid_cdc, country == "Laos", disagg == "KP") %>%
  group_by(funding_agency, indicator) %>%
  summarise(sum.cumulative=sum(cumulative),
            sum.targets=sum(targets)) %>%
  mutate(ach = sum.cumulative/sum.targets) %>% glimpse()

ach_country$indicator <- factor(ach_country$indicator,
                                      levels = c("KP_PREV", "HTS_SELF", "HTS_TST", "HTS_TST_POS", "TX_NEW",
                                                 "TX_CURR", "TX_PVLS_D", "TX_PVLS_N", "PrEP_NEW", "PrEP_CURR", "PrEP_CT"))
library(gt)

ach_country %>%
  arrange(desc(funding_agency), indicator) %>% filter(!is.na(indicator)) %>%
    gt() %>%
  tab_header(
    title = "KP Target Achievement by Agency, FY22") %>%
  fmt_number(
    columns = sum.cumulative,
    decimals = 0)  %>%
  fmt_number(
    columns = sum.targets,
    decimals = 0) %>%
  fmt_percent(
    columns = ach,
    decimals = 0) %>%
  cols_align(
    align = "left",
    columns = indicator
  ) %>%
  print()


# 4 Positivity ----------------------------------------------------------------------
tst <- c("HTS_TST", "HTS_TST_POS")
pos_by_percent <- check %>% filter(fy == "2022", funding_agency == "USAID", country == "Liberia", disagg == "KP", cumulative!=0, indicator %in% tst) %>%
  group_by(keypop, indicator) %>%
  summarise(sum.cumulative=sum(cumulative)) %>%
  pivot_wider(values_from = sum.cumulative, names_from = indicator) %>%
  mutate(pos = HTS_TST_POS/HTS_TST) %>% glimpse()


pos_by_percent %>% ggplot2::ggplot(aes(x = HTS_TST, y = pos, group=keypop)) +
  geom_point(aes(color = keypop)) +
  geom_label(aes(label = percent(pos, accuracy = 1), color = keypop)) +
  geom_label(aes(label = HTS_TST, color = keypop), position = position_nudge(y=.001))

#modality
pos_by_mod <- modality %>% filter(fy == "2022", funding_agency == "USAID", country == "Liberia", cumulative!=0) %>%
  group_by(modality, indicator) %>% summarise(sum.cumulative=sum(cumulative)) %>%
  pivot_wider(names_from = indicator, values_from = sum.cumulative) %>% mutate(pos = HTS_TST_POS/HTS_TST) %>%
  glimpse()

pos_by_mod %>% ggplot(aes(x=modality, y = pos)) + geom_point() + geom_label(aes(label = percent(pos, accuracy = 1)))
pos_by_mod %>% ggplot(aes(x=modality, y = HTS_TST)) + geom_col() + geom_label(aes(label = HTS_TST))

# percentage by age/sex
pos_by_agesex <- modality %>% filter(fy == "2022", funding_agency == "USAID", country == "Liberia", cumulative!=0) %>%
  group_by(age, indicator) %>% summarise(sum.cumulative=sum(cumulative)) %>%
  pivot_wider(names_from = indicator, values_from = sum.cumulative) %>% mutate(pos = HTS_TST_POS/HTS_TST) %>%
  glimpse()

pos_by_agesex %>% ggplot(aes(x=age, y = pos)) + geom_point() + geom_label(aes(label = percent(pos, accuracy = 1)))
pos_by_agesex %>% ggplot(aes(x=age, y = HTS_TST)) + geom_col() + geom_label(aes(label = HTS_TST))

#
glimpse(modality)
pos_by_im <- modality %>% filter(fy == "2022", funding_agency == "USAID", operatingunit == "Asia Region", cumulative!=0) %>%
  group_by(country, indicator) %>% summarise(sum.cumulative=sum(cumulative)) %>%
  pivot_wider(names_from = indicator, values_from = sum.cumulative) %>% mutate(pos = HTS_TST_POS/HTS_TST) %>%
  glimpse()

  pos_by_im %>% ggplot(aes(x=HTS_TST, y = pos, group = country)) + geom_point() + geom_label(aes(label = percent(pos, accuracy = 1))) +  geom_label(aes(label = country), position = position_nudge(y=.05, x = 1000))



# 5 pvls ----------------------------------------------------------------------
#VL by KP scatter plot
vl <- check %>% filter(country == "Tanzania", disagg == "KP", str_detect(indicator, "PVLS"), fy == 2022) %>%
  group_by(keypop, indicator) %>%
  summarise(sum.cum=sum(cumulative)) %>%
  pivot_wider(names_from = indicator, values_from = sum.cum) %>%
  mutate(vls = TX_PVLS_N/TX_PVLS_D) %>%
  glimpse() %>%
  ggplot2::ggplot(aes(x = TX_PVLS_D, y = TX_PVLS_N, fill = keypop))

vl + geom_point() + geom_label(aes(label = percent(vls, accuracy = 1)))

table(targets$keypop)

measure_trends_partner <- check %>% filter(country=="Malawi", fy == 2022, funding_agency == "USAID", disagg == "KP", cumulative!=0, indicator %in% measure_indicators) %>%
  group_by(partner, indicator) %>%
  summarise(sum.results=sum(cumulative)) %>%
  pivot_wider(values_from = sum.results, names_from = indicator) %>%
  mutate(vls = TX_PVLS_N/TX_PVLS_D,
         vlc = TX_PVLS_D/TX_CURR_Lag2,
         linkage = TX_NEW/HTS_TST_POS,
  ) %>% glimpse()

measure_trends_partner  %>% ggplot2::ggplot(aes(x = vlc, y = vls, group=partner)) +
  geom_point() +
  geom_label(aes(label = paste0(percent(vlc, accuracy = 1), " VLC\n",
                                percent(vls, accuracy = 1), " VLS",
                                partner)))




#
measure_trends_snu <- check %>% filter(country=="Malawi", fy == 2022, funding_agency == "USAID", country %in% sa, disagg == "KP", cumulative!=0, indicator %in% measure_indicators) %>%
  group_by(snu1, indicator) %>%
  summarise(sum.results=sum(cumulative)) %>%
  pivot_wider(values_from = sum.results, names_from = indicator) %>%
  mutate(vls = TX_PVLS_N/TX_PVLS_D,
         vlc = TX_PVLS_D/TX_CURR_Lag2,
         linkage = TX_NEW/HTS_TST_POS,
  ) %>% glimpse()

measure_trends_snu  %>% ggplot2::ggplot(aes(x = vlc, y = vls, group=snu1)) +
  geom_point() +
  geom_label(aes(label = paste0(percent(vlc, accuracy = 1), " VLC\n",
                                percent(vls, accuracy = 1), " VLS")))


measure_trends_psnu <- qcheck %>% filter(country=="Malawi", fy > 2021, results > 0, funding_agency == "USAID", indicator %in% measure_indicators) %>%
  group_by(psnu, disagg, qtr, indicator, partner) %>%
  summarise(sum.results=sum(results)) %>%
  pivot_wider(values_from = sum.results, names_from = indicator) %>%
  mutate(vls = TX_PVLS_N/TX_PVLS_D,
         vlc = TX_PVLS_D/TX_CURR_Lag2,
         linkage = TX_NEW/HTS_TST_POS) %>%
  select(psnu, partner, disagg, qtr, vlc, vls) %>%
  filter(partner == "Family Health International") %>%
  select(-partner)


measure_trends_psnu %>%
  ggplot2::ggplot(aes(x = vlc, y = vls, group=psnu)) +
  geom_point() +
  geom_label(aes(label = paste0(percent(vlc, accuracy = 1), " VLC\n",
                                percent(vls, accuracy = 1), " VLS", " ",
                                psnu)))



tab <- measure_trends_psnu %>%
  gt() %>%
  tab_header(
    title = "KP VLC/VLS by psnu and quarter, FY22") %>%
  fmt_percent(
    columns = vlc,
    decimals = 0) %>%
  fmt_percent(
    columns = vls,
    decimals = 0) %>%
  cols_align(
    align = "left",
    columns = psnu  ) %>%
  cols_label(qtr = "quarter") %>%
  print()

tab %>%   print()



# 6 TX ----------------------------------------------------------------------
vl_cascade <- c("TX_CURR", "TX_PVLS_D", "TX_PVLS_N", "TX_ML")
tx <- check %>% filter(fy == 2022, funding_agency== "USAID", country == "Nepal", disagg == "KP", indicator %in% vl_cascade ) %>%
  group_by(indicator, psnu) %>%
  summarise(sum.cumulative=sum(cumulative)) %>%
  pivot_wider(names_from = indicator, values_from = sum.cumulative) %>%
  glimpse()


tx %>%   arrange(desc(TX_CURR)) %>%
  gt() %>%
  cols_align(
    align = "left",
    columns = psnu
  ) %>%
  print()


#6.bc
tx_curr <- check %>% filter(fy == 2022, funding_agency== "USAID", country == "Nepal", disagg == "KP", indicator == "TX_CURR" ) %>%
  group_by() %>%
  summarise(sum.cumulative=sum(cumulative)) %>%
  glimpse()


#
tx_ml_reason <- check %>% filter(fy == 2022, funding_agency== "USAID", country == "Nepal", disagg == "KP", indicator == "TX_ML" ) %>%
  group_by(tx_ml_reason) %>%
  summarise(sum.cumulative=sum(cumulative)) %>%
  glimpse()
tx_ml_reason %>% ggplot(aes(y=sum.cumulative, x=tx_ml_reason)) + geom_col() + geom_label(aes(label = sum.cumulative))

tx_ml_kp <- check %>% filter(fy == 2022, funding_agency== "USAID", country == "Nepal", disagg == "KP", indicator == "TX_ML" ) %>%
  group_by(keypop) %>%
  summarise(sum.cumulative=sum(cumulative)) %>%
  glimpse()

tx_ml_kp %>% ggplot(aes(x=sum.cumulative, y=keypop)) + geom_col() + geom_label(aes(label = sum.cumulative))

# 7 MMD ----------------------------------------------------------------------
mmd_check <- mmd %>% filter(fy == 2022, funding_agency== "USAID", country == "Zambia", indicator == "TX_CURR") %>%
  group_by(arv) %>%
  summarise(sum.cumulative=sum(cumulative)) %>%
  glimpse()

mmd_check$arv <- factor(mmd_check$arv,
                                levels = c("Less than 3 months", "3 to 5 months", "6 or more months"))

mmd_check %>% arrange(arv) %>%
  gt() %>%
  cols_align(
    align = "left",
    columns = arv
  ) %>%
  print()
