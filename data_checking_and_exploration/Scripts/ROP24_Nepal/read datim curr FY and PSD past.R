library(tidyverse)
library(janitor)
library(gagglr)
library(grabr)


# nepal data + genie ------------------------------------------------------



nepal_genie <- read_delim("Data/ROP24_Nepal/Genie-PSNU_IM-Asia Region-Daily-2024-02-20.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE) |> 
  filter(fy)
  mutate(targets = 0) |> 
  select(-starts_with("approv"), -dataelementuid, -categoryoptioncombouid) |> 
  glimpse()

janitor::compare_df_cols(nepal_msd, nepal_genie)	

all.equal(nepal_msd, nepal_genie)	

nepal_data <- nepal_msd |> rbind(nepal_genie) |> 
  select(1:42, 49:51, 43:48) |> 
  group_by(across(c(1:45))) |> 
  # glimpse() |> 
  summarize(across(c("targets", "qtr1", "qtr2", "qtr3", "qtr4", "cumulative"), ~sum(., na.rm = TRUE)), .groups = "drop") |> 
  glimpse()

nepal_long <- nepal_data |> pivot_longer(cols = c("targets", "qtr1", "qtr2", "qtr3", "qtr4", "cumulative"), 
                           names_to = "results_or_targets", values_to = "values") |> 
  mutate(qtr = as.numeric(str_extract(results_or_targets, "[1-4]$")),
         results_or_targets = case_when(str_detect(results_or_targets, "^qtr") ~ "quarterly results",
                                        .default = results_or_targets)) |> 
  glimpse()



# nepal_cascades ----------------------------------------------------------
## set indicator order
cascade_order <- c("PLHIV", "Know their status", "PLHIV received ART", 
                   "Eligible for VL test", "Received VL test", "Suppressed Viral Load")


## read data
cascades_wide <- read_csv("Data/ROP24_Nepal/cascade_contributions.csv", col_select = c(1:8)) |> 
  # rename(National_Picture_FY25 = Notional_FY25) |> 
  filter(indicator != "TX_NEW") |> glimpse()

cascades_long <- cascades_wide |> pivot_longer(cols = 2:8, values_to = "values", names_sep = "_", names_to = c("org", "ach_or_targ", "fy")) |> 
  mutate(type = str_c(org, ach_or_targ, sep = " "),
         indicator = if_else(str_detect(indicator, "Number of"), "Know their status", indicator),
         indicator = reorder(indicator, match(indicator, cascade_order))) |>  
  mutate(pepfar = case_when(org == "PEPFAR" ~ values),
         national = case_when(org == "National" ~ values),
         notional = case_when(org == "Notional" ~ values),
  ) |> 
  group_by(indicator, fy) |> 
  mutate(pepfar_contribution = (sum(pepfar, na.rm = TRUE)/sum(national, na.rm = TRUE)),
         difference = sum(national, na.rm = TRUE) - sum(pepfar, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(pepfar_contribution = case_when(org == "PEPFAR" & !is.infinite(pepfar_contribution) ~ pepfar_contribution,
                                         .default = NA)) |> 
  print(n=42)

write_csv(cascades_long, "Data/ROP24_Nepal/cascade_contributions_tableau.csv")



# Visualize FY23 PEPFAR contributions ----------------------------------------------


cascades_long |> filter(fy=="FY23", indicator != "Know their status") |> 
  ggplot(aes(x=indicator, fill = org)) + 
  geom_col(aes(y = values), position = position_dodge(width = 0.5)) + 
  #label national
  geom_text(aes(y = values + 1200, 
                label = scales::comma(national)),
            position = position_dodge(width = 0.5),
            size = 3.5,
            color = grey80k) +
  # label percentage PEPFAR
  geom_text(aes(y = values + 1500,
                label = scales::percent(pepfar_contribution, accuracy = 1)),
            position = position_dodge(width = 1.2),
            size = 5,
            color = denim) +
  # geom_text(aes(y = values - 1500, 
  #               label = scales::percent(pepfar_contribution, accuracy = 1)),
  #           position = position_dodge(width = 0.5),
  #           size = 4.5,
  #           color = grey10k
  #           ) +
  #label PEPFAR
  geom_text(aes(y = values - 1500, 
                label = scales::comma(pepfar)),
            position = position_dodge(width = 0.5),
            size = 3.5,
            color = grey10k) +
  # facet_grid(cols = vars(fy)) +
  si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    # axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.justification = "right",
    legend.box.just = "center",
    legend.title = element_blank()) + 
  scale_fill_manual(values = c("#B3B5B8", denim)) +
  ggtitle(label = "FY23 National HIV cascade and PEPFAR Contributions to Results")


ggsave(filename = "Images/ROP24_Nepal/fy23_national_pepfar_cascades.png", plot = last_plot(), width = 8, height = 5)


# Visualize present to planned scenario ----------------------------------------------
context_order <- c("PEPFAR FY24 Q1 Results", "PEPFAR FY25 Targets", "National 95-95-95 Goals")

cascades_iterate <- cascades_long |> filter(org == "Notional" | 
                          (fy=="FY24" & type  == "PEPFAR Results") |
                          (fy=="FY25" & type  == "PEPFAR Targets")  ,
                        !indicator %in% c("PLHIV", "Know their status")
) |> 
  mutate(fy = if_else(fy == "FY24" & ach_or_targ == "Results", "FY24 Q1", fy),
         context = if_else(!is.na(fy),
                           str_c(org, fy, ach_or_targ, sep = " "),
                           "National 95-95-95 Goals"),
         context = reorder(context, match(context, context_order))
  ) 

## original
cascades_iterate |> 
  ggplot(aes(x=indicator, fill = context)) + 
  geom_col(aes(y = values), 
           position = position_dodge(width = 0.5)
           ) + 
si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    # axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.justification = "right",
    legend.box.just = "center",
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(b = 10, unit = "mm"))) + 
  scale_fill_manual(values = c(denim, denim_light, "#B3B5B8"))

ggsave(filename = "Images/ROP24_Nepal/aspirational_national_pepfar_cascades2.png", plot = last_plot(), width = 9, height = 5)



# visual with 95s prominent
cascades_iterate |> 
  ggplot(aes(x=indicator, fill = context)) + 
  geom_col(aes(y = values), 
           position = position_dodge(width = 0.5)
  ) + 
  geom_text(aes(y = notional + 1200, label = scales::comma(notional),
                ),
            color = grey80k,
            size = 5,
            position = position_dodge(width = 0.5)) +
  si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    # axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.justification = "right",
    legend.box.just = "center",
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(b = 10, unit = "mm"))) + 
  scale_fill_manual(values = c(denim, denim_light, "#B3B5B8")) +
  ggtitle(label = "Nepal 2nd and 3rd 95 goals")

ggsave(filename = "Images/ROP24_Nepal/aspirational_national_pepfar_cascades_a.png", plot = last_plot(), width = 9, height = 5)

# visual with PEPFAR targets prominent
cascades_iterate |> 
  mutate(pepfar = if_else(ach_or_targ == "Results", NA, pepfar)) |> 
  ggplot(aes(x=indicator, fill = context)) + 
  geom_col(aes(y = values), 
           position = position_dodge(width = 0.5)
  ) + 
#label national
  geom_text(aes(y = notional + 1200, label = scales::comma(notional),
  ),
  color = grey80k,
  size = 4,
  position = position_dodge(width = 0.5)) +
  # label pepfar targets
  geom_text(aes(y = pepfar - 1200, label = scales::comma(pepfar),),
    color = denim,
    size = 4,
    position = position_dodge(width = 0.5)) +
  si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    # axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.justification = "right",
    legend.box.just = "center",
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(b = 10, unit = "mm"))) + 
  scale_fill_manual(values = c(denim, denim_light, "#B3B5B8")) +
  ggtitle(label = "Nepal 2nd and 3rd 95 goals")

ggsave(filename = "Images/ROP24_Nepal/aspirational_national_pepfar_cascades_b.png", plot = last_plot(), width = 9, height = 5)


# PrEP cascade ------------------------------------------------------------
prev_order <- c("HTS_TST_NEG",	"PrEP_Offer",	"PrEP_NEW",	"PrEP_CT")
pop_order <- c("FSW", "MSM", "TG", "Clients of FSW")

nepal_prep <- read_csv("Data/ROP24_Nepal/epic_prep_cascade.csv") |> 
  select(1:5) |> 
  pivot_longer(cols = 2:5, names_to = "indicator", values_to = "values") |> 
  mutate(indicator = reorder(indicator, match(indicator, prev_order)),
         population = reorder(population, match(population, pop_order))) |> 
  glimpse()

nepal_prep |> 
  filter(indicator != "PrEP_CT") |> 
  mutate(prep_new = case_when(indicator == "PrEP_NEW" ~ values)) |> 
  ggplot(aes(x=indicator, y = values, fill = population)) +
  geom_col() + facet_grid(cols = vars(population)) +
  geom_text(aes(y=prep_new + 150, label = prep_new, color = population, size =4)) +
  si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  theme( axis.title = element_blank(),
         axis.text.x = element_text(angle = 90),
         axis.line.y = element_blank(),
         panel.background = element_blank(),
         legend.position = "none",
         plot.title = element_text(margin = margin(b = 4, unit = "mm"))) + 
  scale_fill_manual(values = c(FSW = "#005e7a", MSM = "#f28965", TG = "#7ecfc0", "Clients of FSW" = scooter_light)) +
  scale_color_manual(values = c(FSW = "#005e7a", MSM = "#f28965", TG = "#7ecfc0", "Clients of FSW" = scooter_light)) +
  ggtitle(label = "Prevention cascades by key population, FY24 Q1")

ggsave(filename = "Images/ROP24_Nepal/prep_cascades.png", plot = last_plot(), width = 6, height = 3)



nepal_prep |> filter(indicator == "PrEP_NEW") |> 
  group_by(indicator) |> 
  mutate(percent_of_total = values/sum(values),
         population = fct_reorder(population, percent_of_total),
         perc2 = case_when(population != "Clients of FSW" ~ percent_of_total),
         val2 = case_when(population != "Clients of FSW" ~ values),
         pop2 = case_when(population != "Clients of FSW" ~ population),
         
  ) |> 
  ungroup() |> 
  ggplot(aes(x = percent_of_total, fill = population, y=indicator)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(perc2, accuracy = 1)), 
            position = position_stack(vjust = 0.5),
            hjust = 0.5,
            size = 4,
            color = "white",
            vjust = -0.5) +   
  geom_text(aes(label = val2), 
            position = position_stack(vjust = 0.5),
            hjust = 0.6,
            size = 3,
            color = "white",
            vjust = 1.5) +
  geom_text(aes(label = pop2, color = pop2), 
            position = position_stack(vjust = 0.5),
            hjust = 0.9,
            size = 4,
            vjust = 4.3) +
  # labs(x = "Percent of Total", y = NULL, fill = "Population") +
  # theme_minimal() +
  scale_fill_manual(values = c(FSW = "#005e7a", MSM = "#f28965", TG = "#7ecfc0", "Clients of FSW" = scooter_light)) +
  scale_color_manual(values = c(FSW = "#005e7a", MSM = "#f28965", TG = "#7ecfc0", "Clients of FSW" = scooter_light)) +
  si_style_void() +
  # scale_y_continuous(labels = scales::percent(percent_of_total)) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5,
      margin = margin(b = 4, unit = "mm"))
  ) +
  ggtitle(label = "PrEP_NEW distribution, FY24 Q1")
  
ggsave(filename = "Images/ROP24_Nepal/prep_horizontal_bar.png", plot = last_plot(), width = 3.5, height = 1.5)


# PreP Ach ----------------------------------------------------------------

nepal_rop24 <- data.frame(
  country = "Nepal",
  indicator = c("PrEP_NEW", 
                "PrEP_CT"
                ),
  fy = "FY25",
  results_or_targets = "targets",
  values = c(5815, 1860)
) |> glimpse()

nepal_prep <- nepal_long |> filter(indicator %in% c("PrEP_NEW", "PrEP_CT"), 
                     standardizeddisaggregate == "Total Numerator",
                     results_or_targets != "quarterly results") |> 
  mutate(fy = if_else(fiscal_year == 2024, "FY24 Q1", 
                      str_c("FY", str_extract(as.character(fiscal_year), "[0-9]{2}$")))) |> 
  group_by(country, indicator, fy, results_or_targets) |> 
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop") |> 
  rbind(nepal_rop24) |>
  pivot_wider(names_from = results_or_targets, values_from = values, values_fill = list(value = NA)) |> 
  mutate(indicator = reorder(indicator, match(indicator, prev_order)),
         targets = if_else(targets == 0, NA, targets),
         ach = cumulative/targets ) |> 
  print()



nepal_prep |> 
  filter(indicator == "PrEP_NEW") |> 
  ggplot(aes(x = fy)) + 
  # facet_grid(~indicator) +
  geom_col(aes(y = targets), fill = grey10k) +
  geom_col(aes(y = cumulative, fill = indicator), width = 0.6) +
  geom_text(aes(y = cumulative + 300, color = indicator,
                label = scales::percent(ach, accuracy = 1))) +
  scale_fill_manual(values = c(PrEP_NEW = "#8980cb", PrEP_CT = grey60k)) +
  scale_color_manual(values = c(PrEP_NEW = "#8980cb", PrEP_CT = grey60k)) +
  si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  theme( axis.title = element_blank(),
         axis.text.x = element_text(),
         axis.line.y = element_blank(),
         panel.background = element_blank(),
         legend.position = "none",
         plot.title = element_text(margin = margin(b = 5, unit = "mm"))
         ) +
  ggtitle(label = "PrEP_NEW targets and achievement")

ggsave(filename = "Images/ROP24_Nepal/prep_new_ach.png", plot = last_plot(), width = 4, height = 3)



# prep_ct trends-----------------------------------------------------------------
nepal_prep_ct <- read_csv("Data/ROP24_Nepal/prep_ct_results_targets.csv") |> 
  mutate(fyq = str_c(fy, q, sep = " "),
         fyq_wrap = str_wrap(fyq, width = 4),
         ach = results/targets,
         labels = case_when(
           q %in% c("Q1", "Q4") ~ results,
         )) |>  glimpse()

nepal_prep_ct |> ggplot(aes(x = fyq_wrap, group = "1")) +
  geom_line(aes(y=results), size = 1, color = "#5a559b") +
  geom_col(aes(y=targets, width=1), fill = "#e9ddff") + 
  geom_text(aes(y=labels + 180, label = scales::comma(labels)), color = "#5a559b") +
  geom_text(aes(y=targets - 100, label = scales::comma(targets))) +
  geom_text(aes(y=results-180, label = scales::percent(ach, accuracy = 1)),
            size = 4, fontface = "bold") +
  # geom_text() +
  si_style_ygrid() +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  theme( axis.title = element_blank(),
         panel.background = element_blank(),
         legend.position = "none",
         plot.title = element_text(margin = margin(b = 5, unit = "mm"))
  ) +
  ggtitle(label = "PrEP_CT results (line) and last quarter targets (bars)")
  
ggsave(filename = "Images/ROP24_Nepal/prep_ct_ach.png", plot = last_plot(), width = 5.5, height = 3.2)




# visualize target progress -----------------------------------------------
indicator_order <- c("HTS_TST_POS",
                     "TX_NEW",
                     "TX_CURR",
                     "TX_PVLS (D)",
                     "TX_PVLS (N)",
                     "HTS_TST",
                     "PrEP_NEW", 
                     "PrEP_CT",
                     "TB_PREV (D)",
                     "TB_PREV (N)",
                     "Time-lapse")


nepal_rop24_full <- data.frame(
  country = "Nepal",
  indicator = c("PrEP_NEW", 
                "PrEP_CT",
                "TX_CURR",
                "TX_NEW",
                "HTS_TST_POS",
                "TX_PVLS (D)",
                "TX_PVLS (N)",
                "HTS_TST",
                "KP_PREV",
                "TB_PREV (D)",
                "TB_PREV (N)",
                "Time-lapse"
  ),
  fy = "FY25",
  results_or_targets = "targets",
  values = c(5815, 1860, 24495, 1925, 1330, 21925, 20830, 
             18347, 15730, 10654, 5327, 0)
) |> print()

nepal_rop <- nepal_long |> 
              mutate(indicator = case_when(
                indicator %in% c("TB_PREV", "TX_PVLS") ~ 
                  str_c(indicator, " (", numeratordenom, ")"),
                .default = indicator
              )) |>  
                            filter(fiscal_year == 2024,
                                  indicator %in% indicator_order,
                                   standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
                                   results_or_targets != "quarterly results") |> 
  mutate(fy = if_else(fiscal_year == 2024, "FY24 Q1", 
                      str_c("FY", str_extract(as.character(fiscal_year), "[0-9]{2}$")))) |> 
  group_by(country, indicator, fy, results_or_targets) |> 
  summarise(values = sum(values, na.rm = TRUE), .groups = "drop") |> 
  rbind(nepal_rop24_full) |>
  pivot_wider(names_from = results_or_targets, values_from = values, values_fill = list(value = NA)) |> 
  mutate(indicator = reorder(indicator, match(indicator, indicator_order)),
         targets = if_else(targets == 0, NA, targets),
         ach_label = cumulative/targets,
         ach = if_else(ach_label > 1, 1, ach_label),
         unach = if_else(ach > 1, NA, 1 - ach)
         ) |> 
  print()




# current FY ach 100% -----------------------------------------------------
rev_indicator_order <- rev(indicator_order)
ach_order <- c("unach", "ach")

nepal_rop |> filter(fy == "FY24 Q1" | indicator == "Time-lapse",
                    !indicator %in% c("TB_PREV (D)", "TB_PREV (N)")
                    ) |> 
  pivot_longer(cols = c(4:8),
               names_to = "measure",
               values_to = "values") |> 
  mutate(cumulative = case_when(measure == "cumulative" ~ values),
         targets = case_when(measure == "targets" ~ values),
         ach_label = case_when(
                               indicator == "Time-lapse" & measure == "ach" ~ 0.25,
                               indicator == "Time-lapse" & measure == "unach" ~ 0.75,
                               measure == "ach_label" ~ values),
         values = case_when(measure %in% c("cumulative", "targets", "ach_label") ~ NA, 
                            indicator == "Time-lapse" & measure == "ach" ~ 0.25,
                            indicator == "Time-lapse" & measure == "unach" ~ 0.75,
                            .default = values),
         indicator = reorder(indicator, match(indicator, rev_indicator_order)),
         ach = case_when(measure %in% c("ach", "unach") ~ measure,
                         .default = "ach"),
         ) |> 
  group_by(country, indicator, fy, ach) |> 
  summarize(across(c("values":"ach_label"), ~sum(., na.rm = TRUE)), .groups = "drop") |>
  mutate(         ach = reorder(ach, match(ach, ach_order)),
                  ach_label = case_when(ach=="unach" ~ NA,
                                        .default = ach_label),
                  cumulative = if_else(cumulative == 0, NA, cumulative),
                  targets = if_else(targets == 0, NA, targets),   
                  )  |> 
  ggplot(aes(fill = ach, y=indicator)) +
  geom_bar(aes(x = values, ), stat = "identity") +
#label achievement
  geom_text(aes(x = values, label = scales::percent(ach_label, accuracy = 1)), 
            # position = position_stack(vjust = 0.5),
            hjust = 1.2,
            # size = 4,
            color = "white",
            # vjust = -0.5
            ) +
#label result/target bar end
  geom_text(aes(x = 1.02, label = str_c(
                  scales::comma(cumulative),
                  "  /  ",
                  scales::comma(targets))
                ),
            hjust = 0,
            size = 3,
            ) +
  labs(x = "% Target Achievement") +
  # theme_minimal() +
  scale_fill_manual(values = c(ach = "#005e7a", unach = grey20k)) +
  scale_x_continuous(limits = c(0, 1.3), 
                     # expand = c(0, 0.2)
                     ) +
  si_style_nolines() +
  theme(
    # plot.margin = margin(r = 2, unit = "cm")
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(
                              # hjust = 0.45,
                              margin = margin(b = 4, unit = "mm"))
  ) +
  ggtitle(label = "FY24 Q1 achievement against targets for key indicators")

ggsave(filename = "Images/ROP24_Nepal/fy24_ach.png", plot = last_plot(), width = 5, height = 3)


