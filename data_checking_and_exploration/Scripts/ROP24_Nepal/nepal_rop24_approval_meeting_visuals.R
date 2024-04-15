library(tidyverse)
library(janitor)
library(gagglr)
library(grabr)





# nepal_cascades ----------------------------------------------------------
## set indicator order
cascade_order <- c("PLHIV", "Know their status", "PLHIV received ART", 
                   "Eligible for VL test", "Received VL test", "Suppressed Viral Load")

## read data
cascades_wide <- read_csv("Data/ROP24_Nepal/cascade_contributions.csv", col_select = c(1:9)) |> 
  # rename(National_Picture_FY25 = Notional_FY25) |> 
  filter(indicator != "TX_NEW") |> glimpse()


#y2, y1
cascades_wide |> select(1,6:9) |> 
  mutate(diff10 = PEPFAR_Targets_FY25 - PEPFAR_Targets_FY24,
         diff21 = PEPFAR_Targets_FY26 - PEPFAR_Targets_FY25 ) |> print()




cascades_long <- cascades_wide |> pivot_longer(cols = 2:9, values_to = "values", names_sep = "_", names_to = c("org", "ach_or_targ", "fy")) |> 
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
            position = position_dodge(width = 1.4),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(
    # axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    legend.text = element_text(size=8),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.title = element_blank(),
    # plot.title = element_text(margin = margin(b = 20, unit = "mm"))
    ) + 
  scale_fill_manual(values = c("#B3B5B8", denim)) +
  ggtitle(label = "FY23 National HIV cascade and PEPFAR Contributions")


ggsave(filename = "Images/ROP24_Nepal/take2/fy23_national_pepfar_cascades.png", plot = last_plot(), width = 10, height = 4.5)


# Visualize present to planned scenario ----------------------------------------------
context_order <- c("PEPFAR FY24 Q1 Results", "PEPFAR FY24 Targets", "Y1 Targets", "PEPFAR FY25 Targets", "Y2 Targets", "PEPFAR FY26 Targets", "National 95-95-95 Goals")


cascades_iterate <- cascades_long |> filter(org == "Notional" | 
                          (fy=="FY24" & type  == "PEPFAR Results") |
                          (fy=="FY24" & type  == "PEPFAR Targets") |
                          (fy=="FY25" & type  == "PEPFAR Targets") |
                          (fy=="FY26" & type  == "PEPFAR Targets") ,
                        !indicator %in% c("PLHIV", "Know their status")) |> 

  mutate(fy = if_else(fy == "FY24" & ach_or_targ == "Results", "FY24 Q1", fy),
         context = if_else(!is.na(fy),
                           str_c(org, fy, ach_or_targ, sep = " "),
                           "National 95-95-95 Goals"),
         
         #rework targets
         context = case_when( fy=="FY25" ~ "Y1 Targets",
                              fy=="FY26" ~ "Y2 Targets",
                                     .default = context), 
         # context = reorder(context, match(context, context_order))
         # context = fct_reorder(context, .x = context, .fun = function(x) match(x, context_order))
         ) 

cascades_iterate$context <- fct_relevel(cascades_iterate$context, context_order)

## original
cascades_iterate |> 
  filter(context!="Y2 Targets") |> 
  ggplot(aes(x=indicator, fill = context)) + 
  geom_col(aes(y = values), 
           position = position_dodge(width = 0.5)
           ) + 
si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(
    # axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    legend.text = element_text(size=8),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(b = 28, unit = "mm"))) + 
  scale_fill_manual(values = c(denim,
                               "#436ec1",
                               "#7396ee", 
                               # denim_light,
                               "#B3B5B8")) +
  ggtitle(label = "Current FY results and target with ROP Y1 target progress")


ggsave(filename = "Images/ROP24_Nepal/take2/year1_cascade_progress.png", plot = last_plot(), width = 5.8, height = 5.5)


cascades_iterate |> 
  filter(!context %in% c("PEPFAR FY24 Targets")) |> 
  ggplot(aes(x=indicator, fill = context)) + 
  geom_col(aes(y = values), 
           position = position_dodge(width = 0.5)
  ) + 
  si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(
    # axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    legend.text = element_text(size=8),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(b = 28, unit = "mm"))) + 
  scale_fill_manual(values = c(denim, "#7396ee", denim_light, "#B3B5B8")) +
  ggtitle(label = "ROP24 Y1 & Y2 inputs to support National 2nd & 3rd 95 goals")


ggsave(filename = "Images/ROP24_Nepal/take2/year12_cascade_progress.png", plot = last_plot(), width = 5.8, height = 5.5)



cascades_iterate |> count(context)
cascades_iterate |> filter(fy=="FY26")

# visual with 95s prominent
cascades_iterate |> 
  ggplot(aes(x=indicator, fill = context)) + 
  geom_col(aes(y = values), 
           position = position_dodge(width = 0.5)
  ) + 
  geom_text(aes(y = notional + 1800, label = scales::comma(notional),
                ),
            color = grey80k,
            # size = ,
            position = position_dodge(width = 0.5)) +
  si_style_ygrid() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(
    # axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank(),
    legend.text = element_text(size=8),
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(b = 24.5, unit = "mm"))) + 
  scale_fill_manual(values = c(denim, "#7396ee", denim_light, "#B3B5B8")) +
  ggtitle(label = "ROP24 inputs to support the National 2nd and 3rd 95 goals")

ggsave(filename = "Images/ROP24_Nepal/take2/aspirational_national_pepfar_cascades.png", plot = last_plot(), width = 5.5, height = 4.5)

 # visual with PEPFAR targets prominent
# cascades_iterate |> 
#   mutate(pepfar = if_else(ach_or_targ == "Results", NA, pepfar)) |> 
#   ggplot(aes(x=indicator, fill = context)) + 
#   geom_col(aes(y = values), 
#            position = position_dodge(width = 0.5)
#   ) + 
# #label national
#   geom_text(aes(y = notional + 1200, label = scales::comma(notional),
#   ),
#   color = grey80k,
#   size = 4,
#   position = position_dodge(width = 0.5)) +
#   # label pepfar targets
#   geom_text(aes(y = pepfar - 1200, label = scales::comma(pepfar),),
#     color = denim,
#     size = 4,
#     position = position_dodge(width = 0.5)) +
#   si_style_ygrid() +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
#   theme(
#     # axis.text.x = element_text(angle = 90),
#     axis.title = element_blank(),
#     axis.line.y = element_blank(),
#     panel.background = element_blank(),
#     legend.text = element_text(size=8),
#     legend.position = "bottom",
#     legend.justification = "right",
#     legend.box.just = "center",
#     legend.title = element_blank(),
#     plot.title = element_text(margin = margin(b = 10, unit = "mm"))) + 
#   scale_fill_manual(values = c(denim, "#7396ee", denim_light, "#B3B5B8")) +
#   ggtitle(label = "Nepal 2nd and 3rd 95 goals")
# 
# ggsave(filename = "Images/ROP24_Nepal/take2/aspirational_national_pepfar_cascades_b.png", plot = last_plot(), width = 6.5, height = 4)



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

rev_indicator_order <- rev(indicator_order)
ach_order <- c("unach", "ach")

ref <- data.frame(
  indicator = "Time-lapse",
  pop = c("TG", "MSM", "FSW", "All People"),
  cumulative = 1,
  targets = 4
  )  





# nepal data  ------------------------------------------------------
ady <- c("15-19", "20-24", "15-24")
nepal_msd <- mer_df |> filter(country == "Nepal") |> kp_setup() |> kp_clean(time="fy") |> glimpse()

# nepal_data <- 
nepal_rop <- nepal_msd |> filter(fy==2024, indicator %in% indicator_order) |> 
  mutate(pop = case_when(
                         # sex == "Female" & ageasentered %in% ady ~ "AGYW (15-24)",
                         disagg == "KP" ~ keypop,
                         disagg == "Total" ~ "All People")) |> 
  filter(!is.na(pop)) |> group_by(indicator, pop) |>
  summarize(cumulative=sum(cumulative, na.rm = TRUE), 
            targets=sum(targets, na.rm = TRUE), 
            .groups = "drop") |> 
  rbind(ref) |> 
  mutate(indicator = reorder(indicator, match(indicator, indicator_order)),
         targets = if_else(targets == 0, NA, targets),
         ach_label = cumulative/targets,
         ach = if_else(ach_label > 1, 1, ach_label),
         unach = if_else(ach > 1, NA, 1 - ach),
         
         indicator = reorder(indicator, match(indicator, rev_indicator_order)),
         ach2 = reorder(ach, match(ach, ach_order)),
         ach_label = case_when(ach=="unach" | indicator == "PrEP_CT" ~ NA,
                               .default = ach_label),
         cumulative = if_else(cumulative == 0, NA, cumulative),
         targets = if_else(targets == 0, NA, targets),
         cumulative = if_else(indicator == "Time-lapse", NA, cumulative),
         targets = if_else(indicator == "Time-lapse", NA, targets),
         )

# nepal_rop$pop <- fct_relevel(nepal_rop$pop, c("All People", "KP", "AGYW"))

# current FY ach 100% -----------------------------------------------------


nepal_rop |> 
  ggplot(aes(y=indicator)) +
  facet_grid(cols = vars(pop)) +
  geom_bar(aes(x = 1 ), stat = "identity", fill = grey20k) +
  geom_bar(aes(x = ach, fill=pop), stat = "identity") +
#label achievement
  geom_text(aes(x = ach, label = scales::percent(ach_label, accuracy = 1)), 
            # position = position_stack(vjust = 0.5),
            hjust = 1.1,
            # size = 4,
            color = "white",
            # vjust = -0.5
            ) +
#label result bar end
  geom_text(aes(x = 1.02, label = str_c(
                  scales::comma(cumulative),
                  " /"),                ),
            hjust = 0,
            vjust = 0,
            size = 3,
            color = grey50k
            ) +
#label target bar end
  geom_text(aes(x = 1.02, label =
    scales::comma(targets)),
  hjust = 0,
  vjust = 1.5,
  size = 3,
  color = grey50k
  ) +
  labs(x = "% Target Achievement") +
  # theme_minimal() +
  scale_fill_manual(values = c(FSW = "#005e7a", MSM = "#f28965", TG = "#7ecfc0", "All People" = grey70k, Prisoners = "#990d2e", PWID = "#fbcc50")) +
  # scale_fill_manual(values = c("KP" = "#005e7a", "All People" = grey70k, "AGYW (15-24)" = genoa)) +
  scale_x_continuous(limits = c(0, 1.2), 
                     # expand = c(0, 0.2)
                     ) +
  si_style_nolines() +
  theme(
    # plot.margin = margin(r = 2, unit = "cm")
    
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text = element_text(hjust = 0.5, face = "bold", ),
    plot.title = element_text(
                              # hjust = 0.45,
                              margin = margin(b = 4, unit = "mm"))
  ) +
  ggtitle(label = "FY24 Q1 achievement by population against targets for key indicators")

ggsave(filename = "Images/ROP24_Nepal/take2/fy24_ach.png", plot = last_plot(), width = 11, height = 4.5)



# budget viz --------------------------------------------------------------

budget <- data.frame(fy=c(2023,2024,2025), 
           type = c("International", "International", "International", "Local", "Local", "Local"),
           budget = c(9925000, 10225000,9925000-2500000, 0, 0, 2500000)
           ) |> 
  mutate(budget_last = case_when(fy==2025 ~ budget,
                                 .default = NA))

# Custom formatting function
currency_millions <- function(x) {
  paste0(dollar(round(x/1000000, 1)), "M")
}

budget |> ggplot(aes(x = fy, fill = type)) +
  geom_area(aes(y=budget)) +
  # geom_text(data = budget |> filter(fy == 2025),
  #           aes(y=budget,
  #               # color = type,
  #               label = currency_millions(budget)),
  #           vjust = 0, hjust = 1.1,
  #           position = position_stack(vjust = 0.5),
  #           fontface = "bold"
  #           ) +
  scale_x_continuous(labels = function(x) as.integer(x), breaks = budget$fy) +
  scale_y_continuous(labels = currency_millions, 
                     # limits = c(0,11000000)
                     ) +
  scale_fill_manual(values = c(International = "#005e7a", Local = "#a6fdff")) + 
    si_style_ygrid() +
  theme(
    # plot.margin = margin(r = 2, unit = "cm")
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text = element_text(hjust = 0.5, face = "bold", ),
    plot.title = element_text(
      # hjust = 0.45,
      margin = margin(b = 5, unit = "mm"))
  ) +
  ggtitle(label = "PEPFAR Nepal Annual Budget trends")

ggsave(filename = "Images/ROP24_Nepal/take2/pepfar_budget_trends.png", plot = last_plot(), width = 10, height = 4.3)
