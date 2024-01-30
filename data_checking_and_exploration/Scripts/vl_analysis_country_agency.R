vl_check <- check |> filter(indicator %in% c("TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag2"),
                            # funding_agency == "USAID",
                            fy >= 2022 & fy <= 2023) |> 
  mutate(fy = str_c("FY", as.character(fy-2000), sep="_"),
         indicator = recode(indicator, "TX_PVLS_D" = "TX_PVLS (D)"),
         indicator = recode(indicator, "TX_PVLS_N" = "TX_PVLS (N)")) |> 
  glimpse()

vl_fy22_fy23 <- vl_check |> group_by(operatingunit, country, funding_agency, indicator, keypop, fy) |> 
  summarise(cumulative = sum(cumulative), .groups="drop") |> 
  pivot_wider(names_from = c(fy, indicator), values_from = cumulative) |>
  # mutate(vls = TX_PVLS_N/TX_PVLS_D,
  #        vlc)
  glimpse()

write_csv(vl_fy22_fy23, "Dataout/kp vl by country fy pop.csv")
