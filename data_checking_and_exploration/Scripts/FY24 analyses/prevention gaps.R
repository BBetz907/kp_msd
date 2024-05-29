totals <- qcheck |> 
  filter(indicator %in% c("KP_PREV", "PrEP_CT", "PrEP_NEW"),
         str_detect(standardizeddisaggregate, "Total|Key"),
         ! standardizeddisaggregate == "KeyPop/Status", 
         funding_agency == "USAID"
         ) |> 
  group_by(indicator, standardizeddisaggregate, fy, qtr, fyq) |> 
  summarise(results = sum(results, na.rm = TRUE), .groups = "drop")

non_kp <- totals |> filter(indicator != "KP_PREV") |> 
  mutate(results = if_else(str_detect(standardizeddisaggregate, "KeyPop"), -results, results)) |> 
  group_by(indicator, fy, qtr, fyq) |> 
  summarise(results = sum(results, na.rm = TRUE), .groups = "drop") |> 
  mutate(pop = "GP")
  
prev <- totals |> filter(str_detect(standardizeddisaggregate, "KeyPop")) |> 
  mutate(pop = "KP") |> select(-standardizeddisaggregate) |> bind_rows(non_kp) |> 
  filter(!results == 0) |> 
  group_by(indicator, fy, qtr, fyq) |> 
  mutate(total = case_when(indicator != "KP_PREV" ~ sum(results, na.rm = TRUE)),
         kp_share = case_when(pop=="KP" ~ results/total),
         quarterly_kp_prev = case_when(indicator == "KP_PREV" ~ results/2)) |> 
  ungroup() |> 
  print(n=22)

prev |> filter(pop == "KP") |> group_by(indicator, fy) |> 
  summarize(
            q_avg = sum(results, na.rm = TRUE)/4,
                              # TRUE ~ mean(results),
                              .groups = "drop"
                              ) |> 
  mu

  
