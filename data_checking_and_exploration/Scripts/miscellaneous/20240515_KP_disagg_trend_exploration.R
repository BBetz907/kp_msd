df_kp <- ou_df %>% 
  filter(country %in% c("Malawi", "Ghana", "Kenya", "Tanzania"),
         funding_agency == "USAID",
         #fiscal_year %in% c(2022, 2023, 2024),
         #funding_agency == "USAID",
         fiscal_year %in% c(2023, 2024),
         indicator %in% c("TX_CURR", "TX_NEW", "PrEP_NEW", "HTS_TST"),
         otherdisaggregate %in% c("MSM", "TG") | (sex == "Male" & !str_detect(standardizeddisaggregate, "ARVDisp"))
         ) %>% 
  mutate(otherdisaggregate = if_else(otherdisaggregate %in% c("MSM", "TG"), otherdisaggregate, sex)) |> 
  group_by(country, fiscal_year, otherdisaggregate, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") |> 
  reshape_msd() %>% 
  group_by(country, indicator, otherdisaggregate) %>% 
  mutate(lag_val = lag(value, 1, order_by = period),
         pct_change = (value - lag_val) / lag_val) %>% 
  ungroup() %>% 
  mutate(pd = stringr::str_remove(period, "FY"),
         date = lubridate::yq(pd)-months(3)) %>% 
  select(-pd, period_type) %>% 
  mutate(bar_color = ifelse(pct_change <0 & !is.na(pct_change), old_rose, trolley_grey_light)) %>%
  #mutate(pct_change = ifelse(is.na(pct_change), " ", pct_change)) %>% 
  mutate(full_lab = glue::glue("{clean_number(value)}\n ({percent(pct_change, 1)})")) 


df_kp |> count(country, otherdisaggregate, indicator) |> gt::gt()

# VIZ FUNCTIONS -------------------------------------------------------------------


# VIZ FUNCTIONS -------------------------------------------------------------------


viz_kp_bar <- function(df, cntry, kp, save = F) {
  
  viz <- df  %>% 
    filter(country == cntry,
           otherdisaggregate == kp) %>% 
    ggplot(aes(x = date, y = value, fill = bar_color)) +
    geom_col(width = 50) +
    facet_grid(indicator~otherdisaggregate, scales = "free_y", switch = "y") +
    geom_text(aes(label = full_lab, family = "Source Sans Pro")) +
    scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
    scale_fill_identity() +
    si_style_xline() + 
    labs(x = NULL, y = NULL,
         title = glue::glue("KP disaggregate trends across clinical indicators" %>% toupper()),
         subtitle = glue("Country of Interest: {cntry}"),
         caption = glue("Source: {metadata$caption}
                        Note: Uganda does not report on Key Population disaggregates")) +
    theme(axis.text.y = element_blank(),
          legend.position = "none",
          strip.text.y.left = element_text(angle=360))
  
  
  if (save == T) {
    glitr::si_save(
      plot = viz,
      filename = glue::glue("./Graphics/KP legislation/{cntry}_{kp}_fy24.svg"))
  }
  
  return(viz)
}  


viz_kp_line <- function(df, cntry, kp, save = F) {
  
  viz <- df  %>%
    filter(country == cntry,
           otherdisaggregate == kp) %>%
    ggplot(aes(x = date, y = value, color = bar_color, group = period_type)) +
    geom_line() +
    geom_point(size = 2) +
    facet_grid(indicator~otherdisaggregate, scales = "free_y", switch = "y") +
    geom_text(aes(label = full_lab, family = "Source Sans Pro", color = "black")) +
    #scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
    scale_color_identity() +
    si_style_xline() +
    labs(x = NULL, y = NULL,
         title = glue::glue("KP disaggregate trends across clinical indicators" %>% toupper()),
         subtitle = glue("Country of Interest: {cntry}"),
         caption = glue("Source: {metadata$caption}
                        Note: Uganda does not report on Key Population disaggregates")) +
    theme(axis.text.y = element_blank(),
          legend.position = "none",
          strip.text.y.left = element_text(angle=360))
  
  if (save == T) {
    glitr::si_save(
      plot = viz,
      filename = glue::glue("./Graphics/KP legislation/{cntry}_{kp}_line.svg"))
  }
  
  return(viz)
  
}

# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()
ref_id <- "602a37f6"

# Grab metadata

filepath <- si_path() %>% return_latest("OU_IM_FY22")

metadata <- get_metadata(filepath) 

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

# VIZ ITERATE -------------------------------------------------------------------


cntry_list <- c("Ghana", "Kenya" ,"Malawi", "Tanzania",
                "Ghana", "Kenya" ,"Malawi", "Tanzania",
                "Ghana", "Kenya" ,"Malawi", "Tanzania")

kp_list <- c("MSM","MSM", "MSM", "MSM",
             "TG","TG", "TG", "TG",
             "Male", "Male", "Male", "Male")

map2_dfr(cntry_list, kp_list,
         ~viz_kp_bar(df_kp, cntry = .x, kp = .y, save = T))

#use this one for the KP deck
map2_dfr(cntry_list, kp_list,
         ~viz_kp_line(df_kp, cntry = .x, kp = .y, save = T))


viz_kp_line(df_kp, cntry = cntry_list[4], kp = kp_list[1], save = T)
viz_kp_line(df_kp, cntry = cntry_list[4], kp = kp_list[9], save = T)

