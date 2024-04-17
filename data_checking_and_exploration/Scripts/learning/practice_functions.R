
#create functions
```{r functions,  include=FALSE}

filter_scope <- function(df, funding_agency_selection = NULL, mech_type = NULL) {
  if (!is.null(funding_agency_selection)) {
    df2 <-  df |> filter(funding_agency == {{funding_agency_selection}})
  }
  else if (!is.null(mech_type)) {
    df2 <-    df |> 
      inner_join({{mech_type}}, by = "mech_code")
  }
  else {
    df2 <- df
  }
  return(df2)
}

test1 <- plhiv_by_age |> filter_scope(mech_type = central_kp_mech_ims)
test1 |> count(funding_agency)


sum_values_by_agecat <- function (df, agecat) {
  df3 <- df |>   
    group_by({{agecat}}, fiscal_year) |> 
    summarise(values=sum(values, na.rm = TRUE), .groups = "drop") |> 
    rename_with(~ "age", 1) |> 
    group_by(fiscal_year) |> 
    mutate(total=sum(values, na.rm = TRUE),
           proportion = scales::percent(values/total)) |> ungroup() |> 
    filter(!str_detect(age, "^not"))
  #   }    
  return(df3)
}


#test
test2 <- plhiv_by_age |> 
  sum_values_by_agecat(agecat = age50plus) |> print()


#iterate
# agecolumns <- c("age50plus", "age40plus", "age40s")

#CANNOT ITERATE
# result_df <- map_df(agecolumns, ~ sum_values_by_agecat(plhiv_by_age, agecat = .x))

```


New enhanced function with 3 parts, to be used after filter_scope()
```{r functions_plus,  include=FALSE}

sum_values_by_agecat_plus <- function (df, agecat, keep_funding_agency = NULL, keep_mech_type = NULL) {
  if (keep_funding_agency == TRUE & is.null(keep_mech_type)) {
    df3 <- df |>
      group_by({{agecat}}, fiscal_year, funding_agency) |>
      summarise(values=sum(values, na.rm = TRUE), .groups = "drop") |>
      rename_with(~ "age", 1) |>
      group_by(fiscal_year) |>
      mutate(total=sum(values, na.rm = TRUE),
             proportion = scales::percent(values/total)) |> ungroup() |>
      filter(!str_detect(age, "^not"))
  }
  else if (keep_mech_type == TRUE & is.null(keep_funding_agency)) {
    df3 <- df |>
      group_by({{agecat}}, fiscal_year, mech_type, funding_agency) |>
      summarise(values=sum(values, na.rm = TRUE), .groups = "drop") |>
      rename_with(~ "age", 1) |>
      group_by(fiscal_year) |>
      mutate(total=sum(values, na.rm = TRUE),
             proportion = scales::percent(values/total)) |> ungroup() |>
      filter(!str_detect(age, "^not"))
  }
  else if (keep_funding_agency == TRUE & keep_mech_type == TRUE) {
    df3 <- df |>
      group_by({{agecat}}, fiscal_year, mech_type, funding_agency) |>
      summarise(values=sum(values, na.rm = TRUE), .groups = "drop") |>
      rename_with(~ "age", 1) |>
      group_by(fiscal_year) |>
      mutate(total=sum(values, na.rm = TRUE),
             proportion = scales::percent(values/total)) |> ungroup() |>
      filter(!str_detect(age, "^not"))
  }
  else if (is.null(keep_funding_agency) & is.null(keep_mech_type)) {
    df3 <- df |>
      group_by({{agecat}}, fiscal_year) |>
      summarise(values=sum(values, na.rm = TRUE), .groups = "drop") |>
      rename_with(~ "age", 1) |>
      group_by(fiscal_year) |>
      mutate(total=sum(values, na.rm = TRUE),
             proportion = scales::percent(values/total)) |> ungroup() |>
      filter(!str_detect(age, "^not"))
  }
  else {stop("m")}
  
  return(df3)
  
}

test5 <- plhiv_by_age |> 
  filter_scope(mech_type = central_kp_mech_ims) |>
  sum_values_by_agecat_plus(agecat = age50plus,  keep_funding_agency = TRUE, keep_mech_type = TRUE) |> print()



```