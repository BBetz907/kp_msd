# read manual mech inputs -------------------------

poc_input <- read_sheet("https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=797015621",
           sheet = 2) |> filter(funding_agency == "USAID") |> mutate(mech_code = as.double(mech_code))

epic_input <- read_sheet("https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=797015621",
                         sheet = 3) |> mutate(central_kp_mech = "Yes")


# append and input data frames input from different sources 2024 inputs  -----------------------
input_2024_early <- poc_input |> 
      # exclude EpiC from USAID POC input
  anti_join(epic_input, by = "mech_code", "country") |> 
      # append now that EpiC IMs will not be duplicated
  bind_rows(epic_input) |> 
  mutate(type = case_when(
    `poc report: is this an indigenous/local/national prime partner?` == "Yes" ~ "Local Partner Mechanism",
     central_kp_mech == "Yes" ~ "Central KP Mechanism",
     .default = "International Bilateral Partner Mechanism"
      )) |> 
  # rename()
  print()

glimpse(input_2024_early)  
  
# qa/qc --------------------


#read previous inputs. Keep unduplicated, discard previous data
## change to sehet 1 for future
previous_input <- read_sheet("https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=1942630497",
                        sheet = 5) |> select(-`FY of origin`) |> 
  mutate(country = recode(country, 
                          "DR" = "Dominican Republic",
                          "DRC" = "Democratic Republic of the Congo",
                          "PNG" = "Papua New Guinea"
                          ))


# export --------------------
new_input2add <- input_2024_early |>
  rename(focus = `focus (predicted based on MER data reported)`) |> 
  select(
  # group ~ TX approach 
  country, mech_code, focus,  type
) 

previous_input_keep <- previous_input |> anti_join(new_input2add, by = c("mech_code", "country"))

updated_mech_list <- new_input2add |> bind_rows(previous_input_keep) |> 
  relocate(group, .before = country) |> 
  count(group, country, mech_code, focus, type) |> select(-n) 

# updated_mech_list |> count(country, mech_code) |> filter(n>1)


# move new data to sheet 1
sheet_write(data = updated_mech_list, 
            ss = "https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=797015621",
            sheet = 1)
