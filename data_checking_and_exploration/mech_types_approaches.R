# read manual mech inputs -------------------------

poc_input <- read_sheet("https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=797015621",
           sheet = 2) |> filter(funding_agency == "USAID") |> mutate(mech_code = as.double(mech_code))

epic_input <- read_sheet("https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=797015621",
                         sheet = 3) |> mutate(central_kp_mech = "Yes")


# append and input data frames   -----------------------
input_2024_early <- poc_input |> 
      # exclude EpiC from USAID POC input
  anti_join(epic_input, by = "mech_code") |> 
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

previous_input <- read_sheet("https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=797015621",
                        sheet = 1) |> select(-`FY of origin`)

previous_input |> inner_join(input_2024_early, by = "mech_code")

# export --------------------
input2add <- input_2024_early |> select(
  # group ~ TX approach
  country, mech_code, 
  # focus ~ KP, INTEGRATED, ETC.
  type
)

# archive current sheet 1
sheet_write(data = previous_input, 
            ss = "https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=797015621",
            sheet = 5)


# move new data to sheet 1
sheet_write(data = input2add, 
            ss = "https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=797015621",
            sheet = 5)