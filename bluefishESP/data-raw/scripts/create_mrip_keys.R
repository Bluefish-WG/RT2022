# MRIP code keys (more interpretable than ifelse)
area_key <- tibble::tibble(
  area_x = as.character(1:5),
  area_x_words = c(
    "Ocean <= 3 mi (all but WFL)",
    "Ocean > 3 mi (all but WFL)",
    "Ocean <= 10 mi (WFL only)",
    "Ocean > 10 mi (WFL only)",
    "Inland"
  )
)
usethis::use_data(area_key)

fl_key <- tibble::tibble(
  fl_reg = as.character(1:5),
  fl_reg_words = c(
    "BAY, DIXIE, ESCAMBIA, FRANKLIN, GULF, JEFFERSON, OKALOOSA, SANTA ROSA, TAYLOR, WAKULLA, WALTON",
    "CHARLOTTE, CITRUS, COLLIER, HERNANDO, HILLSBOROUGH, LEE, LEVY, MANATEE, PASCO, PINELLAS, SARASOTA",
    "MONROE",
    "BROWARD, DADE, INDIAN RIVER, MARTIN, PALM BEACH ST. LUCIE",
    "BREVARD, CLAY, DUVAL, FLAGLER, NASSAU, ST. JOHNS, VOLUSIA"
  )
)
usethis::use_data(fl_key)

mode_key <- tibble::tibble(
  mode_fx = as.character(3:7),
  # there is a mode 6 in the data, but not in the key??
  # assigning names based on what is in the estimated catch data
  mode_fx_words = c(
    "Shore",
    "Party Boat",
    "Charter Boat",
    "Party/Charter Boat",
    "Private/Rental Boat"
  )
)
usethis::use_data(mode_key)

st_key <- tibble::tibble(
  st = as.character(
    c(1, 9:10, 12:13, 22:25, 28, 33:34, 36:37, 44:45, 51)
  ),
  st_words = c(
    "Alabama", "Connecticut", "Delaware",
    "Florida", "Georgia", "Louisiana",
    "Maine", "Maryland", "Massachusetts",
    "Mississippi", "New Hampshire", "New Jersey",
    "New York", "North Carolina", "Rhode Island",
    "South Carolina", "Virginia"
  )
)
usethis::use_data(st_key)
