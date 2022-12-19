`%>%` <- magrittr::`%>%`

channel <- dbutils::connect_to_database(
  server = "sole",
  uid = "atyrell"
)

# pull all trawl data
pull <- survdat::get_survdat_data(channel,
                                  all.season = TRUE,
                                  getBio = FALSE)

bluefish_nmfs <- pull$survdat %>%
  dplyr::filter(SVSPP == 135)

usethis::use_data(bluefish_nmfs, overwrite = TRUE)

all_nmfs <- pull$survdat %>%
  dplyr::select(-c(SVSPP, ABUNDANCE, BIOMASS, LENGTH, NUMLEN)) %>%
  dplyr::distinct()
usethis::use_data(all_nmfs, overwrite = TRUE)

# pull biological trawl data
`%like%` <- data.table::`%like%`
pull <- survdat::get_survdat_data(channel,
                                  all.season = TRUE,
                                  getBio = TRUE)

bluefish_nmfs_bio <- pull$survdat %>%
  dplyr::filter(SVSPP == 135)

usethis::use_data(bluefish_nmfs_bio, overwrite = TRUE)
