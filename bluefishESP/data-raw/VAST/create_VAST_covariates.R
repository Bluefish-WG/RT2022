library(tidyr)
load(here::here("analysis/data/derived_data/all_nmfs.rda"))

`%>%` <- magrittr::`%>%`

# Data setup ----

## Covariates

depth <- all_nmfs %>%
  dplyr::select(Lat = LAT, Lon = LON, Depth = DEPTH) %>%
  dplyr::distinct() %>%
  tidyr::drop_na() %>%
  dplyr::full_join(tibble::tibble(Year = 1963:2021),
                   by = as.character())

# mean september temperature
# https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/
# september mean temp calculated at: https://github.com/kimberly-bastille/ESPplayground/blob/main/.github/workflows/calculate_temperature_sept.yml

dt_cov_data <- NULL
for(i in 1982:2021){
  raster <- raster::raster(here::here("../../ESPplayground/data-raw/september", paste0(i, ".grd")))

  raster::plot(raster) %>%
    print()
  cat("\n\n")

  this_data <- depth %>%
    dplyr::mutate(Year = i,
                  Temperature = raster::extract(raster,
                                                y = cbind(.data$Lon, .data$Lat))
    )
  if(is.null(dt_cov_data)) {
    dt_cov_data <- this_data
  } else {
    dt_cov_data <- rbind(dt_cov_data, this_data)
  }
}
dt_cov_data <- dt_cov_data %>%
  tidyr::drop_na() %>%
  dplyr::distinct()

dt_cov_data <- read.csv(here::here("density_covariates.csv"))
