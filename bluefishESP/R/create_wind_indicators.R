#' Create wind indicators
#'
#' This function calculates bluefish wind indicators from input .nc files
#' Monthly 10m wind downloaded from PSL: https://downloads.psl.noaa.gov/Datasets/NARR/Monthlies/monolevel/
#' @param vwind_file Filepath to the wind data
#' @param uwind_file Filepath to the wind data
#' @param months Months to use
#' @param return Boolean. Whether to return the indicators as an object in the global environment
#' @return Saves R object `wind_indicators`, returns wind indicators
#' @importFrom magrittr `%>%`
#' @export
#'

create_wind_indicators <- function(vwind_file,
                                   uwind_file,
                                   months = c(paste0("X", 1979:2021, ".04.01"),
                                              paste0("X", 1979:2021, ".05.01")),
                                   return = TRUE){

  # equal area crs
  new_crs <- "+proj=utm +zone=12 +datum=NAD83 +no_defs +ellps=GRS80"

  # shape things ----
  new_shape <- NEesp::shape %>%
    dplyr::select(STRATA, geometry) %>%
    sf::st_transform(proj4string = new_crs)

  large_geom <- new_shape %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    sf::st_crop(y = c(xmin = -80, xmax = -69,
                      ymax = 41.5, ymin = 32))

  # use ecopull function ----
  vwind <- ecopull::nc_to_raster(nc = vwind_file,
                                 varname = "vwnd",
                                 show_images = TRUE)  # converts to NAD83

  vwind <- raster::rotate(vwind)

  uwind <- ecopull::nc_to_raster(nc = uwind_file,
                                 varname = "uwnd",
                                 show_images = TRUE)  # converts to NAD83

  uwind <- raster::rotate(uwind)

  # mask ----
  m_vwind <- raster::mask(x = vwind[[months]],
                          mask = large_geom)
  m_uwind <- raster::mask(x = uwind[[months]],
                          mask = large_geom)

  # convert to dataframe ----
  rast_df_v <- raster::as.data.frame(m_vwind, xy = TRUE)
  rast_df_u <- raster::as.data.frame(m_uwind, xy = TRUE)

  # calculate means ----
  v_mean <- rast_df_v %>%
    dplyr::select(-c(x, y)) %>%
    colMeans(na.rm = TRUE)

  u_mean <- rast_df_u %>%
    dplyr::select(-c(x, y)) %>%
    colMeans(na.rm = TRUE)

  v_monthly <- tibble::tibble(names = names(v_mean),
                              values = v_mean) %>%
    dplyr::mutate(Year = stringr::str_extract(names, pattern = "\\d{4}"),
                  names = stringr::str_remove(names, pattern = "X"),
                  DOY = lubridate::as_date(names),
                  month = lubridate::month(DOY),
                  year = lubridate::year(DOY)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(mean_wind = mean(values))

  u_monthly <- tibble::tibble(names = names(u_mean),
                              values = u_mean) %>%
    dplyr::mutate(Year = stringr::str_extract(names, pattern = "\\d{4}"),
                  names = stringr::str_remove(names, pattern = "X"),
                  DOY = lubridate::as_date(names),
                  month = lubridate::month(DOY),
                  year = lubridate::year(DOY)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(mean_wind = mean(values))

  # combine ----
  all_wind <- dplyr::full_join(u_monthly %>%
                                 dplyr::rename(u_mean = mean_wind),
                               v_monthly %>%
                                 dplyr::rename(v_mean = mean_wind)
  ) %>%
    dplyr::mutate(wind_magnitude = sqrt(v_mean^2 + u_mean^2),

                  ns = ifelse(v_mean > 0, "north", "south"),
                  ew = ifelse(u_mean > 0, "east", "west"),
                  wind_blowing_to = paste0(ns, ew),

                  coarse_angle = atan(abs(v_mean/u_mean)),
                  coast_angle = (pi/4),

                  adjusted_angle = dplyr::case_when(
                    wind_blowing_to == "northeast" ~ coarse_angle,
                    wind_blowing_to == "northwest" ~ pi - coarse_angle,
                    wind_blowing_to == "southwest" ~ pi + coarse_angle,
                    wind_blowing_to == "southeast" ~ 2*pi - coarse_angle),

                  new_angle = (adjusted_angle + (pi/2 - coast_angle)),

                  new_angle = ifelse(new_angle > 2*pi, new_angle - 2*pi, new_angle),

                  new_wind_blowing_to = dplyr::case_when(
                    new_angle < 0 & new_angle > -pi/2 ~ "down_away",
                    new_angle < 2*pi & new_angle > 3*pi/2 ~ "down_away",
                    new_angle < 3*pi/2 & new_angle > pi ~ "down_towards",
                    new_angle < pi & new_angle > pi/2 ~ "up_towards",
                    new_angle > 0 & new_angle < pi/2 ~ "up_away"
                  ),

                  # have to add sign back somehow
                  longshore_wind = (wind_magnitude / sqrt(tan(new_angle)^2 + 1)),
                  crossshore_wind = sqrt(wind_magnitude^2 - longshore_wind^2),

                  longshore_wind = ifelse(stringr::str_detect(new_wind_blowing_to, "down"),
                                          -longshore_wind, longshore_wind),

                  crossshore_wind = ifelse(stringr::str_detect(new_wind_blowing_to, "towards"),
                                           -crossshore_wind, crossshore_wind),

                  check_magnitude = sqrt(longshore_wind^2 + crossshore_wind^2) == wind_magnitude

                  # along = ifelse(new_angle > 0, "up_coast", "down_coast"),
                  # across = ifelse(abs(new_angle) < pi/2, "away_coast", "towards_coast"),
                  # wind_blowing_to2 = paste(along, across)
    )

  wind_indicators <-  all_wind %>%
    dplyr::select(Year, longshore_wind, crossshore_wind) %>%
    tidyr::pivot_longer(cols = c(crossshore_wind, longshore_wind)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(CATEGORY = "Climate",
                  INDICATOR_TYPE = "Ecosystem") %>%
    dplyr::rename(YEAR = Year,
                  INDICATOR_NAME = name,
                  DATA_VALUE = value) %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  usethis::use_data(wind_indicators, overwrite = TRUE)

  if(return) { return(wind_indicators) }
}
