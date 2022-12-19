#' Create bluefish NMFS bottom trawl indicators
#'
#' This function creates bluefish NMFS bottom trawl indicators: min, mean, and max latitude, longitude, and temperature
#' @param data The full bluefish survey data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `bluefish_bottom_trawl`
#' @importFrom magrittr `%>%`
#' @import mgcv
#' @export
#'

# should these calculations be stratified in any way?

# bluefish_nmfs is bluefish NMFS survey data pulled from svdbs in script `pull_nmfs_bottom_trawl.R`

create_bluefish_bottom_trawl <- function(data = readRDS(here::here("data-raw/nmfs_survey_expanded_9c.RDS")),
                                         method = "gam",
                                         return = TRUE){

dat <- data %>%
  tidyr::drop_na(LENGTH) %>%
  dplyr::mutate(doy = lubridate::yday(EST_TOWDATE),
                size_group = ifelse(LENGTH <= 32.1,
                                    "small (<=32.1cm)",
                                    ifelse(LENGTH >= 49.6,
                                           "large (>=49.6cm)",
                                           "medium (31.2-49.6cm)"))) %>%
  tidyr::drop_na(SURFTEMP) %>%
  dplyr::filter(SURFTEMP > 0,
                SEASON == "FALL",
                doy < 305) # don't include November

if(method == "analog"){
  bluefish_bottom_trawl <- dat %>%
    dplyr::group_by(YEAR, SEASON, size_group) %>%
    # limit to sampling over the same 20 days of the year
    dplyr::filter(doy <= 290 & doy > 270) %>%
    dplyr::summarise(min_lat = min(LAT, na.rm = TRUE),
                     max_lat = max(LAT, na.rm = TRUE),
                     weighted_mean_lat = mean(rep(LAT, NUMLEN), na.rm = TRUE),

                     min_lon = min(LON, na.rm = TRUE),
                     max_lon = max(LON, na.rm = TRUE),
                     weighted_mean_lon = mean(rep(LON, NUMLEN), na.rm = TRUE),

                     min_temp = min(SURFTEMP, na.rm = TRUE),
                     max_temp = max(SURFTEMP, na.rm = TRUE),
                     weighted_mean_temp = mean(rep(SURFTEMP, NUMLEN), na.rm = TRUE),

                     min_doy = min(doy, na.rm = TRUE),
                     max_doy = max(doy, na.rm = TRUE),
                     weighted_mean_doy = mean(rep(doy, NUMLEN), na.rm = TRUE),

                     n_lat_events = sum(!is.na(LAT)),
                     n_lon_events = sum(!is.na(LON)),
                     n_temp_events = sum(!is.na(SURFTEMP)),
                     n_doy_events = sum(!is.na(doy))) %>%
    dplyr::mutate(min_lat = ifelse(n_lat_events < 5, NA, min_lat),
                  max_lat = ifelse(n_lat_events < 5, NA, max_lat),
                  weighted_mean_lat = ifelse(n_lat_events < 5, NA, weighted_mean_lat),

                  min_lon = ifelse(n_lon_events < 5, NA, min_lon),
                  max_lon = ifelse(n_lon_events < 5, NA, max_lon),
                  weighted_mean_lon = ifelse(n_lon_events < 5, NA, weighted_mean_lon),

                  min_temp = ifelse(n_temp_events < 5, NA, min_temp),
                  max_temp = ifelse(n_temp_events < 5, NA, max_temp),
                  weighted_mean_temp = ifelse(n_temp_events < 5, NA, weighted_mean_temp),

                  min_doy = ifelse(n_doy_events < 5, NA, min_doy),
                  max_doy = ifelse(n_doy_events < 5, NA, max_doy),
                  weighted_mean_doy = ifelse(n_doy_events < 5, NA, weighted_mean_doy)
    ) %>%
    tidyr::pivot_longer(cols = c("min_lat", "max_lat", "weighted_mean_lat",
                                 "min_lon", "max_lon", "weighted_mean_lon",
                                 "min_temp", "max_temp", "weighted_mean_temp",
                                 "min_doy", "max_doy", "weighted_mean_doy")) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(INDICATOR_NAME = paste(SEASON, size_group, name)) %>%
    dplyr::select(-c(SEASON, size_group, n_lat_events, n_lon_events, n_temp_events, n_doy_events, name)) %>%
    dplyr::rename(DATA_VALUE = value) %>%
    dplyr::mutate(INDICATOR_TYPE = "Ecosystem",
                  CATEGORY = "Distribution")
} else if (method == "gam") {
  dat <- dat %>%
    # make sure each year has enough of a spread of data
    dplyr::group_by(YEAR, size_group) %>%
    dplyr::mutate(range_doy = max(doy) - min(doy)) %>%
    dplyr::filter(range_doy > 19)

  small <- dat %>%
    dplyr::filter(size_group == "small (<=32.1cm)")
  medium <- dat %>%
    dplyr::filter(size_group == "medium (31.2-49.6cm)")
  large <- dat%>%
    dplyr::filter(size_group == "large (>=49.6cm)")

  make_gam_indicator <- function(data, formula, indicator){
    SEASON <- unique(data$SEASON)
    size_group <- unique(data$size_group)
    indicator <- data %>%
      gam(formula = as.formula(formula),
          family = gaussian()) %>%
      ggeffects::ggpredict(terms = "YEAR") %>%
      tibble::as_tibble() %>%
      dplyr::select(x, predicted) %>%
      dplyr::rename(YEAR = x,
                    DATA_VALUE = predicted) %>%
      dplyr::mutate(INDICATOR_NAME = paste(SEASON, indicator, size_group) %>%
                      stringr::str_to_sentence(),
                    YEAR = YEAR %>%
                      as.character() %>%
                      as.numeric())
  }

    bluefish_bottom_trawl <- rbind(make_gam_indicator(data = small,
                                                        formula = "LAT ~ s(LON) + as.factor(YEAR)*doy",
                                                        indicator = "latitude"),
                                     make_gam_indicator(data = medium,
                                                        formula = "LAT ~ s(LON) + as.factor(YEAR)*doy",
                                                        indicator = "latitude"),
                                     make_gam_indicator(data = large,
                                                        formula = "LAT ~ s(LON) + as.factor(YEAR)*doy",
                                                        indicator = "latitude"),
                                     make_gam_indicator(data = small,
                                                        formula = "LON ~ s(LAT) + as.factor(YEAR)*doy",
                                                        indicator = "longitude"),
                                     make_gam_indicator(data = medium,
                                                        formula = "LON ~ s(LAT) + as.factor(YEAR)*doy",
                                                        indicator = "longitude"),
                                     make_gam_indicator(data = large,
                                                        formula = "LON ~ s(LAT) + as.factor(YEAR)*doy",
                                                        indicator = "longitude"),
                                     make_gam_indicator(data = small,
                                                        formula = "SURFTEMP ~ s(LAT) + s(LON) + te(LAT, LON, k = 7) + as.factor(YEAR)*doy",
                                                        indicator = "temperature"),
                                     make_gam_indicator(data = medium,
                                                        formula = "SURFTEMP ~ s(LAT) + s(LON) + te(LAT, LON, k = 7) + as.factor(YEAR)*doy",
                                                        indicator = "temperature"),
                                     make_gam_indicator(data = large,
                                                        formula = "SURFTEMP ~ s(LAT) + s(LON) + te(LAT, LON, k = 7) + as.factor(YEAR)*doy",
                                                        indicator = "temperature")) %>%
      dplyr::mutate(INDICATOR_TYPE = "Ecosystem",
                    CATEGORY = "Distribution")

    print(head(bluefish_bottom_trawl))
  }
usethis::use_data(bluefish_bottom_trawl,
                  overwrite = TRUE)
if(return) return(bluefish_bottom_trawl)
}
# create_bluefish_bottom_trawl()
