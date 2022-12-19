#' Create bluefish trips indicator
#'
#' This function creates the bluefish trips indicator
#' @param files A list of the full file names of annual directed trip data (.csv format)
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Updates the package object `bluefish_trips`
#'
#' Directed trip data must be downloaded manually from the MRIP query tool
#' https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries
#' 2022 bluefieh ESP: downloaded atlantic coast by state, all modes by mode,
#' all waves, all areas by area;
#' selected all bluefish associated trips - if it was a target species, if it was
#' caught, if it was released
#'
#' @export

create_bluefish_trips <- function(files, return = TRUE) {
  bluefish_directed_trips <- c()
  for (i in files) {
    this_dat <- read.csv(i,
      skip = 24,
      na.strings = "."
    )
    message(unique(this_dat$Year)) # make sure all years are downloaded
    bluefish_directed_trips <- rbind(bluefish_directed_trips, this_dat)
  }

  bluefish_trips <- bluefish_directed_trips %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(DATA_VALUE = sum(Directed.Trips, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "Bluefish_trips") %>%
    dplyr::rename(YEAR = Year)

  usethis::use_data(bluefish_trips, overwrite = TRUE)

  if(return) return(bluefish_trips)

}

#' Create proportion trips targeting bluefish indicator
#'
#' This function creates the proportion trips targeting bluefish indicator
#' @param total The mrip trip data (R object `mrip_effort`)
#' @param bluefish The bluefish directed trips (R object `bluefish_trips`)
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `prop_bluefish_trips`
#' @export


create_prop_bluefish_trips <- function(total = mrip_effort,
                                       bluefish = bluefish_trips,
                                       return = TRUE){
  total_trips <- total %>%
    dplyr::filter(sub_reg %in% 4:6) %>% # atlantic coast only
    dplyr::group_by(year) %>%
    dplyr::summarise(total_trips = sum(as.numeric(estrips), na.rm = TRUE)) %>%
    dplyr::mutate(YEAR = as.numeric(year)) %>%
    dplyr::select(-year)

  prop_bluefish_trips <- dplyr::full_join(total_trips,
                                          bluefish,
                                          by = c("YEAR")) %>%
    dplyr::mutate(DATA_VALUE = DATA_VALUE/total_trips,
                  INDICATOR_NAME = "proportion_bluefish_trips") %>%
    dplyr::select(-total_trips)

  usethis::use_data(prop_bluefish_trips, overwrite = TRUE)
  if(return) return(prop_bluefish_trips)
}

#' Create total recreational catch indicator
#'
#' This function creates the total recreational catch indicator
#' @param data The mrip data (R object `mrip_catch`), already subset to bluefish only
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `mrip_catch`
#' @export
#'

create_total_rec_catch <- function(data = mrip_catch, return = TRUE){
  total_rec_catch <- data %>%
    dplyr::filter(sub_reg %in% 4:6) %>% # atlantic coast only
    dplyr::group_by(year) %>%
    dplyr::summarise(DATA_VALUE = sum(tot_cat, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "total_recreational_catch_n") %>%
    dplyr::rename(YEAR = year) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))

  usethis::use_data(total_rec_catch, overwrite = TRUE)
  if(return) return(total_rec_catch)
}

#' Create bluefish recreational landings indicator
#'
#' This function creates the bluefish recreational landings indicator
#' @param data The mrip catch data (R object `mrip_catch`)
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `total_rec_landings`
#' @export

create_total_rec_landings <- function(data = mrip_catch, return = TRUE){
  total_rec_landings <- data %>%
    dplyr::filter(sub_reg %in% 4:6) %>% # atlantic coast only
    dplyr::group_by(year) %>%
    dplyr::summarise(DATA_VALUE = sum(lbs_ab1, na.rm = TRUE)) %>%
    dplyr::mutate(CATEGORY = "Recreational",
                  INDICATOR_TYPE = "Socioeconomic",
                  INDICATOR_NAME = "total_recreational_landings_lbs") %>%
    dplyr::rename(YEAR = year) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))

  usethis::use_data(total_rec_landings, overwrite = TRUE)

  if(return) return(total_rec_landings)
}


