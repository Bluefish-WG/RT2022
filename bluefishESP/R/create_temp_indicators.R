#' Create first 18 degree day temperature indicator
#'
#' This function transforms a bluefish temperature indicator from an input spreadsheet to an R object.
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `first_18_day`, returns temperature indicator
#' @importFrom magrittr `%>%`
#' @export
#'
create_first_18_day <- function(data, return = TRUE){
  first_18_day <- data %>%
    read.csv() %>%
    dplyr::select(years, first) %>%
    dplyr::mutate(CATEGORY = "Climate",
                  INDICATOR_TYPE = "Ecosystem",
                  INDICATOR_NAME = "first_18c_day") %>%
    dplyr::rename(YEAR = years,
                  DATA_VALUE = first) %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  usethis::use_data(first_18_day, overwrite = TRUE)

  if(return) return(first_18_day)
}

#' Create last 18 degree day temperature indicator
#'
#' This function transforms a bluefish temperature indicator from an input spreadsheet to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `last_18_day`, returns temperature indicator
#' @importFrom magrittr `%>%`
#' @export
#'
create_last_18_day <- function(data, return = TRUE){
  last_18_day <- data %>%
    read.csv() %>%
    dplyr::select(years, last) %>%
    dplyr::mutate(CATEGORY = "Climate",
                  INDICATOR_TYPE = "Ecosystem",
                  INDICATOR_NAME = "last_18c_day") %>%
    dplyr::rename(YEAR = years,
                  DATA_VALUE = last) %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  usethis::use_data(last_18_day, overwrite = TRUE)

  if(return) return(last_18_day)
}

#' Create number of 18 degree days temperature indicator
#'
#' This function transforms a bluefish temperature indicator from an input spreadsheet to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `n_18_day`, returns temperature indicator
#' @importFrom magrittr `%>%`
#' @export
#'
create_n_18_day <- function(data, return = TRUE){
  n_18_day <- data %>%
    read.csv() %>%
    dplyr::select(years, n_days) %>%
    dplyr::mutate(CATEGORY = "Climate",
                  INDICATOR_TYPE = "Ecosystem",
                  INDICATOR_NAME = "n_18c_day") %>%
    dplyr::rename(YEAR = years,
                  DATA_VALUE = n_days) %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))
  usethis::use_data(n_18_day, overwrite = TRUE)

  if(return) return(n_18_day)
}

#' Create July temperature indicator
#'
#' This function transforms a bluefish temperature indicator from an input spreadsheet to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `july_proportion`, returns temperature indicator
#' @importFrom magrittr `%>%`
#' @export
#'
create_july_proportion <- function(data, return = TRUE){
  july_proportion <- data %>%
    read.csv() %>%
    dplyr::select(-X) %>%
    tidyr::pivot_longer(cols = c("prop_july", "prop_july_bf",
                                 "prop_july_cold", "prop_july_bf_cold",
                                 "prop_july_warm", "prop_july_bf_warm")) %>%
    dplyr::filter(stringr::str_detect(name, "bf", negate = TRUE)) %>%
    dplyr::mutate(INDICATOR_NAME = ifelse(stringr::str_detect(name, "cold"), "july_proportion_under_18C",
                                          ifelse(stringr::str_detect(name, "warm"), "july_proportion_over_25.6C",
                                                 "july_proportion_18-25.6C"))) %>%
    dplyr::select(-name) %>%
    dplyr::rename(YEAR = years,
                  DATA_VALUE = value) %>%
    dplyr::mutate(CATEGORY = "Climate",
                  INDICATOR_TYPE = "Ecosystem") %>%
    dplyr::mutate(YEAR = as.numeric(YEAR))

  usethis::use_data(july_proportion, overwrite = TRUE)

  if(return) return(july_proportion)
}
