
#' Create commercial indicators
#'
#' This function transforms bluefish commercial indicators from an input spreadsheet to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicators as an object in the global environment
#' @return Saves R object `comm_indicators`, returns commercial indicators
#' @export
#'

create_comm_indicators <- function(data = here::here("data-raw/share/SOCIEOECONOMIC_COMMERCIAL_INDICATORS_FINAL.csv"),
                                   return = TRUE){
  comm_indicators <- read.csv(data)

  usethis::use_data(comm_indicators, overwrite = TRUE)

  if(return) return(comm_indicators)
}

