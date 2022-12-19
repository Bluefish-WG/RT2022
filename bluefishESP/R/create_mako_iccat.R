#' Create mako shark indicator
#'
#' This function creates the mako shark indicator
#' @param file A .xlsx with the ICCAT mako population model outputs
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `mako`
#' @export
#'

create_mako <- function(file = here::here("data-raw/SMA2017_Bratio.xlsx"),
                        return = TRUE) {
  mako <- readxl::read_excel(file,
    skip = 12
  ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(YEAR = Runs) %>%
    # dplyr::select(YEAR, Pella_C1,
    #               Schaefer_C1...4,
    #               Schaefer_C1...6,
    #               generalized_C1,
    #               run3) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::mutate(DATA_VALUE = mean(c(
      Pella_C1,
      Pella_C2,
      Schaefer_C1...4,
      Schaefer_C2...5,
      Schaefer_C1...6,
      Schaefer_C2...7,
      generalized_C1,
      generalized_C2,
      run3
    ),
    na.rm = TRUE
    )) %>%
    dplyr::select(YEAR, DATA_VALUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(CATEGORY = "Natural mortality",
                  INDICATOR_TYPE = "Ecosystem",
                  INDICATOR_NAME = "mako_avg_bbmsy")

  usethis::use_data(mako, overwrite = TRUE)
  if(return) return(mako)
}
