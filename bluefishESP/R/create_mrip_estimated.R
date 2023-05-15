#' Create MRIP *estimated* catch
#'
#' This function creates MRIP estimated catch from a .RDS file
#' @param rds The .RDS file with the MRIP estimated catch data
#' @return Updates the package object `mrip_catch`
#' @importFrom magrittr `%>%`
#' @export

create_mrip_catch <- function(rds = here::here("data-raw/catch_wave.RDS"),
                              return = TRUE) {
  mrip_catch <- readRDS(rds)
  mrip_catch <- mrip_catch %>%
    dplyr::select(-c(status, wave, st, sp_code, mode_fx, area_x)) %>%
    as.data.frame()

  for (i in 8:24) {
    mrip_catch[, i] <- as.numeric(gsub(",", "", mrip_catch[, i]))
  }

  mrip_catch <- tibble::as_tibble(mrip_catch)
  usethis::use_data(mrip_catch, overwrite = TRUE)

  if(return) return(mrip_catch)
}

#' Create MRIP *estimated* effort
#'
#' This function creates MRIP estimated effort from a .RDS file
#' @param rds The .RDS file with the MRIP estimated effort data
#' @return Updates the package object `mrip_effort`
#' @importFrom magrittr `%>%`
#' @export

create_mrip_effort <- function(rds = here::here("data-raw/effort.RDS"),
                               return = TRUE) {
  mrip_effort <- readRDS(rds)

  mrip_effort <- mrip_effort %>%
    dplyr::mutate(
      estrips = estrips %>%
        stringr::str_remove_all(",")
    ) %>%
    dplyr::left_join(area_key, by = "area_x") %>%
    dplyr::left_join(mode_key, by = "mode_fx") %>%
    dplyr::left_join(st_key, by = "st") %>%
    dplyr::select(-c(area_x, #fl_reg,
                     mode_fx, st)) %>%
    dplyr::rename(
      area_x = area_x_words,
      # fl_reg = fl_reg_words,
      mode_fx = mode_fx_words,
      st = st_words
    ) %>%
    tidyr::drop_na(st) # don't include states outside of the atlantic coast

  usethis::use_data(mrip_effort, overwrite = TRUE)

  if(return) return(mrip_effort)
}
