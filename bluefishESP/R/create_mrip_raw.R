#' Create MRIP *raw* catch
#'
#' This function creates MRIP raw catch from a .RDS file
#' @param rds The .RDS file with the MRIP raw catch data
#' @return Updates the package object `mrip_catch_raw`
#' @importFrom magrittr `%>%`
#' @export

create_mrip_catch_raw <- function(rds = here::here("data-raw/catch.RDS")) {
  mrip_catch_raw <- readRDS(rds) %>%
    dplyr::mutate(kod = ifelse(kod == "wd", "weekday", "weekend")) %>%
    dplyr::left_join(area_key, by = "area_x") %>%
    dplyr::left_join(fl_key, by = "fl_reg") %>%
    dplyr::left_join(mode_key, by = "mode_fx") %>%
    dplyr::left_join(st_key, by = "st") %>%
    dplyr::select(-c(area_x, fl_reg, mode_fx, st)) %>%
    dplyr::rename(
      area_x = area_x_words,
      fl_reg = fl_reg_words,
      mode_fx = mode_fx_words,
      st = st_words
    ) %>%
    dplyr::select(-c(strat_id, psu_id, id_code, sp_code))

  # save data to package
  usethis::use_data(mrip_catch_raw, overwrite = TRUE)
}

#' Create MRIP *raw* trips
#'
#' This function creates MRIP raw trips from a .RDS file
#' @param rds The .RDS file with the MRIP raw trip data
#' @return Updates the package object `mrip_trip_raw`
#' @importFrom magrittr `%>%`
#' @export

create_mrip_trip_raw <- function(rds = here::here("data-raw/trip.RDS")) {
  mrip_trip_raw <- readRDS(rds)

  # try making mrip_trip_raw smaller because it takes too long to load
  mrip_trip_raw <- mrip_trip_raw %>%
    dplyr::select(
      prim2_common, prim1_common, mode_fx, mode_f,
      area_x, area, st, wave, year, month
    )

  # save to package
  usethis::use_data(mrip_trip_raw, overwrite = TRUE)
}
