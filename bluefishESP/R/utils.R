#' Download MRIP data
#'
#' This function downloads MRIP data from a URL and unzips it into the specified folder
#' @param url A list of the URLs to download
#' @return Downloads MRIP data
#' @export
#'

download_and_extract <- function(url,
                                 pattern = "ps_.{1,}",
                                 exdir = "MRIP_data") {
  name <- stringr::str_extract(url, pattern = pattern)
  download.file(url, name)

  unzip(here::here(name),
    exdir = exdir
  )
}

#' Extract bluefish MRIP data
#'
#' This function compiles MRIP data from individual spreadsheets into an intermediate .RDS file
#' @param type The type of MRIP data to compile. One of c("effort", "catch")
#' @param dir The directory with the spreadsheets
#' @param outdir Where to save the .RDS file
#' @param species The species common name as it appears in the MRIP data (probably in all caps)
#' @return Saves the intermediate .RDS object of compiled MRIP effort or catch
#' @importFrom magrittr `%>%`
#' @export
#'

compile <- function(type,
                    dir = "MRIP_data",
                    outdir = "data-raw",
                    species = "BLUEFISH") {
  files <- list.files(here::here(dir),
    pattern = type,
    full.names = TRUE
  )

  big_data <- tibble::tibble()
  for (i in 1:length(files)) {
    this_data <- read.csv(files[i],
      colClasses = "character"
    ) %>%
      tibble::as_tibble()

    colnames(this_data) <- stringr::str_to_lower(colnames(this_data))

    if (nrow(this_data) > 0) {
      if (type == "catch" |
        type == "catch_wave") {
        this_data <- this_data %>%
          dplyr::filter(stringr::str_detect(
            common,
            species
          ))
      }
      # keep all species for trip data - to calculate % bluefish trips

      if (nrow(big_data) == 0) {
        big_data <- this_data
      }

      big_data <- dplyr::full_join(big_data, this_data) %>%
        invisible()

      print((i / length(files)) %>% round(digits = 2)) # keep track of progress
    }
  }
  saveRDS(big_data,
    file = paste0(outdir, "/", type, ".RDS")
  )
}

#' Combine indicator data
#'
#' This function combines indicator data
#' @param ... R objects
#' @return Combined indicator data
#' @export
#'

test_combine <- function(...){
  data <- rbind(...)
  print(data)
}
