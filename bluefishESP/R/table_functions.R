
#' Create indicator traffic light table - LONG data
#'
#' This function creates an ESP indicator traffic light table
#' @param data The ESP indicator data.
#' @param year The year(s) to use for the traffic light analysis. Either a single number or a numeric vector.
#' @param cap The table caption.
#' @return A flextable
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

esp_traffic_tab <- function(data, year, cap = "Summary of recent indicator values") {

  dat <- data %>%
    dplyr::group_by(.data$INDICATOR_NAME) %>%
    dplyr::mutate(
      name = .data$facet_label,
      this_year = (.data$YEAR %in% year),
      avg = mean(.data$DATA_VALUE,
        na.rm = TRUE
      ),
      stdev = stats::sd(.data$DATA_VALUE,
        na.rm = TRUE
      )
    )  %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$this_year == TRUE) %>%
    dplyr::select(
      .data$CATEGORY, .data$YEAR, .data$name, .data$DATA_VALUE,
      .data$avg, .data$stdev, .data$INDICATOR_TYPE, .data$indicator_order,
      .data$figure_number
    ) %>%
    dplyr::mutate(status = dplyr::case_when(.data$DATA_VALUE > (.data$avg + .data$stdev) ~ "high",
                                            .data$DATA_VALUE < (.data$avg - .data$stdev) ~ "low",
                                            .data$DATA_VALUE <= (.data$avg + .data$stdev) &
                                              .data$DATA_VALUE >= (.data$avg - .data$stdev) ~ "neutral"
                                            ))

  # dat <- join_order(dat)
  tbl_dat <- dat %>%
    dplyr::select(.data$CATEGORY, .data$name, .data$YEAR, .data$status,
                  .data$indicator_order, .data$figure_number) %>%
    tidyr::pivot_wider(
      id_cols = c(.data$CATEGORY, .data$name, .data$indicator_order, .data$figure_number),
      names_from = .data$YEAR,
      values_from = .data$status,
      names_sort = TRUE
    ) %>%
    dplyr::arrange(figure_number, indicator_order) %>%
    dplyr::select(-indicator_order, -figure_number) %>%
    dplyr::rename(
      Indicator = .data$name,
      "Indicator category" = .data$CATEGORY
    )

  colnames(tbl_dat)[3:ncol(tbl_dat)] <- paste(
    colnames(tbl_dat)[3:ncol(tbl_dat)], "Status"
  )

  flextable::set_flextable_defaults(na_str = "NA")

  ft <- flextable::flextable(tbl_dat) %>%
    flextable::theme_vanilla() %>%
    flextable::set_caption(caption = cap) %>%
    flextable::autofit() %>%
    flextable::align(
      align = "center",
      j = 3:ncol(tbl_dat)
    )
#
#   ft <- ft %>%
#     flextable::merge_v(j = 1)

  return(ft)
}

# esp_traffic_tab(data, 2017:2021)

