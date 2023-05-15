#' Plot indicator traffic light figure
#'
#' This function plots ESP indicator traffic light figures
#' @param data The ESP indicator data. Should have a column for Year and a column with indicator values.
#' @param name The file name for the image. Will be saved relative to the working directory.
#' @param out Whether the function should save the plot, print a ggplot object in markdown, or return a ggplot object to the working environment (for use with `one_pager()`). One of c("ggplot", "save", "one_pager")
#' @param paginate Whether to paginate the plots with `ggforce::facet_wrap_paginate`
#' @param label Whether to label the facets with a, b, c, etc.
#' @param status Whether to label the facets with the indicator status
#' @param caption A caption for the figure
#' @param ncolumn How many columns the figure should have (1 by default)
#' @param silent Whether to print the caption
#' @param min_year The minimum year to show on the plots. If left NULL (the default), the minimum year will be the first year of the dataset.
#' @param ... Passed to `ggplot2::ggsave`
#' @return An image file
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

esp_traffic_fig <- function(data,
                             name,
                             out = "one_pager",
                             paginate = FALSE,
                             label = TRUE,
                             status = TRUE,
                             caption = "",
                             ncolumn = 1,
                             silent = FALSE,
                             min_year = 1985,
                             ...) {
  maxyear <- max(data$YEAR)
  minyear <- maxyear - 1

  ### data prep ----
  ### write code
  ### colnames being used in current code:
  ### YEAR, DATA_VALUE, mean, sd, name (indicator name), label_num, score,

  dat <- data %>%
    dplyr::group_by(INDICATOR_NAME) %>%
    dplyr::mutate(mean = mean(DATA_VALUE, na.rm = TRUE),
                  sd = sd(DATA_VALUE, na.rm = TRUE),
                  name = stringr::str_wrap(INDICATOR_NAME,
                                           width = 75)) %>%
    dplyr::arrange(indicator_order)

  dat$name <- factor(dat$name, levels = unique(dat$name))

  # base figure ----
  plt <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$YEAR,
      y = .data$DATA_VALUE,
      group = .data$name
    )
  ) +
    ggplot2::geom_hline(ggplot2::aes(
      yintercept = .data$mean + .data$sd,
      group = .data$name
    ),
    color = "darkgreen",
    linetype = "solid"
    ) +
    ggplot2::geom_hline(ggplot2::aes(
      yintercept = .data$mean - .data$sd,
      group = .data$name
    ),
    color = "darkgreen",
    linetype = "solid"
    ) +
    ggplot2::geom_hline(ggplot2::aes(
      yintercept = .data$mean,
      group = .data$name
    ),
    color = "darkgreen",
    linetype = "dotted"
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(data = dat %>%
                         tidyr::drop_na(.data$DATA_VALUE)) +
    ggplot2::ylab("") +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                n.breaks = 4) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 10),
                   aspect.ratio = 0.25)

  # test linear trend and add line if there is a trend
  pdat <- dat %>%
    tidyr::drop_na(DATA_VALUE) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(n_obs = dplyr::n()) %>%
    dplyr::filter(n_obs > 30)

  if(nrow(pdat) > 0){
    pdat <- pdat %>%
      # Fit the linear model for each grouping
      dplyr::do(fit = broom::tidy(lm(DATA_VALUE ~ YEAR, data = .))) %>%
      tidyr::unnest(fit) %>%
      dplyr::mutate(sig = ifelse(p.value < 0.05, "yes", "no")) %>%
      dplyr::filter(term == "YEAR") %>%
      dplyr::select(name, sig)

    dat <- dplyr::full_join(dat,
                            pdat,
                            by = "name") %>%
      dplyr::mutate(sig = ifelse(is.na(sig), "no", sig))

    plt <- plt +
      ggplot2::geom_smooth(data = dat %>%
                             dplyr::filter(sig == "yes"),
                           method = "lm",
                           se = FALSE,
                           color = "darkgreen",
                           cex = 1.5)
  }

    # add status ----
  if (status) {
    stat_dat <- dat %>%
      dplyr::filter(
        .data$YEAR == maxyear
      )

    # status shapes/colors
    plt <- plt + ggplot2::geom_point(
      data = stat_dat,
      ggplot2::aes(
        x = .data$YEAR + 1,
        y = .data$mean,
        shape = as.factor(.data$label_num),
        fill = as.factor(.data$score)
      ),
      show.legend = FALSE,
      cex = 4
    ) +
      ggplot2::scale_shape_manual(values = c("-1" = 25, "0" = 21, "1" = 24)) +
      ggplot2::scale_fill_manual(values = c(
        "-1" = "brown1",
        "0" = "beige",
        "1" = "cornflowerblue"
      ))

    # also add + - for 508
    plt <- plt +
      ggnewscale::new_scale(new_aes = "shape") +
      ggplot2::geom_point(
        data = stat_dat,
        ggplot2::aes(
          x = .data$YEAR + 1.05,
          y = .data$mean,
          shape = as.factor(.data$score)
        ),
        show.legend = FALSE,
        cex = 4,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_shape_manual(values = c("-1" = "-", "0" = NA, "1" = "+"))
  }

  if (status) {
    if (is.null(min_year)) {
      plt <- plt +
        ggplot2::xlim(c(min(dat$YEAR), max(dat$YEAR) + 1.5))
    } else {
      plt <- plt +
        ggplot2::xlim(c(min_year, max(dat$YEAR) + 1.5))
    }
  }

  if (!status) {
    if (is.null(min_year)) {
      plt <- plt +
        ggplot2::xlim(c(min(dat$YEAR), max(dat$YEAR) + 0.5))
    } else {
      plt <- plt +
        ggplot2::xlim(c(min_year, max(dat$YEAR) + 0.5))
    }
  }

  # label facets ----
  finish_fig <- function() {
    if (label) {
      plt <- plt %>%
        AKesp::label_facets(open = "", close = "")
    }

    if (out == "save") {
      ggplot2::ggsave(plt, filename = paste0(name, "_page", i, ".png"), ...)
    } else if (out == "ggplot") {
      print(plt)
      cat("\n\n")
      if (silent == FALSE) {
        cat("##### Figure \\@ref(fig:traffic).", caption, "{-}")
      }
      cat("\n\n")
    } else if (out == "one_pager") {
      return(plt)
    }
    else {
      stop("Please specify output format")
    }
  }

  if (paginate == TRUE) {
    plt2 <- plt +
      ggforce::facet_wrap_paginate(~name,
                                   ncol = ncolumn,
                                   nrow = 5,
                                   scales = "free_y"
      )

    n <- ggforce::n_pages(plt2)

    for (i in 1:n) {
      plt <- plt +
        ggforce::facet_wrap_paginate(~name,
                                     ncol = ncolumn,
                                     nrow = 5,
                                     scales = "free_y",
                                     page = i
        )

      finish_fig()
    }
  } else {
    plt <- plt +
      ggplot2::facet_wrap(~name,
                          ncol = ncolumn,
                          scales = "free_y"
      )

    finish_fig()
  }
}
