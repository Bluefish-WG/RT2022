plot_corr <- function(data, x, y, title){

  if(stringr::str_detect(x, "condition")){
    data <- data %>%
      dplyr::mutate(Condition := !!rlang::sym(x))

    plt <- data %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "Condition",
                                          y = y,
                                          color = "YEAR"))
  } else {
    plt <- data %>%
      ggplot2::ggplot(ggplot2::aes_string(x = x,
                                          y = y,
                                          color = "YEAR"))
  }

 plt <- plt +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::theme_bw() +
    viridis::scale_color_viridis() +
    ggplot2::ggtitle(title)

  print(plt)

  cc <- cor(data[,x], data[,y]) %>%
    round(digits = 3)

  cat("\n\n", "Correlation coefficient:", cc, "\n\n")

#
#   model <- lm(data[,y] ~ data[,x])
#
#   print("Model summary:\n")
#   print(summary(model))
}
