conn <- read.csv(here::here("data-raw/Conn YOY.csv")) %>%
  dplyr::select(Year, Index)

bluefish <- NEesp::asmt %>%
  dplyr::filter(Species == "Bluefish",
                Metric == "Recruitment" |
                  Metric == "Biomass",
                AssessmentYear == 2019) %>%
  dplyr::select(Year, Metric, Value) %>%
  tidyr::pivot_wider(names_from = "Metric", values_from = "Value") %>%
  dplyr::mutate(rssb = Recruitment/Biomass)

bluefish <- bluefish %>%
  dplyr::full_join(conn) %>%
  dplyr::rename(YEAR = Year,
                Conn = Index) %>%
  dplyr::mutate(conn_per_ssb = Conn/Biomass) %>%
  dplyr::select(-Biomass) %>%
  tidyr::pivot_longer(cols = c("Recruitment", "rssb", "Conn", "conn_per_ssb"),
                      names_to = "Bluefish") #%>%
  # dplyr::full_join(total_rec_catch %>%
  #                    dplyr::select(YEAR, DATA_VALUE, INDICATOR_NAME),
  #                  by = c('YEAR',
  #                         "Bluefish" = "INDICATOR_NAME",
  #                           "value" = "DATA_VALUE")) %>%
  # dplyr::full_join(comm_indicators %>%
  #                    dplyr::filter(INDICATOR_NAME == "Commercial_Bluefish_Landings_LBS") %>%
  #                    dplyr::select(YEAR, DATA_VALUE, INDICATOR_NAME),
  #                  by = c('YEAR',
  #                         "Bluefish" = "INDICATOR_NAME",
  #                         "value" = "DATA_VALUE"))

plt_cor <- function(data){
  plt <- data %>%
    dplyr::left_join(bluefish, by = "YEAR") %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot(ggplot2::aes(y = value,
                                 x = DATA_VALUE,
                                 color = YEAR)) +
    ggplot2::geom_point() +
    ggplot2::stat_smooth(method = "lm") +
    ggplot2::facet_grid(cols = ggplot2::vars(INDICATOR_NAME),
                        rows = ggplot2::vars(Bluefish),
                        scales = "free") +
    ggplot2::theme_bw() +
    nmfspalette::scale_color_nmfs(palette = "regional web",
                                  discrete = FALSE)
  return(plt)
}


wind_indicators %>%
  plt_cor()

first_18_day %>%
  plt_cor()

last_18_day %>%
  plt_cor()

n_18_day %>%
  plt_cor()

july_proportion %>%
  plt_cor()

relative_condition %>%
  dplyr::filter(stringr::str_detect(INDICATOR_NAME, "spring")) %>%
  plt_cor()

relative_condition %>%
  dplyr::filter(stringr::str_detect(INDICATOR_NAME, "fall")) %>%
  dplyr::mutate(YEAR = YEAR + 1) %>%
  plt_cor()

big_dat <- july_proportion %>%
  dplyr::full_join(mako) %>%
  dplyr::full_join(first_18_day) %>%
  dplyr::full_join(last_18_day) %>%
  dplyr::full_join(n_18_day) %>%
  dplyr::full_join(wind_indicators) %>%
  dplyr::full_join(relative_condition) %>%
  dplyr::full_join(comm_indicators %>%
                     dplyr::filter(INDICATOR_NAME == "AVG_ANNUAL_DIESEL_FUEL_PRICE_REAL_DOLLARS")) %>%
  dplyr::left_join(bluefish, by = "YEAR") %>%
  tidyr::drop_na(DATA_VALUE, value) %>%
  dplyr::group_by(Bluefish, INDICATOR_NAME, CATEGORY) %>%
  dplyr::summarise(cor = cor(DATA_VALUE, value),
                   pval = summary(lm(value ~ DATA_VALUE))$coefficients[2,4],
                   info = paste("Correlation coefficient:", round(cor, digits = 3),
                                "p-value:", round(pval, digits = 3))) %>%
  dplyr::select(-cor, -pval) %>%
  tidyr::pivot_wider(names_from = "Bluefish", values_from = "info") %>%
  dplyr::arrange(CATEGORY) %>%
  dplyr::mutate(INDICATOR_NAME = INDICATOR_NAME %>%
                  stringr::str_replace_all("_", " ") %>%
                  stringr::str_to_sentence())
big_dat

colnames(big_dat) <- colnames(big_dat) %>%
  stringr::str_replace_all("_", " ") %>%
  stringr::str_to_sentence()

write.csv(big_dat, file = here::here("data-raw/yoy_correlations_2022-08-26.csv"))

dat <- relative_condition %>%
  dplyr::filter(INDICATOR_NAME == "medium (31.2-49.6cm) spring condition") %>%
  dplyr::full_join(bluefish %>%
                     dplyr::filter(Bluefish == "Conn"))

lm(DATA_VALUE ~ value, data = dat) %>%
  summary()


par(mfrow = c(2,2))
for(j in unique(relative_condition$INDICATOR_NAME)) {

  x <- relative_condition %>%
    dplyr::filter(INDICATOR_NAME == j) %>%
    dplyr::select(YEAR, DATA_VALUE)

for(i in unique(bluefish$Bluefish)) {

  y <- bluefish %>%
    dplyr::filter(Bluefish == i) %>%
    dplyr::select(YEAR, value)

  if(i == "Biomass"){

  } else {

    dat <- dplyr::full_join(x, y)

    # print(dat)

    ccf(dat$DATA_VALUE, dat$value, na.action = na.pass,
        main = paste(j, "vs", i), xlim = c(-3, 0))
    }
    }
}

pounds_landed_per_n_caught <-
  dplyr::full_join(total_rec_landings %>%
                     dplyr::select(YEAR, DATA_VALUE) %>%
                     dplyr::rename(Landings = DATA_VALUE),
                   total_rec_catch %>%
                     dplyr::select(YEAR, DATA_VALUE) %>%
                     dplyr::rename(Catch = DATA_VALUE)) %>%
  dplyr::mutate(landed_per_catch = Landings/Catch)

pounds_landed_per_n_caught %>%
  ggplot2::ggplot(ggplot2::aes(x = YEAR,
                               y = landed_per_catch)) +
  ggplot2::geom_line() +
  ggplot2::geom_point()
