#' Create relative condition
#'
#' This function creates relative condition indicators
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `relative_condition`
#' @importFrom magrittr `%>%`
#' @export
#'

create_relative_condition <- function(#data = bluefishLifeHistory::raw_survey_data,
                                      return = TRUE){
  # bad practice but necessary until available from github
  devtools::load_all("../bluefishLifeHistory")
  data <- bluefishLifeHistory::raw_survey_data

  # message("dummy message")

  # make bio data - no age adjustments
  bio <- data %>%
    bluefishLifeHistory::interpolate_lengths() %>%
    bluefishLifeHistory::remove_outliers() %>%
    tidyr::drop_na(ForkLengthCM,
                   WeightKG,
                   Semester) %>%
    dplyr::ungroup()

  # model <- bio %>%
  #   dplyr::group_by(Semester) %>%
  #   dplyr::mutate(logForkLengthCM = log(ForkLengthCM),
  #                 logWeightKG = log(WeightKG)) %>%
  #   dplyr::group_by(Semester, ForkLengthCM) %>%
  #   dplyr::mutate(Wt = 1/ dplyr::n()) %>%
  #   dplyr::ungroup () %>%
  #   dplyr::group_by(Semester) %>%
  #   do(fit = tidy(lm(logWeightKG ~ logForkLengthCM, data = .,
  #                    weights = Wt))) %>%
  #   tidyr::unnest(fit) %>%
  #   dplyr::ungroup() %>%
  #
  #   # Remove unnecessary columns and re-organize the data
  #   dplyr::select(-c(std.error, statistic, p.value)) %>%
  #   tidyr::pivot_wider(names_from = term, values_from = estimate) %>%
  #   dplyr::rename('beta0' = `(Intercept)`,
  #                 'beta1' = logForkLengthCM) %>%
  #   dplyr::mutate(a = exp(beta0),
  #                 b = beta1)

  # use model output from LH paper
  model <- tibble::tibble(a = c(0.0000140, 0.0000120),
                          b = c(2.957, 3.027),
                          Semester = c("1", "2"))

  condition_data <- bio %>%
    dplyr::select(Year, WeightKG, ForkLengthCM, Semester) %>%
    dplyr::left_join(model %>%
                       dplyr::select(a, b, Semester)) %>%
    dplyr::mutate(predicted_weight = a * ForkLengthCM^b,
                  relative_condition = WeightKG/predicted_weight,
                  size_group = ifelse(ForkLengthCM <= 30.3,
                                      "small (<=30.3cm)",
                                      ifelse(ForkLengthCM >= 50.0,
                                             "large (>=50.0cm)",
                                             "medium (30.3-50.0cm)")))

  relative_condition <- condition_data %>%
    dplyr::group_by(Year, size_group, Semester) %>%
    dplyr::summarise(mean_condition = mean(relative_condition),
                     n_fish = dplyr::n()) %>%
    dplyr::filter(n_fish > 30) %>%
    tidyr::drop_na(Semester) %>%
    dplyr::mutate(name = ifelse(Semester == 1,
                                paste(size_group, "spring condition"),
                                paste(size_group, "fall condition"))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(Semester, size_group, n_fish)) %>%
    dplyr::rename(DATA_VALUE = mean_condition,
                  INDICATOR_NAME = name,
                  YEAR = Year) %>%
    dplyr::mutate(INDICATOR_TYPE = "Ecosystem",
                  CATEGORY = "Natural mortality")

  usethis::use_data(relative_condition, overwrite = TRUE)

  if(return) return(relative_condition)
}



# plots
#
# summary <- summary %>%
#   dplyr::rename(DATA_VALUE = mean_condition,
#                 YEAR = Year) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(size_group, Semester) %>%
#   dplyr::mutate(mean = mean(DATA_VALUE, na.rm = TRUE),
#                 sd = sd (DATA_VALUE, na.rm = TRUE),
#                 name = ifelse(size_group == "small (<=35cm)",
#                               "small fish relative condition",
#                               "large fish relative condition"),
#                 Semester = ifelse(Semester == 1, "Spring", "Fall"),
#                 name = paste(name, Semester))
# summary %>%
#   tidyr::drop_na(Semester) %>%
#   esp_traffic_fig(status = FALSE)
#
# large <- summary %>% dplyr::filter(size_group != "small (<=35cm)")
# small <- summary %>% dplyr::filter(size_group == "small (<=35cm)")
# plot(large$YEAR - 3, large$DATA_VALUE, type = "b", ylim = c(0.9, 1.2))
# lines(small$YEAR, small$DATA_VALUE, type = "b", col = "red")
#
# dat <- dplyr::full_join(large %>% dplyr::filter(Semester == 1),
#                         NEesp::asmt %>%
#                           dplyr::filter(Species == "Bluefish",
#                                         Metric == "Recruitment",
#                                         AssessmentYear == 2019),
#                         by = c("YEAR" = "Year"))
# plot(dat$YEAR, dat$DATA_VALUE/mean(dat$DATA_VALUE, na.rm = TRUE), type = "b", ylim = c(0.5, 2.2))
# lines(dat$YEAR, dat$Value/mean(dat$Value, na.rm = TRUE), type = "b", col = "red")
#
# plot(dat$DATA_VALUE, dat$Value)
#
# dat <- dplyr::full_join(large %>% dplyr::filter(Semester == 2),
#                         NEesp::asmt %>%
#                           dplyr::filter(Species == "Bluefish",
#                                         Metric == "Recruitment",
#                                         AssessmentYear == 2019),
#                         by = c("YEAR" = "Year"))
# plot(dat$YEAR, dat$DATA_VALUE/mean(dat$DATA_VALUE, na.rm = TRUE), type = "b", ylim = c(0.5, 2.2))
# lines(dat$YEAR - 1, dat$Value/mean(dat$Value, na.rm = TRUE), type = "b", col = "red")
#
# plot(dat$DATA_VALUE, dat$Value)
#
# dat <- dplyr::full_join(large %>%
#                           tidyr::drop_na(Semester),
#                         NEesp::asmt %>%
#                           dplyr::filter(Species == "Bluefish",
#                                         Metric == "Recruitment",
#                                         AssessmentYear == 2019) %>%
#                           dplyr::mutate(Year = Year - 1),
#                         by = c("YEAR" = "Year"))
# dat %>% ggplot2::ggplot(ggplot2::aes(x = DATA_VALUE,
#                                      y = Value,
#                                      color = Semester)) +
#   ggplot2::geom_line() +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method = "lm")
#
# large %>%
#   tidyr::drop_na(Semester) %>%
#   ggplot2::ggplot(ggplot2::aes(x = YEAR,
#                                      y = DATA_VALUE,
#                                      color = Semester)) +
#   ggplot2::geom_line() +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method = "lm")
#
# small %>%
#   tidyr::drop_na(Semester) %>%
#   ggplot2::ggplot(ggplot2::aes(x = YEAR,
#                                y = DATA_VALUE,
#                                color = Semester)) +
#   ggplot2::geom_line() +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method = "lm")
