#' Create community indicators
#'
#' This function creates community indicators: mean regional engagement and mean regional reliance
#' @param data The engagement and reliance data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves the R data object `community_indicators`
#' @importFrom magrittr `%>%`
#' @export
#'

create_community_indicators <- function(filepath = here::here("data-raw/2009-2019 Rec Scores_ME_MS_031522 cw.xlsx"),
                                        return = TRUE){
  data <- c()
    for(i in 2009:2019){
    this_data <- readxl::read_excel(filepath,
                               sheet = as.character(i),
                               na = "N/A") %>%
      dplyr::mutate(YEAR = i)

    data <- rbind(data, this_data)
  }

    community_indicators <- data %>%
      dplyr::mutate(REGION = ifelse(STATEABBR %in% c("NJ", "DE", "MD", "NY", "VA"),
                                    "Mid Atlantic", REGION)) %>%
      dplyr::filter(PRIMARY_LONGITUDE > -81.25) %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(regional_mean_eng = mean(RecEng, na.rm = TRUE),
                       regional_mean_rel = mean(RecRel, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(cols = c("regional_mean_eng", "regional_mean_rel"),
                          names_to = "INDICATOR_NAME",
                          values_to = "DATA_VALUE") %>%
      dplyr::mutate(INDICATOR_NAME = paste(INDICATOR_NAME, REGION),
                    CATEGORY = "Community",
                    INDICATOR_TYPE = "Socioeconomic") %>%
      dplyr::select(-REGION)

    usethis::use_data(community_indicators,
                      overwrite = TRUE)
    if(return) return(community_indicators)
}
# create_community_indicators()

# overall <- c()
# for(i in 2009:2019){
#   data <- readxl::read_excel(here::here("data-raw/2009-2019 Rec Scores_ME_MS_031522 cw.xlsx"),
#                              sheet = as.character(i),
#                              na = "N/A")
#
#   data <- data %>%
#     dplyr::mutate(REGION = ifelse(STATEABBR %in% c("NJ", "DE", "MD", "NY"),
#                                   "Mid Atlantic", REGION)) %>%
#     dplyr::filter(PRIMARY_LONGITUDE > -81.25) %>%
#     dplyr::group_by(REGION) %>%
#     dplyr::summarise(regional_mean_eng = mean(RecEng, na.rm = TRUE),
#                      regional_mean_rel = mean(RecRel, na.rm = TRUE)) %>%
#     dplyr::mutate(Year = i)
#
#  overall <- rbind(overall, data)
# }
# overall
#
# overall %>%
#   ggplot2::ggplot(ggplot2::aes(x = Year,
#                                # y = regional_mean_eng,
#                                y = regional_mean_rel,
#                                color = REGION)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_line()
#
#
#
# ## check region/season
# data %>%
#   dplyr::select(REGION, STATEABBR) %>%
#   dplyr::distinct() %>%
#   dplyr::arrange(REGION)
#
# data %>%
#   dplyr::filter(STATEABBR == "FL") %>%
#   summary()
#
# # have to split FL to omit gulf of mexico
# data %>%
#   dplyr::filter(PRIMARY_LONGITUDE > -81.25) %>%
#   dplyr::group_by(REGION) %>%
#   dplyr::summarise(regional_mean_eng = mean(RecEng, na.rm = TRUE),
#                    regional_mean_rel = mean(RecRel, na.rm = TRUE))
#
# data %>%
#   dplyr::mutate(REGION = ifelse(STATEABBR %in% c("NJ", "DE", "MD", "NY"),
#                                 "Mid Atlantic", REGION)) %>%
#   dplyr::filter(PRIMARY_LONGITUDE > -81.25) %>%
#   dplyr::group_by(REGION) %>%
#   dplyr::summarise(regional_mean_eng = mean(RecEng, na.rm = TRUE),
#                    regional_mean_rel = mean(RecRel, na.rm = TRUE))
#
#
# # map setup
# xmin <- -88
# xmax <- -80
# ymin <- 22
# ymax <- 33
# xlims <- c(xmin, xmax)
# ylims <- c(ymin, ymax)
# crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#
# ne_countries <- rnaturalearth::ne_countries(
#   scale = 10,
#   continent = "North America",
#   returnclass = "sf"
# ) %>%
#   sf::st_transform()
#
# ne_states <- rnaturalearth::ne_states(
#   country = "united states of america",
#   returnclass = "sf"
# ) %>% sf::st_transform()
#
# # plot
# p1 <- ggplot2::ggplot() +
#   ggplot2::geom_sf(
#     data = ne_countries,
#     color = "grey60",
#     size = 0.25
#   ) +
#   ggplot2::geom_sf(
#     data = ne_states,
#     color = "grey60",
#     size = 0.05
#   ) +
#   ggplot2::geom_point(data = data %>%
#                         dplyr::filter(PRIMARY_LONGITUDE > -81.25),
#                       ggplot2::aes(x = PRIMARY_LONGITUDE,
#                                    y = PRIMARY_LATITUDE)) +
#   ggplot2::coord_sf(crs = crs,
#                     xlim = xlims,
#                     ylim = ylims) +
#   ggthemes::theme_map() +
#   ggplot2::theme(legend.direction = "horizontal")
#
# p1
#
