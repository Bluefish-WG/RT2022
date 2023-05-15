#' Create center of gravity indicators
#'
#' This function creates center of gravity indicators from VAST results
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `relative_condition`, returns relative condition indicators
#' @importFrom magrittr `%>%`
#' @export
#'
create_cog <- function(data = here::here("../../VAST/neusvast/final/doy2/cog_100.csv"),
  return = TRUE){

  center_of_gravity <- read.csv(data) %>%
    dplyr::mutate(m = ifelse(m == 1, "eastings", "northings"),
                  Category = dplyr::case_when(Category == 1 ~ "small",
                                              Category == 2 ~ "medium",
                                              Category == 3 ~ "large"),
                  INDICATOR_NAME = paste(Category, m),
                  INDICATOR_TYPE = "Ecosystem",
                  CATEGORY = "Distribution") %>%
    dplyr::rename(DATA_VALUE = COG_hat,
                  YEAR = Year) %>%
    dplyr::select(-c(X, Category, m, SE))

  usethis::use_data(center_of_gravity, overwrite = TRUE)

  if(return) return(center_of_gravity)
}

# for(i in unique(center_of_gravity$INDICATOR_NAME)) {
#   print(i)
#   dat <- center_of_gravity %>%
#     dplyr::filter(INDICATOR_NAME == i)
#   mod <- lm(DATA_VALUE ~ YEAR, data = dat)
#   summary(mod) %>%
#     print()
# }

