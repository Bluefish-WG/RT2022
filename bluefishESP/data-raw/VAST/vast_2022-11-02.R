
`%>%` <- magrittr::`%>%`

library(VAST)
library(dplyr)
library(tidyr)

load(here::here("analysis/data/derived_data/bluefish_nmfs.rda"))
load(here::here("analysis/data/derived_data/all_nmfs.rda"))

depth <- all_nmfs %>%
  dplyr::select(Lat = LAT, Lon = LON, Depth = DEPTH) %>%
  dplyr::distinct() %>%
  tidyr::drop_na() %>%
  dplyr::full_join(tibble::tibble(Year = 1963:2021),
                   by = as.character())

covariates <- read.csv(here::here("density_covariates.csv"))

# univariate model ----
bfish <- bluefish_nmfs %>%
  # add zeros
  dplyr::full_join(all_nmfs %>%
                     dplyr::mutate(BIOMASS = 0)) %>%
  dplyr::filter(SEASON == "FALL"
  ) %>%
  dplyr::select(Year = YEAR,
                Lat = LAT,
                Lon = LON,
                Biomass = BIOMASS,
                EST_TOWDATE
  ) %>%
  dplyr::distinct() %>% # remove duplicate biomass entries
  dplyr::group_by(Year, Lat, Lon) %>%
  dplyr::summarise(Biomass = sum(Biomass),
                   AreaSwept_km2 = 0.0384,
                   doy = lubridate::yday(EST_TOWDATE),
                   Vessel = "NEFSC") %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  na.omit() %>%
  as.data.frame()

dat <- bfish %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(total_biomass = sum(Biomass)) %>%
  dplyr::filter(total_biomass > 0) %>%
  dplyr::ungroup() %>%
  as.data.frame()

bluefish <- list(sampling_data = dat,
                 Region = "northwest_atlantic",
                 strata.limits = list('All_areas' = 1:1e5),# full area
                 covariate_data = with(depth, data.frame(Year = Year,
                                                              Lat = Lat,
                                                              Lon = Lon,
                                                              Depth = (log(Depth) - mean(log(Depth))) / sd(log(Depth))
                 )),
                 catchability_data =  with(dat,
                                           data.frame(Year = Year,
                                                      Lat = Lat,
                                                      Lon = Lon,
                                                      doy = doy))
)

# only depth covariate

name <- "final/univariate2"
dir.create(here::here(name),
           recursive = TRUE)
setwd(here::here(name))

settings <- make_settings( n_x = 100,
                           Region = "northwest_atlantic",
                           strata.limits = bluefish$strata.limits,
                           purpose = "index2",
                           bias.correct = TRUE,
                           use_anisotropy = TRUE,
                           fine_scale = FALSE,
                           n_categories = 3,
                           FieldConfig = c("Omega1" = 1, "Epsilon1" = 1,
                                           "Omega2" = 1, "Epsilon2" = 1),
                           RhoConfig = c("Beta1" = 0,
                                         "Beta2" = 0,
                                         "Epsilon1" = 0,
                                         "Epsilon2" = 0),
                           OverdispersionConfig = c(0,0),
                           ObsModel = c(1,1),
                           Options = c("Calculate_Range" = TRUE,
                                       "Calculate_effective_area" = TRUE),
                           treat_nonencounter_as_zero = FALSE)

fit <- fit_model(settings = settings,
                 Lat_i = bluefish$sampling_data$Lat,
                 Lon_i = bluefish$sampling_data$Lon,
                 t_i = bluefish$sampling_data$Year,
                 c_i = rep(0, nrow(bluefish$sampling_data)),
                 b_i = as_units(bluefish$sampling_data$Biomass, "kg"),
                 a_i = as_units(bluefish$sampling_data$AreaSwept_km2, "km^2"),

                 covariate_data = bluefish$covariate_data,

                 X1_formula = ~ splines::bs(Depth, knots = 3, intercept = FALSE),
                 X2_formula = ~ splines::bs(Depth, knots = 3, intercept = FALSE),

                 test_fit = FALSE,
                 run_model = TRUE,
                 getsd = TRUE,
                 optimize_args = list("lower" = -Inf,
                                      "upper" = Inf)
)

results <- plot(fit,
                working_dir = paste0(here::here(name), "/")) %>%
  try()

saveRDS(results, here::here(name, "results_100.RDS"))
saveRDS(fit, here::here(name, "fit_100.RDS"))

cog <- results$Range$COG_Table
write.csv(cog, here::here(name, "cog_100.csv"))


# multivariate model data ----
bfish <- bluefish_nmfs %>%
  dplyr::mutate(size = ifelse(LENGTH >= 50, 2, # large
                              ifelse(LENGTH <= 30.3,
                                     0, # small
                                     1 # medium
                              ))) %>%
  dplyr::full_join(all_nmfs %>%
                     dplyr::mutate(NUMLEN = 0,
                                   size = 0)) %>%
  dplyr::full_join(all_nmfs %>%
                     dplyr::mutate(NUMLEN = 0,
                                   size = 1)) %>%
  dplyr::full_join(all_nmfs %>%
                     dplyr::mutate(NUMLEN = 0,
                                   size = 2)) %>%
  dplyr::filter(SEASON == "FALL",
                YEAR > 1981) %>%
  dplyr::select(Year = YEAR,
                EST_TOWDATE,
                Lat = LAT,
                Lon = LON,
                N = NUMLEN,
                size
  ) %>%
  dplyr::group_by(Year, EST_TOWDATE, Lat, Lon, size) %>%
  dplyr::summarise(N = sum(N),
                   doy = lubridate::yday(EST_TOWDATE),
                   AreaSwept_km2 = 0.0384,
                   Vessel = "NEFSC") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(doy = (doy - mean(doy)) / sd(doy)) %>%
  dplyr::arrange(EST_TOWDATE) %>%
  dplyr::distinct() %>%
  na.omit() %>%
  # remove years when one or more group had 0 observations
  dplyr::group_by(Year, size) %>%
  dplyr::mutate(max_n = max(N)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(group_with_0 = min(max_n) == 0) %>%
  dplyr::filter(group_with_0 == FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Year, size) %>%
  dplyr::mutate(pos_tows = sum(as.numeric(N > 0))) %>%
  as.data.frame()

dat <- bfish %>%
  dplyr::group_by(Year, size) %>%
  dplyr::mutate(n_tows = dplyr::n()) %>%
  dplyr::filter(n_tows > 300) %>%
  dplyr::ungroup() %>%
  as.data.frame()

bluefish <- list(sampling_data = dat,
                 Region = "northwest_atlantic",
                 strata.limits = list('All_areas' = 1:1e5),# full area
                 covariate_data = with(covariates,
                                       data.frame(Year = Year,
                                                  Lat = Lat,
                                                  Lon = Lon,
                                                  Depth = (log(Depth) - mean(log(Depth))) / sd(log(Depth)),
                                                  Temperature = ((Temperature) - mean((Temperature))) / sd((Temperature))
                                       )),
                 catchability_data =  with(dat,
                                           data.frame(Year = Year,
                                                      Lat = Lat,
                                                      Lon = Lon,
                                                      doy = doy))
)

# base model ----
name <- "final/base3"
dir.create(here::here(name),
           recursive = TRUE)
setwd(here::here(name))

settings <- make_settings( n_x = 100,
                           Region = "northwest_atlantic",
                           strata.limits = bluefish$strata.limits,
                           purpose = "index2",
                           bias.correct = TRUE,
                           use_anisotropy = TRUE,
                           fine_scale = FALSE,
                           n_categories = 3,
                           FieldConfig = c("Omega1" = 3, "Epsilon1" = 3,
                                           "Omega2" = 3, "Epsilon2" = 3),
                           RhoConfig = c("Beta1" = 0,
                                         "Beta2" = 0,
                                         "Epsilon1" = 0,
                                         "Epsilon2" = 0),
                           OverdispersionConfig = c(0,0),
                           ObsModel = c(1,1),
                           Options = c("Calculate_Range" = TRUE,
                                       "Calculate_effective_area" = TRUE),
                           treat_nonencounter_as_zero = FALSE)

fit <- fit_model(settings = settings,
                 Lat_i = bluefish$sampling_data$Lat,
                 Lon_i = bluefish$sampling_data$Lon,
                 t_i = bluefish$sampling_data$Year,
                 c_i = bluefish$sampling_data$size,
                 b_i = as_units(bluefish$sampling_data$N, "count"),
                 a_i = as_units(bluefish$sampling_data$AreaSwept_km2, "km^2"),

                 test_fit = FALSE,
                 run_model = TRUE,
                 getsd = TRUE,
                 optimize_args = list("lower" = -Inf,
                                      "upper" = Inf),
                 newtonsteps = 5
)

results <- plot(fit,
                working_dir = paste0(here::here(name), "/")) %>%
  try()

saveRDS(results, here::here(name, "results_100.RDS"))
saveRDS(fit, here::here(name, "fit_100.RDS"))

cog <- results$Range$COG_Table
write.csv(cog, here::here(name, "cog_100.csv"))

# depth model ----
name <- "final/depth2"
dir.create(here::here(name),
           recursive = TRUE)
setwd(here::here(name))

settings <- make_settings( n_x = 100,
                           Region = "northwest_atlantic",
                           strata.limits = bluefish$strata.limits,
                           purpose = "index2",
                           bias.correct = TRUE,
                           use_anisotropy = TRUE,
                           fine_scale = FALSE,
                           n_categories = 3,
                           FieldConfig = c("Omega1" = 3, "Epsilon1" = 3,
                                           "Omega2" = 3, "Epsilon2" = 3),
                           RhoConfig = c("Beta1" = 0,
                                         "Beta2" = 0,
                                         "Epsilon1" = 0,
                                         "Epsilon2" = 0),
                           OverdispersionConfig = c(0,0),
                           ObsModel = c(1,1),
                           Options = c("Calculate_Range" = TRUE,
                                       "Calculate_effective_area" = TRUE),
                           treat_nonencounter_as_zero = FALSE)

fit <- fit_model(settings = settings,
                 Lat_i = bluefish$sampling_data$Lat,
                 Lon_i = bluefish$sampling_data$Lon,
                 t_i = bluefish$sampling_data$Year,
                 c_i = bluefish$sampling_data$size,
                 b_i = as_units(bluefish$sampling_data$N, "count"),
                 a_i = as_units(bluefish$sampling_data$AreaSwept_km2, "km^2"),

                 covariate_data = bluefish$covariate_data,

                 X1_formula = ~ splines::bs(Depth, knots = 3, intercept = FALSE),
                 X2_formula = ~ splines::bs(Depth, knots = 3, intercept = FALSE),

                 test_fit = FALSE,
                 run_model = TRUE,
                 getsd = TRUE,
                 optimize_args = list("lower" = -Inf,
                                      "upper" = Inf)
)

results <- plot(fit,
                working_dir = paste0(here::here(name), "/")) %>%
  try()

saveRDS(results, here::here(name, "results_100.RDS"))
saveRDS(fit, here::here(name, "fit_100.RDS"))

cog <- results$Range$COG_Table
write.csv(cog, here::here(name, "cog_100.csv"))

## day of year model ----
name <- "final/doy2"
dir.create(here::here(name))
setwd(here::here(name))

settings <- make_settings( n_x = 100,
                           Region = "northwest_atlantic",
                           strata.limits = bluefish$strata.limits,
                           purpose = "index2",
                           bias.correct = TRUE,
                           use_anisotropy = TRUE,
                           fine_scale = FALSE,
                           n_categories = 3,
                           FieldConfig = c("Omega1" = 3, "Epsilon1" = 3,
                                           "Omega2" = 3, "Epsilon2" = 3),
                           RhoConfig = c("Beta1" = 0,
                                         "Beta2" = 0,
                                         "Epsilon1" = 0,
                                         "Epsilon2" = 0),
                           OverdispersionConfig = c(0,0),
                           ObsModel = c(1,1),
                           Options = c("Calculate_Range" = TRUE,
                                       "Calculate_effective_area" = TRUE),
                           treat_nonencounter_as_zero = FALSE)

fit <- fit_model(settings = settings,
                 Lat_i = bluefish$sampling_data$Lat,
                 Lon_i = bluefish$sampling_data$Lon,
                 t_i = bluefish$sampling_data$Year,
                 c_i = bluefish$sampling_data$size,
                 b_i = as_units(bluefish$sampling_data$N, "count"),
                 a_i = as_units(bluefish$sampling_data$AreaSwept_km2, "km^2"),

                 catchability_data = bluefish$sampling_data,
                 covariate_data = bluefish$covariate_data,

                 X1_formula = ~ splines::bs(Depth, knots = 3, intercept = FALSE),
                 X2_formula = ~ splines::bs(Depth, knots = 3, intercept = FALSE),
                 Q1_formula = ~ doy,
                 Q2_formula = ~ doy,

                 test_fit = FALSE,
                 run_model = TRUE,
                 getsd = TRUE,
                 optimize_args = list("lower" = -Inf,
                                      "upper" = Inf)
)

results <- plot(fit,
                working_dir = paste0(here::here(name), "/")) %>%
  try()

saveRDS(results, here::here(name, "results_100.RDS"))
saveRDS(fit, here::here(name, "fit_100.RDS"))

cog <- results$Range$COG_Table
write.csv(cog, here::here(name, "cog_100.csv"))


# temperature model ----
name <- "final/temp3"
dir.create(here::here(name))
setwd(here::here(name))

settings <- make_settings( n_x = 100,
                           Region = "northwest_atlantic",
                           strata.limits = bluefish$strata.limits,
                           purpose = "index2",
                           bias.correct = TRUE,
                           use_anisotropy = TRUE,
                           fine_scale = FALSE,
                           n_categories = 3,
                           FieldConfig = c("Omega1" = 3, "Epsilon1" = 3,
                                           "Omega2" = 3, "Epsilon2" = 3),
                           RhoConfig = c("Beta1" = 0,
                                         "Beta2" = 0,
                                         "Epsilon1" = 0,
                                         "Epsilon2" = 0),
                           OverdispersionConfig = c(0,0),
                           ObsModel = c(1,1),
                           Options = c("Calculate_Range" = TRUE,
                                       "Calculate_effective_area" = TRUE),
                           treat_nonencounter_as_zero = FALSE)

fit <- fit_model(settings = settings,
                 Lat_i = bluefish$sampling_data$Lat,
                 Lon_i = bluefish$sampling_data$Lon,
                 t_i = bluefish$sampling_data$Year,
                 c_i = bluefish$sampling_data$size,
                 b_i = as_units(bluefish$sampling_data$N, "count"),
                 a_i = as_units(bluefish$sampling_data$AreaSwept_km2, "km^2"),

                 catchability_data = bluefish$sampling_data,
                 covariate_data = bluefish$covariate_data,

                 X1_formula = ~ splines::bs(Depth, knots = 3, intercept = FALSE) +
                   splines::bs(Temperature, knots = 3, intercept = FALSE),
                 X2_formula = ~ splines::bs(Depth, knots = 3, intercept = FALSE) +
                   splines::bs(Temperature, knots = 3, intercept = FALSE),
                 Q1_formula = ~ doy,
                 Q2_formula = ~ doy,

                 test_fit = FALSE,
                 run_model = TRUE,
                 getsd = TRUE,
                 optimize_args = list("lower" = -Inf,
                                      "upper" = Inf),
                 newtonsteps = 5
)

results <- plot(fit,
                working_dir = paste0(here::here(name), "/")) %>%
  try()

saveRDS(results, here::here(name, "results_100.RDS"))
saveRDS(fit, here::here(name, "fit_100.RDS"))

cog <- results$Range$COG_Table
write.csv(cog, here::here(name, "cog_100.csv"))

###

fit <- readRDS(here::here("final/temp/fit_100.RDS"))

predD = fit$Report$D_i

predSE = fit$Report$R1_i

test <- cbind(bluefish$sampling_data,
              predD,
              predSE)

test %>%
  dplyr::filter(Year == 1988) %>%
  ggplot2::ggplot(ggplot2::aes(x = Biomass,
                               y = predD,
                               color = Year)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~size,
                      ncol = 1) +
  nmfspalette::scale_color_nmfs(palette = "regional web", discrete = FALSE) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

###

library(effects)  # Used to visualize covariate effects

# Must add data-frames to global environment (hope to fix in future)
covariate_data_full = fit$effects$covariate_data_full
catchability_data_full = fit$effects$catchability_data_full

# Plot 1st linear predictor, but could use `transformation` to apply link function
pred = Effect.fit_model( fit,
                         focal.predictors = c("Temperature"),
                         category_number = 3,
                         which_formula = "X1",
                         xlevels = 100,
                         transformation = list(link=identity, inverse=identity) )
plot(pred)


library(pdp)

# Make function to interface with pdp
pred.fun = function( object, newdata ){
  predict( x=object,
           Lat_i = object$data_frame$Lat_i,
           Lon_i = object$data_frame$Lon_i,
           t_i = object$data_frame$t_i,
           a_i = object$data_frame$a_i,
           what = "P1_iz",
           new_covariate_data = newdata,
           do_checks = FALSE)
}

# Run partial
Partial = partial( object = fit,
                   pred.var = "Temperature",
                   pred.fun = pred.fun,
                   train = fit$covariate_data )

# Make plot using ggplot2
library(ggplot2)
autoplot(Partial)
