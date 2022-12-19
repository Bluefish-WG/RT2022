`%>%` <- magrittr::`%>%`

# download data
url <- "https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis.derived/surface_gauss/vwnd.10m.mon.mean.nc"
download.file(url, destfile = here::here("data-raw/vwnd.10m.mon.mean.nc"))

url <- "https://downloads.psl.noaa.gov/Datasets/ncep.reanalysis.derived/surface_gauss/uwnd.10m.mon.mean.nc"
download.file(url, destfile = here::here("data-raw/uwnd.10m.mon.mean.nc"))

# R can't open the file (will have to do this in a gh action...)

# read in data
vwind <- ecopull::nc_to_raster(nc = here::here("data-raw/vwnd.10m.mon.mean.nc"),
                               varname = "vwnd",
                               show_images = TRUE)
vwind <- raster::rotate(vwind)

uwind <- ecopull::nc_to_raster(nc = here::here("data-raw/uwnd.10m.mon.mean.nc"),
                               varname = "uwnd",
                               show_images = TRUE)
uwind <- raster::rotate(uwind)

# mask
m_vwind <- raster::mask(x = vwind,
                        mask = large_geom)
m_uwind <- raster::mask(x = uwind,
                        mask = large_geom)

# extract into dataframe

## v
"names" <- NULL
"values" <- NULL
df<- data.frame()
for(i in 1:raster::nlayers(vwind)){
  names <- vwind[[i]]@data@names
  mean_wind <- mean(vwind[[i]]@data@values, na.rm = TRUE)
  min_wind <- vwind[[i]]@data@min
  max_wind <- vwind[[i]]@data@max

  dat<-cbind(names, mean_wind, min_wind, max_wind
  )
  df<-rbind(df, dat)
}

v_monthly_wind <- df %>%
  dplyr::mutate(Year = stringr::str_extract(names, pattern = "\\d{4}"),
                names = stringr::str_remove(names, pattern = "X"),
                DOY = lubridate::as_date(names),

                week = lubridate::week(DOY),
                month = lubridate::month(DOY),
                year = lubridate::year(DOY),

                mean_wind = as.numeric(mean_wind),
                min_wind = as.numeric(min_wind),
                max_wind = as.numeric(max_wind))

## u
"names" <- NULL
"values" <- NULL
df<- data.frame()
for(i in 1:raster::nlayers(uwind)){
  names <- uwind[[i]]@data@names
  mean_wind <- mean(uwind[[i]]@data@values, na.rm = TRUE)
  min_wind <- uwind[[i]]@data@min
  max_wind <- uwind[[i]]@data@max

  dat<-cbind(names, mean_wind, min_wind, max_wind)
  df<-rbind(df, dat)
}

u_monthly_wind <- df %>%
  dplyr::mutate(Year = stringr::str_extract(names, pattern = "\\d{4}"),
                names = stringr::str_remove(names, pattern = "X"),
                DOY = lubridate::as_date(names),
                week = lubridate::week(DOY),
                month = lubridate::month(DOY),
                year = lubridate::year(DOY))

# combine u and v winds
all_wind <- dplyr::full_join(u_monthly_wind %>%
                               dplyr::rename(u_mean = mean_wind) %>%
                               dplyr::select(-c(min_wind, max_wind, year)),
                             v_monthly_wind %>%
                               dplyr::rename(v_mean = mean_wind) %>%
                               dplyr::select(-c(min_wind, max_wind, year))
) %>%
  # rotate
  dplyr::mutate(v_mean = v_mean %>% as.numeric() %>% round(digits = 3),
                u_mean = u_mean %>% as.numeric() %>% round(digits = 3),
                wind_magnitude = sqrt(v_mean^2 + u_mean^2),

                ns = ifelse(v_mean > 0, "north", "south"),
                ew = ifelse(u_mean > 0, "east", "west"),
                wind_blowing_to = paste0(ns, ew),

                coarse_angle = atan(abs(v_mean/u_mean)),
                coast_angle = (pi/4),

                adjusted_angle = dplyr::case_when(
                  wind_blowing_to == "northeast" ~ coarse_angle,
                  wind_blowing_to == "northwest" ~ pi - coarse_angle,
                  wind_blowing_to == "southwest" ~ pi + coarse_angle,
                  wind_blowing_to == "southeast" ~ 2*pi - coarse_angle),

                new_angle = (adjusted_angle + (pi/2 - coast_angle)),

                new_angle = ifelse(new_angle > 2*pi, new_angle - 2*pi, new_angle),

                new_wind_blowing_to = dplyr::case_when(
                  new_angle < 0 & new_angle > -pi/2 ~ "down_away",
                  new_angle < 2*pi & new_angle > 3*pi/2 ~ "down_away",
                  new_angle < 3*pi/2 & new_angle > pi ~ "down_towards",
                  new_angle < pi & new_angle > pi/2 ~ "up_towards",
                  new_angle > 0 & new_angle < pi/2 ~ "up_away"
                ),

                # have to add sign back somehow
                longshore_wind = (wind_magnitude / sqrt(tan(new_angle)^2 + 1)),
                crossshore_wind = sqrt(wind_magnitude^2 - longshore_wind^2),

                longshore_wind = ifelse(stringr::str_detect(new_wind_blowing_to, "down"),
                                        -longshore_wind, longshore_wind),

                crossshore_wind = ifelse(stringr::str_detect(new_wind_blowing_to, "towards"),
                                         -crossshore_wind, crossshore_wind),

                check_magnitude = sqrt(longshore_wind^2 + crossshore_wind^2) == wind_magnitude

                # along = ifelse(new_angle > 0, "up_coast", "down_coast"),
                # across = ifelse(abs(new_angle) < pi/2, "away_coast", "towards_coast"),
                # wind_blowing_to2 = paste(along, across)
  )

# combine april & may winds
apr_may_wind <- all_wind %>%
  dplyr::filter(month == 4 | month == 5) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(mean_longshore = mean(longshore_wind),
                   mean_crossshore = mean(crossshore_wind))

write.csv(apr_may_wind, here::here("data-raw/wind_indicators_coastwide.csv"))
