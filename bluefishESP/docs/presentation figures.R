
### setup ----
`%>%` <- magrittr::`%>%`
devtools::load_all()

# map setup
xlims <- c(-81, -66)
ylims <- c(26, 45)
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

ne_countries <- rnaturalearth::ne_countries(
  scale = 10,
  continent = "North America",
  returnclass = "sf"
) %>%
  sf::st_transform()

ne_states <- rnaturalearth::ne_states(
  country = "united states of america",
  returnclass = "sf"
) %>% sf::st_transform()

# base map plot
p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = ne_countries,
    color = "grey60",
    size = 0.25
  ) +
  ggplot2::geom_sf(
    data = ne_states,
    color = "grey60",
    size = 0.05
  ) +
  ggplot2::coord_sf(
    crs = crs,
    xlim = xlims,
    ylim = ylims
  ) +
  ggthemes::theme_map() +
  ggplot2::theme(legend.direction = "horizontal")

# nmfs bluefish data
dat <- bluefish_nmfs %>%
  dplyr::mutate(
    MONTH = lubridate::month(EST_TOWDATE),
    size_group = ifelse(LENGTH <= 30.3,
      "small (<=30.3cm)",
      ifelse(LENGTH >= 50.0,
        "large (>=50.0cm)",
        "medium (30.3-50.0cm)"
      )
    )
  ) %>%
  dplyr::group_by(size_group, MONTH, EST_TOWDATE, LAT, LON, SURFTEMP, SURFSALIN) %>%
  dplyr::summarise(n_fish = sum(NUMLEN)) %>%
  dplyr::ungroup()

all <- all_nmfs %>%
  dplyr::mutate(MONTH = lubridate::month(EST_TOWDATE))

# indicators
targets::tar_load(test)
order <- read.csv(here::here("data-raw/indicator_key.csv"))

indicators <- test %>%
  dplyr::full_join(order) %>%
  dplyr::select(-INDICATOR_NAME) %>%
  dplyr::rename(INDICATOR_NAME = facet_label)

# functions ----

plot_map <- function(size, title,
                     base_map = p1,
                     tows = all,
                     bluefish = dat) {
  plt <- base_map +
    ggplot2::geom_point(
      data = tows %>%
        dplyr::filter(MONTH == 9 | MONTH == 10),
      ggplot2::aes(
        x = LON,
        y = LAT
      ),
      fill = "white",
      color = "gray80",
      pch = 21
    ) +
    ggplot2::geom_point(
      data = bluefish %>%
        dplyr::filter(
          stringr::str_detect(size_group, size),
          MONTH == 9 | MONTH == 10
        ),
      ggplot2::aes(
        x = LON,
        y = LAT,
        color = log(n_fish)
      ),
      cex = 0.9,
      alpha = 0.5
    ) +
    viridis::scale_color_viridis() +
    ggplot2::facet_wrap(~MONTH,
      labeller =
        ggplot2::labeller(
          MONTH = c(
            "1" = "January", "2" = "February", "3" = "March",
            "4" = "April", "5" = "May", "6" = "June",
            "7" = "July", "8" = "August", "9" = "September",
            "10" = "October", "11" = "November", "12" = "December"
          )
        )
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 12)
    ) +
    ggplot2::coord_sf(
      crs = crs,
      xlim = xlims,
      ylim = c(30, 45)
    )

  return(plt)
}

# small distribution -----
plot_map(size = "small", title = "Small fish (<=30.3cm)")
ggplot2::ggsave(here::here("images/small_map.png"), width = 4, height = 3)

indicators %>%
  dplyr::filter(
    stringr::str_detect(INDICATOR_NAME, "small"),
    stringr::str_detect(INDICATOR_NAME, "ings")
  ) %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 1982,
    ncolumn = 2
  ) +
  ggplot2::theme(
    aspect.ratio = 0.14,
    strip.text = ggplot2::element_text(size = 8)
  )
ggplot2::ggsave(here::here("images/small_distr.png"), width = 10, height = 2)

# medium distribution -----
plot_map(size = "medium", title = "Medium fish (30.3-50.0cm)")
ggplot2::ggsave(here::here("images/medium_map.png"), width = 4, height = 3)

indicators %>%
  dplyr::filter(
    stringr::str_detect(INDICATOR_NAME, "medium"),
    stringr::str_detect(INDICATOR_NAME, "ings")
  ) %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 1982,
    ncolumn = 2
  ) +
  ggplot2::theme(
    aspect.ratio = 0.14,
    strip.text = ggplot2::element_text(size = 8)
  )
ggplot2::ggsave(here::here("images/medium_distr.png"), width = 10, height = 2)

# large distribution -----
plot_map(size = "large", title = "Large fish (>=50.0cm)")
ggplot2::ggsave(here::here("images/large_map.png"), width = 4, height = 3)

indicators %>%
  dplyr::filter(
    stringr::str_detect(INDICATOR_NAME, "large"),
    stringr::str_detect(INDICATOR_NAME, "ings")
  ) %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 1982,
    ncolumn = 2
  ) +
  ggplot2::theme(
    aspect.ratio = 0.14,
    strip.text = ggplot2::element_text(size = 8)
  )
ggplot2::ggsave(here::here("images/large_distr.png"), width = 10, height = 2)

# socioeconomic figures ----
indicators %>%
  dplyr::filter(CATEGORY == "Recreational") %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 1981,
    ncolumn = 2
  ) +
  ggplot2::theme(
    aspect.ratio = 0.3,
    strip.text = ggplot2::element_text(size = 8)
  )
ggplot2::ggsave(here::here("images/recreational.png"), width = 8, height = 3)

indicators %>%
  dplyr::filter(CATEGORY == "Commercial") %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 2000,
    ncolumn = 2
  ) +
  ggplot2::theme(
    aspect.ratio = 0.3,
    strip.text = ggplot2::element_text(size = 8)
  )
ggplot2::ggsave(here::here("images/commercial.png"), width = 8, height = 5)

indicators %>%
  dplyr::filter(CATEGORY == "Community") %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 2009,
    ncolumn = 2
  ) +
  ggplot2::theme(
    aspect.ratio = 0.3,
    strip.text = ggplot2::element_text(size = 8)
  )
ggplot2::ggsave(here::here("images/community.png"), width = 8, height = 5)

# spawning timing ----
indicators %>%
  dplyr::filter(stringr::str_detect(INDICATOR_NAME, "day")) %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 1982,
    ncolumn = 1
  ) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
ggplot2::ggsave(here::here("images/spawn_time.png"), width = 6, height = 6)

# spawning proportion ----
indicators %>%
  dplyr::filter(
    stringr::str_detect(INDICATOR_NAME, "Proportion"),
    stringr::str_detect(INDICATOR_NAME, "Atlantic")
  ) %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 1982,
    ncolumn = 1
  ) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
ggplot2::ggsave(here::here("images/spawn_prop.png"), width = 5, height = 5)

# condition ----
indicators %>%
  dplyr::filter(stringr::str_detect(INDICATOR_NAME, "condition")) %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 1981,
    ncolumn = 2
  ) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
ggplot2::ggsave(here::here("images/cond.png"), width = 10, height = 5)

# mako ----
indicators %>%
  dplyr::filter(stringr::str_detect(INDICATOR_NAME, "mako")) %>%
  esp_traffic_fig(
    status = FALSE,
    min_year = 1950,
    ncolumn = 2
  ) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
ggplot2::ggsave(here::here("images/mako.png"), width = 6, height = 3)

# cohorts ----
devtools::load_all(here::here("../bluefishLifeHistory"))

surv <- raw_survey_data %>%
  bump_ages() %>%
  interpolate_lengths() %>%
  remove_outliers() %>%
  dplyr::ungroup() %>%
  dplyr::select(
    SampleID, State, Year, Month, Source, Program, Area,
    ForkLengthCM, WeightKG, Sex, Maturity, DataSource,
    Station, Age, AgeType, Semester, Geo, Decade,
    Cohort, CohortDecade, MaturityBin
  ) %>%
  dplyr::filter(
    ForkLengthCM <= 30.3,
    ForkLengthCM > 1
  ) %>%
  dplyr::group_by(Month, State, Program) %>%
  dplyr::mutate(
    n_data = dplyr::n(),
    n_year = length(unique(Year))
  ) %>%
  dplyr::filter(
    n_data > 20,
    n_year > 2
  ) %>%
  tidyr::drop_na(State, Program, Month)

# * NY -----
dat <- surv %>%
  dplyr::filter(State == "NY") %>%
  dplyr::filter(
    Program == "Western Long Island Seine Survey",
    Year %in% c(
      1986, 1987, 2001,
      2009, 2013, 2014,
      2021
    )
  ) %>%
  dplyr::mutate(Month = month.abb[Month])
dat$Month <- factor(dat$Month,
  levels = month.abb
)

dat %>%
  dplyr::filter(Month != "May") %>%
  ggplot2::ggplot(ggplot2::aes(
    x = ForkLengthCM,
    color = Month,
    fill = Month
  )) +
  ggplot2::geom_histogram(
    alpha = 0.25,
    binwidth = 2,
    position = "identity"
  ) +
  nmfspalette::scale_color_nmfs(palette = "regional web") +
  nmfspalette::scale_fill_nmfs(palette = "regional web") +
  ggplot2::facet_grid(
    cols = ggplot2::vars(Year),
    rows = ggplot2::vars(Month),
    scales = "free_y"
  ) +
  ggplot2::scale_y_continuous(n.breaks = 3) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("New York Western Long Island Seine Survey") +
  ggplot2::theme(
    legend.position = "none",
    strip.text.y = ggplot2::element_text(angle = 0),
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::ylab("Count") +
  ggplot2::xlab("Fork length (cm)")

ggplot2::ggsave(here::here("images/ny_cohorts.png"), width = 6, height = 3)

# * NC -----
dat <- surv %>%
  dplyr::filter(
    State == "NC",
    Year %in% c(1983, 1993, 2003, 2013, 2021)
  ) %>%
  dplyr::mutate(Month = month.abb[Month])

dat$Month <- factor(dat$Month,
  levels = month.abb
)

dat %>%
  ggplot2::ggplot(ggplot2::aes(
    x = ForkLengthCM,
    fill = Program
  )) +
  ggplot2::geom_histogram(
    alpha = 0.25,
    binwidth = 2,
    position = "identity",
    color = "black"
  ) +
  nmfspalette::scale_fill_nmfs(palette = "regional web") +
  ggplot2::facet_grid(
    cols = ggplot2::vars(Year),
    rows = ggplot2::vars(Month)
  ) +
  ggplot2::scale_y_continuous(n.breaks = 3) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("North Carolina small bluefish") +
  ggplot2::theme(
    legend.position = "bottom",
    strip.text.y = ggplot2::element_text(angle = 0),
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::ylab("Count") +
  ggplot2::xlab("Fork length (cm)") +
  ggplot2::guides(fill = ggplot2::guide_legend(
    nrow = 2,
    title = NULL
  ))
ggplot2::ggsave(here::here("images/nc_cohorts.png"), width = 4, height = 5)

# * FL -----
dat <- surv %>%
  dplyr::filter(
    State == "FL"#,
    # Year %in% c(1983, 1993, 2003, 2013, 2021)
  ) %>%
  dplyr::mutate(Month = month.abb[Month])

dat$Month <- factor(dat$Month,
                    levels = month.abb
)

dat %>%
  # dplyr::filter(ForkLengthCM <= 15) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(n_fish = dplyr::n()) %>%
  dplyr::filter(n_fish > 49) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = ForkLengthCM,
    fill = Program
  )) +
  ggplot2::geom_histogram(
    alpha = 0.25,
    binwidth = 2,
    position = "identity",
    color = "black"
  ) +
  nmfspalette::scale_fill_nmfs(palette = "regional web") +
  ggplot2::facet_grid(
    cols = ggplot2::vars(Year),
    rows = ggplot2::vars(Month),
    scales = "free_y"
  ) +
  ggplot2::scale_y_continuous(n.breaks = 3) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Florida small bluefish") +
  ggplot2::theme(
    legend.position = "bottom",
    strip.text.y = ggplot2::element_text(angle = 0),
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::ylab("Count") +
  ggplot2::xlab("Fork length (cm)") +
  ggplot2::guides(fill = ggplot2::guide_legend(
    nrow = 2,
    title = NULL
  ))
ggplot2::ggsave(here::here("images/fl_cohorts.png"), width = 4, height = 5)

# * NEFSC ----
dat <- bluefish_nmfs %>%
  dplyr::mutate(Month = lubridate::month(EST_TOWDATE)) %>%
  dplyr::rename(Year = YEAR) %>%
  dplyr::ungroup() %>%
  tibble::as_tibble()

dat$Month <- month.abb[dat$Month]
dat$Month <- factor(dat$Month, levels = month.abb)

dat <- expand_n(data = dat, col = "NUMLEN")

nmfs_all <- all %>%
  dplyr::filter(
    MONTH == "9" | MONTH == "10",
    YEAR == 1980 | YEAR == 2003
  ) %>%
  dplyr::rename(
    Year = YEAR,
    Month = MONTH
  ) %>%
  dplyr::mutate(Month = ifelse(Month == "9", "Sep", "Oct"))
nmfs_all$Month <- factor(nmfs_all$Month, levels = c("Sep", "Oct"))

months <- dat %>%
  dplyr::filter(
    Year %in% c(1980, 2003),
    Month == "Sep" | Month == "Oct",
    LENGTH <= 30.3
  ) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = LENGTH,
    fill = lubridate::yday(EST_TOWDATE),
    group = lubridate::yday(EST_TOWDATE)
  )) +
  ggplot2::geom_histogram(
    alpha = 0.5,
    binwidth = 2,
    position = "stack",
    color = "black"
  ) +
  nmfspalette::scale_fill_nmfs(
    palette = "regional web",
    discrete = FALSE,
    limits = c(250, 285)
  ) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(Year),
    rows = ggplot2::vars(Month)
  ) +
  ggplot2::scale_y_continuous(n.breaks = 3) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    aspect.ratio = 0.75
  ) +
  ggplot2::ylab("Count") +
  ggplot2::xlab("Fork length (cm)")

season <- dat %>%
  dplyr::filter(
    Year %in% c(1980, 2003),
    Month == "Sep" | Month == "Oct",
    LENGTH <= 30.3
  ) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = LENGTH,
    # fill = lubridate::yday(EST_TOWDATE),
    # group = lubridate::yday(EST_TOWDATE)
  )) +
  ggplot2::geom_histogram(
    alpha = 0.5,
    binwidth = 2,
    position = "stack",
    color = "black"
  ) +
  # nmfspalette::scale_fill_nmfs(
  #   palette = "regional web",
  #   discrete = FALSE,
  #   limits = c(250, 285)
  # ) +
  ggplot2::facet_grid(cols = ggplot2::vars(Year)) +
  ggplot2::scale_y_continuous(n.breaks = 3) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid = ggplot2::element_blank()
  ) +
  ggplot2::ylab("Count") +
  ggplot2::xlab("Fork length (cm)")

all$MONTH <- factor(all$MONTH, levels = c("9", "10"))
map <- p1 +
  ggplot2::geom_point(
    data = nmfs_all,
    ggplot2::aes(
      x = LON,
      y = LAT
    ),
    fill = "white",
    color = "gray80",
    pch = 21,
    cex = 0.5
  ) +
  ggplot2::geom_point(
    data = dat %>%
      dplyr::filter(
        Year == 1980 | Year == 2003,
        Month == "Sep" | Month == "Oct",
        LENGTH < 30.3
      ),
    ggplot2::aes(
      x = LON,
      y = LAT,
      color = lubridate::yday(EST_TOWDATE)
    ),
    cex = 0.5
  ) +
  nmfspalette::scale_color_nmfs(
    palette = "regional web",
    discrete = FALSE,
    limits = c(250, 285)
  ) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(Year),
    rows = ggplot2::vars(Month)
  ) +
  # ggplot2::guides(color = ggplot2::guide_colorbar(title = "day of year")) +
  # ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(color = ggplot2::guide_colorbar(
    title = "day of year",
    title.position = "top",
    direction = "vertical"
  )) +
  ggplot2::theme(
    strip.text = ggplot2::element_text(size = 8),
    legend.position = "right",
    legend.justification = "center"
  ) +
  ggplot2::coord_sf(
    crs = crs,
    xlim = xlims,
    ylim = c(33, 43)
  )

lat <- dat %>%
  dplyr::filter(
    Year %in% c(1980, 2003),
    Month == "Sep" | Month == "Oct",
    LENGTH <= 30.3
  ) %>%
  ggplot2::ggplot(ggplot2::aes(
    y = LAT,
    x = LENGTH,
    color = lubridate::yday(EST_TOWDATE)
  )) +
  ggplot2::geom_point(alpha = 0.25) +
  nmfspalette::scale_color_nmfs(
    palette = "regional web",
    discrete = FALSE,
    limits = c(250, 285)
  ) +
  ggplot2::facet_grid(rows = ggplot2::vars(Month),
                      cols = ggplot2::vars(Year)) +
  ggplot2::scale_y_continuous(n.breaks = 3) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 8),
    aspect.ratio = 0.75
  ) +
  ggplot2::ylab("Latitude") +
  ggplot2::xlab("Fork length (cm)")

ggpubr::ggarrange(#season,
                  months, map, lat,
  ncol = 3,
  common.legend = TRUE,
  legend = "right",
  legend.grob = ggpubr::get_legend(map)
)
ggplot2::ggsave(here::here("images/nefsc_1980_2003.png"), width = 9.5, height = 3.5)
ggplot2::ggsave(plot = season,
                here::here("images/nefsc_1980_2003_hist.png"),
                width = 4, height = 2.5)

# ** loop over years ----
all <- all_nmfs %>%
  dplyr::mutate(Month = lubridate::month(EST_TOWDATE))
all$Month <- month.abb[all$Month]
all$Month <- factor(all$Month, levels = month.abb)

for (i in c(1980, 1988, 1992, 2003, 2005, 2008) # unique(dat$Year)
) {
  this_dat <- dat %>%
    dplyr::filter(
      Year == i,
      SEASON == "FALL",
      LENGTH <= 30.3
    ) %>%
    dplyr::group_by(Month) %>%
    dplyr::mutate(n_fish = dplyr::n()) %>%
    dplyr::filter(n_fish > 2)

  this_all <- all %>%
    dplyr::filter(
      YEAR == i,
      Month %in% c(unique(this_dat$Month))
    ) # , "Sep", "Oct"))

  if (nrow(this_dat) > 2) {
    histogram <- this_dat %>%
      ggplot2::ggplot(ggplot2::aes(
        x = LENGTH,
        fill = lubridate::yday(EST_TOWDATE),
        group = lubridate::yday(EST_TOWDATE)
      )) +
      ggplot2::geom_histogram(
        alpha = 0.5,
        binwidth = 1,
        position = "stack",
        color = "black",
      ) +
      nmfspalette::scale_fill_nmfs(
        palette = "regional web",
        discrete = FALSE,
        limits = c(250, 285)
      ) +
      ggplot2::facet_grid(rows = ggplot2::vars(Month)) +
      ggplot2::scale_y_continuous(n.breaks = 3) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(i) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 8),
        aspect.ratio = 0.75
      ) +
      ggplot2::ylab("Count") +
      ggplot2::xlab("Fork length (cm)")

    map <- p1 +
      ggplot2::geom_point(
        data = this_all,
        ggplot2::aes(
          x = LON,
          y = LAT
        ),
        fill = "white",
        color = "gray80",
        pch = 21,
        cex = 0.5
      ) +
      ggplot2::geom_point(
        data = this_dat,
        ggplot2::aes(
          x = LON,
          y = LAT,
          color = lubridate::yday(EST_TOWDATE)
        ),
        cex = 0.5
      ) +
      nmfspalette::scale_color_nmfs(
        palette = "regional web",
        discrete = FALSE,
        n.breaks = 4,
        limits = c(250, 285)
      ) +
      ggplot2::facet_grid(rows = ggplot2::vars(Month)) +
      ggplot2::guides(color = ggplot2::guide_colorbar(
        title = "day of year",
        title.position = "top",
        direction = "vertical"
      )) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 8),
        legend.position = "right",
        legend.justification = "center"
      ) +
      ggplot2::coord_sf(
        crs = crs,
        xlim = xlims,
        ylim = c(33, 45)
      )

    lat <- this_dat %>%
      ggplot2::ggplot(ggplot2::aes(
        y = LAT,
        x = LENGTH,
        color = lubridate::yday(EST_TOWDATE)
      )) +
      ggplot2::geom_point(alpha = 0.25) +
      nmfspalette::scale_color_nmfs(
        palette = "regional web",
        discrete = FALSE,
        limits = c(250, 285)
      ) +
      ggplot2::facet_grid(rows = ggplot2::vars(Month)) +
      ggplot2::scale_y_continuous(n.breaks = 3) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("") +
      ggplot2::theme(
        legend.position = "none",
        panel.grid = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 8),
        aspect.ratio = 0.75
      ) +
      ggplot2::ylab("Latitude") +
      ggplot2::xlab("Fork length (cm)")

    ggpubr::ggarrange(histogram, map, lat,
      ncol = 3,
      common.legend = TRUE,
      legend = "right",
      align = "h",
      widths = c(1.2, 1, 1.2),
      legend.grob = ggpubr::get_legend(map)
    )

    ggplot2::ggsave(here::here("images", paste0(i, ".png")), width = 6.5, height = 4.5)
  }
}

# ** tow calendar ----
coast <- all %>%
  dplyr::mutate(
    DOY = lubridate::yday(EST_TOWDATE),
    Month = lubridate::month(EST_TOWDATE) %>%
      as.factor(),
    Week = lubridate::week(EST_TOWDATE) %>%
      as.factor(),
    Decade = floor(YEAR / 10) * 10,
    Summer_cohort = dplyr::case_when(
      YEAR %in% c(
        1972, 1974, 1976:1979,
        1981, 1982, 1984, 1987,
        1989
      ) ~ "small summer cohort",
      YEAR %in% c(
        1980, 1990, 1995, 1997,
        2001, 2004, 2006
      ) ~ "medium summer cohort",
      YEAR %in% c(
        1992, 1994, 2002, 2003,
        2005, 2007
      ) ~ "large summer cohort"
    ),
    Summer_cohort = ifelse(is.na(Summer_cohort), "unimodal", Summer_cohort)
  ) %>%
  dplyr::filter(
    SEASON == "FALL",
    YEAR %in% 1972:2008,
    LAT >= 35
  ) %>%
  dplyr::select(YEAR, LAT, Month, Week, DOY, Decade, Summer_cohort) %>%
  dplyr::distinct() %>%
  ggplot2::ggplot(ggplot2::aes(
    x = YEAR,
    y = LAT,
    color = Week
  )) +
  ggplot2::geom_point(
    pch = "-",
    cex = 3
  ) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 43),
    lty = 2
  ) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 40.5),
    lty = 2
  ) +
  viridis::scale_color_viridis(
    discrete = TRUE,
    option = "turbo"
  ) +
  ggplot2::facet_wrap(~Summer_cohort) +
  ggplot2::theme_bw() +
  ggplot2::guides(color = ggplot2::guide_legend(ncol = 2))

nyb <- all %>%
  dplyr::mutate(
    DOY = lubridate::yday(EST_TOWDATE),
    Month = lubridate::month(EST_TOWDATE) %>%
      as.factor(),
    Week = lubridate::week(EST_TOWDATE) %>%
      as.factor(),
    Decade = floor(YEAR / 10) * 10,
    Summer_cohort = dplyr::case_when(
      YEAR %in% c(
        1972, 1974, 1976:1979,
        1981, 1982, 1984, 1987,
        1989
      ) ~ "small summer cohort",
      YEAR %in% c(
        1980, 1990, 1995, 1997,
        2001, 2004, 2006
      ) ~ "medium summer cohort",
      YEAR %in% c(
        1992, 1994, 2002, 2003,
        2005, 2007
      ) ~ "large summer cohort"
    ),
    Summer_cohort = ifelse(is.na(Summer_cohort), "unimodal", Summer_cohort)
  ) %>%
  dplyr::filter(
    SEASON == "FALL",
    YEAR %in% 1972:2008,
    LAT >= 40.5,
    LAT <= 43
  ) %>%
  dplyr::select(YEAR, LAT, Month, Week, DOY, Decade, Summer_cohort) %>%
  dplyr::distinct() %>%
  ggplot2::ggplot(ggplot2::aes(
    x = YEAR,
    y = LAT,
    color = Week
  )) +
  ggplot2::geom_point(
    pch = "-",
    cex = 3
  ) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 43),
    lty = 2
  ) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 40.5),
    lty = 2
  ) +
  viridis::scale_color_viridis(
    discrete = TRUE,
    option = "turbo"
  ) +
  ggplot2::facet_wrap(~Summer_cohort) +
  ggplot2::theme_bw() +
  ggplot2::guides(color = ggplot2::guide_legend(ncol = 2))

ggpubr::ggarrange(coast + ggplot2::ggtitle("Atlantic coast"),
  nyb + ggplot2::ggtitle("Approx. NY Bight"),
  common.legend = TRUE,
  legend = "right"
)
ggplot2::ggsave(here::here("images/sampling_cohorts_nyb.png"), width = 8.5, height = 3.5)

# spawning calendar -----
devtools::load_all(here::here("../bluefishLifeHistory"))

surv <- raw_survey_data %>%
  bump_ages() %>%
  interpolate_lengths() %>%
  remove_outliers() %>%
  dplyr::ungroup() %>%
  dplyr::select(
    SampleID, State, Year, Month, Source, Program, Area,
    ForkLengthCM, WeightKG, Sex, Maturity, DataSource,
    Station, Age, AgeType, Semester, Geo, Decade,
    Cohort, CohortDecade, MaturityBin
  ) %>%
  dplyr::filter(
    ForkLengthCM <= 30.3,
    ForkLengthCM > 1
  ) %>%
  dplyr::mutate(size_group = ifelse(ForkLengthCM < 15, "under_15", "over_15")) %>%
  dplyr::group_by(Month, State, Program, size_group) %>%
  dplyr::mutate(
    n_data = dplyr::n(),
    n_year = length(unique(Year))
  ) %>%
  dplyr::filter(
    n_data > 20,
    n_year > 2
  ) %>%
  tidyr::drop_na(State, Program, Month)

surv$Program <- stringr::str_wrap(surv$Program, 15)

plot_summary <- function(data, title) {
  plt <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = Month,
      y = State,
      fill = fish
    )) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_fill_manual(
      values = c(
        "gray90",
        nmfspalette::nmfs_palette(palette = "oceans")(3)[2]
      ),
      name = "Fish observed*?"
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      minor_breaks = seq(0.5, 12.5, by = 1)
    ) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::ggtitle(title)

  return(plt)
}
all_obs <- raw_survey_data %>%
  tidyr::drop_na(Month, State) %>%
  dplyr::ungroup() %>%
  dplyr::select(Month, State) %>%
  dplyr::distinct() %>%
  dplyr::mutate(survey = TRUE)

dat <- surv %>%
  dplyr::ungroup() %>%
  dplyr::filter(ForkLengthCM < 15) %>%
  dplyr::select(Month, State) %>%
  dplyr::distinct() %>%
  dplyr::mutate(fish = "yes") %>%
  dplyr::full_join(all_obs) %>%
  dplyr::mutate(fish = ifelse(survey == TRUE & is.na(fish), "no", fish))

figa <- plot_summary(dat, title = "Bluefish <15cm")

dat <- surv %>%
  dplyr::ungroup() %>%
  dplyr::filter(ForkLengthCM >= 15 & ForkLengthCM <= 30.3) %>%
  dplyr::group_by(Month, State) %>%
  dplyr::distinct() %>%
  dplyr::mutate(fish = "yes") %>%
  dplyr::full_join(all_obs) %>%
  dplyr::mutate(fish = ifelse(survey == TRUE & is.na(fish), "no", fish))

figb <- plot_summary(dat, title = "Bluefish 15-30.3cm")

ggpubr::ggarrange(figa, figb, ncol = 1, common.legend = TRUE, legend = "bottom")
ggplot2::ggsave(here::here("images/cohort_calendar2.png"), width = 3, height = 5)

## natural mortality ----

# calculate bootstrap intervals for Then M estimate

m_then <- tibble::tibble(const = rnorm(10000, mean = 4.899, sd = 0.33),
                         exponent = rnorm(10000, mean = -0.916, sd = 0.04),
                         M_est = const * 14^exponent)
hist(m_then$M_est)
quantile(m_then$M_est, c(0.025, 0.975))

# lorenzen estimates
m_lor <- tibble::tibble(Age = 0:6,
                        M_est = c(0.859, 0.578, 0.458, 0.379, 0.326, 0.294, 0.269),
                        lower = c(0.538, 0.330, 0.246, 0.193, 0.158, 0.137, 0.121),
                        upper = c(1.195, 0.843, 0.687, 0.583, 0.511, 0.468, 0.432))

m_dat <- dplyr::full_join(tibble::tibble(M_est = mean(m_then$M_est),
                                         lower = quantile(m_then$M_est, 0.025),
                                         upper = quantile(m_then$M_est, 0.975),
                                         Age = 0:6) %>%
                            dplyr::mutate(Method = "Then et al."),
                          m_lor %>%
                            dplyr::mutate(Method = "Lorenzen"))

m_dat %>%
ggplot2::ggplot(ggplot2::aes(x = Age,
                             y = M_est,
                             color = Method,
                             fill = Method)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower,
                                    ymax = upper),
                       alpha = 0.25) +
  nmfspalette::scale_color_nmfs(palette = "regional web") +
  nmfspalette::scale_fill_nmfs(palette = "regional web") +
  ggplot2::theme_bw() +
  ggplot2::ylab("Natural mortality estimate") +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(here::here("images/natural_mortality.png"), width = 3, height = 3)
