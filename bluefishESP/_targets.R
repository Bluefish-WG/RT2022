
# Set target options:
# devtools::load_all("../bluefishLifeHistory")
targets::tar_option_set(envir = getNamespace("bluefishESP"),
                        packages = c("bluefishESP"),
                        imports = c("bluefishESP"))
library(bluefishESP)
library(mgcv)

#repull data if necessary
# source(here::here("data-raw/data-pulls/pull_nmfs_bottom_trawl.R"))
# source(here::here("data-raw/data-pulls/pull_MRIP_data.R"))

# Target list:
list(

  ## read in bottom trawl data
  # re-run pull script manually if needed, this just checks the if the file has been updated
  targets::tar_target(bluefish_nmfs,
                      bluefishESP::bluefish_nmfs),

  # calculations/aggregations on downloaded data ----

  ### estimated MRIP catch
  targets::tar_target(catch_rds,
                      here::here("data-raw/catch_wave.RDS"),
                      format = "file"),
  targets::tar_target(mrip_catch,
             create_mrip_catch(rds = catch_rds)),

  ### estimated MRIP effort
  targets::tar_target(effort_rds,
                      here::here("data-raw/effort.RDS"),
                      format = "file"),
  targets::tar_target(mrip_effort,
             create_mrip_effort(rds = effort_rds)),

  # create indicators from spreadsheets ----

  ## * community indicators ----
  targets::tar_target(community_file,
                      here::here("data-raw/2009-2019 Rec Scores_ME_MS_031522 cw.xlsx"),
                      format = "file"),
  targets::tar_target(community_indicators,
                      create_community_indicators(filepath = community_file)),

  ## * recreational indicators ----

  ### total rec catch
  targets::tar_target(rec_catch,
             create_total_rec_catch(data = mrip_catch)),

  ### bluefish trips
  targets::tar_target(trip_files,
                      list.files(here::here("data-raw/MRIP_directed_trips"),
                                 full.names = TRUE),
                      format = "file"),
  targets::tar_target(bluefish_trips,
             create_bluefish_trips(files = trip_files)),

  ### prop bluefish trips
  targets::tar_target(prop_bluefish_trips,
             create_prop_bluefish_trips(
               total = mrip_effort,
               bluefish = bluefish_trips
             )),

  ### total rec landings
  targets::tar_target(rec_landings,
             create_total_rec_landings(data = mrip_catch)),

  ## * commercial indicators ----
  targets::tar_target(comm_file,
                      here::here("data-raw/SOCIEOECONOMIC_COMMERCIAL_INDICATORS_FINAL_2020Constantdols.csv"),
                      format = "file"),
  targets::tar_target(comm_indicators,
                      create_comm_indicators(data = comm_file)),

  ## * natural mortality indicators ----

  ### mako
  targets::tar_target(mako_file,
                      here::here("data-raw/SMA2017_Bratio.xlsx"),
                      format = "file"),
  targets::tar_target(mako,
             create_mako(file = mako_file)),

  # condition function loads bluefishLifeHistory R package for calculations
  targets::tar_target(condition,
                      create_relative_condition()),

  ## * distribution indicators ----

  ### bottom trawl (VAST)
  targets::tar_target(VAST,
                      create_cog()),

  ## * climate indicators ----

  ### raster indicators
  # .nc data needs to be downloaded through a gh action, so not adding to {targets} pipeline
  # just monitor files for changes
  targets::tar_target(temp_indicators,
                      here::here("data-raw/temperature_indicators.csv"),
                      format = "file"),
  targets::tar_target(jtemp_indicator,
                      here::here("data-raw/temperature_july_bfstrata.csv"),
                      format = "file"),

  targets::tar_target(first_18_day,
                      create_first_18_day(data = temp_indicators)),
  targets::tar_target(last_18_day,
                      create_last_18_day(data = temp_indicators)),
  targets::tar_target(n_18_day,
                      create_n_18_day(data = temp_indicators)),
  targets::tar_target(july_proportion,
                      create_july_proportion(data = jtemp_indicator)),

  targets::tar_target(wind_indicators_u_nc,
                      here::here("data-raw/uwnd.10m.mon.mean.nc"),
                      format = "file"),
  targets::tar_target(wind_indicators_v_nc,
                      here::here("data-raw/vwnd.10m.mon.mean.nc"),
                      format = "file"),
  targets::tar_target(wind_indicators,
                      create_wind_indicators(vwind_file = wind_indicators_v_nc,
                                             uwind_file = wind_indicators_u_nc)),

  # pull indicators together into one dataframe ----
  targets::tar_target(test,
                      {
                        # socioeconomic
                        rec_catch %>%
                        dplyr::full_join(rec_catch) %>%
                          dplyr::full_join(bluefish_trips) %>%
                          dplyr::full_join(prop_bluefish_trips) %>%
                          dplyr::full_join(rec_landings) %>%

                          dplyr::full_join(community_indicators) %>%

                          # commercial
                          dplyr::full_join(comm_indicators) %>%

                          # distribution
                          dplyr::full_join(VAST) %>%

                          # natural mortality
                          dplyr::full_join(mako) %>%
                          dplyr::full_join(condition) %>%

                          # climate
                          dplyr::full_join(first_18_day) %>%
                          dplyr::full_join(last_18_day) %>%
                          dplyr::full_join(n_18_day) %>%
                          dplyr::full_join(july_proportion) %>%
                          dplyr::full_join(wind_indicators)
                      }),

  # create .Rmd ----
  tarchetypes::tar_render(indicator_report,
                        path = here::here("docs/indicator_figs.Rmd"),
                        params = list(data = test,
                                      mrip_catch = mrip_catch,
                                      trip_files = trip_files,
                                      bluefish_nmfs = bluefish_nmfs,
                                      # dummy calls to fxns used in .Rmd
                                      # so targets "knows" about them
                                      find_fxns = list(try(esp_traffic_fig()),
                                                       try(plot_corr()))),
                        output_file = c(here::here("docs/indicator_figs.docx"))
                      )

)

#tarchetypes::tar_knitr_deps_expr(indicator_report)
