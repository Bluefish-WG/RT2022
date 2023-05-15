`%>%` <- magrittr::`%>%`

# download data ----

# MRIP data was accessed through the links on this public-facing page:
# https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads

# Survey data was accessed through the URLs linked here:
# https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/
# Survey trip and catch data description (copied from website):
# TRIP_YYYYW.sas7bdat: Trip-level data (analogous to MRFSS i1 dataset)
# and variables required for estimation.
# Contains one record per angler trip interview (identified by id_code).
# CATCH_YYYYW.sas7bdat: Catch-level data and variables required for estimation.
# Contains one record per species for each angler trip interview.

# Estimate data was accessed through the URLs linked here:
# https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/
# Estimate data description (copied from website):
# Calibrated estimates are available for the Atlantic and Gulf coasts beginning in 1981.


## SURVEY data ----
urls <- paste0("https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/CSV/ps_",
                 c("1981_1989", "1990_1994", "1995_1999", "2000_2004", "2005_2009",
                   "2010_2014", "2015_2016", "2017", "2018", "2019", "2020",
                   "2021_preliminary"),
                 "_csv.zip")

# download and extract all existing data
lapply(urls, download_and_extract, exdir = "data-raw/MRIP_survey")

# compile all years
compile("catch",
        dir = "data-raw/MRIP_survey",
        outdir = "data-raw")
compile("trip",
        dir = "data-raw/MRIP_survey",
        outdir = "data-raw")

# ESTIMATE data ----
urls <- paste0("https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/mrip_",
       c("catch_1981_1989", "catch_1990_1999", "catch_2000_2009", "catch_2010_2016",
       "catch_2017", "catch_2018", "catch_2019", "catch_2020",
       "catch_2021",
       "effort_1981_1989", "effort_1990_1999", "effort_2000_2009", "effort_2010_2016",
       "effort_2017", "effort_2018", "effort_2019", "effort_2020",
       "effort_2021"),
       "_csv.zip")

# download and extract all existing data
lapply(urls, download_and_extract, pattern = "mrip_")

compile(type = "catch_wave", dir = "data-raw")
compile(type = "effort", dir = "data-raw")

