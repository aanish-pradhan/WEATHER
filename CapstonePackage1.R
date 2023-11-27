# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


intstall.packages(tidyverse) # if not installed run install.packages('tidyverse')
install.packages(arrow) # install.packages('arrow')
install.packages(lubridate)
install.packages(dpylr)
install.packages(tidyr)

hello <- function() {
  print("Hello, world!")
}




load_data<- function(){
  obs_met <- read_csv("https://s3.flare-forecast.org/targets/fcre_v2/fcre/observed-met_fcre.csv")

  # Forecasted weather
  forecast_dir <- arrow::s3_bucket(bucket = "drivers/noaa/gefs-v12/stage2/parquet/0",
                                   endpoint_override =  "s3.flare-forecast.org",
                                   anonymous = TRUE)
  forecast_dates1 <- seq.Date(lubridate::as_date('2020-06-01'), lubridate::as_date('2020-06-30'), by = 'day')
  forecast_dates2 <- seq.Date(lubridate::as_date('2021-06-01'), lubridate::as_date('2021-06-30'), by = 'day')
  forecast_dates3 <- seq.Date(lubridate::as_date('2022-06-01'), lubridate::as_date('2022-06-30'), by = 'day')

  forecast_dates <- c(forecast_dates1, forecast_dates2, forecast_dates3)

  # running this will show you what the column names are
  arrow::open_dataset(forecast_dir)

  #this dataset is VERY large and needs to be filtered before collecting
  forecasted_met <- arrow::open_dataset(forecast_dir) |>
    filter(site_id == 'fcre', # Falling Creek Reservoir (site_id code)
           reference_datetime %in% forecast_dates) |>
    # you can also filter/select based on other columns in the dataset
    # collect brings the data into your local environment
    collect()

  #===================================#
  # Do joiny
  # Wrangle the data into the same formats
  forecasted_met <-
    forecasted_met |>
    tidyr::pivot_wider(names_from = variable,
                       id_cols = c(horizon, parameter, reference_datetime, datetime),
                       values_from = prediction) |>

    # calculate wind speed from eastward and northward directions
    dplyr::mutate(wind_speed = sqrt(eastward_wind^2 + northward_wind^2)) |>
    dplyr::select(#'site_id',
      #'height',
      'horizon',
      'parameter',
      'reference_datetime',
      'datetime',
      "air_temperature",
      "air_pressure",
      "relative_humidity",
      "surface_downwelling_longwave_flux_in_air",
      "surface_downwelling_shortwave_flux_in_air",
      "precipitation_flux",
      "wind_speed") |>
    tidyr::pivot_longer(cols = air_temperature:wind_speed,
                        names_to = 'variable', values_to = 'prediction')

  met_joined <- dplyr::inner_join(forecasted_met,
                                  obs_met,
                                  by = c('datetime', 'variable'))

}

