source("/Users/Adam/Capstone Functions.R")
library(readr)
data <- read_csv('/Users/Adam/Downloads/Lake Sunapee Data.csv')
colnames(data)

indices <- which(!is.na(data$temperature_degC))

data_clean <- cbind(as.numeric(data$temperature_degC[indices]), data$instrument_datetime[indices])


head(data_clean)


b2 <- c(0.6, 2.5, 0.15)

# DATA COLLECTION SECTION 1

library(dplyr)
library(tidyverse) # if not installed run install.packages('tidyverse')
library(arrow) # install.packages('arrow')
library(lubridate) # install.packages('lubridate')
# Observed weather
obs_met <- read_csv("https://s3.flare-forecast.org/targets/fcre_v2/fcre/observed-met_fcre.csv")

# Forecasted weather
forecast_dir <- arrow::s3_bucket(bucket = "drivers/noaa/gefs-v12/stage2/parquet/0",
                                 endpoint_override =  "s3.flare-forecast.org", 
                                 anonymous = TRUE)
forecast_dates1 <- seq.Date(lubridate::as_date('2019-12-01'), lubridate::as_date('2020-01-30'), by = 'day')
forecast_dates2 <- seq.Date(lubridate::as_date('2020-12-01'), lubridate::as_date('2021-01-30'), by = 'day')


forecast_dates <- c(forecast_dates1, forecast_dates2)

# running this will show you what the column names are
arrow::open_dataset(forecast_dir) 

#this dataset is VERY large and needs to be filtered before collecting
forecasted_met <- arrow::open_dataset(forecast_dir) |> 
  filter(site_id == 'sunp', # Falling Creek Reservoir (site_id code) #sunp for sunapee
         reference_datetime %in% forecast_dates) |> 
  # you can also filter/select based on other columns in the dataset
  # collect brings the data into your local environment
  collect()

#===================================#
# Do join
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


forecasted_met <- forecasted_met[which(forecasted_met$variable == "air_temperature"),]



data_clean <- as.data.frame(cbind(as.numeric(data$temperature_degC[indices]), data$instrument_datetime[indices]))
colnames(data_clean) <- c('observation', 'orig_datetime')


data_clean = data_clean %>% select('observation', 'orig_datetime') %>%
  mutate(datetime = mdy_hm(orig_datetime))

data_clean[,1] <- as.numeric(data_clean[,1])

met_joined <- dplyr::inner_join(forecasted_met, 
                                data_clean, 
                                by = c('datetime'), copy = TRUE)
met_joined <- met_joined[-8]

met_joined[7] <- met_joined[7] + 273.15

obs <- met_joined[7]

params <- ensemble_parameters_seperate(met_joined)

adj_ens <- ensemble_converter(met_joined, params)

ref_datetime <- lubridate::as_date('2021-01-01')

adj_ens_dec1 <- adj_ens[which(adj_ens$reference_datetime == ref_datetime),]

ens_dec1_data <- adj_ens_dec1[4:33]

param_joined <- arrange(met_joined, parameter)

wider_joined <- pivot_wider(data = param_joined, id_cols = c(horizon, datetime,
                                                             reference_datetime), 
                            names_from = parameter, 
                            values_from = 'observation')

wider_predicted <- pivot_wider(data = param_joined, id_cols = c(horizon, datetime,
                                                             reference_datetime), 
                            names_from = parameter, 
                            values_from = 'prediction')

wider_predicted <- arrange(wider_predicted, datetime)

orig_pred = wider_predicted[,4:34]

orig_pred = orig_pred[which(wider_predicted$reference_datetime == ref_datetime),]

wider_joined <- arrange(wider_joined, datetime)

obs_dec1 <- wider_joined[which(adj_ens$reference_datetime == ref_datetime),]

ens_dec1_data <- as.data.frame(cbind(adj_ens_dec1, obs_dec1))

ens_dec1_data <- ens_dec1_data[4:34]

length(ens_dec1_data$`1`)

library(ggmatplot)

ens_dec1_data <- (ens_dec1_data  - 273.15) * 9/5 + 32


p1 <- ggmatplot(x = c(1:796), y = ens_dec1_data, plot_type = 'line',
                linetype = 'solid', 
                color = c(rep('black', 30), 'red'))
p1 = p1 + xlab("Hours Ahead") + ylab("Air Temperature (F)") +
  ggtitle("Adjusted Ensemble and Observed Temperature")



orig_ens_dec1_data <- as.data.frame(cbind(orig_pred, obs_dec1))

orig_ens_dec1_data <- (orig_ens_dec1_data  - 273.15) * 9/5 + 32


p2 <- ggmatplot(x = c(1:796), y = orig_ens_dec1_data, plot_type = 'line',
                      linetype = 'solid', 
                      color = c(rep('black', 30), 'red'))
p2 = p2 + xlab("Hours Ahead") + ylab("Air Temperature (F)") +
  ggtitle("Original Ensemble and Observed Temperature")




data_range <- data$datetime_noDST[indices]
max(data_range)
