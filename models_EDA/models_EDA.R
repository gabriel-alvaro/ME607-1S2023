## Gabriel Alvaro Batista
## RA 171822

library(fable)
library(fpp3)
library(tidyverse)

## DATA TREATMENT
## dataset obtained from Kaggle: https://www.kaggle.com/datasets/arashnic/max-planck-weather-dataset

# data = readr::read_csv("max_planck_weather_ts.csv")
# 
# data = data |> select(`Date Time`,
#                 `T (degC)`,
#                 `rh (%)`)
# 
# # daily
# data_daily = data |>
#   mutate(Date = format(as.Date(dmy_hms(`Date Time`)), "%Y-%m-%d")) |>
#   filter(year(Date) %in% c(2014, 2015, 2016)) |>
#   group_by(Date) |>
#   summarise(Temperature = mean(`T (degC)`),
#             RelativeHumidity = mean(`rh (%)`))
# 
# # monthly
# data_monthly = data |>
#   mutate(Date = format(as.Date(dmy_hms(`Date Time`)), "%Y-%m")) |>
#   group_by(Date) |>
#   summarise(Temperature = mean(`T (degC)`),
#             RelativeHumidity = mean(`rh (%)`)) |>
#   filter(year(yearmonth(Date)) %in% c(2014, 2015, 2016))
# 
# readr::write_csv(as.data.frame(data_daily), file = "max_planck_weather_daily.csv")
# readr::write_csv(as.data.frame(data_monthly), file = "max_planck_weather_monthly.csv")

# importing
weather = read_csv("https://raw.githubusercontent.com/gabriel-alvaro/ME607-1S2023/main/models_EDA/max_planck_weather_daily.csv")
weather = as_tsibble(weather)

# treating missing data
scan_gaps(weather)
weather = weather |> fill_gaps(Temperature = mean(Temperature),
                               RelativeHumidity = mean(RelativeHumidity))

# plot
autoplot(weather, .vars = Temperature) +
  xlab("Date") +
  ylab("Temperature (Celsius)") +
  ggtitle("Temperature (°C) in Jena, Germany from 2014 to 2016") +
  theme_bw() +
  scale_y_continuous(n.breaks = 6) +
  scale_x_yearmonth(date_breaks = "1 year",
                    date_label = "%d-%m-%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust=1))

## MODELS
## mean, naive, seasonal naive, drift, linear regression, exponential smoothing

## sampling
weather_train_rw = weather |>
  tile_tsibble(.size = 50)

## training all models
weather_acc = weather_train_rw |>
  model(mean = MEAN(Temperature),
        naive = NAIVE(Temperature),
        snaive = SNAIVE(Temperature ~ lag(12)),
        drift = RW(Temperature ~ drift())) |>
  forecast(h = 1) |>
  accuracy(weather) |>
  arrange(RMSE)

weather_acc

## training all models (h = 3)
weather_train_rw |>
  model(mean = MEAN(Temperature),
        naive = NAIVE(Temperature),
        snaive = SNAIVE(Temperature ~ lag(12)),
        drift = RW(Temperature ~ drift())) |>
  forecast(h = 3) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Temperature", distribution = Temperature) |>
  accuracy(weather, by = c("h", ".model")) |>
  arrange(RMSE)

# check accuracy statistics
weather_acc

# use best performing model
fit = weather |>
  model(mean = NAIVE(Temperature))

# fit model to series and plot
fit |> 
  forecast(h = 1) |> 
  autoplot(
    weather |>
      filter(Date >= "2016-06-01"),
    level = NULL
  )

# check residuals
fit |> gg_tsresiduals()


