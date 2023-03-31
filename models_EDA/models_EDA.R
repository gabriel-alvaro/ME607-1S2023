## Gabriel Alvaro Batista
## RA 171822

library(fable)
library(fpp3)
library(tidyverse)

## TRATAMENTO DATASET
## dataset obtido no Kaggle: https://www.kaggle.com/datasets/arashnic/max-planck-weather-dataset

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

# importando e tratando dados
weather = read_csv("https://raw.githubusercontent.com/gabriel-alvaro/ME607-1S2023/main/models_EDA/max_planck_weather_daily.csv")
weather = as_tsibble(weather)

scan_gaps(weather)

# adiciona dados faltantes (2016-10-26; 2016-10-27) com a media das colunas
weather = weather |> fill_gaps(Temperature = mean(Temperature),
                               RelativeHumidity = mean(RelativeHumidity))

autoplot(weather, .vars = Temperature) +
  xlab("Date") +
  ylab("Temperature (Celsius)") +
  ggtitle("Temperature (°C) in Jena, Germany from 2014 to 01-01-2016") +
  theme_bw() +
  scale_y_continuous(n.breaks = 6) +
  scale_x_yearmonth(date_breaks = "1 year",
                    date_label = "%d-%m-%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust=1))

## MODELOS
## media, naive, naive sazonal, drift, regressao linear, suavizacao exponencial

## sampling
weather_train_rw = weather |>
  tile_tsibble(.size = 50)

weather_train_sw = weather |>
  stretch_tsibble(.step = 1, .init = 50)

weather_train_rw |>
  model(mean = MEAN(Temperature),
        naive = NAIVE(Temperature),
        snaive = SNAIVE(Temperature ~ lag(12)),
        drift = RW(Temperature ~ drift())) |>
  forecast(h = 1) |>
  accuracy(weather)

weather_train_sw |>
  model(mean = MEAN(Temperature),
        naive = NAIVE(Temperature),
        snaive = SNAIVE(Temperature ~ lag(12)),
        drift = RW(Temperature ~ drift())) |>
  forecast(h = 1) |>
  accuracy(weather)





