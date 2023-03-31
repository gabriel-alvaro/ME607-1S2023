## Gabriel Alvaro Batista
## RA 171822

library(fable)
library(fpp3)
library(tidyverse)

## TRATAMENTO DATASET
## dataset obtido no Kaggle: https://www.kaggle.com/datasets/arashnic/max-planck-weather-dataset

# data = readr::read_csv("max_planck_weather_ts.csv")
# 
# data = data %>% select(`Date Time`,
#                 `T (degC)`,
#                 `rh (%)`)
# 
# # daily
# data_daily = data %>%
#   mutate(Date = format(as.Date(dmy_hms(`Date Time`)), "%Y-%m-%d")) %>%
#   group_by(Date) %>%
#   summarise(Temperature = mean(`T (degC)`),
#             RelativeHumidity = mean(`rh (%)`))
# 
# # monthly 
# data_monthly = data %>%
#   mutate(Date = format(as.Date(dmy_hms(`Date Time`)), "%Y-%m")) %>%
#   group_by(Date) %>%
#   summarise(Temperature = mean(`T (degC)`),
#             RelativeHumidity = mean(`rh (%)`))
# 
# readr::write_csv(as.data.frame(data_daily), file = "max_planck_weather_daily.csv")
# readr::write_csv(as.data.frame(data_monthly), file = "max_planck_weather_monthly.csv")

# importando dados
weather = read_csv("https://raw.githubusercontent.com/gabriel-alvaro/ME607-1S2023/main/models_EDA/max_planck_weather_daily.csv")
weather = as_tsibble(weather)

autoplot(weather, .vars = Temperature) +
  xlab("Date") +
  ylab("Temperature (Celsius)") +
  theme_bw() +
  scale_y_continuous(n.breaks = 6) +
  scale_x_yearmonth(date_breaks = "1 year",
                    date_label = "%d-%m-%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust=1))

## MODELOS
## media, naive, naive sazonal, drift, regressao linear, suavizacao exponencial

weather_train_rw = weather |>
  tile_tsibble(.size = 50)

weather_train_sw = weather |>
  stretch_tsibble(.step = 1, .init = 50)

weather_train_rw |>
  model(mean = MEAN(Temperature)) |>
  forecast(h = 1) |>
  accuracy(weather)

weather_train_sw |>
  model(mean = MEAN(Temperature)) |>
  forecast(h = 1) |>
  accuracy(weather)




