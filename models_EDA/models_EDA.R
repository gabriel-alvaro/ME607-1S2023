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
# data = data %>%
#   mutate(Date = format(as.Date(dmy_hms(`Date Time`)), "%Y-%m")) %>%
#   group_by(Date) %>%
#   summarise(Temperature = mean(`T (degC)`),
#             RelativeHumidity = mean(`rh (%)`))
# 
# readr::write_csv(as.data.frame(data), file = "max_planck_weather_monthly.csv")

# importando dados
data = read_csv("https://raw.githubusercontent.com/gabriel-alvaro/ME607-1S2023/main/models_EDA/max_planck_weather_monthly.csv") %>%
  mutate(Date = yearmonth(Date)) %>%
  as_tsibble()

autoplot(data, .vars = Temperature) +
  xlab("Date") +
  ylab("Temperature (Celsius)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_yearmonth(date_breaks = "1 year",
                    date_label = "%Y-%m") +
  theme_bw()

## MODELOS
## media, naive, naive sazonal, drift, regressao linear, suavizacao exponencial

data_train = data |>
  stretch_tsibble(.step = 1, .init = 100)

data_train |>
  model(RW(Temperature ~ drift())) |>
  forecast(h = 1) |>
  accuracy(data)
  


