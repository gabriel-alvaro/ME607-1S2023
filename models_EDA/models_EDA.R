## Gabriel Alvaro Batista
## RA 171822

## MODELOS:
## media, naive, naive sazonal, drift, regressao, suavizacao exponencial

library(fable)
library(fpp3)
library(ggplot2)
library(dplyr)

## TRATAMENTO DATASET
## dataset obtido no Kaggle: https://www.kaggle.com/datasets/arashnic/max-planck-weather-dataset
# tratamento feito localmente
# data = readr::read_csv("max_planck_weather_ts.csv")
#
# data = data %>% select(`Date Time`,
#                 `T (degC)`,
#                 `rh (%)`)
#
# data = data %>%
#   mutate(index = yearmonth(as.Date(dmy_hms(`Date Time`)))) %>%
#   group_by(index) %>%
#   summarise(Temperature = mean(`T (degC)`),
#             RelativeHumidity = mean(`rh (%)`))
#
# readr::write_csv(as.data.frame(data), file = "max_planck_weather_monthly.csv")

data = readr::read_csv("https://raw.githubusercontent.com/gabriel-alvaro/ME607-1S2023/main/models_EDA/max_planck_weather_monthly.csv")




