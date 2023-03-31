## Gabriel Alvaro Batista
## RA 171822

## MODELOS:
## media, naive, naive sazonal, drift, regressao, suavizacao exponencial

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
data = read_csv("https://raw.githubusercontent.com/gabriel-alvaro/ME607-1S2023/main/models_EDA/max_planck_weather_monthly.csv")

# grafico
data %>% ggplot(aes(x = as.Date(paste(Date, "-01", sep="")), y = Temperature)) +
  geom_line() +
  xlab("Data") +
  ylab("Temperatura (Celsius)") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m",
               date_breaks = "1 year") +
  scale_y_continuous(n.breaks = 6) +
  theme(axis.text.x=element_text(angle=60, hjust=1))



