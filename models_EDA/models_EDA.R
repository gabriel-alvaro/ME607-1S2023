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
#             RelativeHumidity = mean(`rh (%)`))
# 
# readr::write_csv(as.data.frame(data_daily), file = "max_planck_weather_daily.csv")
# readr::write_csv(as.data.frame(data_monthly), file = "max_planck_weather_monthly.csv")

# importing
weather = read_csv("https://raw.githubusercontent.com/gabriel-alvaro/ME607-1S2023/main/models_EDA/max_planck_weather_monthly.csv")

# for daily data
# weather = as_tsibble(weather) %>%
#   select(Date, Temperature)

# for monthly data
weather = weather |>
  mutate(Date = yearmonth(Date)) |>
  select(Date, Temperature) |>
  as_tsibble()

# treating missing data
scan_gaps(weather)
weather = weather |> 
  fill_gaps(Temperature = mean(Temperature))

# plot
autoplot(weather, .vars = Temperature) +
  xlab("Date") +
  ylab("Temperature (Celsius)") +
  ggtitle("Monthly average of temperature (°C) in Jena, Germany from 2009 to 2017") +
  theme_bw() +
  scale_y_continuous(n.breaks = 6) +
  scale_x_yearmonth(date_breaks = "1 year",
                    date_label = "%d-%m-%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust=1))

## MODELS
## mean, naive, seasonal naive, drift, linear regression, exponential smoothing
## sampling
# weather_train_rw = weather |>
#   tile_tsibble(.size = 180)

weather_train_sw = weather |>
  stretch_tsibble(.step = 1, .init = 12)

## training models (h = 5)
accuracy_h5 = weather_train_sw |>
  model(mean = MEAN(Temperature),
        naive = NAIVE(Temperature),
        snaive = SNAIVE(Temperature ~ lag(12)),
        drift = RW(Temperature ~ drift()),
        lm = TSLM(Temperature ~ Date),
        exphw = ETS(Temperature ~ error("A"))) |>
  forecast(h = 5) |>
  group_by(.id, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Temperature", distribution = Temperature) |>
  accuracy(weather, by = c("h", ".model"))

accuracy_h5 |>
  select(h, .model, RMSE) |>
  arrange(h, RMSE) |>
  pivot_wider(names_from = .model, values_from = RMSE)

# using best performing model (SNAIVE)
fit = weather |>
  model(snaive = SNAIVE(Temperature ~ lag(12)))

fit |> 
  forecast(h = 5) |> 
  autoplot(weather, level = NULL)


snaive = forecast::snaive(weather)

# diagnostic
fit |> gg_tsresiduals()

snaive_res = residuals(fit) |> na.exclude()

# plot and mean
autoplot(snaive_res, .vars = .resid) +
  geom_hline(yintercept = mean(snaive_res$.resid),
             col = 'red', linetype = 'dashed')

# histogram
hist(snaive_res$.resid, breaks = 20)

# autocorrelation
acf(snaive_res,
    type = "correlation")

# autocorelation tests
Box.test(snaive_res$.resid, type = "Box-Pierce", lag = 12)
Box.test(snaive_res$.resid, type = "Ljung-Box" ,lag = 12)

# normality test
shapiro.test(snaive_res$.resid)




