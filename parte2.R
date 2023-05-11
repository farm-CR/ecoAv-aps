library(readr)
library(tidyverse)
library(forecast)
library(mFilter)
library(modelsummary)

ibc <- read_csv("IBC-Br dessazonalizado (2002 = 100).csv")
ipca <- read_csv("IPCA variacao.csv")

colnames(ibc) <- c("Data", "Value", "1")
ibc <- ibc %>%
  select(Data, Value) %>% 
  filter(!is.na(Value))

colnames(ipca) <- c("Data", "Value", "1")
ipca <- ipca %>% 
  select(Data, Value) %>% 
  filter(Data >= 2003.01 & Data <= 2023.02)

ibc <- ts(log(ibc$Value), start = c(2003, 1), frequency = 12)
ipca <- ts(ipca$Value, start = c(2003, 1), frequency = 12)



fit <- tslm(ibc ~ trend)
modelsummary(list("(2.1)" = fit), stars = c("*" = .1, "**" = .05, "***" = .01), gof_map = c("nobs", "r.squared"))
acf(residuals(fit))
pacf(residuals(fit))
plot(residuals(fit))

data <- data.frame(
  month = as.numeric(time(ibc)),
  trend = fit$fitted.values,
  residual = fit$residuals,
  series = fit$data$ibc
) %>% 
  pivot_longer(c(series, trend, residual), names_to = "serie", values_to = "value")

# ggplot(data, aes(x = month)) +
#   geom_line(aes(y = residual)) +
#   geom_line(aes(y = trend-4)) +
#   geom_line(aes(y = series-4)) +
#   scale_y_continuous(
#     name = "Residuals",
#     sec.axis = sec_axis(~.+4)
#   )

data %>% 
  mutate(residual = serie == "residual") %>% 
  ggplot(aes(month, value, color = serie)) +
  geom_line() +
  facet_wrap(~residual, nrow = 2, scales = "free")


ibc.hp <- hpfilter(ibc, freq = 14400)
# par(mfrow=c(2,1))
# ts.plot(df, col="blue")
# lines(df.hp$trend, col="red")
# ts.plot(df.hp$cycle, col="green4")
data <- data.frame(
  month = as.numeric(time(ibc)),
  series = ibc,
  trend = as.numeric(ibc.hp$trend),
  residual = ibc.hp$cycle
) %>% 
  pivot_longer(c(series, trend, residual), names_to = "serie", values_to = "value")

data %>% 
  mutate(residual = serie == "residual") %>% 
  ggplot(aes(month, value, color = serie)) +
  geom_line() +
  facet_wrap(~residual, nrow = 2, scales = "free")

data <- data.frame(
  month = as.numeric(time(df)),
  hiato = df.hp$cycle,
  ipca = as.numeric(ipca)
)
data %>% 
  ggplot(aes(x = month)) +
  geom_line(aes(y = hiato), color = "red") +
  geom_line(aes(y = ipca*5)) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~./5, name = "")
  )
