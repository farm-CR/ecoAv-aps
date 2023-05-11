library(readr)
library(tidyverse)
library(forecast)
library(mFilter)
library(modelsummary)
library(lmtest)

### Series Temporais ----

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

### Tendencias ----

# Tendencia linear
fit <- tslm(ibc ~ trend)
modelsummary(list("(2.1)" = fit), stars = c("*" = .1, "**" = .05, "***" = .01), gof_map = c("nobs", "r.squared"))
acf(residuals(fit))
pacf(residuals(fit))
plot(residuals(fit))

data <- tibble(
  month = as.numeric(time(ibc)),
  trend = fit$fitted.values,
  residual = fit$residuals,
  series = fit$data$ibc
) %>% 
  pivot_longer(c(series, trend, residual), names_to = "serie", values_to = "value")

data %>% 
  mutate(residual = serie == "residual") %>% 
  ggplot(aes(month, value, color = serie)) +
  geom_line() +
  facet_wrap(~residual, nrow = 2, scales = "free")

# Filtro HP
ibc.hp <- hpfilter(ibc, freq = 14400)
data <- tibble(
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

# Comparação
data <- tibble(
  month = as.numeric(time(ibc)),
  linear_trend = fit$fitted.values,
  hp_filter = as.numeric(ibc.hp$trend)
) %>% 
  pivot_longer(c(linear_trend, hp_filter), names_to = "serie", values_to = "value")

data %>% 
  ggplot(aes(month, value, color = serie)) +
  geom_line()

### Produto e Inflacao ----

plot(ipca)
acf(ipca)
pacf(ipca)

data <- tibble(
  month = as.numeric(time(ibc)),
  hiato = as.numeric(ibc.hp$cycle),
  ipca = as.numeric(ipca),
  ipca_lag = lag(as.numeric(ipca))
) %>% slice(-1)

# OLS
fit <- lm(ipca ~ ipca_lag + hiato,
                   data = data)
modelsummary(list("(2.2)" = fit), stars = c("*" = .1, "**" = .05, "***" = .01), gof_map = c("nobs", "r.squared"))
acf(fit$residuals)
pacf(fit$residuals)

# Arima
fit <- arima(ipca, order = c(1, 0, 0), xreg = ibc.hp$cycle)
# modelsummary(list("(2.3)" = fit), stars = c("*" = .1, "**" = .05, "***" = .01), gof_map = c("nobs", "rmse"))
coeftest(fit)
acf(fit$residuals)
pacf(fit$residuals)

# Teste Ljung-Box
LB <- NA
lags <- 1:12
for (i in lags) {
  LB[i] <- Box.test(fit$residuals, lag = i, type = "Ljung-Box", fitdf = 1)$p.value
}

tibble(
  pvalue = LB,
  lag = as_factor(lags)
) %>% 
  ggplot(aes(lag, pvalue)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed")
  
# Produto e Inflacao
data <- tibble(
  month = as.numeric(time(ibc)),
  hiato = ibc.hp$cycle,
  ipca = as.numeric(ipca)/100
) %>% 
  pivot_longer(c(hiato, ipca), names_to = "serie", values_to = "value")
data %>%
  ggplot(aes(month, value, color = serie)) +
  geom_line() +
  geom_hline(yintercept = 0)
