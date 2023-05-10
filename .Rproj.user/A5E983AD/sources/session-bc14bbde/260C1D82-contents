library(readr)
library(tidyverse)
library(forecast)
library(mFilter)

df <- read_csv("IBC-Br dessazonalizado (2002 = 100).csv")
ipca <- read_csv("IPCA variacao.csv")

colnames(df) <- c("Data", "Value", "1")
df <- df %>%
  select(Data, Value) %>% 
  filter(!is.na(Value))

colnames(ipca) <- c("Data", "Value", "1")
ipca <- ipca %>% 
  select(Data, Value) %>% 
  filter(Data >= 2003.01 & Data <= 2023.02)

df <- ts(df$Value, start = c(2003, 1), frequency = 12)
ipca <- ts(ipca$Value, start = c(2003, 1), frequency = 12)



fit <- tslm(df ~ trend)
summary(fit)
acf(residuals(fit))
pacf(residuals(fit))
plot(residuals(fit))

data <- data.frame(
  month = as.numeric(time(df)),
  trend = fit$fitted.values,
  residual = fit$residuals,
  series = fit$data$df
)
ggplot(data, aes(x = month)) +
  geom_line(aes(y = residual)) +
  geom_line(aes(y = trend-70)) +
  geom_line(aes(y = series-70)) +
  scale_y_continuous(
    name = "Residuals",
    sec.axis = sec_axis(~.*2)
  )

df.hp <- hpfilter(df, freq = 14400)
# par(mfrow=c(2,1))
# ts.plot(df, col="blue")
# lines(df.hp$trend, col="red")
# ts.plot(df.hp$cycle, col="green4")

data <- data.frame(
  month = as.numeric(time(df)),
  series = df,
  trend = as.numeric(df.hp$trend),
  residual = df.hp$cycle
)
data <- data %>% 
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
