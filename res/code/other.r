rm(list = ls())

library(magrittr)
library(dplyr)
library(latex2exp)
require(reshape2)
library(forecast)
library(cowplot)
library(lubridate)

source("res/code/functions.r")
source("res/code/plotting.r")

BASE_PATH <- './'
BASE_IMG_PATH <- paste0(BASE_PATH, 'res/img/')
BASE_DATA_PATH <- paste0(BASE_PATH, 'res/data/')

weightloss <- read.csv(paste0(BASE_DATA_PATH, 'weight-loss.csv'))
weightloss$Month <- ymd(weightloss$Month, truncated = 2)
colnames(weightloss) <- c("index", "values")

variances <- data.frame()
variances <- rbind(variances, list(name="$X_{t}$", variance=var(weightloss$values)), stringsAsFactors = FALSE)
PlotTimeSeries(weightloss, seasonality = 12, lags = 84) %>%
  { save_plot(paste0(BASE_IMG_PATH, 'weightloss.png'), .,
             base_aspect_ratio = 1, base_height = 12) }

values <- diff(weightloss$values, 1)
variances <- rbind(variances, list(name="$\\nabla X_{t}$", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(weightloss$values, 12)
variances <- rbind(variances, list(name="$\\nabla_{12} X_{t}$", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(weightloss$values, 12, 2)
variances <- rbind(variances, list(name="$\\nabla_{12}^{2} X_{t}$", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-12-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(diff(weightloss$values, 1), 12)
variances <- rbind(variances, list(name="$\\nabla \\nabla_{12} X_{t}$", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(diff(weightloss$values, 1), 12, 2)
variances <- rbind(variances, list(name="$\\nabla \\nabla_{12}^{2} X_{t}$", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
  { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-12-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(diff(weightloss$values, 1, 2), 12)
variances <- rbind(variances, list(name="$\\nabla^{2} \\nabla_{12} X_{t}$", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-1-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

variances$variance <- round(variances$variance, digits = 2)
write.csv(variances, paste0(BASE_DATA_PATH, 'variance-differences.csv'), row.names = FALSE, quote = FALSE)


(m1 <- arima(weightloss$values, c(0, 1, 0), list(order = c(0, 1, 1), period = 12), method = "ML"))
(m2 <- arima(weightloss$values, c(0, 1, 1), list(order = c(0, 1, 0), period = 12), method = "ML"))
(m3 <- arima(weightloss$values, c(0, 1, 1), list(order = c(0, 1, 1), period = 12), method = "ML", include.mean=FALSE))




coeftest(m2)



Box.test(weightloss$residuals, type="Ljung-Box")


weightloss$fitted <- fitted(m3)
weightloss$residuals <- residuals(m3)

ggplot(weightloss) +
    aes(x = index) +
    xlab("Fecha") +
    ylab("Valor") +
    geom_hline(yintercept = 0, color = "gray") +
    geom_point(aes(y = values)) +
    geom_line(aes(y = fitted), col = "red") +
    theme_bw()


library(nortest)
lillie.test(weightloss$residuals)


plot(weightloss$residuals, type = "p", pch = 20)
PlotTimeSeries(data.frame(index = weightloss$index, values = weightloss$residuals), seasonality = 12, lags = 84)


coeftest(m3, df = length(weightloss$values) - 12)


qqnorm(weightloss$residuals)
qqline(weightloss$residuals)

auto.arima(ts(weightloss$values, start = 2004, frequency = 12))









#
