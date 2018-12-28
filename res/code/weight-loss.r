## Author: Sergio García Prado
## Title: Time Series - Weight Loss - EDA

rm(list = ls())

library(magrittr)
library(dplyr)
library(ggplot2)
library(latex2exp)
require(reshape2)
library(forecast)
library(cowplot)
library(lubridate)

RangeMean <- function(x, seasonality) {
    n <- length(x)
    seq(1, n, by=seasonality) %>%
    sapply(function(i){
        a <- x[i:(i + seasonality - 1)]
        c(mean=mean(a, na.rm=TRUE), range=diff(range(a, na.rm = TRUE)))
    }) %>%
    t() %>%
    as.data.frame()
}

Correlogram <- function(x, n = length(x) - 1, with.first = FALSE) {
    result <- acf(x, lag.max=n, plot=FALSE)$acf[1:n + !with.first]
    data.frame(lag = 1:length(result), values = result)
}

Periodogram <- function(x) {
    result <- TSA::periodogram(x, plot=FALSE)
    data.frame(freq = c(0, result$freq), spec = c(0, result$spec))
}

PlotTimeSeries <- function(df, seasonality, armonics = c(), lags = MAX_LAG){
    p.a <- ggplot(df) +
        aes(x = index, y = values) +
        xlab("Fecha") +
        ylab("Valor") +
        geom_line()
    p.b <- ggplot(RangeMean(df$values, seasonality)) +
        aes(x = mean, y = range) +
        geom_point() +
        xlab("Media") +
        ylab("Rango") +
        expand_limits(y=0)
    p.c <- ggplot(Correlogram(df$values, lags)) +
        aes(x = lag, y = values) +
        xlab("Retardo") +
        ylab("Correlación") +
        geom_bar(stat="identity") +
        geom_hline(yintercept = 2/sqrt(nrow(df)), color = "red") +
        geom_hline(yintercept = -2/sqrt(nrow(df)), color = "red")
    p.d <- ggplot(Periodogram(df$values)) +
        aes(x = freq, y = spec) +
        xlab("Frecuencia") +
        ylab("Valor") +
        geom_line()

    for (a in armonics) {
        p.d <- p.d + geom_vline(xintercept = a, color = "red", alpha = 0.4)
    }
    plot_grid(p.a, p.b, p.c, p.d)
}

BASE_PATH <- './'
BASE_IMG_PATH <- paste0(BASE_PATH, 'res/img/')
BASE_DATA_PATH <- paste0(BASE_PATH, 'res/data/')

weightloss <- read.csv(paste0(BASE_DATA_PATH, 'weight-loss.csv'))
weightloss$Month <- ymd(weightloss$Month, truncated = 2)
colnames(weightloss) <- c("index", "values")

PlotTimeSeries(weightloss, seasonality = 12, lags = 84) %>%
  { save_plot(paste0(BASE_IMG_PATH, 'weightloss.png'), .,
             base_aspect_ratio = 1.75, base_height = 8) }

weightloss.diff <- data.frame(index = 1:(nrow(weightloss) - 1), values=diff(weightloss$values, 1))
PlotTimeSeries(weightloss.diff, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff.png'), .,
           base_aspect_ratio = 1.75, base_height = 8) }
