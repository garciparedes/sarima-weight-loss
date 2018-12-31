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

Correlogram <- function(x, n = length(x) - 1) {
    result <- acf(x, lag.max=n, plot=FALSE)$acf[0:n]
    data.frame(lag = 1:length(result), values = result)
}

PartialCorrelogram <- function(x, n = length(x) - 1) {
    result <- pacf(x, lag.max=n, plot=FALSE)$acf
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
        geom_hline(yintercept = 0, color = "gray") +
        geom_line() +
        theme_bw() +
        # scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.border = element_rect(colour = "black", fill=NA)) +
        ggtitle('Serie')

    p.b <- ggplot(RangeMean(df$values, seasonality)) +
        aes(x = mean, y = range) +
        geom_point() +
        xlab("Media") +
        ylab("Rango") +
        expand_limits(y=0) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.border = element_rect(colour = "black", fill=NA)) +
        ggtitle('Rango-Media')

    p.c <- ggplot(Correlogram(df$values, lags)) +
        aes(x = lag, y = values) +
        xlab("Retardo") +
        ylab("Correlación") +
        geom_bar(stat="identity") +
        geom_hline(yintercept = 2/sqrt(nrow(df)), color = "red") +
        geom_hline(yintercept = -2/sqrt(nrow(df)), color = "red") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.border = element_rect(colour = "black", fill=NA)) +
        ggtitle('Correlograma')

    p.partial.correlogram <- ggplot(PartialCorrelogram(df$values, lags)) +
        aes(x = lag, y = values) +
        xlab("Retardo") +
        ylab("Correlación") +
        geom_bar(stat="identity") +
        geom_hline(yintercept = 2/sqrt(nrow(df)), color = "red") +
        geom_hline(yintercept = -2/sqrt(nrow(df)), color = "red") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.border = element_rect(colour = "black", fill=NA)) +
        ggtitle('Correlograma Parcial')

    p.d <- ggplot(Periodogram(df$values)) +
        aes(x = freq, y = spec) +
        xlab("Frecuencia") +
        ylab("Valor") +
        geom_line() +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.border = element_rect(colour = "black", fill=NA)) +
        ggtitle('Periodograma')

    for (a in armonics) {
        p.d <- p.d + geom_vline(xintercept = a, color = "red", alpha = 0.4)
    }
    plot_grid(p.a, plot_grid(p.c, p.partial.correlogram, p.d, p.b, ncol = 2),
              ncol = 1, rel_heights = c(1, 2))
}


BASE_PATH <- './'
BASE_IMG_PATH <- paste0(BASE_PATH, 'res/img/')
BASE_DATA_PATH <- paste0(BASE_PATH, 'res/data/')

weightloss <- read.csv(paste0(BASE_DATA_PATH, 'weight-loss.csv'))
weightloss$Month <- ymd(weightloss$Month, truncated = 2)
colnames(weightloss) <- c("index", "values")

variances <- data.frame()
variances <- rbind(variances, list(name="Base", variance=var(weightloss$values)), stringsAsFactors = FALSE)
PlotTimeSeries(weightloss, seasonality = 12, lags = 84) %>%
  { save_plot(paste0(BASE_IMG_PATH, 'weightloss.png'), .,
             base_aspect_ratio = 1, base_height = 12) }


values <- diff(weightloss$values, 1, 1)
variances <- rbind(variances, list(name="diff-1", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1.png'), .,
            base_aspect_ratio = 1, base_height = 12) }


values <- diff(weightloss$values, 12, 1)
variances <- rbind(variances, list(name="diff-12", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }


values <- diff(weightloss$values, 12, 2)
variances <- rbind(variances, list(name="diff-12-12", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-12-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(diff(weightloss$values, 1, 1), 12, 1)
variances <- rbind(variances, list(name="diff-1-12", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }


values <- diff(diff(weightloss$values, 1, 1), 12, 2)
variances <- rbind(variances, list(name="diff-1-12-12", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
  { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-12-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(diff(weightloss$values, 1, 2), 12, 1)
variances <- rbind(variances, list(name="diff-1-1-12", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-1-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }


values <- diff(diff(weightloss$values, 1, 2), 12, 2)
variances <- rbind(variances, list(name="diff-1-1-12-12", variance=var(values)), stringsAsFactors = FALSE)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-1-12-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

variances$variance <- round(variances$variance, digits = 2)
write.csv(variances, paste0(BASE_DATA_PATH, 'variance-differences.csv'), row.names = FALSE, quote = FALSE)
