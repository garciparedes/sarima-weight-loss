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

PlotTimeSeries(weightloss, seasonality = 12, lags = 84) %>%
  { save_plot(paste0(BASE_IMG_PATH, 'weightloss.png'), .,
             base_aspect_ratio = 1, base_height = 12) }

values <- diff(weightloss$values, 1)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(weightloss$values, 12)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(weightloss$values, 12, 2)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-12-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(diff(weightloss$values, 1), 12)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(diff(weightloss$values, 1), 12, 2)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
  { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-12-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

values <- diff(diff(weightloss$values, 1, 2), 12)
df <- data.frame(index = 1:length(values), values=values)
PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'weightloss-diff-1-1-12.png'), .,
            base_aspect_ratio = 1, base_height = 12) }
