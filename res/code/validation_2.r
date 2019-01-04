rm(list = ls())

library(ggplot2)
library(magrittr)
library(cowplot)
library(nortest)

source("res/code/functions.r")
source("res/code/plotting.r")

BASE_PATH <- './'
BASE_IMG_PATH <- paste0(BASE_PATH, 'res/img/')
BASE_DATA_PATH <- paste0(BASE_PATH, 'res/data/')

validation.2 <- read.csv(paste0(BASE_DATA_PATH, 'validation-2.csv'))

values <- validation.2$RESIDUAL[!is.na(validation.2$RESIDUAL)]
df <- data.frame(index = 1:length(values), values=values)

PlotTimeSeries(df, seasonality = 12, lags = 84) %>%
 { save_plot(paste0(BASE_IMG_PATH, 'validation-2-residuals.png'), .,
            base_aspect_ratio = 1, base_height = 12) }

Box.test(values, lag = 1, type="Ljung-Box")

Box.test(values, lag = 12, type="Ljung-Box")

lillie.test(values)

sf.test(values)

{ggplot(df, aes(sample = values)) +
  geom_qq() +
  geom_qq_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA)) +
  ggtitle('QQ Plot')} %>%
   { save_plot(paste0(BASE_IMG_PATH, 'validation-2-normality.png'), .,
              base_aspect_ratio = 1, base_height = 6) }
