rm(list = ls())

library(ggplot2)
library(lubridate)
library(magrittr)
library(dplyr)
library(cowplot)

source("res/code/functions.r")

BASE_PATH <- './'
BASE_IMG_PATH <- paste0(BASE_PATH, 'res/img/')
BASE_DATA_PATH <- paste0(BASE_PATH, 'res/data/')

comparison.1 <- read.csv(paste0(BASE_DATA_PATH, 'comparison-1.csv'))
comparison.1$Month <- dmy(comparison.1$Month)

comparison.2 <- read.csv(paste0(BASE_DATA_PATH, 'comparison-2.csv'))
comparison.2$Month <- dmy(comparison.2$Month)

errors <- data.frame()
errors <- rbind(errors, list(name="{$\\text{SARIMA}(0, 1, 1)(0, 1, 1)_{12}$}",
                             error=PredictionError(comparison.1, 12)),
                stringsAsFactors = FALSE)
errors <- rbind(errors, list(name="{$\\text{SARIMA}(0, 1, 1)(0, 1, 1)_{12}(0, 0, 1)_{17}$}",
                             error=PredictionError(comparison.2, 12)),
                stringsAsFactors = FALSE)

errors$error <- round(errors$error, digits = 2)
write.csv(errors, paste0(BASE_DATA_PATH, 'predict-error.csv'), row.names = FALSE, quote = FALSE)

comparison.1.reduced <- comparison.1[(nrow(comparison.1) - 12 + 1):nrow(comparison.1), ]
comparison.2.reduced <- comparison.2[(nrow(comparison.2) - 12 + 1):nrow(comparison.2), ]

plot.1 <- comparison.1.reduced %>%
  mutate(L95 = L95 - Weight_loss,
         U95 = U95 - Weight_loss,
         FORECAST = FORECAST - Weight_loss,
         Weight_loss = 0) %>%
  {ggplot(., aes(y = FORECAST, ymin = L95, ymax = U95, x = Month)) +
    geom_line() +
    geom_point(aes(y = Weight_loss), size = 0.75, alpha = 0.5) +
    geom_ribbon(alpha = 0.2) +
    coord_flip()  +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(colour = "black", fill=NA)) +
    ggtitle('SARIMA(0, 1, 1)(0, 1, 1)12')}

plot.2 <- comparison.2.reduced %>%
  mutate(L95 = L95 - Weight_loss,
         U95 = U95 - Weight_loss,
         FORECAST = FORECAST - Weight_loss,
         Weight_loss = 0) %>%
  {ggplot(., aes(y = FORECAST, ymin = L95, ymax = U95, x = Month)) +
    geom_line() +
    geom_point(aes(y = Weight_loss), size = 0.75, alpha = 0.5) +
    geom_ribbon(alpha = 0.2) +
    coord_flip()  +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(colour = "black", fill=NA)) +
    ggtitle('SARIMA(0, 1, 1)(0, 1, 1)12(0, 0, 1)17')}

save_plot(paste0(BASE_IMG_PATH, 'comparison-ci-amplitude.png'), plot_grid(plot.1, plot.2),
           base_aspect_ratio = 1.75, base_height = 6)

t.test((comparison.1.reduced$U95 - comparison.1.reduced$L95) -
       (comparison.2.reduced$U95 - comparison.2.reduced$L95), mu = 0)
