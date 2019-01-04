rm(list = ls())

library(ggplot2)
library(lubridate)
library(magrittr)
library(cowplot)

BASE_PATH <- './'
BASE_IMG_PATH <- paste0(BASE_PATH, 'res/img/')
BASE_DATA_PATH <- paste0(BASE_PATH, 'res/data/')

predictions <- read.csv(paste0(BASE_DATA_PATH, 'predict.csv'))
predictions$Month <- dmy(predictions$Month)

head(predictions)
predictions[1:180, 'L95'] <- NA
predictions[1:180, 'U95'] <- NA
{ggplot(predictions, aes(y = FORECAST, ymin = L95, ymax = U95, x = Month)) +
  geom_line() +
  geom_point(aes(y = Weight_loss), size = 0.75, alpha = 0.5) +
  geom_ribbon(alpha = 0.2, colour = NA, fill = "Red") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA)) +
  ggtitle('Ajuste')} %>%
   { save_plot(paste0(BASE_IMG_PATH, 'predict-complete.png'), .,
              base_aspect_ratio = 1.5, base_height = 6) }

{ggplot(predictions[156:nrow(predictions), ], aes(y = FORECAST, ymin = L95, ymax = U95, x = Month)) +
  geom_line() +
  geom_point(aes(y = Weight_loss), size = 0.75, alpha = 0.5) +
  geom_ribbon(alpha = 0.2, colour = NA, fill = "Red") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA)) +
  ggtitle('Predicción')} %>%
   { save_plot(paste0(BASE_IMG_PATH, 'predict-ending.png'), .,
              base_aspect_ratio = 1.5, base_height = 6) }
