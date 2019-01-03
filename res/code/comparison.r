rm(list = ls())

source("res/code/functions.r")

BASE_PATH <- './'
BASE_DATA_PATH <- paste0(BASE_PATH, 'res/data/')

comparison.1 <- read.csv(paste0(BASE_DATA_PATH, 'comparison-1.csv'))
comparison.2 <- read.csv(paste0(BASE_DATA_PATH, 'comparison-2.csv'))

errors <- data.frame()
errors <- rbind(errors, list(name="{$\\text{SARIMA}(0, 1, 1)(0, 1, 1)_{12}$}",
                             error=PredictionError(comparison.1, 12)),
                stringsAsFactors = FALSE)
errors <- rbind(errors, list(name="{$\\text{SARIMA}(0, 1, 1)(0, 1, 1)_{12}(0, 0, 1)_{17}$}",
                             error=PredictionError(comparison.2, 12)),
                stringsAsFactors = FALSE)

errors$error <- round(errors$error, digits = 2)
write.csv(errors, paste0(BASE_DATA_PATH, 'predict-error.csv'), row.names = FALSE, quote = FALSE)
