source("res/code/functions.r")

library(ggplot2)


PlotTimeSeries <- function(df, seasonality, armonics = c(), lags = MAX_LAG){
    p.a <- ggplot(df) +
        aes(x = index, y = values) +
        xlab("Fecha") +
        ylab("Valor") +
        geom_hline(yintercept = 0, color = "gray") +
        geom_line() +
        theme_bw() +
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
