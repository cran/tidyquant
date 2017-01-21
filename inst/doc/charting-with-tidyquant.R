## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      fig.width = 6)
# devtools::load_all() # Travis CI fails on load_all()

## ------------------------------------------------------------------------
# Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR 
library(tidyquant)  

## ------------------------------------------------------------------------
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2015-09-01", to = "2017-01-01")
AMZN <- tq_get("AMZN", get = "stock.prices", from = "2000-01-01", to = "2017-01-01")
FANG <- c("FB", "AMZN", "NFLX", "GOOG") %>%
    tq_get(get = "stock.prices", from = "2015-09-01", to = "2017-01-01") 

## ------------------------------------------------------------------------
end <- as_date("2017-01-01")

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +
    labs(title = "AAPL Line Chart", y = "Closing Price", x = "")

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close)) +
    labs(title = "AAPL Bar Chart", y = "Closing Price", x = "")

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close)) +
    labs(title = "AAPL Bar Chart", 
         subtitle = "Zoomed in using coord_x_date",
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(6), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close),
                     color_up = "darkgreen", color_down = "darkred", size = 1) +
    labs(title = "AAPL Bar Chart", 
         subtitle = "Zoomed in, Experimenting with Formatting",
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(6), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    labs(title = "AAPL Candlestick Chart", y = "Closing Price", x = "")

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    labs(title = "AAPL Candlestick Chart", 
         subtitle = "Zoomed in using coord_x_date",
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(6), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),
                        color_up = "darkgreen", color_down = "darkred", 
                        fill_up  = "darkgreen", fill_down  = "darkred") +
    labs(title = "AAPL Candlestick Chart", 
         subtitle = "Zoomed in, Experimenting with Formatting",
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(6), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    geom_line(color = "pink", size = 1.5) +
    geom_point(color = "purple", size = 3) +
    labs(title = "AAPL Candlestick Chart", 
         subtitle = "Combining Chart Geoms",
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(6), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
FANG %>%
    ggplot(aes(x = date, y = close, group = symbol.x)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    labs(title = "FANG Candlestick Chart", 
         subtitle = "Experimenting with Mulitple Stocks",
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(6), end)) +
    facet_wrap(~ symbol.x, ncol = 2, scale = "free_y")

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(ma_fun = SMA, n = 50, linetype = 5) +
    geom_ma(ma_fun = SMA, n = 200, color = "red") + 
    labs(title = "AAPL Candlestick Chart", 
         subtitle = "50 and 200-Day SMA", 
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(24), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(ma_fun = EMA, n = 50, wilder = TRUE, linetype = 5) +
    geom_ma(ma_fun = EMA, n = 200, wilder = TRUE, color = "red") + 
    labs(title = "AAPL Bar Chart", 
         subtitle = "50 and 200-Day EMA", 
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(24), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
FANG %>%
    ggplot(aes(x = date, y = close, volume = volume, group = symbol.x)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(ma_fun = VWMA, n = 50, wilder = TRUE, linetype = 5) +
    geom_ma(ma_fun = VWMA, n = 200, wilder = TRUE, color = "red") + 
    labs(title = "FANG Bar Chart", 
         subtitle = "50 and 200-Day EMA, Experimenting with Multiple Stocks", 
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(24), end)) +
    facet_wrap(~ symbol.x, ncol = 2, scales = "free_y")

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close, open = open,
               high = high, low = low, close = close)) +
    geom_candlestick() +
    geom_bbands(ma_fun = SMA, sd = 2, n = 50) +
    labs(title = "AAPL Candlestick Chart", 
         subtitle = "BBands with SMA Applied", 
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(24), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
AAPL %>%
    ggplot(aes(x = date, y = close, open = open,
               high = high, low = low, close = close)) +
    geom_candlestick() +
    geom_bbands(ma_fun = SMA, sd = 2, n = 50, 
                   linetype = 1, alpha = 0.4, fill = "bisque", 
                   color_bands = "aquamarine4", color_ma = "steelblue4") +
    labs(title = "AAPL Candlestick Chart", 
         subtitle = "BBands with SMA Applied, Experimenting with Formatting", 
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(24), end),
                 ylim = c(100, 120))

## ------------------------------------------------------------------------
FANG %>%
    ggplot(aes(x = date, y = close, 
               open = open, high = high, low = low, close = close, 
               group = symbol.x)) +
    geom_barchart() +
    geom_bbands(ma_fun = SMA, sd = 2, n = 50, linetype = 5) +
    labs(title = "FANG Bar Chart", 
         subtitle = "BBands with SMA Applied, Experimenting with Multiple Stocks", 
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(24), end)) +
    facet_wrap(~ symbol.x, ncol = 2, scales = "free_y")

## ------------------------------------------------------------------------
AMZN %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() + 
    scale_y_continuous() +
    labs(title = "AMZN Line Chart", 
         subtitle = "Continuous Scale", 
         y = "Closing Price", x = "")

## ------------------------------------------------------------------------
AMZN %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() + 
    scale_y_log10() +
    labs(title = "AMZN Line Chart", 
         subtitle = "Log Scale", 
         y = "Closing Price", x = "")

## ------------------------------------------------------------------------
AMZN %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() + 
    scale_y_log10() +
    geom_smooth(method = "lm") +
    labs(title = "AMZN Line Chart", 
         subtitle = "Log Scale, Applying Linear Trendline", 
         y = "Adjusted Closing Price", x = "")

## ------------------------------------------------------------------------
AMZN %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() + 
    scale_y_log10() +
    geom_smooth(method = "loess") +
    labs(title = "AMZN Line Chart", 
         subtitle = "Log Scale, Applying Loess Trendline", 
         y = "Adjusted Closing Price", x = "")

## ------------------------------------------------------------------------
AMZN %>%
    ggplot(aes(x = date, y = volume)) +
    geom_bar(stat = "identity") + 
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = "AMZN Bar Chart", 
         subtitle = "Charting Daily Volume", 
         y = "Volume", x = "")  

## ------------------------------------------------------------------------
AMZN %>%
    ggplot(aes(x = date, y = volume, fill = volume)) +
    geom_bar(stat = "identity") + 
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = "AMZN Bar Chart", 
         subtitle = "Charting Daily Volume, Zooming In", 
         y = "Volume", x = "") + 
    scale_x_date(limits = c(end - weeks(24), end)) +
    scale_fill_gradient(low = "red", high = "darkblue") +
    theme(legend.position = "none")

