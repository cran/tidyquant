## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      fig.width = 6)
# devtools::load_all() # Travis CI fails on load_all()

## ------------------------------------------------------------------------
# Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR 
library(tidyquant)  

## ------------------------------------------------------------------------
c("AAPL", "GOOG", "FB") %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")

## ------------------------------------------------------------------------
stock_list <- tibble(stocks = c("AAPL", "JPM", "CVX"),
                     industry = c("Technology", "Financial", "Energy"))
stock_list

## ------------------------------------------------------------------------
stock_list %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")

## ------------------------------------------------------------------------
tq_index("DOWJONES")

## ------------------------------------------------------------------------
tq_exchange("NYSE")

## ------------------------------------------------------------------------
tq_index("DOWJONES") %>%
    slice(1:3) %>%
    tq_get(get = "stock.prices")

## ------------------------------------------------------------------------
tibble(symbol = c("AAPL", "GOOG", "AMZN", "FB")) %>%
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")))

## ------------------------------------------------------------------------
c("AAPL", "GOOG") %>%
    tq_get(get = c("stock.prices", "financials"))

## ------------------------------------------------------------------------
c("AAPL", "GOOG", "FB") %>%
    tq_get(get = "stock.prices", from = "2012-01-01", to = "2017-01-01") %>%
    group_by(symbol) %>%
    tq_transform(Ad, transform_fun = periodReturn, period = "yearly", 
                 col_rename = "yearly.returns") %>%
    ggplot(aes(x = year(date), y = yearly.returns, fill = symbol)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(2008, 2017, by = 1)) +
    labs(title = "AAPL, GOOG, FB: Annual Returns", 
         subtitle = "Transforming using quantmod functions is easy!", 
         x = "") +
    theme(legend.position = "bottom")

## ------------------------------------------------------------------------
my_stock_analysis_fun <- function(stock.symbol) {
    period.returns <- stock.symbol %>%
        tq_get(get = "stock.prices") %>%
        tq_transform(ohlc_fun = Ad, transform_fun = periodReturn, 
                     type = "log", period = "monthly")
    mean(period.returns$monthly.returns)
}

## ------------------------------------------------------------------------
my_stock_analysis_fun("AAPL")

## ------------------------------------------------------------------------
set.seed(100)
stocks <- tq_index("SP500") %>%
    sample_n(10)
stocks

## ------------------------------------------------------------------------
stocks <- stocks %>%
    mutate(mmlr = map_dbl(symbol, my_stock_analysis_fun)) %>%
    arrange(desc(mmlr))
stocks

## ------------------------------------------------------------------------
tq_get("XYZ", "stock.prices")

## ---- warning = TRUE-----------------------------------------------------
c("AAPL", "GOOG", "BAD APPLE") %>%
    tq_get(get = "stock.prices", complete_cases = TRUE)

## ---- warning = TRUE-----------------------------------------------------
c("AAPL", "GOOG", "BAD APPLE") %>%
    tq_get(get = "stock.prices", complete_cases = FALSE)

