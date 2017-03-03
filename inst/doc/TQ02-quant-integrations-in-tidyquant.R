## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      fig.width  = 6,
                      fig.height = 4.5)
# devtools::load_all() # Travis CI fails on load_all()

## ------------------------------------------------------------------------
# Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR 
library(tidyquant)

## ------------------------------------------------------------------------
tq_transmute_fun_options() %>% str()

## ------------------------------------------------------------------------
# Get zoo functions that work with tq_transmute and tq_mutate
tq_transmute_fun_options()$zoo

## ------------------------------------------------------------------------
# Get xts functions that work with tq_transmute and tq_mutate
tq_transmute_fun_options()$xts

## ------------------------------------------------------------------------
# Get quantmod functions that work with tq_transmute and tq_mutate
tq_transmute_fun_options()$quantmod

## ------------------------------------------------------------------------
# Get TTR functions that work with tq_transmute and tq_mutate
tq_transmute_fun_options()$TTR

## ------------------------------------------------------------------------
data(FANG)
FANG

## ------------------------------------------------------------------------
FANG_annual_returns <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(ohlc_fun   = Ad, 
                 mutate_fun = periodReturn, 
                 period     = "yearly", 
                 type       = "arithmetic")
FANG_annual_returns

## ---- fig.height = 4.5---------------------------------------------------
FANG_annual_returns %>%
    ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "FANG: Annual Returns",
         subtitle = "Get annual returns quickly with tq_transmute!",
         y = "Annual Returns", x = "") + 
    facet_wrap(~ symbol, ncol = 2) +
    theme_tq() + 
    scale_fill_tq()

## ------------------------------------------------------------------------
FANG_daily_log_returns <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(ohlc_fun   = Ad, 
                 mutate_fun = periodReturn, 
                 period     = "daily", 
                 type       = "log",
                 col_rename = "monthly.returns")

## ---- fig.height = 4.5---------------------------------------------------
FANG_daily_log_returns %>%
    ggplot(aes(x = monthly.returns, fill = symbol)) +
    geom_density(alpha = 0.5) +
    labs(title = "FANG: Charting the Daily Log Returns",
         x = "Monthly Returns", y = "Density") +
    theme_tq() +
    scale_fill_tq() + 
    facet_wrap(~ symbol, ncol = 2)

## ------------------------------------------------------------------------
FANG %>%
    group_by(symbol) %>%
    tq_transmute(ohlc_fun = OHLCV, mutate_fun = to.period, period = "months")

## ------------------------------------------------------------------------
FANG_daily <- FANG %>%
    group_by(symbol)

FANG_daily %>%
    ggplot(aes(x = date, y = adjusted, color = symbol)) +
    geom_line(size = 1) +
    labs(title = "Daily Stock Prices",
         x = "", y = "Adjusted Prices", color = "") +
    facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
    scale_y_continuous(labels = scales::dollar) +
    theme_tq() + 
    scale_color_tq()

## ------------------------------------------------------------------------
FANG_monthly <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(ohlc_fun = Ad, mutate_fun = to.period, period = "months")

FANG_monthly %>%
    ggplot(aes(x = date, y = adjusted, color = symbol)) +
    geom_line(size = 1) +
    labs(title = "Monthly Stock Prices",
         x = "", y = "Adjusted Prices", color = "") +
    facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
    scale_y_continuous(labels = scales::dollar) +
    theme_tq() + 
    scale_color_tq()

## ------------------------------------------------------------------------
# Asset Returns
FANG_returns_monthly <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(ohlc_fun   = Ad, 
                 mutate_fun = periodReturn,
                 period = "monthly")

# Baseline Returns
baseline_returns_monthly <- "XLK" %>%
    tq_get(get  = "stock.prices",
           from = "2013-01-01", 
           to   = "2016-12-31") %>%
    tq_transmute(ohlc_fun   = Ad, 
                 mutate_fun = periodReturn,
                 period = "monthly")

## ------------------------------------------------------------------------
returns_joined <- left_join(FANG_returns_monthly, 
                            baseline_returns_monthly,
                            by = "date")
returns_joined

## ------------------------------------------------------------------------
FANG_rolling_corr <- returns_joined %>%
    tq_transmute_xy(x = monthly.returns.x, 
                    y = monthly.returns.y,
                    mutate_fun = runCor,
                    n = 6,
                    col_rename = "rolling.corr.6")

## ------------------------------------------------------------------------
FANG_rolling_corr %>%
    ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    geom_line(size = 1) +
    labs(title = "FANG: Six Month Rolling Correlation to XLK",
         x = "", y = "Correlation", color = "") +
    facet_wrap(~ symbol, ncol = 2) +
    theme_tq() + 
    scale_color_tq()

## ------------------------------------------------------------------------
FANG_macd <- FANG %>%
    group_by(symbol) %>%
    tq_mutate(ohlc_fun   = Cl, 
              mutate_fun = MACD, 
              nFast      = 12, 
              nSlow      = 26, 
              nSig       = 9, 
              maType     = SMA) %>%
    mutate(diff = macd - signal) %>%
    select(-(open:volume))
FANG_macd

## ------------------------------------------------------------------------
FANG_macd %>%
    filter(date >= as_date("2016-10-01")) %>%
    ggplot(aes(x = date)) + 
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    geom_line(aes(y = macd, col = symbol)) +
    geom_line(aes(y = signal), color = "blue", linetype = 2) +
    geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) +
    facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
    labs(title = "FANG: Moving Average Convergence Divergence",
         y = "MACD", x = "", color = "") +
    theme_tq() +
    scale_color_tq()

## ------------------------------------------------------------------------
FANG_max_by_qtr <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(ohlc_fun   = Ad, 
                 mutate_fun = apply.quarterly, 
                 FUN        = max, 
                 col_rename = "max.close") %>%
    mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
    select(-date)
FANG_max_by_qtr

## ------------------------------------------------------------------------
FANG_min_by_qtr <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(ohlc_fun   = Ad, 
                 mutate_fun = apply.quarterly, 
                 FUN        = min, 
                 col_rename = "min.close") %>%
    mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
    select(-date)

FANG_by_qtr <- left_join(FANG_max_by_qtr, FANG_min_by_qtr,
                         by = c("symbol"   = "symbol",
                                "year.qtr" = "year.qtr"))
FANG_by_qtr

## ------------------------------------------------------------------------
FANG_by_qtr %>%
    ggplot(aes(x = year.qtr, color = symbol)) +
    geom_segment(aes(xend = year.qtr, y = min.close, yend = max.close),
                 size = 1) +
    geom_point(aes(y = max.close), size = 2) +
    geom_point(aes(y = min.close), size = 2) +
    facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
    labs(title = "FANG: Min/Max Price By Quarter",
         y = "Stock Price", color = "") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_blank())

