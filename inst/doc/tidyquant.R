## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE)
# devtools::load_all() # Travis CI fails on load_all()

## ------------------------------------------------------------------------
# Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR 
library(tidyquant)  

## ------------------------------------------------------------------------
tq_get_options()

## ------------------------------------------------------------------------
tq_get_stock_index_options()

## ------------------------------------------------------------------------
tq_get("sp500", get = "stock.index")

## ------------------------------------------------------------------------
aapl_prices  <- tq_get("AAPL", get = "stock.prices", from = " 1990-01-01")
aapl_prices 

## ------------------------------------------------------------------------
aapl_divs <- tq_get("AAPL", get = "dividends", from = "1990-01-01")
aapl_divs

## ------------------------------------------------------------------------
aapl_splits <- tq_get("AAPL", get = "splits", from = "1990-01-01")
aapl_splits

## ------------------------------------------------------------------------
fb_financials <- tq_get("FB", get = "financials")
fb_financials

## ------------------------------------------------------------------------
fb_financials %>%
    filter(type == "IS") %>%
    select(annual) %>%
    unnest()

## ------------------------------------------------------------------------
fb_financials %>%
    unnest(quarter) %>% 
    spread(key = date, value = value)

## ------------------------------------------------------------------------
wti_price_usd <- tq_get("DCOILWTICO", get = "economic.data")
wti_price_usd 

## ------------------------------------------------------------------------
eur_usd <- tq_get("EUR/USD", get = "exchange.rates", from = "2000-01-01")
eur_usd 

## ------------------------------------------------------------------------
plat_price_eur <- tq_get("plat", get = "metal.prices", 
                         from = "2000-01-01", base.currency = "EUR")
plat_price_eur 

## ------------------------------------------------------------------------
fb_prices <- tq_get("FB") 
fb_prices %>%
    tq_transform(x_fun = OHLCV, transform_fun = to.monthly)

## ------------------------------------------------------------------------
fb_prices %>%
    tq_mutate(x_fun = Cl, mutate_fun = MACD)

## ---- message=FALSE, warning=FALSE---------------------------------------
fb_prices %>%
    tq_mutate_xy(.x = close, .y = volume, mutate_fun = EVWMA)

## ---- message=FALSE, warning=FALSE---------------------------------------
wti_prices <- tq_get("DCOILWTICO", get = "economic.data") 
wti_prices %>%    
    tq_transform_xy(.x = price, transform_fun = to.period,
                    period = "months")

## ------------------------------------------------------------------------
# Create xts object from a matrix
vals = matrix(c(500, 504, 503))
date = c("2016-01-01", "2016-01-02", "2016-01-03") 
rownames(vals) <- date
time_series_xts <- as_xts(vals)
time_series_xts

## ------------------------------------------------------------------------
time_series_tbl <- as_tibble(time_series_xts, preserve_row_names = TRUE)
time_series_tbl

## ------------------------------------------------------------------------
time_series_tbl <- time_series_tbl %>%
    mutate(row.names = lubridate::ymd(row.names))
time_series_tbl

## ------------------------------------------------------------------------
time_series_xts <- time_series_tbl %>%
    as_xts(date_col = row.names)
time_series_xts

## ------------------------------------------------------------------------
aapl_prices %>%
    select(date, adjusted) %>%
    filter(date >= today() - years(1))

## ------------------------------------------------------------------------
aapl_prices %>%
    select(date, adjusted) %>%
    filter(date >= today() - years(1)) %>%
    mutate(baseline = first(adjusted))

## ------------------------------------------------------------------------
aapl_prices %>%
    select(date, adjusted) %>%
    filter(date >= today() - years(1)) %>%
    mutate(baseline = first(adjusted),
           growth = adjusted - baseline,
           growth_pct = growth / baseline * 100) %>%
    select(-(baseline:growth))

## ------------------------------------------------------------------------
tq_transform_fun_options() %>% str()

## ------------------------------------------------------------------------
# Get xts functions that work with tq_transform and tq_mutate
tq_transform_fun_options()$xts

## ------------------------------------------------------------------------
# Get quantmod functions that work with tq_transform and tq_mutate
tq_transform_fun_options()$quantmod

## ------------------------------------------------------------------------
# Get TTR functions that work with tq_transform and tq_mutate
tq_transform_fun_options()$TTR

## ------------------------------------------------------------------------
AAPL <- tq_get("AAPL")

## ------------------------------------------------------------------------
AAPL %>%
    tq_transform(x_fun = Cl, transform_fun = apply.quarterly, FUN = max)

## ------------------------------------------------------------------------
AAPL %>%
    tq_transform(x_fun = Ad, transform_fun = periodReturn, 
                 type = "log", period = "daily")

## ------------------------------------------------------------------------
AAPL %>%
    tq_mutate(Cl, MACD) %>%
    tq_mutate(HLC, BBands)

## ------------------------------------------------------------------------
AAPL %>%
    tq_mutate_xy(.x = open, .y = close, mutate_fun = Delt, k = 0:5) %>%
    select(-c(high, low, volume, adjusted))

## ------------------------------------------------------------------------
AAPL %>%
    tq_mutate(OHLC, OpCl) %>%
    select(-c(high, low, volume, adjusted))

## ------------------------------------------------------------------------
my_stock_analysis_fun <- function(stock.symbol) {
    period.returns <- stock.symbol %>%
        tq_get(get = "stock.prices") %>%
        tq_transform(x_fun = Ad, transform_fun = periodReturn, 
                     type = "log", period = "monthly")
    mean(period.returns$monthly.returns)
}

## ------------------------------------------------------------------------
my_stock_analysis_fun("AAPL")

## ------------------------------------------------------------------------
set.seed(100)
stocks <- tq_get("SP500", get = "stock.index") %>%
    sample_n(10)
stocks

## ------------------------------------------------------------------------
stocks <- stocks %>%
    mutate(mmlr = map_dbl(symbol, my_stock_analysis_fun)) %>%
    arrange(desc(mmlr))
stocks

## ---- warning = TRUE-----------------------------------------------------
stock_list_with_one_bad_apple <- tibble( 
    symbol = c("AAPL", "GOOG", "AMZN", "FB", "BAD APPLE",
               "AVGO", "SWKS","NVDA", "V", "MA")
)
stock_list_with_one_bad_apple <- stock_list_with_one_bad_apple %>%
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")))

## ------------------------------------------------------------------------
stock_list_with_one_bad_apple

## ------------------------------------------------------------------------
tryCatch({
    stock_list_with_one_bad_apple %>%
    mutate(annual.returns = map(.x = stock.prices, 
                                ~ tq_transform(.x,
                                               x_fun = Ad, 
                                               transform_fun = periodReturn, 
                                               period = "yearly")
                                )
           )
}, error = function(e) {
    print(e)
})



## ------------------------------------------------------------------------
stock_list_with_one_bad_apple <- tibble( 
    symbol = c("AAPL", "GOOG", "AMZN", "FB", "BAD APPLE",
               "AVGO", "SWKS","NVDA", "V", "MA")
    ) %>%
    # Step 1: Get stock prices
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")),
           class = map_chr(stock.prices, ~ class(.x)[[1]])) %>%
    # Step 2: Filter out errors; errors have a class of "logical"
    filter(class != "logical") %>%
    select(-class) %>%
    # Step 3: Perform period returns
    mutate(annual.returns = map(.x = stock.prices, 
                                ~ tq_transform(.x,
                                               x_fun = Ad, 
                                               transform_fun = periodReturn, 
                                               period = "yearly")
                                )
           )
stock_list_with_one_bad_apple

## ---- warning = TRUE, message = TRUE-------------------------------------
tq_get("SP500", get = "stock.index", use_fallback = TRUE)

