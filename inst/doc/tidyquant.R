## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      fig.width = 6)
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
aapl_financials <- tq_get("AAPL", get = "financials")
aapl_financials

## ------------------------------------------------------------------------
aapl_financials %>%
    filter(type == "IS") %>%
    select(annual) %>%
    unnest()

## ------------------------------------------------------------------------
aapl_financials %>%
    unnest(quarter) %>% 
    spread(key = date, value = value)

## ------------------------------------------------------------------------
aapl_key_ratios <- tq_get("AAPL", get = "key.ratios")
aapl_key_ratios

## ------------------------------------------------------------------------
aapl_key_ratios %>%
    filter(section == "Valuation Ratios") %>%
    unnest()

## ------------------------------------------------------------------------
aapl_key_ratios %>%
    filter(section == "Valuation Ratios") %>%
    unnest() %>%
    ggplot(aes(x = date, y = value)) + 
    geom_line(aes(col = forcats::fct_reorder2(category, date, value))) +
    labs(title = "10-Year Historical Valuation Ratios for AAPL", x = "", 
         y = "", col = "") 

## ------------------------------------------------------------------------
aapl_key_stats <- tq_get("AAPL", get = "key.stats")
aapl_key_stats

## ------------------------------------------------------------------------
c("AAPL", "FB", "GOOG") %>%
    tq_get(get = "key.stats") %>%
    select(symbol.x, Ask, Ask.Size, Bid, Bid.Size, Days.High, Days.Low)

## ---- eval = FALSE-------------------------------------------------------
#  collect_real_time_data <- function(x, interval_sec, n) {
#      data <- tibble()
#      while (n > 0) {
#          data <- bind_rows(data, tq_get(x, get = "key.stats"))
#          Sys.sleep(interval_sec)
#          n <- n - 1
#      }
#      return(data)
#  }
#  collect_real_time_data("AAPL", interval_sec = 3, n = 5) %>%
#      select(Ask, Ask.Size, Bid, Bid.Size, Open, Change)

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
    tq_transform(ohlc_fun = OHLCV, transform_fun = to.monthly)

## ------------------------------------------------------------------------
fb_prices %>%
    tq_mutate(ohlc_fun = Cl, mutate_fun = MACD, col_rename = c("MACD", "Signal"))

## ---- message=FALSE, warning=FALSE---------------------------------------
fb_prices %>%
    tq_mutate_xy(x = close, y = volume, mutate_fun = EVWMA, col_rename = "EVWMA")

## ---- message=FALSE, warning=FALSE---------------------------------------
wti_prices <- tq_get("DCOILWTICO", get = "economic.data") 
wti_prices %>%    
    tq_transform_xy(x = price, transform_fun = to.period,
                    period = "months", col_rename = "WTI Price")

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
aapl_growth <- aapl_prices %>%
    select(date, adjusted) %>%
    filter(date >= today() - years(1)) %>%
    mutate(baseline = first(adjusted),
           growth = adjusted - baseline,
           growth.pct = growth / baseline) %>%
    select(-(baseline:growth))
aapl_growth

## ------------------------------------------------------------------------
aapl_growth %>%
    ggplot(aes(x = date, y = growth.pct)) + 
    geom_line() +
    labs(title = "AAPL: Growth Over One Year", x = "", y = "Growth") +
    scale_y_continuous(labels = scales::percent)

## ------------------------------------------------------------------------
tq_transform_fun_options() %>% str()

## ------------------------------------------------------------------------
# Get zoo functions that work with tq_transform and tq_mutate
tq_transform_fun_options()$zoo

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
    tq_transform(ohlc_fun = Cl, transform_fun = apply.quarterly, FUN = max)

## ------------------------------------------------------------------------
AAPL %>%
    tq_transform(ohlc_fun = Ad, transform_fun = periodReturn, 
                 type = "log", period = "daily")

## ------------------------------------------------------------------------
AAPL %>%
    tq_mutate(Cl, MACD) %>%
    tq_mutate(HLC, BBands)

## ------------------------------------------------------------------------
AAPL %>%
    tq_mutate_xy(x = open, y = close, mutate_fun = Delt, k = 0:5) %>%
    select(-c(high, low, volume, adjusted))

## ------------------------------------------------------------------------
AAPL %>%
    tq_mutate(OHLC, OpCl) %>%
    select(-c(high, low, volume, adjusted))

## ------------------------------------------------------------------------
AAPL %>%
    tq_mutate(Ad, rollapply, width = 5, FUN = min, col_rename = "roll.min.5") %>%
    tq_mutate(Ad, rollapply, width = 10, FUN = min, col_rename = "roll.min.10") %>%
    tq_mutate(Ad, rollapply, width = 15, FUN = min, col_rename = "roll.min.15") %>%
    tq_mutate(Ad, rollapply, width = 5, FUN = max, col_rename = "roll.max.5") %>%
    tq_mutate(Ad, rollapply, width = 10, FUN = max, col_rename = "roll.max.10") %>%
    tq_mutate(Ad, rollapply, width = 15, FUN = max, col_rename = "roll.max.15")

## ------------------------------------------------------------------------
c("AAPL", "GOOG", "FB") %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")

## ------------------------------------------------------------------------
stock_list <- tibble(symbols = c("AAPL", "JPM", "CVX"),
                     industry = c("Technology", "Financial", "Energy"))
stock_list

## ------------------------------------------------------------------------
stock_list %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")

## ------------------------------------------------------------------------
tibble(symbol = c("AAPL", "GOOG", "AMZN", "FB", "AVGO", "SWKS","NVDA")) %>%
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")))

## ---- eval = F-----------------------------------------------------------
#  tq_get("SP500", get = "stock.index") %>%
#      tq_get(get = "stock.prices")

## ------------------------------------------------------------------------
c("AAPL", "GOOG", "FB") %>%
    tq_get(get = "stock.prices") %>%
    group_by(symbol.x) %>%
    tq_transform(Ad, transform_fun = periodReturn, period = "yearly")

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
    mutate(stock.prices = map(.x = symbol, ~ tq_get(x = .x, get = "stock.prices")))

## ------------------------------------------------------------------------
stock_list_with_one_bad_apple

## ------------------------------------------------------------------------
tryCatch({
    stock_list_with_one_bad_apple %>%
    mutate(annual.returns = map(.x = stock.prices, 
                                ~ tq_transform(x = .x,
                                               ohlc_fun = Ad, 
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
    mutate(stock.prices = map(.x = symbol, ~ tq_get(x = .x, get = "stock.prices")),
           class = map_chr(.x = stock.prices, ~ class(.x)[[1]])) %>%
    # Step 2: Filter out errors; errors have a class of "logical"
    filter(class != "logical") %>%
    select(-class) %>%
    # Step 3: Perform period returns
    mutate(annual.returns = map(.x = stock.prices, 
                                ~ tq_transform(data = .x,
                                               ohlc_fun = Ad, 
                                               transform_fun = periodReturn, 
                                               period = "yearly")
                                )
           )
stock_list_with_one_bad_apple

