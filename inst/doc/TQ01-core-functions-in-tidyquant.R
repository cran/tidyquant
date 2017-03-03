## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      fig.width = 6)
# devtools::load_all() # Travis CI fails on load_all()

## ------------------------------------------------------------------------
# Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR 
library(tidyquant)

## ------------------------------------------------------------------------
tq_index_options()

## ------------------------------------------------------------------------
tq_index("SP500")

## ------------------------------------------------------------------------
tq_exchange("NASDAQ")

## ------------------------------------------------------------------------
tq_get_options()

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
    geom_line(aes(col = forcats::fct_reorder2(category, date, value)),
              size = 1) +
    labs(title = "10-Year Historical Valuation Ratios for AAPL", x = "", 
         y = "", col = "") +
    theme_tq() +
    scale_color_tq()

## ------------------------------------------------------------------------
aapl_key_stats <- tq_get("AAPL", get = "key.stats")
aapl_key_stats %>%
    colnames() %>%
    cat() # Print in condensed format

## ------------------------------------------------------------------------
c("AAPL", "FB", "GOOG") %>%
    tq_get(get = "key.stats") %>%
    select(symbol, Ask, Ask.Size, Bid, Bid.Size, Change, Days.High, Days.Low)

## ---- eval = FALSE-------------------------------------------------------
#  # Not evaluated; When run during active trading, will return real-time values
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
data(FANG)
FANG

## ------------------------------------------------------------------------
FANG %>%
    group_by(symbol) %>%
    tq_transmute(ohlc_fun = Ad, mutate_fun = to.monthly)

## ------------------------------------------------------------------------
FANG %>%
    group_by(symbol) %>%
    tq_mutate(ohlc_fun = Cl, mutate_fun = MACD, col_rename = c("MACD", "Signal"))

## ---- message=FALSE, warning=FALSE---------------------------------------
FANG %>%
    group_by(symbol) %>%
    tq_mutate_xy(x = close, y = volume, mutate_fun = EVWMA, col_rename = "EVWMA")

## ---- message=FALSE, warning=FALSE---------------------------------------
wti_prices <- tq_get("DCOILWTICO", get = "economic.data") 
wti_prices %>%    
    tq_transmute_xy(x = price, mutate_fun = to.period,
                    period = "months", col_rename = "WTI Price")

## ------------------------------------------------------------------------
AAPL_xts <- quantmod::getSymbols("AAPL", auto.assign = FALSE) 
AAPL_xts %>% head()

## ------------------------------------------------------------------------
AAPL_tbl <- as_tibble(AAPL_xts, preserve_row_names = TRUE)
AAPL_tbl

## ------------------------------------------------------------------------
AAPL_tbl <- AAPL_tbl %>%
    mutate(row.names = lubridate::ymd(row.names))
AAPL_tbl

## ------------------------------------------------------------------------
AAPL_xts_2 <- AAPL_tbl %>%
    as_xts(date_col = row.names)
AAPL_xts_2 %>% head()

