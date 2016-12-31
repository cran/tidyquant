
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyquant
=========

[![Travis-CI Build Status](https://travis-ci.org/mdancho84/tidyquant.svg?branch=master)](https://travis-ci.org/mdancho84/tidyquant) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidyquant)](https://cran.r-project.org/package=tidyquant)

`tidyquant` integrates the best quantitative resources for collecting and analyzing quantitative data, `xts`, `quantmod` and `TTR`, with the tidy data infrastructure of the `tidyverse` allowing for seamless interaction between each and working within the `tidyverse`.

Benefits
--------

**The `tidyquant` philosophy:**

-   **A few core functions with a lot of power, that**
-   **leverage the quantitative analysis power of `xts`, `quantmod` and `TTR`, and are**
-   **designed to be used and scaled with the `tidyverse`.**

Installation
------------

To install from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("mdancho84/tidyquant")
```

Examples
--------

``` r
# Loads tidyquant, tidyverse, lubridate, quantmod, TTR, and xts
library(tidyquant) 
```

### Getting Data in Tibble Format:

`tq_get()` is the one-stop shop for retrieving data. The full list of get options are:

``` r
tq_get_options()
#> [1] "stock.prices"   "stock.index"    "dividends"      "splits"        
#> [5] "financials"     "economic.data"  "exchange.rates" "metal.prices"
```

Set `get = "stock.prices"` to get stock prices. Notice the output is *always* a `tibble`.

``` r
aapl_prices <- tq_get("AAPL", get = "stock.prices")
aapl_prices
#> # A tibble: 2,769 × 7
#>          date  open  high   low close    volume  adjusted
#>        <date> <dbl> <dbl> <dbl> <dbl>     <dbl>     <dbl>
#> 1  2006-01-03 72.38 74.75 72.25 74.75 201808600  9.726565
#> 2  2006-01-04 75.13 75.98 74.50 74.97 154900900  9.755191
#> 3  2006-01-05 74.83 74.90 73.75 74.38 112355600  9.678420
#> 4  2006-01-06 75.25 76.70 74.55 76.30 176114400  9.928252
#> 5  2006-01-09 76.73 77.20 75.74 76.05 168760200  9.895722
#> 6  2006-01-10 76.25 81.89 75.83 80.86 569967300 10.521606
#> 7  2006-01-11 83.84 84.80 82.59 83.90 373448600 10.917174
#> 8  2006-01-12 84.97 86.40 83.62 84.29 320202400 10.967921
#> 9  2006-01-13 84.99 86.01 84.60 85.59 194076400 11.137079
#> 10 2006-01-17 85.70 86.38 83.87 84.71 208905900 11.022573
#> # ... with 2,759 more rows
```

Set `get = "financials"` to get financial statements. The statements are returned as nested tibbles, that can be unnested and analyzed together.

``` r
tq_get("AAPL", get = "financials")
#> # A tibble: 3 × 3
#>    type             annual            quarter
#> * <chr>             <list>             <list>
#> 1    BS <tibble [168 × 4]> <tibble [210 × 4]>
#> 2    CF  <tibble [76 × 4]>  <tibble [76 × 4]>
#> 3    IS <tibble [196 × 4]> <tibble [245 × 4]>
```

There are many other get options including stock indexes, dividends, splits, economic data from the FRED, and exchange rates and metals from Oanda.

### Working in the tidyverse

You probably already know and love `tidyverse` packages like `dplyr`, `tidyr`, `purrr`, `readr`, and `tibble` along with `lubridate` for working with date and datetime. `tidyquant` works solely in tibbles, so all of the `tidyverse` functionality is intact.

A simple example inspired by [Kan Nishida's blog](https://blog.exploratory.io/introducing-time-series-analysis-with-dplyr-60683587cf8a#.w6pvyi3d2) shows the `dplyr` and `lubridate` capability: Say we want the growth in the stock over the past year. We can do this with `dplyr` operations.

Getting the last year is simple with `dplyr` and `lubridate`. We first `select` the date and adjusted price (adjusted for stock splits). We then `filter` using `lubridate` date functions. We can also get a baseline price using the `first` function. Growth and growth percent versus baseline columns can be added now. We tack on a final select statement to remove unnecessary columns. The final workflow looks like this:

``` r
aapl_prices %>%
    select(date, adjusted) %>%
    filter(date >= today() - years(1)) %>%
    mutate(baseline = first(adjusted),
           growth = adjusted - baseline,
           growth_pct = growth / baseline * 100) %>%
    select(-(baseline:growth))
#> # A tibble: 253 × 3
#>          date  adjusted growth_pct
#>        <date>     <dbl>      <dbl>
#> 1  2015-12-31 102.96903  0.0000000
#> 2  2016-01-04 103.05706  0.0854995
#> 3  2016-01-05 100.47452 -2.4225751
#> 4  2016-01-06  98.50827 -4.3321348
#> 5  2016-01-07  94.35077 -8.3697559
#> 6  2016-01-08  94.84967 -7.8852393
#> 7  2016-01-11  96.38550 -6.3936946
#> 8  2016-01-12  97.78438 -5.0351540
#> 9  2016-01-13  95.27031 -7.4767271
#> 10 2016-01-14  97.35395 -5.4531690
#> # ... with 243 more rows
```

### Transforming & Mutating Data with xts, quantmod, and TTR Functions

You may already know and love `xts`, `quantmod`, and `TTR`, which is why the core functionality is fully integrated. The workhorse functions are `tq_transform()` and `tq_mutate()`. These functions leverage the power of `xts`, `quantmod`, and `TTR`. The full list of `xts`, `quantmod`, and `TTR` functions that can be used are:

``` r
tq_transform_fun_options()
#> $xts
#>  [1] "apply.daily"     "apply.monthly"   "apply.quarterly"
#>  [4] "apply.weekly"    "apply.yearly"    "diff.xts"       
#>  [7] "lag.xts"         "period.apply"    "period.max"     
#> [10] "period.min"      "period.prod"     "period.sum"     
#> [13] "periodicity"     "to.daily"        "to.hourly"      
#> [16] "to.minutes"      "to.minutes10"    "to.minutes15"   
#> [19] "to.minutes3"     "to.minutes30"    "to.minutes5"    
#> [22] "to.monthly"      "to.period"       "to.quarterly"   
#> [25] "to.weekly"       "to.yearly"       "to_period"      
#> 
#> $quantmod
#>  [1] "allReturns"      "annualReturn"    "ClCl"           
#>  [4] "dailyReturn"     "Delt"            "HiCl"           
#>  [7] "Lag"             "LoCl"            "LoHi"           
#> [10] "monthlyReturn"   "Next"            "OpCl"           
#> [13] "OpHi"            "OpLo"            "OpOp"           
#> [16] "periodReturn"    "quarterlyReturn" "seriesAccel"    
#> [19] "seriesDecel"     "seriesDecr"      "seriesHi"       
#> [22] "seriesIncr"      "seriesLo"        "weeklyReturn"   
#> [25] "yearlyReturn"   
#> 
#> $TTR
#>  [1] "adjRatios"          "ADX"                "ALMA"              
#>  [4] "aroon"              "ATR"                "BBands"            
#>  [7] "CCI"                "chaikinAD"          "chaikinVolatility" 
#> [10] "CLV"                "CMF"                "CMO"               
#> [13] "DEMA"               "DonchianChannel"    "DPO"               
#> [16] "DVI"                "EMA"                "EMV"               
#> [19] "EVWMA"              "GMMA"               "growth"            
#> [22] "HMA"                "KST"                "lags"              
#> [25] "MACD"               "MFI"                "momentum"          
#> [28] "OBV"                "PBands"             "ROC"               
#> [31] "rollSFM"            "RSI"                "runCor"            
#> [34] "runCov"             "runMAD"             "runMax"            
#> [37] "runMean"            "runMedian"          "runMin"            
#> [40] "runPercentRank"     "runSD"              "runSum"            
#> [43] "runVar"             "SAR"                "SMA"               
#> [46] "SMI"                "stoch"              "TDI"               
#> [49] "TRIX"               "ultimateOscillator" "VHF"               
#> [52] "VMA"                "volatility"         "VWAP"              
#> [55] "VWMA"               "wilderSum"          "williamsAD"        
#> [58] "WMA"                "WPR"                "ZigZag"            
#> [61] "ZLEMA"
```

#### tq\_transform

`tq_transform()` returns a new data set that can either be in the same periodicity or a different periodicity as the original data set. Let's use `tq_transform` to transform the periodicity of the `aapl_prices`. The `quantmod` OHLC codes are used to select the open, high, low, close, and volume (OHLCV) columns, which are then sent to the transformation function, `xts::to.period`, for transformation to monthly periodicity. We now have a much smaller data set containing the monthly prices.

``` r
aapl_prices %>%
    tq_transform(x_fun = OHLCV, transform_fun = to.period, period = "months")
#> # A tibble: 132 × 6
#>          date  open  high   low close    volume
#>        <dttm> <dbl> <dbl> <dbl> <dbl>     <dbl>
#> 1  2006-01-31 75.50 76.34 73.75 75.51 228385500
#> 2  2006-02-28 71.58 72.40 68.10 68.49 316745100
#> 3  2006-03-31 63.25 63.61 62.24 62.72 203839300
#> 4  2006-04-28 69.38 71.30 69.20 70.39 190009400
#> 5  2006-05-31 61.76 61.79 58.69 59.77 320244400
#> 6  2006-06-30 57.59 57.75 56.50 57.27 184923900
#> 7  2006-07-31 66.83 68.63 66.28 67.96 223210400
#> 8  2006-08-31 67.28 68.30 66.66 67.85 143674300
#> 9  2006-09-29 77.11 77.52 76.68 76.98 101453100
#> 10 2006-10-31 81.45 81.68 80.23 81.08 125368600
#> # ... with 122 more rows
```

#### tq\_mutate

The cousin of `tq_transform()` is `tq_mutate()`. While `tq_transform()` produces a new, transformed data set, `tq_mutate()` modifies the existing data set. This is very useful for applying `TTR` functions like `BBands`, `MACD`, Moving Averages, etc. There is one caveat: the mutation must be in the same periodicity as the original data set (otherwise you can't add columns because the rows will not match up). Let's use `tq_mutate()` to add some Bollinger Bands and MACD using the closing prices (Cl in OHLC notation).

``` r
aapl_prices %>%
    tq_mutate(x_fun = Cl, mutate_fun = MACD) %>%
    tq_mutate(x_fun = HLC, mutate_fun = BBands)
#> # A tibble: 2,769 × 13
#>          date  open  high   low close    volume  adjusted  macd signal
#>        <date> <dbl> <dbl> <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl>
#> 1  2006-01-03 72.38 74.75 72.25 74.75 201808600  9.726565    NA     NA
#> 2  2006-01-04 75.13 75.98 74.50 74.97 154900900  9.755191    NA     NA
#> 3  2006-01-05 74.83 74.90 73.75 74.38 112355600  9.678420    NA     NA
#> 4  2006-01-06 75.25 76.70 74.55 76.30 176114400  9.928252    NA     NA
#> 5  2006-01-09 76.73 77.20 75.74 76.05 168760200  9.895722    NA     NA
#> 6  2006-01-10 76.25 81.89 75.83 80.86 569967300 10.521606    NA     NA
#> 7  2006-01-11 83.84 84.80 82.59 83.90 373448600 10.917174    NA     NA
#> 8  2006-01-12 84.97 86.40 83.62 84.29 320202400 10.967921    NA     NA
#> 9  2006-01-13 84.99 86.01 84.60 85.59 194076400 11.137079    NA     NA
#> 10 2006-01-17 85.70 86.38 83.87 84.71 208905900 11.022573    NA     NA
#> # ... with 2,759 more rows, and 4 more variables: dn <dbl>, mavg <dbl>,
#> #   up <dbl>, pctB <dbl>
```

### Scaling with the tidyverse

All functions return data sets as `tibbles`, which allows for interaction within the `tidyverse`. This means we can:

-   Use `dplyr` and `tidyr` to select, filter, nest/unnest, etc.
-   Use the pipe (`%>%`) for chaining operations.
-   Seamlessly scale data retrieval and transformations/mutations using `purrr` to map functions.

A very basic example is retrieving the stock prices for multiple stocks. We can do this by piping a tibble of stock symbols to a mutation that maps the `tq_get(get = "stock.prices")` function.

``` r
tibble(symbol = c("AAPL", "GOOG", "AMZN", "FB", "AVGO", "SWKS","NVDA")) %>%
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")))
#> # A tibble: 7 × 2
#>   symbol         stock.prices
#>    <chr>               <list>
#> 1   AAPL <tibble [2,769 × 7]>
#> 2   GOOG <tibble [2,769 × 7]>
#> 3   AMZN <tibble [2,769 × 7]>
#> 4     FB <tibble [1,163 × 7]>
#> 5   AVGO <tibble [1,865 × 7]>
#> 6   SWKS <tibble [2,769 × 7]>
#> 7   NVDA <tibble [2,769 × 7]>
```

Further Information
-------------------

This just scratches the surface of the features. See the [`tidyquant` vignette](vignettes/tidyquant.md) for further details on the package.
