
# Install and load the quantmod package
if (!require(quantmod)) install.packages("quantmod")
library(quantmod)

# Set a symbol for a stock (e.g., AAPL for Apple Inc.)
symbol = "AAPL"

# Get stock data from Yahoo Finance
getSymbols(symbol, src = "yahoo", from = "2020-01-01", to = "2023-12-31")

# Access the retrieved data
stock_data <- get(symbol)

# Plot the closing prices
chartSeries(stock_data, type = "line", TA = "addVo();addSMA(20)", main = "AAPL Stock Price")

# Calculate daily returns
returns <- dailyReturn(AAPL)

# Plot the returns
chartSeries(returns, name="Daily Returns of AAPL")

# Calculate 30-day rolling standard deviation
rolling_sd <- runSD(returns, n = 30)

# Plot the rolling volatility
chartSeries(rolling_sd, name="30-Day Rolling Volatility of AAPL")

## library(httr)
## 
## apikey<-'omZ73xAKdSL2V3gY7DZn.....'# truncated NY Times apikey
## 
## base_url <- "http://api.nytimes.com/svc/search/v2/articlesearch.json" # Where to query
## pricing <- GET(base_url, query=list(q="pricing","api-key"=apikey)) # search term -
## pricing
## 

library(httr)

apikey<-'omZ73xAKdSL2V3gY7DZnUjxF8Xt9Hkb5'# NY Times apikey

base_url <- "http://api.nytimes.com/svc/search/v2/articlesearch.json" # Where to query
pricing <- GET(base_url, query=list(q="pricing","api-key"=apikey)) # search term - 
pricing

pricing <- GET(base_url, query=list(q="pricing",
                              "api-key"=apikey,
                              "begin_date"=20230101,
                              "end_date"=20240401))



library(jsonlite)
library(tidyverse)
data <-  httr::content(pricing, as = "text")
data <- fromJSON(data)
my_df<-tibble(data$response$docs$headline$main,data$response$docs$section_name, data$response$docs$abstract, data$response$docs$pub_date)

# Rename the columns
names(my_df)[1] <- "Title"
names(my_df)[2] <- "Section"
names(my_df)[3] <- "Abstract"
names(my_df)[4] <- "Date"

knitr::kable(tibble(my_df))

