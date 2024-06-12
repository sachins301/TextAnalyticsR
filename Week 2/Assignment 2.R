# Load required libraries
library(httr)
library(jsonlite)

# Define the API key
api_key <- "1d34dcca8a9b4d5a8e435de8af933314"

# Define the API endpoint
api_endpoint <- "https://newsapi.org/v2/everything"

# Define the query parameters
query_params <- list(
  q = "finance",  # search for articles with 'finance' in them
  language = "en",  # get articles in English
  sortBy = "publishedAt",  # sort by publication date
  apiKey = api_key  # your API key
)

# Make the GET request
response <- GET(url = api_endpoint, query = query_params)

# Parse the JSON response
data <- fromJSON(content(response, "text", encoding = "UTF-8"))

# Print the articles


print(data$articles)

query_params <- list(
 q = "economics",
 language = "en",
 sortBy = "publishedAt",
 apiKey = api_key
)

# query_params <- list(
#   query = "economics",
#   language = "en",
#   sortBy = "publishedAt",
#   apiKey = api_key
# )

response <- GET(url = api_endpoint, query = query_params)

# Parse the JSON response
data <- fromJSON(content(response, "text", encoding = "UTF-8"))
data

data_df <- data.frame(data[["articles"]])
data_df

length(data$articles)
nrow(data$articles)
length(data)
ncol(data$articles)

query_params <- list(
  q = "economics & finance",
  language = "en",
  sortBy = "publishedAt",
  apiKey = api_key
)
