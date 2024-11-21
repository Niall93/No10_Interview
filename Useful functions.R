#Example JSON from API
library(httr)
library(jsonlite)
response <- GET("https://api.example.com/data")
parsed_data <- content(response, as = "text", encoding = "UTF-8")
parsed_data <- fromJSON(parsed_data)

