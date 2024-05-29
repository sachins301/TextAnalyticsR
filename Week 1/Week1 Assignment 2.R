library(tidyverse)
library(tidytext)
library(quanteda)

reviews <- c("This product is amazing! I highly recommend it.",
             
             "I'm quite disappointed with the quality. It broke after a few uses.",
             
             "The customer service was excellent. They promptly resolved my issue.",
             
             "The price is reasonable for the features it offers.",
             
             "I had a terrible experience with this product. It doesn't work as advertised.")

ratings <- c(5, 2, 4, 4, 1)

data <- data.frame(review = reviews, rating = ratings)

data %>% mutate(review = tolower(review))

data %>% mutate(word_count = str_count(review, "\\w+"))

textstat.collocations(data$review)

text <- c("It was clean, good service and suite style rooms; however, it’s a little older than some Residence Marriotts’s I’ve stayed at.")
corpus(text) %>% tokens()
