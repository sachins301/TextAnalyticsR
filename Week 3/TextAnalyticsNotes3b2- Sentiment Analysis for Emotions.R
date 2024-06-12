
library(syuzhet)
library(tidyverse)
hotel_raw <- read_csv("C:/Users/u0474728/Dropbox/Utah Department Stuff/Teaching/Text Analysis/Summer 2024/Course Materials/RMarkdown/Data/hotel-reviews.csv")
set.seed(122335)
hotel_raw <- slice_sample(hotel_raw,n= 5000)
nrc_data <- get_nrc_sentiment(hotel_raw$Description)
df_combined <- bind_cols(hotel_raw, nrc_data)


## Visualize Emotions
#transpose
td<-data.frame(t(nrc_data))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[1:ncol(td)]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Hotel sentiments")

