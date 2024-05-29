
library(tidyverse)# For loading bunch of packages such as dplyr, stringi etc. 
library(tidytext) # for tidy data format text processing 

# Load up the .CSV data.
hotel_raw <- read_csv("C:/Users/u0474728/Dropbox/Utah Department Stuff/Teaching/Text Analysis/Summer 2024/Course Materials/RMarkdown/data/hotel-reviews.csv")

set.seed(1245654)# for replicability 
hotel_raw <- slice_sample(hotel_raw,n=5000)# take a small sample

hotel_raw_token<-hotel_raw %>% unnest_tokens(word, Description) # Description variable is where the reviews are stored 

head(hotel_raw_token)

data(stop_words) # comes from tidytext package
hotel_raw_token <- hotel_raw_token %>% anti_join(stop_words) 
# anti_join takes the dataframe and eliminates the common words


hotel_raw_token %>% count(word, sort = TRUE)%>% filter(n > 2000)

hotel_raw_token %>% count(word, sort = TRUE) %>% 
    filter(n > 2000) %>% mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip() 
#we plotted only those words that appeared more than 2000 times
