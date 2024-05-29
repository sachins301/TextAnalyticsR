
library(quanteda)
library(readtext)# https://readtext.quanteda.io/articles/readtext_vignette.html
library(tidyverse)# For loading bunch of packages such as dplyr, stringi etc.

# Load up the .CSV data.
hotel_raw <- readtext("C:/Users/u0474728/Dropbox/Utah Department Stuff/Teaching/Text Analysis/Summer 2024/Course Materials/RMarkdown/data/hotel-reviews.csv", text_field = "Description")

set.seed(1245654)# for replicability 
hotel_raw <- slice_sample(hotel_raw,n=5000)# take a small sample

str(hotel_raw)

my_corpus<-corpus(hotel_raw, text_field = "text")
head(my_corpus,3)

head(docvars(my_corpus),3)

table(docvars(my_corpus, field = "Device_Used"))

summary(my_corpus,3)

str_sub(my_corpus[1], start = 1, end = 70)

my_corpus_sentences <- corpus_reshape(my_corpus, to = "sentences")
my_corpus_sentences[3]

my_corpus_edge <- corpus_subset(my_corpus, Browser_Used == "Edge")
summary(my_corpus_edge,5)

my_tokens <- my_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, 
         remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords()) %>% 
  tokens_wordstem()

head(my_tokens,3)

my_dfm_toks <- dfm(my_tokens)
my_dfm_toks



dim(my_dfm_toks)
colnames(my_dfm_toks)[1:20]


head(my_dfm_toks, n = 3)

topfeatures(my_dfm_toks, 9)
