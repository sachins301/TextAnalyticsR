
# Load up the libraries and dataset
library(tidyverse)
library(sentimentr)
library(caret)
library(quanteda)
library(broom)

getwd()
setwd("C:/Users/sachi/OneDrive - University of Utah/Sem3/MKTG6640 TA/Week 3/")

# Load up the .CSV data and explore in RStudio.
hotel_raw <- read_csv("hotel-reviews.csv")
set.seed(1234)# we set seed to replicate our results.
hotel_raw<-hotel_raw[sample(nrow(hotel_raw), 5000), ]# take a small sample
corp_hotel <- corpus(hotel_raw, text_field = "Description")# Create corpus
sample<-corpus_sample(corp_hotel, size = 20)# you can sample a corpus too!


library(quanteda)
test.lexicon <- dictionary(list(positive.terms = c("happy", "joy", "light"),
                                negative.terms = c("sad", "angry", "darkness")))

testtext<-c("I am happy and confident that the paper will be accepted", 
            "Of course, no one can be 100% sure but I am hopeful",
            "In case, it is rejected, I will be sad and angry, we will submit it to another journal")

dfm_sentiment1 <- testtext %>% tokens() %>% dfm() %>% dfm_lookup(test.lexicon)
dfm_sentiment1

positive_words_bing <- scan("positive-words.txt", what = "char", sep = "\n", skip = 35, quiet = T)
negative_words_bing <- scan("negative-words.txt", what = "char", sep = "\n", skip = 35, quiet = T)
sentiment_bing <- dictionary(list(positive = positive_words_bing, negative = negative_words_bing))

dfm_sentiment <- corp_hotel %>% tokens() %>% dfm %>% dfm_lookup(sentiment_bing)
dfm_sentiment
dfm_sentiment_df<-convert(dfm_sentiment, to ='data.frame')
dfm_sentiment_df$net<-(dfm_sentiment_df$positive)-(dfm_sentiment_df$negative)
summary(dfm_sentiment_df)# document level summary

# install.packages("remotes")
remotes::install_github("kbenoit/quanteda.dictionaries")

library("quanteda.dictionaries")
output_mfd <- quanteda.dictionaries::liwcalike(corp_hotel, 
                        dictionary = data_dictionary_MFD)
head(output_mfd)

# Proportions instead of numbers

dfm_sentiment_prop <- dfm_weight(dfm_sentiment, scheme = "prop")
dfm_sentiment_prop

## Plotting the sentiments

sentiment <- convert(dfm_sentiment_prop, "data.frame") %>%
    gather(positive, negative, key = "Polarity", value = "Share") %>% 
    mutate(document = as_factor(doc_id)) %>% 
    rename(Review = document)
sentiment
ggplot(sentiment, aes(Review, Share, fill = Polarity, group = Polarity)) + 
    geom_bar(stat='identity', position = position_dodge(), size = 1) + 
    scale_fill_brewer(palette = "Set1") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    ggtitle("Sentiment scores in Hotel Reviews (relative)")


mytext<-"I am happy and confident that the paper will be accepted.
Of course, no one can be 100% sure but I am hopeful. In case, it is rejected,
I will be sad and angry, but we will submit it to another journal."

mytext <- get_sentences(mytext)
mytext
sentiment(mytext)

mytext2<-"I am happy and confident that the paper will be accepted.
Of course, no one can be 100% sure but I am hopeful. In case, it is rejected,
I will not be sad and angry, but we will submit it to another journal." # with a negator added
mytext2 <- get_sentences(mytext2)
sentiment(mytext2)


out <- with(
    hotel_raw, 
    sentiment_by(
        get_sentences(Description), # Reviews are stored in variable Description
        list(User_ID,Device_Used) # grouping variables
    ))
head(out)

library(magrittr)
library(dplyr)
set.seed(234)

hotel_raw %>%
    filter(User_ID %in% sample(unique(User_ID), 4)) %>% 
    # %in% operator in R, is used to identify if an element belongs to a vector.
    mutate(review = get_sentences(Description)) %$%
    # The “exposition” pipe operator from magrittr package, %$% exposes the names  
    # within the left-hand side object to the right-hand side expression. For  
    # instance,
    # iris %>%
    # subset(Sepal.Length > mean(Sepal.Length)) %$%
    # cor(Sepal.Length, Sepal.Width)
    sentiment_by(review, User_ID) %>%
    highlight()


library(tidyverse)
library(cleanNLP)

cnlp_init_udpipe() # Loading namespace: udpipe which serves backend to cleanNLP

hotel_raw1<-hotel_raw[sample(nrow(hotel_raw), 100), ]# take a small sample as POS 
                                              #tagging is very resource intensive

postag<-hotel_raw1 %>% 
cnlp_annotate(text= "Description") # Outcome is list of tokens and documents

head(postag$token,n=10)

postag$token %>%
  filter(xpos == "JJ") %>% # you can play around with different POS
  group_by(lemma) %>%
  summarize(count = n()) %>%
  top_n(n = 10, count) %>%
  arrange(desc(count)) 

postag$token %>%
  group_by(doc_id) %>%
  summarize(n = n()) %>%
  left_join(postag$document, by="doc_id") %>%
  ggplot(aes(Device_Used, n)) +
    geom_line(color = grey(0.8)) +
    geom_point(aes(color = Is_Response)) +
    geom_smooth(method="loess", formula = y ~ x) +
    theme_minimal()

require(quanteda)
require(quanteda.textmodels)
require(caret)

summary(corp_hotel, 5)# let's check the summary of our original corpus

# generate 3500 numbers without replacement
set.seed(300)
id_train <- sample(1:5000, 3500, replace = FALSE)

# create docvar with ID
corp_hotel$id_numeric <- 1:ndoc(corp_hotel)

# get training set
dfmat_training <- corpus_subset(corp_hotel, id_numeric %in% id_train) %>%
tokens(remove_punct = TRUE) %>%  # Tokenize while removing punctuation
  tokens_remove(stopwords("english"), padding = FALSE) %>%  # Remove stopwords
  tokens_wordstem(language = "english") %>%  # Apply stemming
  dfm() 

#Since we will run  the binary multinomial NB model, let's convert the dfm to a binary matrix before training the model. 

dfmat_training<-dfm_weight(dfmat_training, scheme = "boolean")

# get test set (documents not in id_train)
dfmat_test <- corpus_subset(corp_hotel, !id_numeric %in% id_train) %>%
tokens(remove_punct = TRUE) %>%  # Tokenize while removing punctuation
  tokens_remove(stopwords("english"), padding = FALSE) %>%  # Remove stopwords
  tokens_wordstem(language = "english") %>%  # Apply stemming
  dfm() 

dfmat_test<-dfm_weight(dfmat_test, scheme = "boolean") 

tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$Is_Response)
summary(tmod_nb)


dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))


actual_class <- dfmat_matched$Is_Response
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

#confusionMatrix(tab_class, mode = "everything")
