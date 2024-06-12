library(quanteda)
product.lexicon <- dictionary(list(satisfied.terms = c("satisfied", "good", "great"),
                                   dissatisfied.terms = c("terrible", "poor", "rubbish")))

text<-c("The product was pretty good, I am satisfied", 
        "What a rubbish product",
        "Terrible customer service but the product was good")

dfm_sentiment1 <- text %>% tokens() %>% dfm() %>% dfm_lookup(product.lexicon)
dfm_sentiment1

library(quanteda)

library(quanteda.dictionaries)

reviews_df <- data.frame(
  User_ID = c(34, 17, 91, 2, 69, 88, 45, 23, 57, 77),
  Review_Text = c("amazing product but the delivery was bad", 
                  "good product but the packaging was awful", 
                  "excellent quality but the size is terrible", 
                  "nice design but the color is the worst", 
                  "best product I have bought but the price disappointed me", 
                  "amazing but bad delivery service", 
                  "good but awful packaging", 
                  "excellent but the color is terrible", 
                  "nice but the price is the worst", 
                  "best but the delivery time disappointed me"),
  Product_ID = c(7, 22, 45, 8, 30, 11, 39, 46, 27, 33),
  Review_Rating = c(3, 4, 2, 3, 5, 3, 4, 2, 3, 5)
)

corp_reviews <- corpus(reviews_df, text_field = "Review_Text")# Create corpus

output_lsd <- liwcalike(corp_reviews, data_dictionary_LSD2015)
output_lsd

library(quanteda)

library(quanteda.dictionaries)

reviews_df <- data.frame(
  User_ID = c(34, 17, 91, 2, 69, 88, 45, 23, 57, 77),
  Review_Text = c("amazing product but the delivery was bad", 
                  "good product but the packaging was awful", 
                  "excellent quality but the size is terrible", 
                  "nice design but the color is the worst", 
                  "best product I have bought but the price disappointed me", 
                  "amazing but bad delivery service", 
                  "good but awful packaging", 
                  "excellent but the color is terrible", 
                  "nice but the price is the worst", 
                  "best but the delivery time disappointed me"),
  Product_ID = c(7, 22, 45, 8, 30, 11, 39, 46, 27, 33),
  Review_Rating = c(3, 4, 2, 3, 5, 3, 4, 2, 3, 5)
)

corp_reviews <- corpus(reviews_df, text_field = "Review_Text")# Create corpus

output_lsd <- liwcalike(corp_reviews, data_dictionary_LSD2015)


library(quanteda)

library(quanteda.dictionaries)

reviews_df <- data.frame(
  User_ID = c(34, 17, 91, 2, 69, 88, 45, 23, 57, 77),
  Review_Text = c("amazing product but the delivery was bad", 
                  "good product but the packaging was awful", 
                  "excellent quality but the size is terrible", 
                  "nice design but the color is the worst", 
                  "best product I have bought but the price disappointed me", 
                  "amazing but bad delivery service", 
                  "good but awful packaging", 
                  "excellent but the color is terrible", 
                  "nice but the price is the worst", 
                  "best but the delivery time disappointed me"),
  Product_ID = c(7, 22, 45, 8, 30, 11, 39, 46, 27, 33),
  Review_Rating = c(3, 4, 2, 3, 5, 3, 4, 2, 3, 5)
)

corp_reviews <- corpus(reviews_df, text_field = "Review_Text")# Create corpus



library(sentimentr)
mytext2 <- corp_reviews %>% get_sentences() %>% sentiment()
mytext2



library(quanteda)

library(quanteda.dictionaries)

reviews_df <- data.frame(
  User_ID = c(34, 17, 91, 2, 69, 88, 45, 23, 57, 77),
  Review_Text = c("amazing product but the delivery was bad", 
                  "good product but the packaging was awful", 
                  "excellent quality but the size is terrible", 
                  "nice design but the color is the worst", 
                  "best product I have bought but the price disappointed me", 
                  "amazing but bad delivery service", 
                  "good but awful packaging", 
                  "excellent but the color is terrible", 
                  "nice but the price is the worst", 
                  "best but the delivery time disappointed me"),
  Product_ID = c(7, 22, 45, 8, 30, 11, 39, 46, 27, 33),
  Review_Rating = c(3, 4, 2, 3, 5, 3, 4, 2, 3, 5)
)

library(magrittr)
library(dplyr)
set.seed(2)

reviews_df %>%
  filter(User_ID %in% sample(unique(User_ID), 3)) %>% 
  mutate(review = get_sentences(Review_Text)) %$%
  sentiment_by(review, User_ID) %>%
  highlight()




suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(cleanNLP)))

cnlp_init_udpipe() # Loading namespace: udpipe which serves backend to cleanNLP

postag<-reviews_df %>% 
  cnlp_annotate(text= "Review_Text") # Outcome is list of tokens and documents

head(postag$token,n=10)



postag$token %>%
  filter(xpos == "NN") %>% # you can play around with different POS
  group_by(lemma) %>%
  summarize(count = n())%>% 
  arrange(desc(count)) 
  



library(quanteda)

library(quanteda.dictionaries)

reviews_df <- data.frame(
  User_ID = c(34, 17, 91, 2, 69, 88, 45, 23, 57, 77),
  Review_Text = c("amazing product but the delivery was bad", 
                  "good product but the packaging was awful", 
                  "excellent quality but the size is terrible", 
                  "nice design but the color is the worst", 
                  "best product I have bought but the price disappointed me", 
                  "amazing but bad delivery service", 
                  "good but awful packaging", 
                  "excellent but the color is terrible", 
                  "nice but the price is the worst", 
                  "best but the delivery time disappointed me"),
  Product_ID = c(7, 22, 45, 8, 30, 11, 39, 46, 27, 33),
  Review_Rating = c(3, 4, 2, 3, 5, 3, 4, 2, 3, 5)
)

require(quanteda.textmodels)
require(caret)

set.seed(145)
id_train <- sample(1:10, 6, replace = FALSE)

# create docvar with ID
corp_reviews$id_numeric <- 1:ndoc(corp_reviews)

# get training set
dfmat_training <- corpus_subset(corp_reviews, id_numeric %in% id_train) %>%
  tokens() %>% dfm() %>%dfm_remove(stopwords("english")) %>%dfm_wordstem()

dfmat_training<-dfm_weight(dfmat_training, scheme = "boolean")

# get test set (documents not in id_train)
dfmat_test <- corpus_subset(corp_reviews, !id_numeric %in% id_train) %>%
  tokens() %>% dfm() %>%dfm_remove(stopwords("english")) %>%dfm_wordstem()
dfmat_test<-dfm_weight(dfmat_test, scheme = "boolean") 

library(quanteda.textmodels)
tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$Review_Rating)
summary(tmod_nb)

dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))
actual_class <- dfmat_matched$Review_Rating
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class
