
library(tidyverse)
library(quanteda)
library(tidytext)
library(topicmodels)
library(stm)
hotel_raw <- read_csv("../hotel-reviews.csv")
set.seed(1234)
hotel_raw<-hotel_raw[sample(nrow(hotel_raw), 1000), ]# take a small sample


hotel_raw_token2 <- tokens(hotel_raw$Description, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE)

#Create DTM, but remove terms which occur in less than 1% of all documents 
# and more than 90%
hotel_raw_dfm<-hotel_raw_token2 %>%
  tokens_remove(stopwords(source = "smart")) %>%
  #tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm()%>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.90, docfreq_type = "prop")

#hotel_raw_dfm<-dfm_tfidf(hotel_raw_dfm, scheme_tf = "prop", scheme_df = "inverse", base = 10)

#hotel_raw_dfm[1:5,1:5]
hotel_demo_token1<-as.matrix(hotel_raw_dfm)
# dim(hotel_demo_token1)
hotel_demo_token1[1:3,1:8] 


library(topicmodels)
set.seed(12345)
K <- 15
hotel_lda <-LDA(hotel_raw_dfm, K, method="Gibbs", control=list(iter = 200, verbose = 25))



term_topics <- tidy(hotel_lda, matrix = "beta")#Topic Term Probabilities # we tidy it up to use tidytext package
term_topics

top_terms <- term_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  arrange(topic, -beta)
top_terms

library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") 

hotel_documents <- tidy(hotel_lda, matrix = "gamma")
hotel_documents

top_documents <- hotel_documents %>%
  group_by(topic) %>%
  slice_max(gamma, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -gamma)
top_documents

tidy(hotel_raw_dfm) %>% # taking the original dfm 
  filter(document == 'text6') %>%
  arrange(desc(count))

assignments <- augment(hotel_lda)
assignments

library(ldatuning)
# Compute the K value "scores" from K=2 to K=25
result <- FindTopicsNumber(
    hotel_raw_dfm,
    topics = seq(from = 2, to = 25, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "VEM",#Gibbs can used too
    control = list(seed = 1971),
    mc.cores = 2L,
    return_models= TRUE,
    verbose = TRUE
)
# Griffiths2004: Implement scoring algorithm. In order to use this algorithm, the LDA model 
# MUST be generated using the keep control parameter >0 (defaults to 50) so that the logLiks 
# vector is retained

# Plot the scores, with graphs labeled "minimize" or
# maximize based on whether it's a metric you want
# to minimize or maximize
FindTopicsNumber_plot(result)


ldaResult <- posterior(hotel_lda)# have a look a some of the results (posterior distributions)
attributes(ldaResult)
library(text2vec)
text2vec::perplexity(hotel_raw_dfm, ldaResult$terms, ldaResult$topics)

library(textmineR)
set.seed(12345)

model <- FitLdaModel(dtm = hotel_raw_dfm, 
                     k = 20,
                     iterations = 200, # I usually recommend at least 500 iterations or more
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2)# allows two cores for processing 

# R-squared 
# - only works for probabilistic models like LDA and CTM
model$r2

# log Likelihood (does not consider the prior) 
plot(model$log_likelihood, type = "l")

summary(model$coherence)

hist(model$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

