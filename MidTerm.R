library(tidyverse)

sharkds <- data.frame(
  SNo = c(1, 2, 3, 4, 5, 6, 7),
  Pitched_Business_Identifier = c("Techgine", "DigiMark", "CloudNet", "Asd", "Awewq", "acrwrqe", "nytrtw"),
  Pitched_Business_Desc  = c("Excellent service and quick delivery. Their customer service was also top-notch.",
                             "Good services overall, but their prices are a bit too high.",
                             "Average service. Nothing too impressive or disappointing.",
                             "Incredible service and speedy delivery. Loved their customer service.",
                             "Disappointed with their services. My project was not completed on time and it was above the estimated cost.",
                             "Decent services but their rates are too high for my liking.",
                             "They are the best in the field. I've been a loyal customer for 4 years and they have never disappointed me."
                             ),
  Deal_Status = c(1, 0, 0, 1, 1, 0, 1),
  Deal_Shark = c("BC+RH", "DJ", "KOL", "LG", "MC", "DJ", "LG+DJ")
)

library(tidyverse)
library(quanteda)
library(readxl)
sharkds<-read_xls('sharkuk.xls')

sharkdfm<-tokens(sharkds$Pitched_Business_Desc,
                 remove_numbers = TRUE, remove_punct = TRUE,
                 remove_symbols = TRUE) %>% 
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm() %>% 
  dfm_tfidf(scheme_tf = "prop", scheme_df = "inverse", base = 10)

head(sharkdfm, n = 5)


sharkds$Deal_Status <- as.factor(sharkds$Deal_Status)
sharkds$Deal_Shark <- as.factor(sharkds$Deal_Shark)

sharkds %>%
  count(Deal_Status)%>%
  mutate(freq=n/sum(n))


sharkdfm<-tokens(sharkds$Pitched_Business_Desc,
                 remove_numbers = TRUE, remove_punct = TRUE,
                 remove_symbols = TRUE) %>% 
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm()

library(quanteda.textstats)
library(quanteda.textplots)
tokenfreq<-textstat_frequency(sharkdfm, n=100, force=T)
head(tokenfreq, 10)
textplot_wordcloud(sharkdfm)

library(lsa)
library(LSAfun)
sharkdfm_LSAspace <- lsa(sharkdfm, dims=dimcalc_share()) 
sharkdfm_LSAspace$tk[1:5,1:5]


library(topicmodels)
library(tidytext)

sharkdfm1<-tokens(sharkds$Pitched_Business_Desc,
                  remove_numbers = TRUE, remove_punct = TRUE,
                  remove_symbols = TRUE, split_hyphens = TRUE) %>% 
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm()

K <- 10
shark_lda <-LDA(sharkdfm1, K, method="Gibbs", control=list(iter = 200, verbose = 25))
abc <- tidy(shark_lda, matrix = "gamma")
abc


terms(shark_lda, 5)

library(tidytext)
x <- tidy(shark_lda, matrix = "gamma")
