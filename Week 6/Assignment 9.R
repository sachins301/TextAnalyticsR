library(dplyr)
library(quanteda)
library(quanteda.textmodels)
library(caret)

corpus <- data_corpus_moviereviews #from quanteda
summary(corpus,5)

set.seed(1234)
id_train <- sample(1:2000,1500, replace=F)
head(id_train, 10)
ndoc(corpus)
docvars(corpus, "id_numeric") <- 1:ndoc(corpus)
dfmat_train <- corpus_subset(corpus, id_numeric %in% id_train) %>% tokens() %>% dfm() %>% dfm_weight(scheme="boolean")
dfmat_test <- corpus_subset(corpus, !(id_numeric %in% id_train)) %>% tokens %>% dfm() %>% dfm_weight(scheme="boolean")

sentmod.nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "sentiment"), distribution = "Bernoulli")
summary(sentmod.nb)

dfmat_matched <- dfm_match(dfmat_test, features=featnames(dfmat_train))

actual_class <- docvars(dfmat_matched, "sentiment")
predicted_class <- predict(sentmod.nb, newdata=dfmat_matched)
tab_class <- table(actual_class,predicted_class)
tab_class