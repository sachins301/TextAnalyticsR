library(quanteda)
library(quanteda.textmodels)
library(caret)
library(tidyverse)
library(readxl)


sharktank_raw <- read_excel("C:/Users/sachi/OneDrive - University of Utah/Sem3/MKTG6640 TA/Week 7/sharkuk.xls")
sharktank_raw$Deal_Status <- as.factor(sharktank_raw$Deal_Status) 



### Class Imbalance Problem
summary(sharktank_raw)

### Test and Train Datasets
set.seed(12345)

sharktank_record <- createDataPartition(sharktank_raw$Deal_Status, times = 1,
                                        p = 0.7, list = FALSE)

sharktank_train <- sharktank_raw[sharktank_record,] 
sharktank_test <- sharktank_raw[-sharktank_record,]

### Verify Proportions
sharktank_train %>%
  count(Deal_Status)%>%
  mutate(freq=n/sum(n))

sharktank_test %>%
  count(Deal_Status)%>%
  mutate(freq=n/sum(n))
##################################################
### Creating training and test DFM for Classifiers
##################################################
df_training <- tokens(sharktank_train$Pitched_Business_Desc) %>%
  dfm() %>% 
  dfm_remove(stopwords("english")) %>% 
  dfm_wordstem()%>% 
  dfm_tfidf()

df_test <- tokens(sharktank_test$Pitched_Business_Desc) %>%
  dfm() %>% 
  dfm_remove(stopwords("english")) %>% 
  dfm_wordstem()%>% 
  dfm_tfidf()

################
#Training Dataset
#################
sharktank_train_dfm<-convert(df_training, "data.frame")
sharktank_train_dfm2<-sharktank_train_dfm[,-1]
sharktank_train_df <- cbind(Label = sharktank_train$Deal_Status, sharktank_train_dfm2)

#################
#Test Dataset
################

sharktank_test_dfm <- dfm_match(df_test, features = featnames(df_training))
sharktank_test_dfm

######################################################
#NAIVE BAYES CLASSIFIER
######################################################

train_nb <- textmodel_nb(df_training, sharktank_train$Deal_Status)
summary(train_nb)




actual_class <- sharktank_train$Deal_Status
predicted_class <- predict(train_nb, newdata = sharktank_train_dfm2)
tab_class <- table(predicted_class,actual_class)
confusionMatrix(tab_class, mode = "everything")





###  Inspecting how well the classification worked

df_matched <- dfm_match(df_test, features = featnames(df_training))

actual_class <- sharktank_test$Deal_Status
predicted_class <- predict(train_nb, newdata = df_matched)
tab_class <- table(predicted_class,actual_class)
confusionMatrix(tab_class, mode = "everything")





###################################################################
# RANDOM FOREST CLASSIFIER
###################################################################

########## CROSS VALIDATION#############
# Use caret to create stratified folds for 10-fold cross validation repeated 
# 2 times (i.e., create 20 random stratified samples)

set.seed(12345)
cv.folds <- createMultiFolds(sharktank_train$Deal_Status, k = 10, times = 2)
# basically this will create 20 random stratified samples

cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 2, index = cv.folds)

library(irlba)
lsa_df <- irlba(t(sharktank_train_dfm2), nv = 50, maxit = 600)

# Take a look at the new feature data up close.
#View(lsa_df$v)

final_df_train <- data.frame(Label = sharktank_train$Deal_Status, lsa_df$v)

############Training RF Model##################
## Random Forest
rf1 <- train(Label ~ ., data = final_df_train,
             method = "rf",
             ntree = 100,
             trControl = cv.cntrl, tuneLength = 7,
             importance = TRUE)

rf1$finalModel

# Let's drill-down on the results.
confusionMatrix(sharktank_train_df$Label, rf1$finalModel$predicted)

library(randomForest)
varImpPlot(rf1$finalModel)

############Testing RF Model##################

sigma.inverse <- 1 / lsa_df$d # taking the tranpose of the singular matrix is # same as calculating it's inverse
u.transpose <- t(lsa_df$u)#transpose of the term matrix
final_df_test <- t(sigma.inverse * u.transpose %*% t(sharktank_test_dfm))
final_df_test<-as.matrix(final_df_test)


final_df_test <- data.frame(Label = sharktank_test$Deal_Status, final_df_test)
preds <- predict(rf1, final_df_test)
confusionMatrix(preds, final_df_test$Label)


###################################################################
# SVM CLASSIFIER
###################################################################

SVM_Linear <- train(Label ~ ., data = final_df_train, method = "svmLinear", 
                    preProcess = c("center", "scale"),
                    trControl = cv.cntrl, tuneLength = 7)
# Predictions

final_df_train$Label<-as.factor(final_df_train$Label)
preds <- predict(SVM_Linear, final_df_train)
confusionMatrix(preds, final_df_train$Label)

## SVM test
final_df_test$Label<-as.factor(final_df_test$Label)
preds <- predict(SVM_Linear, final_df_test)
confusionMatrix(preds, final_df_test$Label)



final_df_test$Label<-as.factor(final_df_test$Label)
preds <- predict(SVM_Linear, final_df_test)
confusionMatrix(preds, final_df_test$Label, mode='everything')

