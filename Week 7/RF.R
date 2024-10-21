
library(tidyverse)
library(e1071)
library(caret)
library(quanteda)
library(irlba)
library(randomForest)

# Load up the .CSV data and explore in RStudio.
hotel_raw <- read_csv("Data/hotel-reviews.csv")
set.seed(1234)
hotel_raw<-hotel_raw[sample(nrow(hotel_raw), 5000), ]# take a small sample
summary(hotel_raw)

# Check for missing data
sum(!complete.cases(hotel_raw))

hotel_raw$Is_Response <- as.factor(hotel_raw$Is_Response) # tells whether happy or not happy
hotel_raw$Device_Used <- as.factor(hotel_raw$Device_Used)
hotel_raw$Browser_Used <- as.factor(hotel_raw$Browser_Used)


hotel_raw %>%
count(Is_Response)%>%
  mutate(freq=n/sum(n))

hotel_raw<-hotel_raw %>%
  mutate(ReviewLength=nchar(Description)) 

hotel_raw%>%
  group_by(Is_Response) %>%
  summarise(AvLength=mean(ReviewLength))

library(caret)
set.seed(12345)
hotel_record <- createDataPartition(hotel_raw$Is_Response, times = 1,
                               p = 0.7, list = FALSE)
hotel_train <- hotel_raw[hotel_record,]
hotel_test <- hotel_raw[-hotel_record,]


hotel_train %>%
count(Is_Response)%>%
  mutate(freq=n/sum(n))

hotel_test %>%
count(Is_Response)%>%
  mutate(freq=n/sum(n))


library(quanteda)
hotel_train_token2 <- tokens(hotel_train$Description, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE) %>% 
                       tokens_tolower()


hotel_train_dfm<-hotel_train_token2 %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  dfm() %>% 
    dfm_trim( min_termfreq = 10, min_docfreq = 2) %>% 
    dfm_tfidf()

hotel_train_dfm1 <- convert(hotel_train_dfm, to ="data.frame")
hotel_train_df <- cbind(Label = hotel_train$Is_Response, data.frame(hotel_train_dfm1))


## # Use caret to create stratified folds for 10-fold cross validation repeated
## # 2 times (i.e., create 20 random stratified samples)
 set.seed(48743)
 cv.folds <- createMultiFolds(hotel_train$Is_Response, k = 10, times = 2)
# # basically this will create 20 random stratified samples
##
 cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                          repeats = 2, index = cv.folds)
#

 library(doSNOW)
##
## # Time the code execution
 start.time <- Sys.time()
##
## # Create a cluster to work on 4 logical cores.
 cl <- makeCluster(3, type = "SOCK") # Simplistically, we are asking computer to run
## # 3 instances of Rstudio for processing. I have total of 4 cores in my computer
 registerDoSNOW(cl) # register the instance
##
 rpart.cv.1 <- train(Label ~ ., data = hotel_train_df, method = "rpart",
                     trControl = cv.cntrl, tuneLength = 7)
##
##
## # Processing is done, stop cluster.
 stopCluster(cl)
##
## # Total time of execution
 total.time <- Sys.time() - start.time
 total.time
##
## # Check out our results.
 rpart.cv.1

## # Perform SVD. Specifically, reduce dimensionality down to 300 columns
## # for our latent semantic analysis (LSA).
 library(irlba)
 
 
 # Load necessary libraries
 library(quanteda)
 library(irlba)
 
 
 train.lsa <- irlba(t(hotel_train_dfm), nv = 300, maxit = 600)

 ## # Take a look at the new feature data up close.
 View(train.lsa$v)
##

 train_svd <- data.frame(Label = hotel_train$Is_Response, train.lsa$v)
## # Time the code execution
 start.time <- Sys.time()
##
## # Create a cluster to work on 4 logical cores.
 cl <- makeCluster(3, type = "SOCK")
 registerDoSNOW(cl) # register the instance
##
 rf1 <- train(Label ~ ., data = train_svd,
                     method = "rf",
                     ntree = 100,
                     trControl = cv.cntrl, tuneLength = 7,
                     importance = TRUE)

## # Processing is done, stop cluster.
 stopCluster(cl)
##
## # Total time of execution
 total.time <- Sys.time() - start.time
 total.time
##
##

 rf1

 str(rf1, max.level = 1)

 rf1$finalModel

## # Let's drill-down on the results.
 confusionMatrix(train_svd$Label, rf1$finalModel$predicted)

 # train_svd$reviewLength <- hotel_raw$ReviewLength
## # Having added the feature, you can train the model again

 library(randomForest)
 varImpPlot(rf1$finalModel)
##

 hotel_test_token <- tokens(hotel_test$Description, what = "word",
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE) %>%
                        tokens_tolower()
#
 hotel_test_dfm<-hotel_test_token %>%
   tokens_remove(stopwords(source = "smart")) %>%
   tokens_wordstem() %>%
   dfm() %>%
   dfm_trim( min_termfreq = 10, min_docfreq = 2) %>%
     dfm_tfidf()
     
##

 
 hotel_test_dfm <- dfm_match(hotel_test_dfm, featnames(hotel_train_dfm))
 
 hotel_test_matrix <- as.matrix(hotel_test_dfm)
 hotel_test_dfm
##

 sigma.inverse <- 1 / train.lsa$d # taking the tranpose of the singular matrix is # same as calculating it's inverse
 u.transpose <- t(train.lsa$u)#transpose of the term matrix
 test_svd <- t(sigma.inverse * u.transpose %*% t(hotel_test_dfm))
 test_svd<-as.matrix(test_svd)

 test_svd <- data.frame(Label = hotel_test$Is_Response, test_svd)

 preds <- predict(rf1, test_svd)
 confusionMatrix(preds, test_svd$Label)
