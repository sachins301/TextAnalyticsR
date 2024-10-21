
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


set.seed(12345)
hotel_record <- createDataPartition(hotel_raw$Is_Response, times = 1,
                               p = 0.7, list = FALSE)
hotel_train <- hotel_raw[hotel_record,]
hotel_test <- hotel_raw[-hotel_record,]


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


# Perform SVD. Specifically, reduce dimensionality down to 300 columns
# for our latent semantic analysis (LSA).
library(irlba)
train_lsa <- irlba(t(hotel_train_dfm), nv = 300, maxit = 600)

train_svd <- data.frame(Label = hotel_train$Is_Response, ReviewLength= nchar(hotel_train$Description),train_lsa$v)


# Use caret to create stratified folds for 10-fold cross validation repeated 
# 2 times (i.e., create 20 random stratified samples)
set.seed(48743)
cv.folds <- createMultiFolds(train_svd$Label, k = 10, times = 2)
# basically this will create 20 random stratified samples

cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 2, index = cv.folds)


library(doSNOW)

# Time the code execution
start.time <- Sys.time()

# Create a cluster to work on 4 logical cores.
cl <- makeCluster(3, type = "SOCK") # Simplistically, we are asking computer to run
# 3 instances of Rstudio for processing. I have total of 4 cores in my computer
registerDoSNOW(cl) # register the instance

SVM_Linear <- train(Label ~ ., data = train_svd, method = "svmLinear", 
                    preProcess = c("center", "scale"),
                    trControl = cv.cntrl, tuneLength = 7)


# Processing is done, stop cluster.
on.exit(stopCluster(cl))
# Total time of execution  
total.time <- Sys.time() - start.time
total.time

train_svd$Label<-as.factor(train_svd$Label)
preds <- predict(SVM_Linear, train_svd)
confusionMatrix(preds, train_svd$Label)


# Time the code execution
start.time <- Sys.time()

# Create a cluster to work on 4 logical cores.
cl <- makeCluster(3, type = "SOCK") # Simplistically, we are asking computer to run
# 3 instances of Rstudio for processing. I have total of 4 cores in my computer
registerDoSNOW(cl) # register the instance

set.seed(12345)
model <- train(
  Label ~., data = train_svd, method = "svmLinear",
  trControl = cv.cntrl,
  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
  preProcess = c("center","scale")
  )


# Processing is done, stop cluster.
on.exit(stopCluster(cl))
# Total time of execution  
total.time <- Sys.time() - start.time
total.time



plot(model)


# Time the code execution
start.time <- Sys.time()

# Create a cluster to work on 4 logical cores.
cl <- makeCluster(3, type = "SOCK") # Simplistically, we are asking computer to run
# 3 instances of Rstudio for processing. I have total of 4 cores in my computer
registerDoSNOW(cl) # register the instance

set.seed(12345)
SVM_RB <- train(
  Label ~., data = train_svd, method = "svmRadial",
  trControl = cv.cntrl,
  preProcess = c("center","scale"),
  tuneLength = 7
  )

# Processing is done, stop cluster.
on.exit(stopCluster(cl))
# Total time of execution  
total.time <- Sys.time() - start.time
total.time


SVM_RB$bestTune

preds <- predict(SVM_RB, train_svd)
confusionMatrix(preds, train_svd$Label)


hotel_test_token <- tokens(hotel_test$Description, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE) %>% 
                       tokens_tolower()

hotel_test_dfm<-hotel_test_token %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  dfm() %>% 
    dfm_trim( min_termfreq = 10, min_docfreq = 2) %>% 
    dfm_tfidf() 


hotel_test_dfm <- dfm_match(hotel_test_dfm, features = featnames(hotel_train_dfm))

hotel_test_matrix <- as.matrix(hotel_test_dfm)
hotel_test_dfm


sigma.inverse <- 1 / train_lsa$d # taking the tranpose of the singular matrix is # same as calculating it's inverse
u.transpose <- t(train_lsa$u)#transpose of the term matrix
test_svd <- t(sigma.inverse * u.transpose %*% t(hotel_test_dfm))
test_svd<-as.matrix(test_svd)

test_svd <- data.frame(Label = hotel_test$Is_Response, ReviewLength= nchar(hotel_test$Description), test_svd)

test_svd$Label<-as.factor(test_svd$Label)
preds <- predict(SVM_RB, test_svd)
confusionMatrix(preds, test_svd$Label)

test_svd$Label<-as.factor(test_svd$Label)
preds <- predict(SVM_Linear, test_svd)
confusionMatrix(preds, test_svd$Label)


