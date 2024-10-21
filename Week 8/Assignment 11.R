library(tidyverse)
hotel_review<-read_csv("C:/Users/sachi/OneDrive - University of Utah/Sem3/MKTG6640 TA/Week 1/hotel-reviews.csv")
hotel_review<-sample_n(hotel_review, 5000)

# Convert binary variable, Is_Response to a continuous ratings variable
convert_fn <- function(x) {
  sapply(x, function(y) {
    if (y == "not happy") {
      return(sample(1:2, size = 1, prob = c(0.5, 0.5)))
    } else { # y == "happy"
      return(sample(3:5, size = 1, prob = c(0.33, 0.33, 0.34)))
    }
  })
}

hotel_review$ratings <- convert_fn(hotel_review$Is_Response)
head(hotel_review)

library(caret)
library(magrittr)

# Set the seed for reproducibility
set.seed(123)

# Define the proportion of data you want to keep for training 
trainIndex <- createDataPartition(hotel_review$ratings, p = 0.70, list = FALSE, times = 1)

# Create the training set
train_set <- hotel_review[trainIndex,]

# Create the testing set
test_set <- hotel_review[-trainIndex,]


library(sentimentr)

t1 <- train_set %>%
  mutate(review = get_sentences(Description)) %$%
  sentiment_by(review, User_ID)

train_set1 <- train_set %>% left_join(t1,by = "User_ID")

Train <- train_set1 %>% select(User_ID,ratings, word_count, ave_sentiment)

ctrl <- trainControl(method="cv", number=10)

# Create a linear regression model
model <- train(ratings ~ word_count+ ave_sentiment, data=Train, method="lm", trControl=ctrl)

# Print model results
print(model)
summary(model)


t2 <- test_set %>%
  mutate(review = get_sentences(Description)) %$%
  sentiment_by(review, User_ID)

test_set1 <- test_set %>% left_join(t2,by = "User_ID")

Test <- test_set1 %>% select(User_ID,ratings, word_count, ave_sentiment)

# Generate predictions on test data
predictions <- predict(model, newdata = Test)

# Calculate R-squared and RMSE
rsq_rmse <- postResample(pred = predictions, obs = Test$ratings)

# Print the results
print(rsq_rmse)

library(caret)
library(quanteda)
review_corpus<-corpus(hotel_review, text_field = "Description")

hotel_review_token <- tokens(review_corpus, what = "word", 
                             remove_numbers = TRUE, remove_punct = TRUE,
                             remove_symbols = TRUE)
#Create DFM
hotel_review_tfidf<-hotel_review_token %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm() %>% 
  dfm_trim( min_termfreq = 50, min_docfreq = 10) %>% 
  dfm_tfidf()

# we don't need to transpose here as we will use other library

library("quanteda.textmodels")
mylsa <- textmodel_lsa(hotel_review_tfidf, nd = 100)

new.dataset <- data.frame(mylsa$docs) 
new.dataset$y.var <- hotel_review$ratings



test_index <- createDataPartition(new.dataset$y.var, p = .2, list = F)
Test_LSA<-new.dataset[test_index,]
Train_LSA<-new.dataset[-test_index,]

ctrl <- trainControl(method="cv", number=10)
#Training data
# Create a linear regression model
model <- train(y.var ~ ., data=Train_LSA, method="lm", trControl=ctrl)
summary(model)

# Generate predictions on test data
predictions1 <- predict(model, newdata = Test_LSA)

# Calculate R-squared and RMSE
rsq_rmse1 <- postResample(pred = predictions1, obs = Test_LSA$y.var)

# Print the results
print(rsq_rmse1)



hotel_review_tfidf1 <- data.frame(y.var=hotel_review$ratings,hotel_review_tfidf )


# Convert the dfm object to a data.frame
hotel_review_tfidf_df <- convert(hotel_review_tfidf, to = "data.frame")
# Combine the ratings with the tfidf data frame
hotel_review_tfidf1 <- data.frame(y.var = hotel_review$ratings, hotel_review_tfidf_df)

#hotel_review_tfidf1 <- hotel_review_tfidf1 %>% select(-doc_id)

test_index <- createDataPartition(hotel_review_tfidf1$y.var, p = .2, list = F)

test_text <- hotel_review_tfidf1[test_index, ]
train_text <- hotel_review_tfidf1[-test_index, ]

## Set up repeated k-fold cross-validation
train_control <- trainControl(method = "cv", number = 2)

# Fit the SVR model
set.seed(12345)
SVM_Linear <- train(
  y.var ~., data = train_text, method = "svmLinear",
  trControl = train_control,
  preProcess = c("center","scale"),
  tuneLength = 7
)

# Predict on the test set
predictions <- predict(SVM_Linear, newdata = test_text)

# Evaluate the model
postResample(pred = predictions, obs = test_text$y.var)


set.seed(1234)
library(seededlda)
# Train a topic model with 5 topics
lda <- seededlda::textmodel_lda(hotel_review_tfidf, k = 8)

df <- data.frame(y.var=hotel_review$ratings,lda$theta )

model <- lm(y.var ~ ., data = df)
# Print the summary of the model
summary(model)


# Load required libraries
library(glmnet)
library(caret)
library(quanteda)

set.seed(12345)

hotel_record <- createDataPartition(hotel_review$ratings, times = 1,
                                    p = 0.7, list = FALSE)

hotel_train <- hotel_review[hotel_record,] # using hotel_record for indexing
hotel_test <- hotel_review[-hotel_record,]

df_training <- tokens(hotel_train$Description) %>%
  dfm() %>%
  dfm_remove(stopwords("english"))%>%
  dfm_wordstem ()

df_test <- tokens(hotel_test$Description) %>%
  dfm() %>%
  dfm_remove(stopwords("english"))%>%
  dfm_wordstem ()


### Train the model

# Set up the outcome variable
y <- hotel_train$ratings

# Create a sparse matrix from the dfm
x <- as(df_training, "sparseMatrix")

# Ridge regression with cross-validation (alpha = 0)
cv_ridge <- cv.glmnet(x, y, alpha = 0)

# Lasso regression with cross-validation (alpha = 1)
cv_lasso <- cv.glmnet(x, y, alpha = 1)

# Elastic Net regression with cross-validation (alpha = 0.5)
cv_elastic_net <- cv.glmnet(x, y, alpha = 0.5)


# Get coefficients at the optimal lambda for Ridge regression
best_coefs_ridge <- coef(cv_ridge, s = "lambda.min")
#print(best_coefs_ridge)

# Plot cross-validation results for Ridge regression
plot(cv_ridge)


# Get coefficients at the optimal lambda for Lasso regression
best_coefs_lasso <- coef(cv_lasso, s = "lambda.min")
#print(best_coefs_lasso)

# Plot cross-validation results for Lasso regression
plot(cv_lasso)


# Get coefficients at the optimal lambda for Elastic Net regression
best_coefs_elastic_net <- coef(cv_elastic_net, s = "lambda.min")
#print(best_coefs_elastic_net)

# Plot cross-validation results for Elastic Net regression
plot(cv_elastic_net)


# Fit the final Ridge model
final_ridge <- glmnet(x, y, alpha = 0, lambda = cv_ridge$lambda.min)

# Fit the final Lasso model
final_lasso <- glmnet(x, y, alpha = 1, lambda = cv_lasso$lambda.min)

# Fit the final Elastic Net model
final_elastic_net <- glmnet(x, y, alpha = 0.5, lambda = cv_elastic_net$lambda.min)



new_data <- dfm_match(df_test, features = featnames(df_training))

# Predict with the final Ridge model
predictions_ridge <- predict(final_ridge, newx = new_data)

# Predict with the final Lasso model
predictions_lasso <- predict(final_lasso, newx = new_data)

# Predict with the final Elastic Net model
predictions_elastic_net <- predict(final_elastic_net, newx = new_data)


library(caret)

# Calculate R-squared for the Ridge model
r_squared_ridge <- postResample(pred = predictions_ridge, obs = hotel_test$ratings)[2]
r_squared_ridge

# Calculate RMSE for the Ridge model
rmse_ridge <- postResample(pred = predictions_ridge, obs = hotel_test$ratings)[1]
rmse_ridge

# Calculate R-squared for the Lasso model
r_squared_lasso <- postResample(pred = predictions_lasso, obs = hotel_test$ratings)[2]
r_squared_lasso

# Calculate RMSE for the Lasso model
rmse_lasso <- postResample(pred = predictions_lasso, obs = hotel_test$ratings)[1]
rmse_lasso

# Calculate R-squared for the Elastic Net model
r_squared_elastic_net <- postResample(pred = predictions_elastic_net, obs = hotel_test$ratings)[2]
r_squared_elastic_net

# Calculate RMSE for the Elastic Net model
rmse_elastic_net <- postResample(pred = predictions_elastic_net, obs = hotel_test$ratings)[1]
rmse_elastic_net

