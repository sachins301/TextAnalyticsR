# Load necessary libraries
library(dplyr)
library(caret)
library(quanteda)
library(quanteda.textmodels)
library(randomForest)
library(e1071)

# Load the dataset
loan_data <- read_csv("loan_data.csv")

# Inspect the dataset
glimpse(loan_data)

# Handle missing values
loan_data <- na.omit(loan_data)

# Encode categorical variables
loan_data$EmploymentStatus <- as.factor(loan_data$EmploymentStatus)
loan_data$LoanPurpose <- as.factor(loan_data$LoanPurpose)
loan_data$Default <- as.factor(loan_data$Default)


# Text preprocessing using quanteda
corpus <- corpus(loan_data$ReviewText)
tokens <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_wordstem()

# Convert the tokens to a Document-Feature Matrix (DFM)
dfm <- dfm(tokens)

# Perform Latent Semantic Analysis (LSA)
lsa_model <- textmodel_lsa(dfm, nd = 10) # Number of dimensions can be adjusted
lsa_features <- as.data.frame(lsa_model$docs)
colnames(lsa_features) <- paste0("Topic", 1:ncol(lsa_features))

# Combine the LSA features with the original dataset
loan_data_combined <- cbind(loan_data, lsa_features)

# Remove the original text column
loan_data_combined <- loan_data_combined %>% select(-ReviewText)

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(loan_data_combined$Default, p = 0.8, list = FALSE)
train_data <- loan_data_combined[trainIndex, ]
test_data <- loan_data_combined[-trainIndex, ]

# Train a Random Forest model
rf_model <- randomForest(Default ~ ., data = train_data, ntree = 100)

# Train an SVM model
svm_model <- svm(Default ~ ., data = train_data, kernel = "radial")

# Evaluate Random Forest model
rf_predictions <- predict(rf_model, test_data)
rf_confusion <- confusionMatrix(rf_predictions, test_data$Default)
print(rf_confusion)

# Evaluate SVM model
svm_predictions <- predict(svm_model, test_data)
svm_confusion <- confusionMatrix(svm_predictions, test_data$Default)
print(svm_confusion)

# Compare accuracy, precision, recall, F1-score, and ROC-AUC for both models
rf_accuracy <- rf_confusion$overall['Accuracy']
svm_accuracy <- svm_confusion$overall['Accuracy']

rf_precision <- rf_confusion$byClass['Pos Pred Value']
svm_precision <- svm_confusion$byClass['Pos Pred Value']

rf_recall <- rf_confusion$byClass['Sensitivity']
svm_recall <- svm_confusion$byClass['Sensitivity']

rf_f1 <- 2 * (rf_precision * rf_recall) / (rf_precision + rf_recall)
svm_f1 <- 2 * (svm_precision * svm_recall) / (svm_precision + svm_recall)

print(paste("Random Forest - Accuracy:", rf_accuracy, "Precision:", rf_precision, "Recall:", rf_recall, "F1-score:", rf_f1))
print(paste("SVM - Accuracy:", svm_accuracy, "Precision:", svm_precision, "Recall:", svm_recall, "F1-score:", svm_f1))
