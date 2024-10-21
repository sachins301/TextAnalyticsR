library(readr)
library(stm)
library(dplyr)
library(quanteda)
library(ggplot2)
library(quanteda.textplots)
library(quanteda.textstats)

# Load the data
hotel_raw <- read_csv("Data/hotel-reviews.csv")

hotel_raw<-hotel_raw[sample(nrow(hotel_raw), 1000), ]# take a small sample


# Check the structure of the dataset to identify covariates
str(hotel_raw)

# Preprocess the data using quanteda
reviews_corpus <- corpus(hotel_raw$Description)
reviews_tokens <- tokens(reviews_corpus, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("english")) 

# Create a document-term matrix
reviews_dtm <- dfm(reviews_tokens)

# Convert to a format suitable for stm, including covariates
reviews_stm <- convert(reviews_dtm, to = "stm", docvars = hotel_raw[, c("User_ID", "Browser_Used", "Device_Used")])

# Fit the STM model
K <- 5 # Number of topics
stm_model <- stm(reviews_stm$documents, reviews_stm$vocab, K = K, max.em.its = 200, data = reviews_stm$meta, init.type = "Spectral")

# View the summary of the model
summary(stm_model)

# View the top words for each topic
labelTopics(stm_model)

# Plot the topic distribution
plot(stm_model, type = "summary")

# Find the most representative documents for each topic
thoughts <- findThoughts(stm_model, texts = hotel_raw$Description, n = 3)

# Extract the topic proportions for each document
theta <- as.data.frame(stm_model$theta)

# Add the topic proportions to the original data
reviews <- cbind(hotel_raw, theta)

# View the updated dataset with topic proportions
head(reviews)

# Visualizations

# Topic Proportions
topic_proportions <- colMeans(stm_model$theta)
barplot(topic_proportions, main = "Topic Proportions", xlab = "Topics", ylab = "Proportion")

# Top Words for Each Topic using Quanteda
#par(mfrow = c(2, 3))
for (i in 1:K) {
  topic_words <- labelTopics(stm_model, topics = i)$prob
  word_freq <- as.data.frame(topic_words)
  colnames(word_freq) <- c("Word", "Freq")
  word_freq <- word_freq[order(word_freq$Freq, decreasing = TRUE), ]
  word_freq <- head(word_freq, 50)
  textplot_wordcloud(reviews_dtm, features = word_freq$Word, max_words = 50, color = RColorBrewer::brewer.pal(8, "Dark2"), 
                     main = paste("Topic", i))
}

# Topic Correlation Visualization
topic_corr <- topicCorr(stm_model)
plot(topic_corr)

# Topic Prevalence Over Covariates
# For Browser_Used
browser_effects <- estimateEffect(1:K ~ Browser_Used, stm_model, meta = reviews_stm$meta, uncertainty = "Global")
plot.estimateEffect(browser_effects, covariate = "Browser_Used", topics = 1:K, 
                    model = stm_model, method = "difference", 
                    cov.value1 = "Chrome", cov.value2 = "Firefox", 
                    xlab = "Browser Used", main = "Effect of Browser Used on Topics")

# For Device_Used
device_effects <- estimateEffect(1:K ~ Device_Used, stm_model, meta = reviews_stm$meta, uncertainty = "Global")
plot.estimateEffect(device_effects, covariate = "Device_Used", topics = 1:K, 
                    model = stm_model, method = "difference", 
                    cov.value1 = "Desktop", cov.value2 = "Mobile", 
                    xlab = "Device Used", main = "Effect of Device Used on Topics")

