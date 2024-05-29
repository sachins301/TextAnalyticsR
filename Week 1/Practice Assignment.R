library(quanteda)
library(readtext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidytext)

getwd()
setwd("C:/Users/sachi/OneDrive - University of Utah/Sem3/MKTG6640 TA/Week 1/")

data <- read.csv("deceptive-opinion-1.csv")
data


# Randomly sample 500 rows from the dataset
sample_data <- data %>% sample_n(500)

sample_data %>%
  filter(deceptive == "truthful") %>%
  group_by(polarity) %>%
  summarize(average_length = mean(str_length(text)), 
            n = n())

sample_data %>%
  filter(deceptive == "deceptive") %>%
  group_by(polarity) %>%
  summarize(average_length = mean(str_length(text)), 
            n = n())

sample_corpus <- corpus(sample_data, text_field = "text")
head(sample_corpus, 3)

sample_dfm <- sample_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, 
         remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords()) %>% 
  tokens_wordstem() %>%
  dfm()

sample_dfm
dim(sample_dfm)
sparsity(sample_dfm)



# Preprocess text for tidytext
tidy_df <- data %>%
  mutate(text = tolower(text)) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", "")) %>%
  mutate(text = str_replace_all(text, "\\d+", ""))

# Tokenize and count word frequencies
tokens <- tidy_df %>%
  unnest_tokens(word, text)

word_counts <- tokens %>%
  count(word, sort = TRUE)

# Plot the most frequent words
top_words <- word_counts %>% top_n(20, n)

ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Most Frequent Words", x = "Words", y = "Frequency")



# Calculate the number of tokens per document
tokens_per_document <- rowSums(sample_dfm)

# View summary statistics
summary(tokens_per_document)

tokens_df <- data.frame(tokens = tokens_per_document)

# Plot the histogram
ggplot(tokens_df, aes(x = tokens)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Tokens Per Document",
       x = "Number of Tokens",
       y = "Frequency") +
  theme_minimal()


textplot_wordcloud(sample_dfm)
