knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE, fig.dim = c(3.5, 3.5))
options(width = 40)


# Load necessary libraries
library(tibble)
library(quanteda)

# Define product categories
product_categories <- c("laptop", "smartphone", "headphones", "smartwatch", "camera")

# Define some example reviews for each product category
laptop_reviews <- c("The battery life is great", "It's super fast", 
                    "The screen is amazing")
smartphone_reviews <- c("The camera is superb", "Love the sleek design", 
                        "It's very user friendly")
headphones_reviews <- c("The sound quality is excellent", "They are very comfortable", 
                        "Too much bass")
smartwatch_reviews <- c("Tracks my workouts accurately", "Love the design", 
                        "The battery life is excellent")
camera_reviews <- c("Takes high quality pictures", "It's very easy to use", 
                    "Love the zoom feature")

# Create a tibble (similar to a data frame) with the reviews and product categories
reviews <- tibble(
  review = c(laptop_reviews, smartphone_reviews, headphones_reviews, 
             smartwatch_reviews, camera_reviews),
  product_category = rep(product_categories, each = 3)
)

# Preprocessing steps

corpus <- corpus(reviews, text_field = "review") # create a corpus from the review text
tokens <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) 
tokens <- tokens_tolower(tokens) # convert all text to lowercase
tokens <- tokens_remove(tokens, stopwords("en")) # remove English stopwords

# Create a Document-Feature Matrix
dfm <- dfm(tokens)

# Train a topic model with 5 topics
#renv::install("seededlda")## Only the first time you run it

library(seededlda)
# Train a topic model with 5 topics
lda <- seededlda::textmodel_lda(dfm, k = 5)

# Display the top words in each topic
knitr::kable(seededlda::terms(lda))

# You can also predict the topics of documents using topics()
dat <- quanteda::docvars(lda$data)
dat$topic <- seededlda::topics(lda)
knitr::kable(head(dat, 10))


library(readr)

# Create synthetic reviews for two hypothetical competitors
set.seed(12345)
product_types <- c("Smartwatch1", "Smartwatch2")
competitors <- c("Competitor1", "Competitor2")
themes <- list(
    "battery life" = c("Great battery life!", "The battery drains too quickly.", 
                       "Battery lasts all day.", "Battery life is not as advertised."),
    "display quality" = c("The display is vibrant and sharp.", "Display is mediocre.", 
                          "Love the OLED display!", "Screen resolution is subpar."),
    "performance" = c("Super fast and responsive!", "Performance is laggy.", 
                      "Handles all apps and games smoothly.", "Tends to freeze and crash."),
    "price" = c("Excellent value for the price.", "Overpriced for what it offers.", 
                "Affordable and well worth it.", "Too expensive."),
    "customer service" = c("Customer service was helpful and prompt.", 
                           "Had a bad experience with customer service.", 
                           "Customer support is top notch.", 
                           "Customer service could be improved.")
)

# Generate synthetic dataset
reviews <- data.frame(
    product = sample(product_types, 500, replace = TRUE),
    competitor = sample(competitors, 500, replace = TRUE),
    review = replicate(500, paste(sample(unlist(themes), 5), collapse = " "))
)

# Print the first few rows of the dataset
#head(reviews)

# Preprocessing steps
corpus <- corpus(reviews$review) # create a corpus from the review text
tokens <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) 
tokens <- tokens_tolower(tokens) # convert all text to lowercase
tokens <- tokens_remove(tokens, stopwords("en")) # remove English stopwords

# Create a Document-Feature Matrix
dfm <- dfm(tokens)

# Train a topic model with 5 topics
model <- seededlda::textmodel_lda(dfm, k = 5)

# Extract the top terms for each topic
top_terms <- seededlda::terms(model)

# Print the top terms for each topic
print(top_terms)

# Assign each document to a topic
doc_topics <- model$theta
doc_max_topics <- apply(doc_topics, 1, which.max)

# Add the dominant topic to each review in the dataset
reviews$dominant_topic <- doc_max_topics

# Now, you can analyze the dominant topics for each competitor
table(reviews$competitor, reviews$dominant_topic)

# Now, you can analyze the dominant topics for each competitor
topic_distribution <- table(reviews$competitor, reviews$dominant_topic)

# Provide meaningful names to the rows and columns
rownames(topic_distribution) <- paste("Competitor", rownames(topic_distribution))
colnames(topic_distribution) <- paste("Topic", colnames(topic_distribution))

# Add margins (totals for each row and column)
topic_distribution <- addmargins(topic_distribution)

# Print the labeled table
print(topic_distribution)


