# Load necessary libraries
library(tidyverse)
library(data.table)

# Create a synthetic dataset of hotel reviews
documents <- list(
  c("room", "clean", "comfortable", "quiet"),
  c("food", "delicious", "breakfast", "fresh"),
  c("service", "friendly", "helpful", "staff"),
  c("room", "spacious", "clean", "view"),
  c("food", "tasty", "dinner", "variety"),
  c("service", "courteous", "professional", "staff"),
  c("room", "quiet", "clean", "comfortable"),
  c("food", "fresh", "lunch", "delicious"),
  c("service", "helpful", "staff", "courteous"),
  c("room", "view", "spacious", "clean")
)

# Define vocabulary and topics
vocab <- c("room", "clean", "comfortable", "quiet", "spacious", "view",
           "food", "delicious", "breakfast", "fresh", "tasty", "dinner", "variety", "lunch",
           "service", "friendly", "helpful", "staff", "courteous", "professional")
K <- 3  # Number of topics
V <- length(vocab)  # Vocabulary size
D <- length(documents)  # Number of documents


# Randomly assign initial topics to words
set.seed(123)
z <- lapply(documents, function(doc) sample(1:K, length(doc), replace = TRUE))

# Count matrices
nw <- matrix(0, nrow = K, ncol = V)  # word-topic counts
nd <- matrix(0, nrow = D, ncol = K)  # document-topic counts
nwsum <- rep(0, K)  # total word count for each topic
ndsum <- sapply(documents, length)  # total word count for each document

# Initialize counts
for (d in 1:D) {
  for (n in 1:length(documents[[d]])) {
    topic <- z[[d]][n]
    word <- match(documents[[d]][n], vocab)
    nw[topic, word] <- nw[topic, word] + 1
    nd[d, topic] <- nd[d, topic] + 1
    nwsum[topic] <- nwsum[topic] + 1
  }
}

# Hyperparameters
alpha <- 0.1
beta <- 0.01

# Gibbs sampling function
gibbs_sampler <- function(iterations) {
  for (iter in 1:iterations) {
    for (d in 1:D) {
      for (n in 1:length(documents[[d]])) {
        word <- match(documents[[d]][n], vocab)
        topic <- z[[d]][n]
        
        # Decrease counts
        nw[topic, word] <- nw[topic, word] - 1
        nd[d, topic] <- nd[d, topic] - 1
        nwsum[topic] <- nwsum[topic] - 1
        
        # Compute probabilities for each topic
        p <- (nw[, word] + beta) / (nwsum + V * beta) * (nd[d, ] + alpha) / (ndsum[d] + K * alpha)
        p <- p / sum(p)
        
        # Sample new topic
        new_topic <- sample(1:K, 1, prob = p)
        
        # Increase counts with new topic
        z[[d]][n] <- new_topic
        nw[new_topic, word] <- nw[new_topic, word] + 1
        nd[d, new_topic] <- nd[d, new_topic] + 1
        nwsum[new_topic] <- nwsum[new_topic] + 1
      }
    }
  }
}

# Run Gibbs sampling for 100 iterations
gibbs_sampler(100)


# Topic-word distribution
phi <- (nw + beta) / (rowSums(nw) + V * beta)
rownames(phi) <- paste("Topic", 1:K)
colnames(phi) <- vocab
phi

# Document-topic distribution
theta <- (nd + alpha) / (rowSums(nd) + K * alpha)
rownames(theta) <- paste("Document", 1:D)
theta

# Final topic assignments for each document
z
