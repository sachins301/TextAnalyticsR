# Load necessary libraries
library(quanteda)
library(stm)

# Create synthetic dataset
reviews <- c("Great product, very high quality.",
             "Terrible customer service.",
             "Good value for money.",
             "Product broke after just a few days.",
             "Excellent product, and excellent customer service.",
             "Not worth the price.",
             "I love this product!",
             "The product didn’t meet my expectations.",
             "Fantastic! Highly recommend.",
             "The customer service was very disappointing.")

ratings <- c(5, 1, 4, 1, 5, 2, 5, 2, 5, 1)

regions <- c("North", "South", "East", "West", "North", "South", "East", "West", "North", "South")

purchase_location <- c("Online", "In-store", "Online", "In-store", "Online", "In-store", "Online", "In-store", "Online", "In-store")

user_type <- c("New", "Returning", "New", "Returning", "New", "Returning", "New", "Returning", "New", "Returning")

# Combine into a data frame
data <- data.frame(reviews, ratings, regions, purchase_location, user_type)

# Prepare covariates
ratings <- as.numeric(data$ratings)
regions <- as.factor(data$regions)
purchase_location <- as.factor(data$purchase_location)
user_type <- as.factor(data$user_type)

# Process the text for STM
processed <- textProcessor(documents = data$reviews, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Define the number of topics
K <- 3

# Run STM
stm_model <- stm(documents = out$documents, vocab = out$vocab, K = K, 
                 prevalence =~ ratings + regions + purchase_location + user_type, 
                 data = out$meta, init.type="Spectral", verbose=FALSE)

# Display the topics
print(stm_model)

# Display the most probable words for each topic
top_words <- labelTopics(stm_model, n = 3)
print(top_words)

# Estimate the effect of the covariates on the topics
effect <- estimateEffect(1:K ~ ratings + regions + purchase_location + user_type, 
                         stm_model, meta = out$meta)

# Visualize the topics
plot(stm_model, type = "summary")

# Visualize the effect of covariates on topics
plot(effect, covariate = "ratings", topics = 1, 
     model = stm_model, method = "continuous", xlab = "Ratings", main = "Effect of Ratings on Topic 1")
plot(effect, covariate = "regions", topics = 2, 
     model = stm_model, method = "pointestimate", xlab = "Regions", main = "Effect of Regions on Topic 2")
plot(effect, covariate = "purchase_location", topics = 3, 
     model = stm_model, method = "pointestimate", xlab = "Purchase Location", main = "Effect of Purchase Location on Topic 3")

# Load the libraries
library(quanteda)

# Create a synthetic dataset
texts <- c("I loved the product", 
           "The service was terrible", 
           "Great value for the price", 
           "Product broke after a few uses", 
           "I had a great experience", 
           "Customer service was rude", 
           "Good quality for the price", 
           "Product was faulty")

# Additional factors
ratings <- c(5, 1, 4, 2, 5, 1, 4, 2)
regions <- c("North", "South", "East", "West", "North", "South", "East", "West")
purchase_location <- c("Online", "In store", "Online", "In store", "Online", "In store", "Online", "In store")
user_type <- c("New", "Existing", "New", "Existing", "New", "Existing", "New", "Existing")

# Create a dataframe
df <- data.frame(texts, ratings, regions, purchase_location, user_type)

# Convert to corpus and preprocess
corpus <- corpus(df$texts)
docvars(corpus) <- df[, -1]  # assign document-level variables
tokens <- tokens(corpus, remove_punct = TRUE)
dfm <- dfm(tokens)

# Prepare the data for stm
out <- convert(dfm, to = "stm")

# Find k: Approach 2
set.seed(835)
system.time({
  findingk_ver2.searchK <- searchK(documents = out$documents, 
                                   vocab = out$vocab,
                                   K = c(3, 4, 5), #specify K to try
                                   N = 7, # matching the size of the dataset
                                   proportion = 0.5, # default
                                   heldout.seed = 1234, # optional
                                   M = 10, # default
                                   cores = 1, # default
                                   prevalence =~ ratings + regions + purchase_location + user_type,
                                   max.em.its = 75,
                                   data = out$meta,
                                   init.type = "Spectral",
                                   verbose=FALSE)
})

# Check the results
print(findingk_ver2.searchK)

# Visualize the searchK results
plot(findingk_ver2.searchK)
