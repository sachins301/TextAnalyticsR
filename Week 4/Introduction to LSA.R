

library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(ggplot2)

# Sample dataset with brand names and associated reviews
reviews <- tibble(
    brand = c("Brand_A", "Brand_B", "Brand_C", "Brand_D", "Brand_E"),
    review = c(
        "Brand A is reliable and has excellent customer service.",
        "Brand B is innovative but has poor customer service.",
        "Brand C is affordable and has a good warranty.",
        "Brand D is expensive but offers high-quality products.",
        "Brand E is eco-friendly and has a great design."
    )
)

# Tokenize and preprocess the text data
tokens <- reviews$review %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("en"))

# Create a Document-Feature Matrix (DFM)
dfm <- tokens %>%
    dfm()

# Perform LSA using quanteda.textmodels
lsa_space <- textmodel_lsa(dfm, nd = 2)

# View the LSA scores for the documents
lsa_scores <- as.data.frame(head(lsa_space$docs))

# Combine brand names and LSA scores
brand_scores <- cbind(reviews$brand, lsa_scores)

# Create the perceptual map
ggplot(brand_scores, aes(x = V1, y = V2, label = reviews$brand)) +
    geom_text(size = 5, color = "blue") +
    labs(title = "Perceptual Map of Brands", x = "LSA Dimension 1", y = "LSA Dimension 2") +
    theme_minimal()

# View the document scores
head(lsa_space$docs)

# View the feature scores
head(lsa_space$features)

library(quanteda)
library(quanteda.textmodels)
library(cluster)

# Synthetic dataset of customer support tickets
tickets <- tibble(
    id = 1:6,
    text = c(
        "I have a problem with my bill. The amount is incorrect.",
        "My phone is not working. It won't turn on.",
        "I was overcharged on my last bill. Can you help?",
        "My device is having issues. The screen is blank.",
        "I have a call-drop issue. It is really bad.",
        "The phone is broken. It's not charging."
    )
)
# Create a Document-Feature Matrix (DFM)
dfm <- tickets$text %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("en"), padding = TRUE) %>%
    dfm()

# Perform LSA
lsa_space <- textmodel_lsa(dfm, nd = 2)

# Get LSA scores for the documents
lsa_scores <- as.data.frame(lsa_space$docs[, 1:2])

# Perform k-means clustering
set.seed(123)  # for reproducibility
clusters <- kmeans(lsa_scores, centers = 2)

# Add cluster assignments to the ticket data
tickets$cluster <- clusters$cluster

# Print tickets with cluster assignments
print(tickets)


library(ralger)
library(polite)
bow("https://www.nytimes.com/2023/05/29/opinion/inflation-groceries-pricing-walmart.html")
my_link <- "https://www.nytimes.com/2023/05/29/opinion/inflation-groceries-pricing-walmart.html"

my_node<-"p"# selector gadget will give these element ID


DF <- scrap(link = my_link, node = my_node)

library(LSAfun)

genericSummary(DF,k=3)# k is the number of sentences to be used in summary

