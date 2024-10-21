# Load required library
library(tidyverse)
library(stm)
library(topicmodels)


# Define some common legal phrases for each category
phrases_mergers <- c("The company hereby agrees to the merger", "The terms of acquisition are as follows", "The shareholders have approved the merger", "This agreement certifies the acquisition")
phrases_ip <- c("The patent rights are granted to", "The copyright agreement states", "The trademark is registered under", "This agreement involves the transfer of intellectual property rights")
phrases_employment <- c("The terms of employment include", "The employment contract states", "The employee is entitled to", "This agreement outlines the employee benefits")
phrases_contracts <- c("This contract is entered into by", "The contract terms are as follows", "The agreement is binding on", "Both parties agree to the contract")

# Define a function to generate a synthetic legal document
generate_document <- function(topic) {
  if (topic == "Mergers & Acquisitions") {
    phrases <- phrases_mergers
  } else if (topic == "Intellectual Property") {
    phrases <- phrases_ip
  } else if (topic == "Employment Law") {
    phrases <- phrases_employment
  } else if (topic == "Contracts") {
    phrases <- phrases_contracts
  }
  
  # Create a document by combining random phrases
  document <- paste(sample(phrases, 5, replace = TRUE), collapse = " ")
  
  return(document)
}

# Define a vector of topics
topics <- c("Mergers & Acquisitions", "Intellectual Property", "Employment Law", "Contracts")

# Generate a data frame of synthetic legal documents
set.seed(123)  # For reproducibility
documents <- tibble(
  Document_ID = 1:100,
  Document_Text = replicate(100, generate_document(sample(topics, 1))),
  Document_Type = replicate(100, sample(topics, 1))
)


  # Preprocessing steps
  library(quanteda)
corpus <- corpus(documents$Document_Text) # create a corpus from the review text
tokens <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) # tokenize the corpus
tokens <- tokens_tolower(tokens) # convert all text to lowercase
tokens <- tokens_remove(tokens, stopwords("en")) # remove English stopwords

# Create a Document-Feature Matrix
dfm <- dfm(tokens)
dfm


model <- seededlda::textmodel_lda(dfm, k = 4)

top_terms <- seededlda::terms(model)

print(top_terms)



# Load the necessary library
library(dplyr)

# Create the data frame
business_reviews <- data.frame(
  Business_ID = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
  Business_Name = c("Techgine", "DigiMark", "CloudNet", "Techgine", "DigiMark", "CloudNet", "Techgine", "DigiMark", "CloudNet",
                    "Techgine", "DigiMark", "CloudNet", "Techgine", "DigiMark", "CloudNet", "Techgine", "DigiMark", "CloudNet"),
  Reviewer_ID = c(345, 876, 159, 753, 392, 482, 625, 278, 593, 214, 935, 147, 786, 396, 492, 625, 298, 591),
  Review_Text = c(
    "Excellent service and quick delivery. Their customer service was also top-notch.",
    "I had a bad experience with their services. My project was delayed and over budget.",
    "Good services overall, but their prices are a bit too high.",
    "The best in the market. I've been their customer for 3 years and never had any complaints.",
    "Average service. Nothing too impressive or disappointing.",
    "Their cloud solutions have really helped streamline our business operations.",
    "Their software is buggy and crashes often. Not recommended.",
    "They were very responsive and understood our requirements well. Great work!",
    "Their team is very knowledgeable and professional. Highly recommend their services.",
    "Incredible service and speedy delivery. Loved their customer service.",
    "Disappointed with their services. My project was not completed on time and it was above the estimated cost.",
    "Decent services but their rates are too high for my liking.",
    "They are the best in the field. I've been a loyal customer for 4 years and they have never disappointed me.",
    "Mediocre service. I didn't find anything extraordinary or very disappointing.",
    "Their cloud solutions have revolutionized our business operations. Very grateful.",
    "Their software has a lot of bugs and crashes a lot. Would not recommend.",
    "They were very quick to respond and they understood our needs perfectly. Impressive work!",
    "Their team is extremely proficient and professional. Would highly recommend their services."
  ),
  Review_Rating = c(5, 2, 3, 5, 3, 4, 1, 5, 5, 5, 2, 3, 5, 3, 4, 1, 5, 5)
)

library(quanteda)

dtm_topicmodels<-tokens(business_reviews$Review_Text) %>% dfm()

num_topics=5

lda_model<- LDA(dtm_topicmodels, k = num_topics, method = "Gibbs")


library(tidytext)

term_topics <- tidy(lda_model, matrix = "beta")

top_terms <- term_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  arrange(topic, -beta)

library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") 

documents <- tidy(lda_model, matrix = "beta")

top_documents <- documents %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_documents

assignments <- augment(lda_model)
assignments


library(ldatuning)
result <- FindTopicsNumber(
  dfm,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "VEM",#Gibbs can used too
  control = list(seed = 1971),
  return_models= TRUE,
  verbose = TRUE
)
FindTopicsNumber_plot(result)
