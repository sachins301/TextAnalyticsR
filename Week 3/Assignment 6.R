# Load required library
library(dplyr)

# Set the seed to make the example reproducible
set.seed(123)

# Create a vector of random ages between 18 and 60
age <- sample(18:60, 20, replace = TRUE)

# Create a vector of genders
gender <- sample(c("Male", "Female"), 20, replace = TRUE)

# Create a vector of random incomes between $20,000 and $100,000
income <- sample(20000:100000, 20, replace = TRUE)

# Create a vector of education levels
education <- sample(c("High School", "Bachelor's", "Master's", "PhD"), 20, replace = TRUE)

# Create a vector of random prices between $20 and $30
price <- round(runif(20, min = 20, max = 30),1)

# Create a vector of ratings and corresponding reviews
rating_and_reviews <- data.frame(
  rating = c(5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 5, 4, 3, 2, 1),
  reviews = c("I loved the toaster, it was amazing!",
              "The toaster was good, but I've used better.",
              "The toaster was okay, but not great.",
              "I was disappointed with the toaster, it didn't meet my expectations.",
              "I did not like the toaster, it was poor quality.",
              "The toaster was excellent, I use it every day!",
              "The toaster was useful, but not worth the price.",
              "The toaster was not what I expected, I wouldn't buy it again.",
              "I was not satisfied with the toaster, it didn't work as advertised.",
              "I didn't like the toaster, it was too complicated to use.",
              "The toaster was fantastic, it exceeded my expectations.",
              "The toaster was good, but it was not worth the price.",
              "I didn't enjoy the toaster, it was not useful to me.",
              "I was unhappy with the toaster, it arrived damaged.",
              "The toaster was terrible, I will not buy it again.",
              "The toaster was wonderful, I will definitely buy it again!",
              "The toaster was decent, but I've seen better.",
              "The toaster was below average, it lacked key features.",
              "I was frustrated with the toaster, it was not user-friendly.",
              "The toaster was the worst I've used, I do not recommend it.")
)

# Sample rows from rating_and_reviews to randomize the order
rating_and_reviews <- sample_n(rating_and_reviews, 20, replace = FALSE)

data_df <- data.frame(age, gender, income, education, price, rating_and_reviews)

# Regression Model 1
model1 <- lm(rating ~ price, data = data_df)

summary(model1)


# Regression Model 2
model2 <- lm(rating ~ age+ gender +income+ education+price, data = data_df)

summary(model2)


library(sentimentr)
data_df <- data_df %>%
  get_sentences('reviews') %>%
  sentiment()

# Regression Model 3
model3 <- lm(rating ~ age+ gender +income+ education+price+word_count+sentiment, data = data_df)

summary(model3)
