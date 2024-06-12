
library(tidyverse)
library(sentimentr)

# Load up the .CSV data and explore in RStudio.
hotel_raw <- read_csv("C:/Users/u0474728/Dropbox/Utah Department Stuff/Teaching/Text Analysis/Summer 2024/Course Materials/RMarkdown/Data/hotel-reviews.csv")

hotel_raw <- hotel_raw %>%
    mutate(hotel_name = case_when(str_detect(Description, "Hilton")~ "Hilton",
                                  str_detect(Description, "Hyatt")~ "Hyatt",
                                  str_detect(Description, "Marriott")~ "Marriott"))
hotel_sub <- hotel_raw %>% filter(hotel_name=="Hilton"|hotel_name=="Hyatt"|hotel_name=="Marriott")

# Create a new column rating based on a string condition
hotel_sub <- hotel_sub %>%
        mutate(rating = ifelse(str_detect(hotel_name, "Marriott"), 
        round(rnorm(nrow(filter(hotel_sub, str_detect(hotel_name, "Marriott"))),
                    mean = 4.2, sd = .25)), NA)) %>%
        mutate(rating = ifelse(str_detect(hotel_name, "Hilton"), 
        round(rnorm(nrow(filter(hotel_sub, str_detect(hotel_name, "Hilton"))),
                    mean = 3.55, sd = .33)), rating)) %>%
        mutate(rating = ifelse(str_detect(hotel_name, "Hyatt"), 
        round(rnorm(nrow(filter(hotel_sub, str_detect(hotel_name, "Hyatt"))),
                    mean = 3.25, sd = .35)), rating))


abc <- hotel_sub %>%
    sentimentr::get_sentences('Description') %>%
    sentiment_by(by = 'User_ID')

joined_data <- inner_join(hotel_sub, abc, by="User_ID")

# Perform an ANOVA
model <- aov(rating ~ as.factor(hotel_name), data = joined_data)

# Print the summary of the model
summary(model)

# Calculate the means for each group
group_means <- aggregate(rating ~ as.factor(hotel_name), hotel_sub, mean)

# Print the group means
print(group_means)

# Perform a Tukey's HSD test
posthoc <- TukeyHSD(model)

# Print the results of the post-hoc test
print(posthoc)


# Run a linear regression
model1 <- lm(rating ~ Is_Response+ hotel_name +word_count, data = joined_data)

# Print the summary of the model
summary(model1)


# Run a linear regression
model2 <- lm(rating ~ Is_Response+ hotel_name+word_count +ave_sentiment, data = joined_data)

# Print the summary of the model
summary(model2)

