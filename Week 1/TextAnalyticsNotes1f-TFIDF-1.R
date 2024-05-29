
library(quanteda) 
a<-"This Residence Marriott is not fancy."
b<-"It was clean, good service and suite style rooms;"
c<-"However, it's a little older than some Residence Marriotts's 
    I've stayed at and probably could use an update soon."
d<-"You can count on Residence Marriott to deliver on its promises"

hotel_demo<-c(a,b,c,d)
hotel_demo_token <- quanteda::tokens(hotel_demo,  
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE)

hotel_demo_token<-dfm(hotel_demo_token)

hotel_demo_token1<-as.matrix(hotel_demo_token)
hotel_demo_token1[1:4,1:10] 



x1<-"My stay at the Residence Marriott was terrible and location was bad"
x2<-"The highlight of my stay at the Residence Marriott was its service"
x3<-"Service at the hotel was very good"
x1<-dfm(tokens(x1))
x2<-dfm(tokens(x2))
x3<-dfm(tokens(x3))
x1
x2
x3

# compute similarity between reviews x1 and x2
library(quanteda.textstats)# for computing stats between different texts
textstat_simil(
  x1,
  x2, 
  margin = ("documents"),
  method = ( "cosine"),
  )

# compute similarity between reviews x2 and x3
textstat_simil(
  x2,
  x3, 
  margin = ("documents"),
  method = ( "cosine"),
  )


hotel_demo_token1[1:4,1:10]

#docfreq(hotel_demo_token)#### gives the frequency of words in DFM

#tfidf<-data.frame(dfm_tfidf(hotel_demo_token, scheme_tf = "prop", scheme_df = "inverse", base = 10))

tfidf <- dfm_tfidf(hotel_demo_token, scheme_tf = "prop", scheme_df = "inverse", base = 10) %>% convert(to = "data.frame")
tfidf[1:4,1:5]


# Preprocessing
library(quanteda)

# Dummy Dataset
reviews <- c("I absolutely love this product. It works perfectly and the design is sleek.", 
"The product is of absolutely poor quality. It broke after a few uses.", 
"The customer service was excellent. They promptly resolved my issue with the product.",
"The price of the product is reasonable for the features it offers.",
"I had a terrible experience with this product's quality. It doesn't work as advertised.")

ratings <- c(5, 2, 4, 3, 1)

data <- data.frame(review = reviews, rating = ratings)


# Create a corpus
corpus_df <- corpus(data$review)

# Tokenization
tokens_df <- tokens(corpus_df, remove_punct = TRUE, remove_numbers = TRUE,
                 remove_symbols = TRUE) 
        
# Create a document-feature matrix (DFM)
dfm_df <- dfm(tokens_df) %>% dfm_remove(stopwords("english"))
           
# Calculate TF-IDF
tfidf_df <- dfm_tfidf(dfm_df)

# Convert TF-IDF to a data.frame for interpretation
tfidf_df <- convert(tfidf_df, to="data.frame")

# Add the rating column back to the TF-IDF dataframe
tfidf_df$rating <- data$rating
head(tfidf_df,c(5,6))

