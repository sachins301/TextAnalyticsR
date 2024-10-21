
library(quanteda)
txt2 <- c("a a a b b c", "a a c e", "a c e f g")
txt2 %>% tokens() %>% dfm() %>% fcm(context = "document", count = "frequency")

library(quanteda)
a<-"Before winter, we generally see a spike in snow blower sales,
the warm forecast will sure reduce sales.."
b<-"The temperature forecast for the coming months is high, 
this will impact snow removal business"

abc<-c(a,b)
abc_token <- tokens(abc,  
                    remove_numbers = TRUE, remove_punct = TRUE,
                    remove_symbols = TRUE)

abc_dfm<-dfm(abc_token)

abc_dfm1<-as.matrix(abc_dfm)
abc_dfm1[1:2,1:12] 


tcm<-fcm(abc_dfm, context = "document", count = "frequency")
tcm<-as.matrix(tcm)
tcm[1:12,1:12]


suppressMessages(library(tidyverse))
set.seed(1234)
hotel_raw <- read_csv("Data/hotel-reviews.csv")
hotel_raw<-hotel_raw[sample(nrow(hotel_raw), 100), ]# randomly select 100 rows
hotel_raw_token2 <- tokens(hotel_raw$Description, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE)


#Create DFM
hotel_raw_dfm<-hotel_raw_token2 %>%
  tokens_remove(stopwords(source = "smart")) %>%
  #tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm()

hotel_tfidf<-t(dfm_tfidf(hotel_raw_dfm, scheme_tf = "prop", scheme_df = "inverse", base = 10))
hotel_tfidf1<-as.matrix(hotel_tfidf)
hotel_tfidf1[1:6,1:6]



library(lsa)
library(LSAfun)
hotel_LSAspace <- lsa(hotel_tfidf, dims=dimcalc_share()) 

dim(hotel_LSAspace$tk)
dim(hotel_LSAspace$dk)
length(hotel_LSAspace$sk) # Because the $sk matrix 
#only has values on the diagonal, R stores it as a numeric vector.

hotel_LSAspace$tk[1:5,1:5]
hotel_LSAspace$dk[1:5,1:5]
hotel_LSAspace$sk[1:10] 

tk2 = t(hotel_LSAspace$sk * t(hotel_LSAspace$tk))
tk2[1:10,1:3]

dk2 = t(hotel_LSAspace$sk * t(hotel_LSAspace$dk))
dk2[1:10,1:3]


plot(tk2[,1], y= tk2[,2], col="red", cex=.50, main="TK Plot")
text(tk2[,1], y= tk2[,2], labels=rownames(tk2) , cex=.70)
# This can be done with the documents too. The added parameter cex determines text size.
plot(dk2[,1], y= dk2[,2], col="blue", pch="+", main="DK Plot")
text(dk2[,1], y= dk2[,2], labels=rownames(dk2), cex=.70)

# Create a cosine similarity between two Terms
myCo <- costring('nice','hotel', tvectors= tk2)
myCo
myCo1 <- costring('hotel','view', tvectors= tk2)
myCo1

myTerms2 <- rownames(tk2)
myCosineSpace2 <- multicos(myTerms2, tvectors=tk2)
#breakdown=TRUE forces data into lower case
myCosineSpace2[1:7,1:7]

#write.csv(myCosineSpace2, file="termCosineResults.csv")#save file for further analysis

myDocs2 <- rownames(dk2)
myCosineSpace3 <- multicos(myDocs2, tvectors=dk2)
myCosineSpace3[1:6,1:6]


neighbors("text5", n=10, tvectors = dk2)
neighbors("food", n=10, tvectors = tk2)


plot_neighbors("location", n=5, tvectors= tk2)
