library(quanteda)
review<- "The service was fine, but the hotel itself
          fell way below my expectations from such a 
          respectable establishment as Hilton."

hotel_sample_token <- quanteda::tokens(review, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)


#Create DFM
hotel_sample_dfm<-hotel_sample_token %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm()

dim(hotel_sample_dfm)
head(hotel_sample_dfm)

jreview<- "The hotel was fine, but the service itself 
           fell way below my expectations from such a
           respectable establishment as Hilton."

hotel_jsample_token <- quanteda::tokens(jreview, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)


#Create DFM
hotel_jsample_dfm<-hotel_jsample_token %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm()

dim(hotel_jsample_dfm)
head(hotel_jsample_dfm)


library(tokenizers)
tokenize_ngrams(review, n = 2, n_min = 2) # generates bigrams
# An n-gram is a contiguous sequence of words 
# containing at least n_min words and at most n words. 


library(quanteda)
library(readtext)# https://readtext.quanteda.io/articles/readtext_vignette.html
library(tidyverse)# For loading bunch of packages such as dplyr, stringi etc.

# Load up the .CSV data.
hotel_raw <- readtext("C:/Users/u0474728/Dropbox/Utah Department Stuff/Teaching/Text Analysis/Summer 2024/Course Materials/RMarkdown/data/hotel-reviews.csv", text_field = "Description")

set.seed(1245654)# for replicability 
hotel_raw <- slice_sample(hotel_raw,n=5000)# take a small sample

my_corpus<-corpus(hotel_raw, text_field = "text")

toks_reviews <- tokens(my_corpus, remove_punct = TRUE) 

tstat_col_caps <-  tokens_select(toks_reviews, pattern = "^[A-Z]", 
                                   valuetype = "regex", 
                                   case_insensitive = FALSE, 
                                   padding = TRUE) %>% 
               quanteda.textstats::textstat_collocations(min_count = 10, size = 2)
head(tstat_col_caps, 10)

hotel_raw_token2 <- quanteda::tokens(hotel_raw$text, 
                           what = "word", 
                           remove_numbers = TRUE, 
                       remove_punct = TRUE,
                       remove_symbols = TRUE)
                      
#More preprocessing
hotel_raw_token2<-hotel_raw_token2 %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() # Note I am not doing tokens_tolower

hotel_raw_dfm<-hotel_raw_token2 %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_tolower %>%   
  dfm()

#Create N Grams with library quanteda
hotel_ngrams<-hotel_raw_token2 %>%
  tokens_ngrams(n=1:2)%>%
    dfm()
dim(hotel_raw_dfm) # The original document feature matrix
dim(hotel_ngrams)


hotel_ngrams<-hotel_raw_token2 %>%
  tokens_ngrams(n=2)%>%
    dfm()

library(quanteda.textplots)# for word clouds
textplot_wordcloud(hotel_ngrams, min_size = 0.5,
  max_size = 4,
  min_count = 3,
  max_words = 200,
  color = "darkblue",
  font = NULL,
  adjust = 0,
  rotation = 0.1,
  random_order = FALSE,
  random_color = FALSE,
  ordered_color = FALSE,
  labelcolor = "gray20",
  labelsize = 1.5,
  labeloffset = 0,
  fixed_aspect = TRUE,
  comparison = FALSE)

hotel_ngrams<-hotel_raw_token2 %>%
  tokens_ngrams(n=2)# Doing this step as tokens_compound works only on tokens

cust_bigram<- tokens_compound(hotel_ngrams, phrase("room*"))
cust_bigram<- tokens_select(cust_bigram, phrase("room_*"))

cust_bigram<-dfm(cust_bigram)

textplot_wordcloud(cust_bigram, min_size = 0.5,   max_size = 4,
  min_count = 3,   max_words = 200,   color = "darkblue",   font = NULL,
  adjust = 0, rotation = 0.1, random_order = FALSE,   random_color = FALSE,
  ordered_color = FALSE,   labelcolor = "gray20",   labelsize = 1.5,
  labeloffset = 0,   fixed_aspect = TRUE,   comparison = FALSE)

library(quanteda.textstats)
library(quanteda.textplots)

text_sentences <- hotel_raw$text %>%
  tolower() %>%
  paste0(collapse= " ") %>%
  stringr::str_split(fixed(".")) %>%
  unlist() %>%
  stringr::str_squish()

text_tokens <- tokens(text_sentences, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english"))
# extract collocations
text_coll <- textstat_collocations(text_tokens, size = 2, min_count = 100)
text_coll[1:10,1:5]

options(stringsAsFactors = FALSE)
library(quanteda)

hotel_corpus <- corpus(hotel_raw$text, docnames = hotel_raw$doc_id)
# original corpus length and its first document
ndoc(hotel_corpus)
substr(as.character(hotel_corpus)[1], 0, 200)
corpus_sentences <- corpus_reshape(hotel_corpus, to = "sentences")

ndoc(corpus_sentences)
as.character(corpus_sentences)[1]

# Preprocessing of the corpus of sentences
corpus_tokens <- corpus_sentences %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() 

# calculate multi-word unit candidates
hotel_collocations <- textstat_collocations(corpus_tokens, min_count = 25)
hotel_collocations <- hotel_collocations[1:250, ]

corpus_tokens <- tokens_compound(corpus_tokens, hotel_collocations)

minimumFrequency <- 10

# Create DTM, prune vocabulary and set binary values for presence/absence of types
binDTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = minimumFrequency) %>% 
  dfm_weight("boolean")

# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTM
as.matrix(coocCounts[192:195, 192:195])

# Read in the source code for the co-occurrence calculation
source("C:/Users/u0474728/Dropbox/Utah Department Stuff/Teaching/Text Analysis/Summer 2024/Course Materials/RMarkdown/Script/Week 1/calculateCoocStatistics.R")
# Definition of a parameter for the representation of the co-occurrences of a concept
numberOfCoocs <- 15
# Determination of the term of which co-competitors are to be measured.
coocTerm <- "marriott"

coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
# Display the numberOfCoocs main terms
print(coocs[1:numberOfCoocs])

resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))


# The structure of the temporary graph object is equal to that of the resultGraph
tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# Fill the data.frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# Entry of the search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs]

# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)

# Iteration over the most significant numberOfCoocs co-occurrences of the search term
for (i in 1:numberOfCoocs){
  
  # Calling up the co-occurrence calculation for term i from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
  
  #print the co-occurrences
  coocs2[1:10]
  
  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  
  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}

# Sample of some examples from resultGraph
resultGraph[sample(nrow(resultGraph), 6), ]

require(igraph)

# set seed for graph plot
set.seed(1)

# Create the graph object as undirected graph
graphNetwork <- graph.data.frame(resultGraph, directed = F)

# Identification of all nodes with less than 2 edges
verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
# These edges are removed from the graph
graphNetwork <- delete.vertices(graphNetwork, verticesToRemove) 

# Assign colors to nodes (search term blue, others orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 

# Set edge colors
E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
# scale significance between 1 and 10 for edge width
E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))

# Set edges with radius
E(graphNetwork)$curved <- 0.15 
# Size the nodes by their degree of networking (scaled between 5 and 15)
V(graphNetwork)$size <- scales::rescale(log(degree(graphNetwork)), to = c(5, 15))

# Define the frame and spacing for the plot
par(mai=c(0,0,1,0)) 

# Final Plot
plot(
  graphNetwork,             
  layout = layout.fruchterman.reingold, # Force Directed Layout 
  main = paste(coocTerm, ' Graph'),
  vertex.label.family = "sans",
  vertex.label.cex = 0.8,
  vertex.shape = "circle",
  vertex.label.dist = 0.5,          # Labels of the nodes moved slightly
  vertex.frame.color = adjustcolor("darkgray", alpha.f = .5),
  vertex.label.color = 'black',     # Color of node names
  vertex.label.font = 2,            # Font of node names
  vertex.label = V(graphNetwork)$name,      # node names
  vertex.label.cex = 1 # font size of node names
)

calculateCoocStatistics <- function(coocTerm, binDTM, measure = "DICE") {
  
  # Ensure Matrix (SparseM} or matrix {base} format
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(binDTM)) {
    binDTM <- sparseMatrix(i=binDTM$i, j=binDTM$j, x=binDTM$v, dims=c(binDTM$nrow, binDTM$ncol), dimnames = dimnames(binDTM))
  }
  
  # Ensure binary DTM
  if (any(binDTM > 1)) {
    binDTM[binDTM > 1] <- 1
  }
  
  # calculate cooccurrence counts
  coocCounts <- t(binDTM) %*% binDTM
  
  # retrieve numbers for statistic calculation
  k <- nrow(binDTM)
  ki <- sum(binDTM[, coocTerm])
  kj <- colSums(binDTM)
  names(kj) <- colnames(binDTM)
  kij <- coocCounts[coocTerm, ]
  
  # calculate statistics
  switch(measure, 
         DICE = {
           dicesig <- 2 * kij / (ki + kj)
           dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
           sig <- dicesig
         },
         LOGLIK = {
           logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
                          + (k - ki - kj + kij) * log(k - ki - kj + kij) 
                          + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
                          - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
           logsig <- logsig[order(logsig, decreasing=T)]
           sig <- logsig    
         },
         MI = {
           mutualInformationSig <- log(k * kij / (ki * kj))
           mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
           sig <- mutualInformationSig    
         },
         {
           sig <- sort(kij, decreasing = TRUE)
         }
        )
  sig <- sig[-match(coocTerm, names(sig))]
  return(sig)
}
