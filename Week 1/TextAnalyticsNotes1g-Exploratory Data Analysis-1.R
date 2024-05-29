getwd()
setwd("/Users/u1452118/Documents/OneDrive - University of Utah/Sem3/MKTG6640 TA/Week 1/")
# Load up the .CSV data and explore.
library(tidyverse)
hotel_raw <- read_csv("hotel-reviews.csv")
glimpse(hotel_raw)



sum(!complete.cases(hotel_raw)) # checking the number of incomplete cases


hotel_raw$Is_Response <- as.factor(hotel_raw$Is_Response)
hotel_raw$Device_Used <- as.factor(hotel_raw$Device_Used)
hotel_raw$Browser_Used <- as.factor(hotel_raw$Browser_Used)


hotel_raw <- hotel_raw %>%
  mutate(hotel_name = case_when(str_detect(Description, "Hilton")~ "Hilton",
                           str_detect(Description, "Hyatt")~ "Hyatt",
                           str_detect(Description, "Marriott")~ "Marriott"))
hotel_sub <- hotel_raw %>% filter(hotel_name=="Hilton"|hotel_name=="Hyatt"|hotel_name=="Marriott")


hotel_sub %>%
count(Is_Response)%>%
  mutate(freq=n/sum(n))

hotel_sub %>%
    group_by(hotel_name) %>% 
count(Is_Response)%>%
  mutate(freq=n/sum(n))

hotel_sub<-hotel_sub %>%
  mutate(ReviewLength=nchar(Description)) 

library(quanteda)
data_corpus_hotelsub <- corpus(hotel_sub, text_field = "Description")# for subsequent analysis

hotel_sub%>%
  group_by( hotel_name, Device_Used) %>%
  summarise(AvLength=mean(ReviewLength))

library(ggplot2)

ggplot(hotel_sub, aes(x = ReviewLength, fill = Is_Response)) +
  geom_histogram(binwidth = 5) +  facet_wrap(~ hotel_name)+
  labs(y = "Review Count", x = "Length of Review",
       title = "Distribution of Review Lengths with Class Labels")

library(quanteda)
hotel_sub_tokens <- quanteda::tokens(hotel_sub$Description, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE)

#Create DFM
hotel_sub_dfm<-hotel_sub_tokens %>%
  tokens_remove(stopwords(source = "smart")) %>%
  tokens_wordstem() %>%
  tokens_tolower() %>%
  dfm()

dim(hotel_sub_dfm)
head(hotel_sub_dfm, n = 5)

hilt <- hotel_sub_tokens %>% 
    kwic(pattern = "Hilton.*", window = 5, valuetype = "regex")
head(hilt)


kwic_hiltonhonors <- quanteda::kwic(x = hotel_sub_tokens, 
                          pattern = quanteda::phrase("Hilton Honors"),
                          window = 5) 
head(kwic_hiltonhonors)

dfmat2 <- corpus_subset(data_corpus_hotelsub, hotel_name == "Hilton") %>%
tokens(remove_punct = TRUE) %>%
tokens_remove(stopwords("en")) %>%
dfm()

library(quanteda.textstats)
tstat1 <- textstat_frequency(dfmat2, groups =hotel_name)
head(tstat1, 10)

library(quanteda.textplots)
textplot_wordcloud(
  hotel_sub_dfm,
  min_size = 0.5,
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
  comparison = FALSE
)

hotel_raw1<-corpus(hotel_sub, text_field="Description")
toks_hotel<-quanteda::tokens(hotel_raw1, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english"))

dfmat2 <- dfm(toks_hotel) %>% dfm_remove(stopwords("english")) %>%  
              dfm_group(groups = Is_Response) %>%
              dfm_trim(min_termfreq = 3)

library(quanteda.textplots)
textplot_wordcloud(dfmat2, comparison = TRUE, max_words = 100,min_size = 0.5,
  max_size = 4,
  min_count = 3,
  color = c("blue", "red"))


library(quanteda.textstats)
tstat1 <- textstat_keyness(dfmat2, target = "happy")
textplot_keyness(tstat1, margin = 0.2, n = 10)

freq_weight <- quanteda.textstats::textstat_frequency(dfmat2, 
                                                      n = 5,
                                                      groups = dfmat2$Is_Response)
freq_weight

ggplot(freq_weight, aes(nrow(freq_weight):1, frequency)) +
     geom_point() +
     facet_wrap(~ group, scales = "free") +
     coord_flip() +
     scale_x_continuous(breaks = nrow(freq_weight):1,
                        labels = freq_weight$feature) +
     labs(x = NULL, y = "Relative frequency")

hotel_ngrams<-toks_hotel %>%
  tokens_ngrams(n=2)
hotel_ngrams_dfm<-dfm(hotel_ngrams)
tstat1 <- textstat_frequency(hotel_ngrams_dfm)
head(tstat1, 10)

text_coll <- textstat_collocations(toks_hotel, size = 2, min_count = 20)
text_coll %>% arrange(across(starts_with("count"), desc)) %>% head()

cust_bigram<- tokens_compound(hotel_ngrams, phrase("room*"))
cust_bigram<- tokens_select(cust_bigram, phrase("room_*"))

cust_bigram<-dfm(cust_bigram)
tstat1 <- textstat_frequency(cust_bigram)
head(tstat1, 10)
# You can visualize it as a word cloud as well
# textplot_wordcloud(cust_bigram, min_size = 0.5,   max_size = 4,
#   min_count = 3,   max_words = 200,   color = "darkblue",   font = NULL,
#   adjust = 0, rotation = 0.1, random_order = FALSE,   random_color = FALSE,
#   ordered_color = FALSE,   labelcolor = "gray20",   labelsize = 1.5,
#   labeloffset = 0,   fixed_aspect = TRUE,   comparison = FALSE)
