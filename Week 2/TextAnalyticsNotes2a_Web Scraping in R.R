
library(polite)
bow("https://www.rottentomatoes.com")


library(polite)
library(rvest)

wiki_cb <- 
  bow("https://en.wikipedia.org/wiki/Consumer_behaviour") %>%  
  scrape()


wiki_cb # to test if we have properly downloaded the webpage

wikitext<-wiki_cb %>%
html_nodes("p") %>% #from rvest package
html_text()

wikitext[2] # output from p2

wikihead<-wiki_cb %>%
html_nodes("h2") %>% #from rvest package
html_text()

head(wikihead)

ul_wiki <- wiki_cb %>%
        html_nodes("ul") %>%
        html_text()
ul_wiki[2]

section_of_wikipedia<-html_node(wiki_cb, xpath='//*[@id="mw-content-text"]/div/table[3]')
percy_table<-html_table(section_of_wikipedia)
head(percy_table)

results <- 
  wiki_cb  %>%  
  html_table()
head(results[[3]])


body_nodes <- wiki_cb %>% 
 html_nodes('body') %>% 
 html_children() # for looking at nested elements

body_nodes %>% 
 html_children()


all_text<-wiki_cb %>%
html_nodes("div") %>% 
html_text2()

# Clean up the plain text by removing extra spaces, newlines, etc.
clean_text <- gsub("[[:space:]]+", " ", all_text)
clean_text <- gsub("\n+", "\n", clean_text)
clean_text <- trimws(clean_text)

# Print the cleaned up text using the command below. 
cat(clean_text)

library(stringr)
clean_text <- gsub("(f|ht)tps?://\\S+", "", clean_text)


clean_text <- gsub("\\.mw-parser-output[^{]*\\{[^}]*\\}", "", clean_text)


clean_text <- gsub("\\^\\s*", "\n", clean_text) # Add newlines after each reference


top20movies <- 
  bow("https://editorial.rottentomatoes.com/guide/100-best-classic-movies/") %>%  
  scrape()

title_html <- html_nodes(top20movies,'.article_movie_title a')

#Converting the ranking data to text
titletext <- html_text(title_html)

#Let's have a look at the titles
length(titletext)
head( titletext)

rank_html <- html_nodes(top20movies,'.countdown-index')

#Converting the ranking data to text
ranktext <- html_text(rank_html)

#Let's have a look at the rankings
head(ranktext)

library(stringr)
ranktext<-str_replace_all(ranktext, "[^[:alnum:]]", "") # remove special characters
ranktext<-as.numeric(ranktext) # convert to numeric

year_html <- html_nodes(top20movies,'.start-year')

#Converting the ranking data to text
yeartext <- html_text(year_html)

#Let's have a look at the rankings
head(yeartext)

yeartext<-str_replace_all(yeartext, "[^[:alnum:]]", "") # remove special characters
yeartext<-as.numeric(yeartext) # convert to numeric

critic_html <- html_nodes(top20movies,'.critics-consensus')

#Converting the critic's consensus data to text
critictext <- html_text(critic_html)

#Let's have a look at the critic's
head(critictext)

synopsis_html <- html_nodes(top20movies,'.synopsis')

#Converting the ranking data to text
synopsistext <- html_text(synopsis_html)

#Let's have a look at the rankings
head(synopsistext)

cast_html <- html_nodes(top20movies,'.cast')

#Converting the cast data to text
casttext <- html_text(cast_html)

#Let's have a look at the casting
head(casttext)

library(tidyverse)
movies_df<-tibble(Rank = ranktext, Title = titletext, Year= yeartext, Critic= critictext,Synopsis= synopsistext, Cast = casttext)
head(movies_df)
