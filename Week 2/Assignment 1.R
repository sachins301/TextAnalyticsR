# Loading the necessary libraries
library(rvest)
library(polite)
library(dplyr)

# Setting the URL of the page to be scraped
url <- "http://books.toscrape.com/"

# Reading the HTML content of the page
bow(url)
webpage <- read_html(url)
webpage
# Extracting a1
a1 <- webpage %>%
  html_nodes(".product_pod h3 a") %>%
  html_attr("title")

# Extracting b1
b1 <- webpage %>%
  html_nodes(".product_pod .price_color") %>%
  html_text() 

# Creating a data frame 
book_data <- data.frame(Name = a1, Price = b1)

a1

webpage %>%
  html_nodes(".product_pod h3 a")

b1

b1 <- stringr::str_remove(b1, "£") %>% as.numeric()

b1


library(ralger)
library(dplyr)
library(stringr)
library(polite)

# Setting the URL of the page to be scraped
my_link <- "https://www.boxofficemojo.com/chart/top_lifetime_gross/?ref_=bo_lnav_hm_shrt"
bow(my_link)
# Defining the nodes to be scraped
my_nodes <- c(
  ".mojo-field-type-title", # The title
  ".mojo-field-type-money", # Lifetime Gross
  ".mojo-field-type-year" # Year
)

# Defining the names of the columns in the output data frame
names <- c("title", "Lifetime_Gross", "Year") # respect the nodes order

# Scraping the data
scraped_data <- tidy_scrap(link = my_link, nodes = my_nodes, colnames = names)
scraped_data
scraped_data <- scraped_data[-1,]
scraped_data
scraped_data$Lifetime_Gross <- str_remove_all(scraped_data$Lifetime_Gross,"[\\$,]")
scraped_data
scraped_data$Lifetime_Gross <- as.numeric(scraped_data$Lifetime_Gross)

#what happens question
#scraped_data$Lifetime_Gross <- str_remove_all(scraped_data$Lifetime_Gross,"[\\$]")

scraped_data1 <- scrap(link = my_link, nodes = my_nodes, colnames = names)


