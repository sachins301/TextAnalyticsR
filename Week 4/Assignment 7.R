library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(lsa)
library(LSAfun)
library(tibble)
reviews <- tibble(
  product = rep(c("Product_A", "Product_B", "Product_C", "Product_D", "Product_E"), each = 20),
  review = sample(c(
    "This product is amazing, it's incredibly durable and high-quality.",
    "I found this product to be only decent, could be better in terms of quality.",
    "This is a very affordable product and quite reliable, but it lacks some features I would like.",
    "This product is expensive, but I think it's worth the price because of the excellent features it has.",
    "This product is reasonably priced and has most of the features I need, I recommend it."
  ), 100, replace = TRUE)
)

dfm <- tokens(reviews$review) %>%
  dfm()
lsa_space <- lsa(t(dfm), dims = 2)
lsa_space2 <-textmodel_lsa(dfm, nd = 2)

lsa_space

lsa_space2

lsa_space3 <- lsa(dfm, dims = 2)

lsa_scores <- as.data.frame(lsa_space$tk[, 1:2])

doc_scores <- as.data.frame(lsa_space2$docs)

lsa_scores

doc_scores

lsa_scores <- as.data.frame(lsa_space$dk[, 1:2]) 

clusters <- kmeans(lsa_scores, centers = 3)
reviews$cluster <- clusters$cluster
head(reviews)


text_df <- "Embracing the Shift: The Paramount Importance of Digital Marketing in Today’s Business Landscape

In today's volatile business climate, companies face a multitude of challenges. One issue, in particular, that stands out is how businesses can effectively harness the power of digital marketing. With the rise of e-commerce, proliferation of social media, and rapid technology advancements, the significance of digital marketing has soared to paramount importance, making it a critical success factor in today's business landscape.

The digital revolution has significantly altered how consumers make purchasing decisions. Gone are the days when the consumers’ journey was a simple linear path. Today, the journey is complex and non-linear, filled with multiple touchpoints in both the physical and digital worlds. Customers today are more informed, demanding, and expect personalized experiences. Hence, businesses need to shift their traditional marketing strategies to be more customer-centric and adaptive to these digital changes.

The advent of big data analytics has amplified the importance of this shift. Businesses now have access to an unprecedented amount of information about their customers. In the face of this data deluge, companies that fail to leverage this valuable asset are missing out on significant opportunities. By harnessing big data, businesses can gain insights into customer behavior, preferences, and trends, allowing them to tailor their marketing strategies and create highly targeted campaigns.

However, the challenge doesn't stop there. Implementing digital marketing strategies isn't merely about possessing vast amounts of data. Businesses must also have the capacity to analyze and interpret this data, which calls for investment in technology and human capital. Data literacy, the ability to derive meaningful insights from data, is now an essential skill for marketers. On the technology front, companies need to invest in advanced data analytics tools and AI-driven technologies that can deliver real-time insights and automate tasks, freeing marketers to focus on strategic decision-making.

Yet, even the best digital marketing strategies can fall short without a robust online presence. Businesses must ensure their brand is visible and engaging across various online platforms, whether it's their website, social media, or e-commerce platforms. Consumers often initiate their purchasing journey with online research, and a lack of online presence could be detrimental, leading to lost opportunities.

Another critical aspect of digital marketing that cannot be overlooked is maintaining a strong brand reputation online. In the digital age, a company's reputation can be bolstered or tarnished in a matter of minutes. Negative reviews, poor customer service, or a public relations mishap can quickly spread across social media and other online platforms, potentially causing significant damage. Hence, businesses need to invest in online reputation management, monitoring their online presence and promptly addressing any potential issues.

In conclusion, the digital marketing shift is a pivotal business issue that companies can no longer afford to ignore. The success of businesses in this era will be largely determined by their ability to adapt to these changes and incorporate effective digital marketing strategies. Businesses that recognize the paramount importance of digital marketing and proactively work towards mastering it are the ones most likely to thrive in the future."

library(LSAfun)
genericSummary(text_df, k = 3)
