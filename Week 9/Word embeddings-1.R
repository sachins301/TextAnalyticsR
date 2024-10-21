
## if (!require(wordVectors)) {
##     if (!(require(devtools))) {
##         install.packages("devtools")
##     }
##     devtools::install_github("bmschmidt/wordVectors")
## }

library(wordVectors)
library(dplyr)
## 
## prep_word2vec(origin="Data/IMDB.txt",destination="Data/IMDB1.txt",
##               lowercase=T,bundle_ngrams=2)

#model = train_word2vec("IMDB1.txt","IMDB_vectors.bin",
#vectors=200,threads=4,window=12,iter=5,negative_samples=0)

model = read.vectors("Data/IMDB_vectors.bin")

model %>% closest_to("connery")# since the embeddings are in lower case,
# our query should be in lower case too

model %>% closest_to(~"batman"+"villain")

model %>% closest_to(~"batman" - "villain")

model %>% closest_to(~ "hero" - "he" + "she")

model[[c("heroine","woman","man","he","she","hero"), average=F]] %>% 
  plot(method="pca")

top_evaluative_words = model %>% 
   closest_to(~ "james"+"bond",n=75)
villany = model %>% 
  closest_to(~ "bad",n=Inf) # 'n=Inf' returns the full list
hero1 = model %>% 
  closest_to(~ "good", n=Inf)

library(ggplot2)
library(dplyr)
top_evaluative_words %>%
  inner_join(villany) %>%
  inner_join(hero1) %>%
  ggplot() + 
  geom_text(aes(x=`similarity to "good"`,
                y=`similarity to "bad"`,
                label=word))

some_chr = closest_to(model,model[[c("batman","robin")]],50)
charac = model[[some_chr$word,average=F]]
plot(charac,method="pca")

set.seed(10)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 40)

sapply(sample(1:centers,10),function(n) {
    names(clustering$cluster[clustering$cluster==n][1:10])
})


cast = model[[c("hero","villain"),average=F]]
# model[1:3000,] here restricts to the 3000 most common words in the set.
hero_and_villain = model[1:3000,] %>% cosineSimilarity(cast)
# Filter to the top 20 sweet or salty.
hero_and_villain = hero_and_villain[
  rank(-hero_and_villain[,1])<20 |
  rank(-hero_and_villain[,2])<20,
  ]
plot(hero_and_villain,type='n')
text(hero_and_villain,labels=rownames(hero_and_villain))

plot(model[1:500,],perplexity=50, method="tsne")
# A low perplexity value means that t-SNE focuses more on local structure (very close neighbors).
#A high perplexity value means that t-SNE tries to capture broader, more global structures in the data.
#Typical values for perplexity range from 5 to 50.
