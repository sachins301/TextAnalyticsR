# Set the working directory to the current directory
setwd(getwd())

# Confirm the working directory
print(getwd())


if (!require(wordVectors)) {
  if (!(require(devtools))) {
    install.packages("devtools")
  }
  devtools::install_github("bmschmidt/wordVectors")
}

library(wordVectors)
library(magrittr)

# Assuming the binary file "cookbook_vectors.bin" has already been created
model = read.vectors("cookbook_vectors.bin")

# Find the closest word to "apricot"
closest_words <- model %>% closest_to("apricot")

# Print the results
print(closest_words)


some_fruit = closest_to(model,model[[c("fruit","peach","cherry","nectarine")]],10)
fruity = model[[some_fruit$word,average=F]]
plot(fruity,method="pca")


set.seed(10)
centers = 50
clustering = kmeans(model,centers=centers,iter.max = 40)

sapply(sample(1:centers,10),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

ingredients = c("madeira","chicken","saucepan","carrots")
term_set = lapply(ingredients, 
                  function(ingredient) {
                    nearest_words = model %>% closest_to(model[[ingredient]],20)
                    nearest_words$word
                  }) %>% unlist

subset = model[[term_set,average=F]]

subset %>% closest_to("saucepan")


tastes = model[[c("sweet","sour"),average=F]]

# model[1:3000,] here restricts to the 3000 most common words in the set.
sweet_and_sourness = model[1:3000,] %>% cosineSimilarity(tastes)

# Filter to the top 10 sweet or sour.
sweet_and_sourness = sweet_and_sourness[
  rank(-sweet_and_sourness[,1])<10 |
    rank(-sweet_and_sourness[,2])<10,
]

plot(sweet_and_sourness,type='n')
text(sweet_and_sourness,labels=rownames(sweet_and_sourness))
