# Load necessary libraries
library(wordVectors)
library(dplyr)
library(text2vec)
library(ggplot2)

# Load your pre-trained word vectors
model = read.vectors("Data/IMDB_vectors.bin")

# Example movie data with real-life movie names and genres
movie_data <- data.frame(
  title = c("Inception", "The Dark Knight", "Forrest Gump", "Pulp Fiction", "The Matrix", 
            "Titanic", "The Avengers", "Get Out", "La La Land", "Interstellar", "Gladiator", 
            "Schindler's List", "The Godfather", "The Shawshank Redemption", "The Lion King", 
            "Jurassic Park", "The Silence of the Lambs", "Star Wars", "Back to the Future", 
            "The Terminator", "Goodfellas", "Braveheart", "The Sixth Sense", "Avatar", 
            "Fight Club", "The Departed", "The Prestige", "The Social Network", 
            "Mad Max: Fury Road", 
            "Guardians of the Galaxy"),
  genres = c("sci-fi, thriller", 
             "action, thriller", 
             "drama, romance", 
             "crime, drama", 
             "sci-fi, action",
             "drama, romance",
             "action, sci-fi",
             "horror, thriller",
             "romance, musical",
             "sci-fi, drama",
             "action, drama", 
             "drama, history", 
             "crime, drama", 
             "drama, crime", 
             "animation, adventure", 
             "sci-fi, adventure",
             "crime, thriller",
             "sci-fi, adventure", 
             "sci-fi, comedy", 
             "sci-fi, action",
             "crime, drama",
             "action, drama",
             "horror, mystery", 
             "sci-fi, adventure", 
             "drama, thriller",
             "crime, thriller",
             "drama, mystery", 
             "drama, biography",
             "action, adventure",
             "action, sci-fi")
)
# Function to get embeddings for a list of genres
get_genre_embedding <- function(genres, model) {
  genre_list <- unlist(strsplit(genres, ",\\s*"))
  genre_list <- tolower(genre_list)
  genre_list <- genre_list[genre_list %in% rownames(model)]
  if (length(genre_list) == 0) return(NULL)
  colMeans(model[[genre_list, average = FALSE]])
}

# Calculate embeddings for each movie based on genres
movie_data$embedding <- lapply(movie_data$genres, get_genre_embedding, model)

# Filter out movies with no valid embeddings
movie_data <- movie_data[!sapply(movie_data$embedding, is.null), ]
embeddings <- do.call(rbind, movie_data$embedding)

# Function to calculate cosine similarity using text2vec
calculate_cosine_similarity <- function(a, b) {
  return(text2vec::sim2(a, b, method = "cosine", norm = "l2"))
}

# Recommendation function
recommend_movies <- function(movie_title, movie_data, top_n = 3) {
  # Find the embedding of the input movie
  movie_index <- which(movie_data$title == movie_title)
  if (length(movie_index) == 0) {
    stop("Movie not found in the dataset.")
  }
  movie_embedding <- matrix(movie_data$embedding[[movie_index]], nrow = 1)
  
  # Calculate cosine similarities
  similarities <- calculate_cosine_similarity(embeddings, movie_embedding)[, 1]
  
  # Exclude the input movie from recommendations
  similarities[movie_index] <- -Inf
  
  # Get the indices of the top_n most similar movies
  top_indices <- order(similarities, decreasing = TRUE)[1:top_n]
  
  # Return the titles and genres of the recommended movies
  recommendations <- movie_data[top_indices, c("title", "genres")]
  return(recommendations)
}

# Example: Recommend movies similar to "The Terminator"
recommendations <- recommend_movies("The Terminator", movie_data)
print(recommendations)
