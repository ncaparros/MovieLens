##############################################
# Script related to the Movie Lens Assessment.
# First part from Edx.org
# Parts two and three by Nina Caparros
##############################################

# ---------------------------------------------------------------------------------------------------------------
# The first part of this script is provided by HarvardX in the Capstone course


################################
# Create edx set, validation set
################################



if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# ---------------------------------------------------------------------------------------------------------------
# This part is relative to the Movie Lens Dataset Quizz


#Q1. Number of rows and columns in the dataset

nrow(edx)
ncol(edx)


#Q2. Number of ratings of 0, then 3

edx %>% filter(rating==0) %>% nrow()
edx %>% filter(rating==3) %>% nrow()


#Q3. and Q4. Number of different movies, then users

edx %>% group_by(movieId) %>% summarize() %>% nrow()
edx %>% group_by(userId) %>% summarize() %>% nrow()


#Q5. Number of movie ratings in the genres Drama, Comedy, Thriller and Romance

edx %>% filter(genres %like% "Drama") %>% nrow()
edx %>% filter(genres %like% "Comedy") %>% nrow()
edx %>% filter(genres %like% "Thriller") %>% nrow()
edx %>% filter(genres %like% "Romance") %>% nrow()


#Q6. The movies ordered by number of ratings (from most rated to least rated)

edx %>% group_by(movieId) %>% summarize(title = title[1], nrate=n()) %>% arrange(desc(nrate))


#Q7. The ratings ordered by number of votes (from most given to least)

edx %>% group_by(rating) %>% summarize(title = title[1], nrating=n()) %>% arrange(desc(nrating))


#Q8. The number of ratings (half star rating, then whole star ratings)

edx %>% filter(rating %in% c(0.5,1.5,2.5,3.5,4.5)) %>% nrow()
edx %>% filter(rating %in% c(0,1,2,3,4,5)) %>% nrow()

# ---------------------------------------------------------------------------------------------------------------
# This part is relative to the Movie Lens Assessment

options("scipen"=100)
top10_genres_by_nrating <- edx %>% 
  group_by(genres) %>% 
  summarize(n=n())%>% 
  top_n(10,n) %>% 
  arrange(desc(n)) %>% 
  ggplot() + geom_bar(aes(x=reorder(genres,-n), y=n), fill = "#FF6666", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Number of ratings") + xlab("Genres (top 10)")
 