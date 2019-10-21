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

edx %>% 
  filter(rating==0) %>% 
  nrow()

edx %>% 
  filter(rating==3) %>% 
  nrow()


#Q3. and Q4. Number of different movies, then users

edx %>% 
  group_by(movieId) %>% 
  summarize() %>% 
  nrow()
edx %>% 
  group_by(userId) %>% 
  summarize() %>% 
  nrow()


#Q5. Number of movie ratings in the genres Drama, Comedy, Thriller and Romance

edx %>% 
  filter(genres %like% "Drama") %>% 
  nrow()

edx %>% 
  filter(genres %like% "Comedy") %>% 
  nrow()
edx %>% 
  filter(genres %like% "Thriller") %>% 
  nrow()
edx %>% 
  filter(genres %like% "Romance") %>% 
  nrow()


#Q6. The movies ordered by number of ratings (from most rated to least rated)

edx %>% 
  group_by(movieId) %>% 
  summarize(title = title[1], 
            nrate=n()) %>% 
  arrange(desc(nrate))


#Q7. The ratings ordered by number of votes (from most given to least)

edx %>% 
  group_by(rating) %>% 
  summarize(title = title[1], 
            nrating=n()) %>% 
  arrange(desc(nrating))


#Q8. The number of ratings (half star rating, then whole star ratings)

edx %>% 
  filter(rating %in% c(0.5,1.5,2.5,3.5,4.5)) %>% 
  nrow()

edx %>% 
  filter(rating %in% c(0,1,2,3,4,5)) %>% 
  nrow()

# ---------------------------------------------------------------------------------------------------------------
# This part is relative to the Movie Lens Assessment

# Packages needed to run the code
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

# Display whole integers, not scientific notation
options("scipen"=100)

#----------------------------------------------------------------------------------------------------------------
#Is there a correlation between the number of ratings for a movie and the value of the rating ?

# Movie ratings analysis : top 10 reviewed movies
dataByMovie <- edx %>% 
  group_by(movieId) %>% 
  summarize(n=n()) %>%
  top_n(10,n)

# Add total ratings count to the top 10 reviewed movies
completeDataByMovie <- left_join(edx %>%
                                   filter(movieId %in% dataByMovie$movieId),
                                 dataByMovie,
                                 by="movieId")

#Initialize the plot
plotByMovie <- ggplot()

#Bar chart of the 10 most rated movies (x) versus the number of ratings for each movie (y)
rating_by_movie <- plotByMovie  + 
  geom_bar(data= complete_data %>%
             group_by(movieId),
           aes(x=reorder(title,-n),
               y=n),
           stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Movies (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

#Boxplot of the ratings for the 10 most rated movies
avg_rating_by_movie <- plotByMovie + 
  geom_boxplot(
    data=completeDataByMovie
    ,aes(
      x=reorder(title,-n),
      y=rating)
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Movies (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

#Ploting the barchart and the boxplot side by side
ggarrange(rating_by_movie,avg_rating_by_movie)

#----------------------------------------------------------------------------------------------------------------
#Is there a correlation between the number of ratings by user and the value of the rating ?

#User ratings analysis : 10 most raters
dataByUser <- edx %>% 
  group_by(userId) %>% 
  summarize(n=n()) %>%
  top_n(10,n)

#Add total ratings count to the top 10 raters
completeDataByUser <- left_join(edx %>% 
                             filter(userId %in% dataByUser$userId),
                           dataByUser, 
                           by="userId")

#Initialize the plot
userPlot <- ggplot()


userRatingsBarPlot <- userPlot  + 
  geom_bar(data=
             complete_data %>% group_by(userId),
           aes(x = reorder(userId,-n),
               y = n),
           stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Users (top 10 raters)") + 
  coord_cartesian(xlim =c(1, 10))

UserAvgRatingBoxplot <- userPlot + 
  geom_boxplot(data=complete_data
    ,aes(x=reorder(userId,-n),
         y=rating)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Users (top 10 raters)") + 
  coord_cartesian(xlim =c(1, 10))

ggarrange(userRatingsBarPlot,UserAvgRatingBoxplot)

#----------------------------------------------------------------------------------------------------------------
#Is there a correlation between the number of ratings by user and the value of the rating ?


dataByGenre <- edx %>% 
  group_by(genres) %>% 
  summarize(n=n()) %>%
  top_n(10,n)

completeDataByGenre <- left_join(edx %>% 
                             filter(genres %in% dataByGenre$genres),
                           dataByGenre, 
                           by="genres")
genrePlot <- ggplot()

genreRatingsBarPlot <- genrePlot  + 
  geom_bar(data=dataByGenre,
           aes(x=reorder(genres,-n), 
               y=n),
           stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Genres (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

UserAvgRatingBoxplot <- genrePlot + 
  geom_boxplot(
    data=completeDataByGenre,
    aes(x=reorder(genres,-n),
        y=rating)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Genres (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

ggarrange(genreRatingsBarPlot,UserAvgRatingBoxplot)

#----------------------------------------------------------------------------------------------------------------
#Extracting date and time of the rating

edx <- edx %>% 
  mutate(
    date = format(as_datetime(timestamp), 
                  format="%Y-%m-%d"), 
    time=format(as_datetime(timestamp), 
                format="%H:%M"),
    year=year(date)) %>%
  select(-timestamp)

head(edx)

validation <- validation %>% 
  mutate(
    date = format(as_datetime(timestamp), 
                  format="%Y-%m-%d"), 
    time=format(as_datetime(timestamp), 
                format="%H:%M")) %>% 
  select(-timestamp)

head(validation)

#----------------------------------------------------------------------------------------------------------------
#Extracting year of release

edx <- edx %>% 
  mutate(
    yearOfRelease = as.numeric(
      substring(
        title, 
        nchar(title)-4,
        nchar(title)-1)),
    title = substring(title, 1,nchar(title)-7))


validation <- validation %>% 
  mutate(
    yearOfRelease = as.numeric(substring(title, 
                                         nchar(title)-4,
                                         nchar(title)-1)),
    title = substring(title, 1,nchar(title)-7))

#----------------------------------------------------------------------------------------------------------------
#Is there a correlation between the number of ratings by year of release and the value of the rating ?

dataYearOfRelease <- edx %>% 
  group_by(yearOfRelease) %>% 
  summarize(n=n()) %>%
  top_n(10,n)

completeDataByYearOfRelease <- left_join(edx %>% 
                             filter(yearOfRelease %in% dataYearOfRelease$yearOfRelease),
                           dataYearOfRelease,
                           by="yearOfRelease")

plotYearOfRelease <- ggplot()

yearOfReleaseRatingsBarPlot <- plotYearOfRelease  + 
  geom_bar(data=dataYearOfRelease,
           aes(x=reorder(yearOfRelease,-n), 
               y=n),
           stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Years (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

YearOfReleaseAvgRatingBoxplot <- plotYearOfRelease + 
  geom_boxplot(
    data=completeDataByYearOfRelease
    ,aes(x=reorder(yearOfRelease,-n),
         y=rating)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Years (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

ggarrange(yearOfReleaseRatingsBarPlot,YearOfReleaseAvgRatingBoxplot)

#----------------------------------------------------------------------------------------------------------------
#Is there a correlation between the number of ratings by year and the value of the rating ?

#year of rating

dataYear <- edx %>% 
  group_by(year) %>% 
  summarize(n=n())

completeDataYear <- left_join(edx %>% 
                             filter(year %in% dataYear$year),
                             dataYear, 
                           by="year")
plotYear <- ggplot()
yearRatingsBarPlot <- plotYear  + 
  geom_bar(data=dataYear,
           aes(x=year, 
               y=n, 
               group=year),
           stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Years (top 10 rated)")

YearAvgRatingBoxplot <- plotYear + 
  geom_boxplot(data=completeDataYear,
               aes(x=year,
                   y=rating, 
                   group=year)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Years (top 10 rated)")

ggarrange(yearRatingsBarPlot,YearAvgRatingBoxplot)

#-------------------------------------------------------------------------------------------------
#genre analysis

#Extracting primary genres
primarygenres <- edx %>% 
  filter(!str_detect(genres,"[|]")) %>% 
  pull(genres) %>% 
  unique()

genreDatas <- sapply(c(1:length(primarygenres)), function(index){
  dataByGenre = edx %>% 
    filter(str_detect(genres, primarygenres[index])) %>% 
    summarize(avg_rating=mean(rating), 
              nrating=n())
})

colnames(genreDatas) = primarygenres

#-------------------------------------------------------------------------------------------------
#Is there a correlation between the number of genres and the value of the rating ?

#Count genres on the edx and validation set
edx <- edx %>% 
  mutate(ngenres = str_count(genres,"[|]") + 1)

validation <- validation %>% 
  mutate(ngenres = str_count(genres,"[|]") + 1)

#Analyse the number of genres : 10 number of genre the most rated
dataNGenres <- edx %>% 
  group_by(ngenres) %>% 
  summarize(n=n()) %>%
  top_n(10,n)
completeDataNGenres <- left_join(edx %>% 
                             filter(ngenres %in% dataNGenres$ngenres),
                           dataNGenres, 
                           by="ngenres")
plotNGenres <- ggplot()

nGenresBarPlot <- plotNGenres  + 
  geom_bar(data=dataNGenres,
           aes(x=ngenres, 
               y=n, 
               group=ngenres),
           stat="identity") +
  theme(axis.text.x = element_text(hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Number of genres (top 10 rated)")

nGenresAvgRatingBoxplot <- plotNGenres + 
  geom_boxplot(
    data=completeDataNGenres,
    aes(x=ngenres,
        y=rating, 
        group=ngenres)) +
  theme(axis.text.x = element_text(hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Number of genres (top 10 rated)")

ggarrange(nGenresBarPlot,nGenresAvgRatingBoxplot)

#-------------------------------------------------------------------------------------------------
#Creation of a train set and a test set from edx dataset for cross-validation

test_edx_index <-  createDataPartition(y = edx$rating, 
                                       times = 1, 
                                       p = 0.1, 
                                       list = FALSE)
train_edx <- edx[-test_edx_index,]

temp_test <- edx[test_edx_index,]

# Make sure userId and movieId in validation set are also in edx set

test_edx <- temp_test %>% 
  semi_join(train_edx, 
            by = "movieId") %>%
  semi_join(train_edx, 
            by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp_test, 
                     test_edx)
train_edx <- rbind(train_edx, 
                   removed)


edx_movies <- edx %>% 
  group_by(movieId) %>% 
  summarize(
    avg_rating=mean(rating), 
    sd_rating=sd(rating), 
    nrating=n()
    )

left_join(edx, 
          edx_movies, 
          by="movieId")

mu <- mean(edx$rating) 

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#determining best lambda
lambdasRough <- seq(0,10,1)

mu <- mean(train_edx$rating)

functionRmses <- function(lambda){
  
  
  #movie effect
  b_i <- train_edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda)) 
  
  #user effect
  b_u <- train_edx %>% 
    left_join(b_i, 
              by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  #genre effect
  b_g <- train_edx %>% 
    left_join(b_i, 
              by="movieId") %>%
    left_join(b_u, 
              by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+lambda))
  
  #year of release effect
  b_y <- train_edx %>% 
    left_join(b_i, 
              by="movieId") %>%
    left_join(b_u, 
              by="userId") %>%
    left_join(b_g, 
              by="genres") %>%
    group_by(yearOfRelease) %>%
    summarize(b_y = sum(rating - b_u - b_i - b_g - mu)/(n()+lambda))
  
  predicted_ratings <- 
    test_edx %>% 
    left_join(b_i, 
              by = "movieId") %>%
    left_join(b_u, 
              by = "userId") %>%
    left_join(b_g, 
              by="genres") %>%
    left_join(b_y, 
              by="yearOfRelease") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
    pull(pred)
  
  
  return(RMSE(predicted_ratings, test_edx$rating))
  
}

rmsesRough <- sapply(lambdasRough, functionRmses)

qplot(lambdas,rmses)

lambdaFine= seq(lambdas[which.min(rmses)] -1, lambdas[which.min(rmses)] + 1, 0.25)

rmses <- sapply(lambdasFine, functionRmses)

mu <- mean(train_edx$rating)

#movie effect
b_i <- train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

#user effect
b_u <- train_edx %>% 
  left_join(b_i, 
            by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

#genre effect
b_g <- train_edx %>% 
  left_join(b_i, 
            by="movieId") %>%
  left_join(b_u, 
            by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+lambda))

#year of release effect
b_y <- train_edx %>% 
  left_join(b_i, 
            by="movieId") %>%
  left_join(b_u, 
            by="userId") %>%
  left_join(b_g, 
            by="genres") %>%
  group_by(yearOfRelease) %>%
  summarize(b_y = sum(rating - b_u - b_i - b_g - mu)/(n()+lambda))

b_ng <- train_edx %>% 
  left_join(b_i, 
            by="movieId") %>%
  left_join(b_u, 
            by="userId") %>%
  left_join(b_g, 
            by="genres") %>%
  left_join(b_y, 
            by="yearOfRelease") %>%
  group_by(ngenres) %>%
  summarize(b_ng = sum(rating - b_u - b_i- b_y - b_g - mu)/(n()+lambda))

b_yn <- train_edx %>% 
  left_join(b_i, 
            by="movieId") %>%
  left_join(b_u, 
            by="userId") %>%
  left_join(b_g, 
            by="genres") %>%
  left_join(b_y, 
            by="yearOfRelease") %>%
  left_join(b_ng, 
            by="ngenres") %>%
  group_by(year) %>%
  summarize(b_yn = sum(rating - b_u - b_i- b_y - b_g - b_ng- mu)/(n()+lambda))

predicted_ratings <- 
  test_edx %>% 
  left_join(b_i, 
            by = "movieId") %>%
  left_join(b_u, 
            by = "userId") %>%
  left_join(b_g, 
            by="genres") %>%
  left_join(b_y, 
            by="yearOfRelease") %>%
  left_join(b_ng, 
            by="ngenres") %>%
  left_join(b_yn, 
            by="year") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

RMSE(predicted_ratings, test_edx$rating)
