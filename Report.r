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
#Aanalysis and effect

#average of ratings
mu_rating = mean(edx$rating)

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
ratingByMovieBarPlot <- completeDataByMovie  + 
  geom_bar(data= dataByMovie %>%
             group_by(movieId),
           aes(x=reorder(title,-n),
               y=n),
           stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Movies (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

#Boxplot of the ratings for the 10 most rated movies
MovieAvgRatingBoxplot <- plotByMovie + 
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
ggarrange(ratingByMovieBarPlot,MovieAvgRatingBoxplot)

#Movie effect
movieAvgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu_rating))

movieAvgs %>% qplot(b_m, geom ="histogram", bins = 10, data = ., color = I("black"))

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
             completeDataByUser %>% group_by(userId),
           aes(x = reorder(userId,-n),
               y = n),
           stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Users (top 10 raters)") + 
  coord_cartesian(xlim =c(1, 10))

UserAvgRatingBoxplot <- userPlot + 
  geom_boxplot(data=completeDataByUser
    ,aes(x=reorder(userId,-n),
         y=rating)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Users (top 10 raters)") + 
  coord_cartesian(xlim =c(1, 10))

ggarrange(userRatingsBarPlot,UserAvgRatingBoxplot)

#user effect
userAvgs <- edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_rating))

userAvgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"))

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

#genre effect
genreAvgs <- edx %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_rating))

genreAvgs %>% qplot(b_g, geom ="histogram", bins = 10, data = ., color = I("black"))

Drama_DramaCrime = dataByGenre %>% 
  arrange(desc(n)) %>% 
  filter(genres %in% c("Drama","Crime|Drama"))

ratioDrama_DramaCrime = Drama_DramaCrime[1,]$n / Drama_DramaCrime[2,]$n
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

#date effect

edx %>% mutate(date2 = week(date)) %>%
  group_by(date2) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date2, rating)) +
  geom_point() +
  geom_smooth()

edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

edx %>%
  group_by(ngenres) %>%
  summarize(rating = mean(rating)) %>%
  mutate(genres = reorder(ngenres, rating)) %>%
  ggplot(aes(ngenres, rating)) +
  geom_point() +
  geom_smooth()

edx %>%
  group_by(year(date)) %>%
  summarize(rating = mean(rating)) %>%
  mutate(genres = reorder(year(date), rating)) %>%
  ggplot(aes(year(date), rating)) +
  geom_point() +
  geom_smooth()

dateAvgs <- edx %>%
  group_by(day(date)) %>%
  summarize(b_d = mean(rating - mu_rating))

dateAvgs %>% qplot(b_d, geom ="histogram", bins = 12, data = ., color = I("black"))


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

# year of release effect
yearOfReleaseAvgs <- edx %>%
  group_by(yearOfRelease) %>%
  summarize(b_yor = mean(rating - mu_rating))

yearOfReleaseAvgs %>% qplot(b_yor, geom ="histogram", bins = 100, data = ., color = I("black"))

 id_max_rated = edx %>% group_by(movieId) %>% summarize(n=n()) %>% filter(n==max(n))
 edx %>% filter(movieId==296 & 
                  !is.na(date)) %>% 
   mutate(ye= year(date)) %>% 
   group_by(ye) %>% 
   summarize(avg=mean(rating), 
             se=sd(rating)) %>% 
   arrange(ye) %>%  ggplot(aes(x = ye, 
                               y = avg, 
                               ymin = avg - 2*se, 
                               ymax = avg + 2*se)) + 
   geom_point() + 
   geom_errorbar()
 
 edx %>% filter(!is.na(date)) %>% 
     mutate(ye= year(date)) %>% 
     group_by(ye) %>% 
     summarize(avg=mean(rating), 
               se=sd(rating)) %>% 
     arrange(ye) %>%  ggplot(aes(x = ye, 
                                 y = avg, 
                                 ymin = avg - 2*se, 
                                 ymax = avg + 2*se)) + 
     geom_point() + 
     geom_errorbar()
 
 edx %>% filter(!is.na(date)) %>% 
     group_by(yearOfRelease) %>% 
     summarize(avg=mean(rating), 
               se=sd(rating)) %>% 
     arrange(yearOfRelease) %>%  ggplot(aes(x = yearOfRelease, 
                                 y = avg, 
                                 ymin = avg - 2*se, 
                                 ymax = avg + 2*se)) + 
     geom_point() + 
     geom_errorbar()
 
 
#-------------------------------------------------------------------------------------------------
#exterior events
 
#2001  Economic Growth and Tax Relief Reconciliation Act 

#26/05/01 Bill to congress
 Bill <- edx %>% filter(date < "2001-06-26" & 
                          date >= "2001-04-26") %>% 
   group_by(date) %>% 
   summarize(n=n(), 
             avg=mean(rating), 
             sd=sd(rating)) %>% 
   arrange(date)
 
 Bill %>% ggplot(aes(x=date,
                     y=avg,
                     ymin=avg-sd, 
                     ymax=avg+sd)) + 
   geom_point() + 
   geom_errorbar() + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
   geom_vline(xintercept = 31, 
              color="red") + 
   geom_hline(yintercept = mean(edx$rating), 
              color="green") +
   ylab("Ratings") + 
   xlab("Date") +
   ggtitle("Average ratings around the passing of the bill")
 
 #07/06/01 Law
 Law <- edx %>% filter(date < "2001-07-07" & 
                          date >= "2001-05-07") %>% 
   group_by(date) %>% 
   summarize(n=n(), 
             avg=mean(rating), 
             sd=sd(rating)) %>% 
   arrange(date)
 
 Law %>% ggplot(aes(x=date,
                     y=avg,
                     ymin=avg-sd, 
                     ymax=avg+sd)) + 
   geom_point() + 
   geom_errorbar() + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
   geom_vline(xintercept = 32, 
              color="red") + 
   geom_hline(yintercept = mean(edx$rating), 
              color="green") +
   ylab("Ratings") + 
   xlab("Date") +
   ggtitle("Average ratings around the passing of the law")

 
 #9/11
 
 NineOneOne <- edx %>% filter(date < "2001-10-11" & 
                         date >= "2001-08-11") %>% 
   group_by(date) %>% 
   summarize(n=n(), 
             avg=mean(rating), 
             sd=sd(rating)) %>% 
   arrange(date)
 
 ratings911 <- NineOneOne %>% ggplot(aes(x=date,
                    y=avg,
                    ymin=avg-sd, 
                    ymax=avg+sd)) + 
   geom_point() + 
   geom_errorbar() + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
   geom_vline(xintercept = 32, 
              color="red") + 
   geom_hline(yintercept = mean(edx$rating), 
              color="green") +
   ylab("Ratings") + 
   xlab("Date") +
   ggtitle("Average ratings around 9/11")
 
 avgRatingByDay <- mean(edx %>% group_by(date) %>% summarize(n=n()) %>% pull(n))
 
 n911 <- NineOneOne %>% ggplot(aes(x=date,
                           y=n)) + 
   geom_point() +  
   geom_line(aes(group=1))+
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
   geom_hline(yintercept = avgRatingByDay, 
              color="green") +
   geom_vline(xintercept = 32, 
              color="red") + 
   ylab("Number of ratings") + 
   xlab("Date") +
   ggtitle("Number of ratings around 9/11")
 
 ggarrange(ratings911, n911, nrow=2)
 
 #2005-08-23 to 31 Hurricane Katrina
 
 Katrina <- edx %>% filter(date < "2005-09-31" & 
                                date >= "2005-07-23") %>% 
   group_by(date) %>% 
   summarize(n=n(), 
             avg=mean(rating), 
             sd=sd(rating)) %>% 
   arrange(date)
 
 ratingsKatrina <- Katrina %>% ggplot(aes(x=date,
                                         y=avg,
                                         ymin=avg-sd, 
                                         ymax=avg+sd)) + 
   geom_point() + 
   geom_errorbar() + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
   geom_vline(xintercept = 32, 
              color="red") + 
   geom_vline(xintercept = 40, 
              color="red") +
   geom_hline(yintercept = mean(edx$rating), 
              color="green") +
   ylab("Ratings") + 
   xlab("Date") +
   ggtitle("Average ratings around Red Dawn")
 
 nRatingsKatrina <- Katrina %>% ggplot(aes(x=date,
                                   y=n)) + 
   geom_point() +  
   geom_line(aes(group=1))+
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
   geom_hline(yintercept = avgRatingByDay, 
              color="green") +
   geom_vline(xintercept = 32, 
              color="red") + 
   geom_vline(xintercept = 40, 
              color="red") +
   ylab("Number of ratings") + 
   xlab("Date") +
   ggtitle("Number of ratings around Red Dawb")
 
 ggarrange(ratingsKatrina, nRatingsKatrina, nrow=2)
  
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

# year of release effect
nGenresAvgs <- edx %>%
  group_by(genres) %>%
  summarize(b_ng = mean(rating - mu_rating))

nGenresAvgs %>% qplot(b_ng, geom ="histogram", bins = 800, data = ., color = I("black"))

edx  %>% group_by(ngenres) %>% 
     summarize(avg=mean(rating), 
              se=sd(rating)) %>% 
     arrange(ngenres) %>%  ggplot(aes(x = ngenres, 
                                 y = avg, 
                                 ymin = avg - 2*se, 
                                 ymax = avg + 2*se)) + 
  geom_point() + 
    geom_errorbar()

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
  
  predicted_ratings <- 
    test_edx %>% 
    left_join(b_i, 
              by = "movieId") %>%
    left_join(b_u, 
              by = "userId") %>%
    left_join(b_g, 
              by="genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  
  return(RMSE(predicted_ratings, test_edx$rating))
  
}

rmsesRough <- sapply(lambdasRough, functionRmses)

qplot(lambdasRough,rmsesRough)

lambdaFine= seq(lambdasRough[which.min(rmsesRough)] -1, lambdasRough[which.min(rmsesRough)] + 1, 0.1)

rmsesFine <- sapply(lambdaFine, functionRmses)

mu <- mean(edx$rating)

lambda = lambdaFine[which.min(rmsesFine)]

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
