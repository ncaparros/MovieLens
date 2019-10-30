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

#Number of rows and columns of the dataset provided by the edx dataset
nrows <- nrow(edx)
ncols <-ncol(edx)

#Number of distinct movies and users
number_of_movies <- edx %>% group_by(movieId) %>% summarize() %>% nrow()
number_of_users <- edx %>% group_by(userId) %>% summarize() %>% nrow()

#---------------------------------------------------------------------------------------------------------------
#Data cleaning
#----------------------------------------------------------------------------------------------------------------
#Extracting date and time of the rating

edx <- edx %>% 
  mutate(
    date = format(as_datetime(timestamp), 
                  format="%Y-%m-%d"), 
    time=format(as_datetime(timestamp), 
                format="%H:%M")) %>% 
  select(-timestamp)


validation <- validation %>% 
  mutate(
    date = format(as_datetime(timestamp), 
                  format="%Y-%m-%d"), 
    time=format(as_datetime(timestamp), 
                format="%H:%M")) %>% 
  select(-timestamp)

#Extracting year of release
edx <- edx %>% mutate(
  yearOfRelease = as.numeric(substring(title, nchar(title)-4,nchar(title)-1)),
  title = substring(title, 1,nchar(title)-7))

validation <- validation %>% mutate(
  yearOfRelease = as.numeric(substring(title, nchar(title)-4,nchar(title)-1)),
  title = substring(title, 1,nchar(title)-7))

#----------------------------------------------------------------------------------------------------------------
#Analysis and effect

#average of ratings
mu_rating = mean(edx$rating)

#----------------------------------------------------------------------------------------------------------------
#Is there a correlation between the number of ratings for a movie and the value of the rating ?

#average of ratings
mu_rating = mean(edx$rating)

# Movie ratings analysis : 10 most reviewed movies
dataByMovie <- edx %>% 
  group_by(movieId) %>% 
  summarize(n=n()) %>%
  top_n(10,n)

# Add total ratings count to the 10 most reviewed movies
completeDataByMovie <- left_join(edx %>%
                                   filter(movieId %in% dataByMovie$movieId),
                                 dataByMovie,
                                 by="movieId")

#Initialize the plot
plotByMovie <- ggplot()

#Bar chart of the 10 most rated movies (x) versus the number of ratings for each movie (y)
ratingByMovieBarPlot <- plotByMovie  + 
  geom_bar(data= completeDataByMovie %>%
             group_by(movieId),
           aes(x=reorder(title,-n),
               y=n),
           stat="identity", 
           fill = "#C4961A",
           color = "#FFDB6D") +
  theme(axis.text.x = element_blank()) + 
  ylab("Number of ratings") + 
  xlab("Movies (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

#Boxplot of the ratings for the 10 most rated movies
MovieAvgRatingBoxplot <- plotByMovie + 
  geom_boxplot(
    data=completeDataByMovie
    ,aes(
      x=reorder(title,-n),
      y=rating),
    fill = "#FFDB6D", 
    color = "#C4961A") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Movies (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10), ylim=c(0,5))

#Ploting the barchart and the boxplot one above the other
egg::ggarrange(ratingByMovieBarPlot,MovieAvgRatingBoxplot, ncol=1)


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

#Ploting the users (10 most prolific raters, x) and the number of ratings (y)
userRatingsBarPlot <- userPlot  + 
  geom_bar(data=
             completeDataByUser %>% group_by(userId),
           aes(x = reorder(userId,-n),
               y = n),
           stat="identity", 
           fill = "#0072B2",
           color = "#56B4E9") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Number of ratings") + 
  xlab("Users (top 10 raters)") + 
  coord_cartesian(xlim =c(1, 10))

#Boxplot of the distribution of the ratings (y) versus the users
UserAvgRatingBoxplot <- userPlot + 
  geom_boxplot(data=completeDataByUser
               ,aes(x=reorder(userId,-n),
                    y=rating),
               fill = "#56B4E9", 
               color = "#0072B2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Users (top 10 raters)") + 
  coord_cartesian(xlim =c(1, 10))

#Plot the charts
ggarrange(userRatingsBarPlot,UserAvgRatingBoxplot)

#----------------------------------------------------------------------------------------------------------------
#Is there a correlation between the genre and the value of the rating ?

#select the 10 most reviewed genres
dataByGenre <- edx %>% 
  group_by(genres) %>% 
  summarize(n=n()) %>%
  top_n(10,n)

#add the total number of ratings to those genres
completeDataByGenre <- left_join(edx %>% 
                                   filter(genres %in% dataByGenre$genres),
                                 dataByGenre, 
                                 by="genres")
#initialize the plots
genrePlot <- ggplot()

#ploting the number of ratings versus the genre
genreRatingsBarPlot <- genrePlot  + 
  geom_bar(data=dataByGenre,
           aes(x=reorder(genres,-n), 
               y=n),
           stat="identity",
           fill = "#C3D7A4", 
           color = "#52854C") +
  theme(axis.text.x = element_blank()) + 
  ylab("Number of ratings") + 
  xlab("Genres (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

#ploting the distribution of the ratings for each genre (10 most reviewed)
UserAvgRatingBoxplot <- genrePlot + 
  geom_boxplot(
    data=completeDataByGenre,
    aes(x=reorder(genres,-n),
        y=rating),
    fill = "#C3D7A4", 
    color = "#52854C") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Genres (top 10 rated)") + 
  coord_cartesian(xlim =c(1, 10))

#display the plots
ggarrange(genreRatingsBarPlot,UserAvgRatingBoxplot)

#calculate ratio of the number of reviews for drama and the number of reviews for drama/crime
Drama_DramaCrime = dataByGenre %>% 
  arrange(desc(n)) %>% 
  filter(genres %in% c("Drama","Crime|Drama"))

ratioDrama_DramaCrime = round(Drama_DramaCrime[1,]$n / Drama_DramaCrime[2,]$n,2)

#Extracting primary genres
primarygenres <- edx %>% 
  filter(!str_detect(genres,"[|]")) %>% 
  pull(genres) %>% 
  unique()

#Extracting all the unique combinations of genres
allGenres <- edx %>% 
  pull(genres) %>% 
  unique()

#----------------------------------------------------------------------------------------------------------------
#Year of release

#extracting the 10 most prolific years of release
dataYearOfReleaseTop <- edx %>% 
  group_by(yearOfRelease) %>% 
  summarize(n=n()) %>%
  top_n(10,n)

#extracting the 10 least prolific years of release
dataYearOfReleaseBottom <- edx %>% 
  group_by(yearOfRelease) %>% 
  summarize(n=n()) %>%
  top_n(10,-n)

#Adding the total count of reviews to the 10 most prolific years of release
completeDataByYearOfReleaseTop <- left_join(edx %>% 
                                              filter(yearOfRelease %in% dataYearOfReleaseTop$yearOfRelease),
                                            dataYearOfReleaseTop,
                                            by="yearOfRelease") 

#Adding the total count of reviews to the 10 least prolific years of release
completeDataByYearOfReleaseBot <- left_join(edx %>% 
                                              filter(yearOfRelease %in% dataYearOfReleaseBottom$yearOfRelease),
                                            dataYearOfReleaseBottom,
                                            by="yearOfRelease")

#Joining the 10 most and 10 least prolific years of release
completeDataByYearOfRelease = rbind(completeDataByYearOfReleaseTop, completeDataByYearOfReleaseBot)

#initializing the plot
plotYearOfRelease <- ggplot()

#ploting the number of reviews versus the year of release
yearOfReleaseRatingsBarPlot <- plotYearOfRelease  + 
  geom_bar(data=completeDataByYearOfRelease,
           aes(x=reorder(yearOfRelease,-n), 
               y=n),
           stat="identity",
           fill = "#c193da", 
           color = "#916eb2") +
  theme(axis.text.x = element_blank()) + 
  ylab("Number of ratings")+ 
  xlab("Years of release") +
  geom_vline(xintercept = 10.5, color="black")+
  geom_text(aes(x=6, label="Years with most releases",y=600000000000)) +
  geom_text(aes(x=15, label="Years with least releases",y=600000000000))

#ploting the distribution of the ratings versus the year of release
YearOfReleaseAvgRatingBoxplot <- plotYearOfRelease + 
  geom_boxplot(
    data=completeDataByYearOfRelease
    ,aes(x=reorder(yearOfRelease,-n),
         y=rating),
    fill = "#c193da", 
    color = "#916eb2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Ratings") + 
  xlab("Years of release")+
  geom_vline(xintercept = 10.5, color="black")

#display the plots
egg::ggarrange(yearOfReleaseRatingsBarPlot,YearOfReleaseAvgRatingBoxplot, ncol=1)

#----------------------------------------------------------------------------------------------------------------
#Date of rating
#---------
#9/11

#filtering the data one month before and after the event
NineOneOne <- edx %>% filter(date < "2001-10-11" & 
                               date >= "2001-08-11") %>% 
  group_by(date) %>% 
  summarize(n=n(), 
            avg=mean(rating), 
            sd=sd(rating)) %>% 
  arrange(date)

#ploting the average ratings and the standard deviation versus the day
ratings911 <- NineOneOne %>% ggplot(aes(x=date,
                                        y=avg,
                                        ymin=avg-sd, 
                                        ymax=avg+sd)) + 
  geom_point() + 
  geom_errorbar() + 
  theme(axis.text.x = element_blank()) + 
  geom_vline(xintercept = 32, 
             color="red") + 
  geom_hline(yintercept = mean(edx$rating), 
             color="green") +
  ylab("Ratings") + 
  xlab("Date") +
  ggtitle("Average ratings around 9/11")

#calculate the average number of rating by dat on the whole dataset edx
avgRatingByDay <- mean(edx %>% group_by(date) %>% summarize(n=n()) %>% pull(n))

#plot the number of ratings versus the date
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

#display the plots
egg::ggarrange(ratings911, n911, nrow=2)


#---------
#Hurricane Katrina

#filtering the data one month before and after the event
Katrina <- edx %>% filter(date < "2005-09-31" & 
                            date >= "2005-07-23") %>% 
  group_by(date) %>% 
  summarize(n=n(), 
            avg=mean(rating), 
            sd=sd(rating)) %>% 
  arrange(date)

#ploting the average ratings and the standard deviation versus the day
ratingsKatrina <- Katrina %>% ggplot(aes(x=date,
                                         y=avg,
                                         ymin=avg-sd, 
                                         ymax=avg+sd)) + 
  geom_point() + 
  geom_errorbar() + 
  theme(axis.text.x = element_blank()) + 
  geom_vline(xintercept = 32, 
             color="red") + 
  geom_vline(xintercept = 40, 
             color="red") +
  geom_hline(yintercept = mean(edx$rating), 
             color="green") +
  ylab("Ratings") + 
  xlab("Date") +
  ggtitle("Average ratings around Hurrican Katrina")

#plot the number of ratings versus the date
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
  ggtitle("Number of ratings around Hurrican Katrina")

#display the plots
egg::ggarrange(ratingsKatrina, nRatingsKatrina, nrow=2)

#----------------------------------------------------------------------------------------------------------------
#Effects

#---------
#b_i

#average of ratings
mu_rating = mean(edx$rating)

#Movie effect
movieAvgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_rating))

#plot the histogram of the distribution
movieAvgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("#C4961A"), fill= I("#FFDB6D"))

#---------
#b_u

#User effect
userAvgs <- edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_rating))

#plot the histogram of the distribution
userAvgs %>% qplot(b_u, geom ="histogram", bins = 10, data = .,
                   fill = I("#56B4E9"), 
                   color = I("#0072B2"))

#---------
#b_g

#Genre effect
genreAvgs <- edx %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_rating))

#plot the histogram of the distribution
genreAvgs %>% qplot(b_g, 
                    geom ="histogram", 
                    bins = 10, data = .,
                    fill = I("#C3D7A4"),
                    color = I("#52854C"))

#----------------------------------------------------------------------------------------------------------------
#Picking lambdas

#first set of lambdas
lambdasRough <- seq(0,10,1)

#overall average on the training set
mu <- mean(train_edx$rating)

#function to compute the rmse depending on lambda using the training set
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
  
  #predict the ratings on the testing set
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

#applying the lambdas to the rmse function
rmsesRough <- sapply(lambdasRough, functionRmses)

#ploting rmse versus lambda
plotRough <- qplot(lambdasRough,
                   rmsesRough,
                   xlab="Lambda",
                   ylab="RMSE")

#picking the lambda for which the rmse is minimal
minRoughLambda <- lambdasRough[which.min(rmsesRough)]

#creating a new set of lambdas
lambdasFine <- seq(minRoughLambda-1,minRoughLambda+1,0.1)

#applying the new lambdas to the rmse function
rmsesFine <- sapply(lambdasFine, functionRmses)

#ploting rmse versus lambda
plotFine <- qplot(lambdasFine,
                  rmsesFine,
                  xlab="Lambda",
                  ylab="RMSE")
#picking the final lambda for which the rmse is minimal
lambda <- lambdasFine[which.min(rmsesFine)]

#display the plots rmse versus lambda
ggarrange(plotRough, plotFine, ncol=1)

#----------------------------------------------------------------------------------------------------------------
#Final RMSE

#calculate mu the overall average of ratings on edx dataset
mu <- mean(edx$rating)

#movie effect
b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

#user effect
b_u <- edx %>% 
  left_join(b_i, 
            by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

#genre effect
b_g <- edx %>% 
  left_join(b_i, 
            by="movieId") %>%
  left_join(b_u, 
            by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+lambda))

#predict ratings on the validation dataset
predicted_ratings <- 
  validation %>% 
  left_join(b_i, 
            by = "movieId") %>%
  left_join(b_u, 
            by = "userId") %>%
  left_join(b_g, 
            by="genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

#return the RMSE
rmse <- RMSE(predicted_ratings, validation$rating)

