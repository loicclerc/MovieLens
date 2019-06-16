#############################################################
#To remove all variables
rm(list=ls())
#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
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


###########Project MovieLens###############################################################


#Brief data base exploration

head(edx)
nrow(edx)
object.size(edx)

#Visualization of the original data

#User impact
edx %>% group_by(userId) %>% mutate(average_rating=round(mean(rating)*8)/8) %>% 
  group_by(average_rating) %>% summarize(n_distinct_user=n_distinct(userId)) %>% 
  ggplot(aes(average_rating, n_distinct_user))+geom_col()

#Movie impact
edx %>% group_by(movieId) %>% mutate(average_rating=round(mean(rating)*8)/8) %>% 
  group_by(average_rating) %>% summarize(n_distinct_movie=n_distinct(movieId)) %>% 
  ggplot(aes(average_rating, n_distinct_movie))+geom_col()

#Reshaping of the data and further visualization

#Extraction of the the year from the title
edx_new <- edx %>% mutate(year=substr(edx$title, nchar(edx$title)-4, nchar(edx$title)-1))

#Year Impact
edx_new %>% group_by(year) %>% mutate(average_rating=round(mean(rating)*8)/8) %>% 
  group_by(average_rating) %>% summarize(year=n_distinct(year)) %>% 
  ggplot(aes(average_rating, year))+geom_col()

#Genre impact
edx %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>%
  summarize(rating_number=n(), average_rating= mean(rating)) %>% 
  ggplot(aes(rating_number, average_rating, color=genres, label = genres)) + geom_point() + geom_text()
edx %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>%
  summarize(rating_number=n(), average_rating= mean(rating)) %>% 
  ggplot(aes(genres, average_rating)) + geom_col()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Further reshaping of the data

#Number of genre in original database
edx$genres %>% n_distinct()

#Reshaping the genre
edx_new_1<- edx[1:2000000,] %>% separate_rows(genres, sep = "\\|") %>% mutate(temp_col=1) %>% spread(genres, temp_col)
edx_new_1[is.na(edx_new_1)] <- 0
edx_new_2<- edx[2000001:4000000,] %>% separate_rows(genres, sep = "\\|") %>% mutate(temp_col=1) %>% spread(genres, temp_col)
edx_new_2[is.na(edx_new_2)] <- 0
edx_new_3<- edx[4000001:6000000,] %>% separate_rows(genres, sep = "\\|") %>% mutate(temp_col=1) %>% spread(genres, temp_col)
edx_new_3[is.na(edx_new_3)] <- 0
edx_new_4<- edx[6000001:nrow(edx),] %>% separate_rows(genres, sep = "\\|") %>% mutate(temp_col=1) %>% spread(genres, temp_col)
edx_new_4[is.na(edx_new_4)] <- 0
edx_new <- bind_rows(edx_new_1,edx_new_2, edx_new_3, edx_new_4)
rm(edx_new_1, edx_new_2, edx_new_3, edx_new_4)
validation_new <- validation %>% separate_rows(genres, sep = "\\|") %>% mutate(temp_col=1) %>% spread(genres, temp_col)
validation_new[is.na(validation_new)] <- 0

#Extraction of the the year from the title
edx_new <- edx_new %>% mutate(year=substr(edx$title, nchar(edx$title)-4, nchar(edx$title)-1))
validation_new <- validation_new %>% mutate(year=substr(validation$title, nchar(validation$title)-4, nchar(validation$title)-1))

#Checking the data base was not modified (except for the genre column)
identical(edx$userId, edx_new$userId)
identical(edx$movieId, edx_new$movieId)
identical(edx$rating, edx_new$rating)
identical(edx$timestamp, edx_new$timestamp)
identical(edx$title, edx_new$title)

#Check that users and movies of the validation set are in the training set

#movie ID
distinct_movie_edx <- edx_new %>% select(movieId) %>% distinct()
distinct_movie_edx <- distinct_movie_edx[,1]
distinct_movie_validation <- validation_new %>% select(movieId) %>% distinct() %>% as.vector()
distinct_movie_validation <- distinct_movie_validation[,1]
distinct_movie_validation %in% distinct_movie_edx %>% table()

#user ID
distinct_user_edx <- edx_new %>% select(userId) %>% distinct()
distinct_user_edx <- distinct_user_edx[,1]
distinct_user_validation <- validation_new %>% select(userId) %>% distinct() %>% as.vector()
distinct_user_validation <- distinct_user_validation[,1]
distinct_user_validation %in% distinct_user_edx %>% table()

#Removing objects that will not be used anymore  
rm(distinct_movie_edx,distinct_movie_validation, distinct_user_edx, distinct_user_validation, edx, validation)

#Renaming problematic column names
names(edx_new)[names(edx_new) == "(no genres listed)"] <- "no_genres_listed"
names(edx_new)[names(edx_new) == "Film-Noir"] <- "Film_Noir"
names(edx_new)[names(edx_new) == "Sci-Fi"] <- "Sci_Fi"
names(validation_new)[names(validation_new) == "Film-Noir"] <- "Film_Noir"
names(validation_new)[names(validation_new) == "Sci-Fi"] <- "Sci_Fi"

#The column no genre listed is removed in the training set as this case does not appear in the validation set
edx_new <- select(edx_new, -no_genres_listed )

#The columns that will not be used in the later are removed as well
edx_new <- select(edx_new, -title, -timestamp)
validation_new <- select(validation_new, -title, -timestamp)

#Modeling

#function RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Computating the global average to have a comparison basis
Global_Average <- mean(edx_new$rating)
RMSE_Global_Average <- RMSE(validation_new$rating, Global_Average)
RMSE_Global_Average

#Table to compare the results of all different approaches
rmse_results <- data_frame(method = "Global Average" , RMSE = RMSE_Global_Average)

#Trying to fit a linear model
fit_lm_movie <- lm (rating~movieId, data = edx_new)
y_hat_lm_movie <- predict(fit_lm_movie, validation_new)
RMSE_lm_movie <- RMSE(validation_new$rating, y_hat_lm_movie)
rmse_results <- bind_rows(rmse_results, data_frame(method="LM with movie effect",
                                                   RMSE = RMSE_lm_movie))
rm(y_hat_lm_movie, fit_lm_movie)

fit_lm_movie_user <- lm (rating~movieId+userId, data=edx_new)
y_hat_lm_movie_user <- predict(fit_lm_movie_user, validation_new)
RMSE_lm_movie_user <- RMSE(validation_new$rating, y_hat_lm_movie_user)
rmse_results <- bind_rows(rmse_results, data_frame(method="LM with movie and user effect",
                                                   RMSE = RMSE_lm_movie_user))
rm(y_hat_lm_movie_user, fit_lm_movie_user)

#Running the model on all the variable would crash R
#fit_lm_all <- lm(rating~., data=edx_new)

#Trying to fit a knn model - create an error even with one predictor and default k=5
#fit_knn <- knn3(rating~movieId, data=edx_new)

#Trying to build a simple decision tree - create an error even with one predictor and default parameters
#if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
#fit_rpart <- rpart(rating~movieId, data=edx_new)

#Following the recommendation systems approach
#Movie effect
movie_avgs <- edx_new %>% group_by(movieId) %>% summarize(b_i=mean(rating-Global_Average))
y_hat_movie_Avg <- Global_Average + validation_new %>% left_join(movie_avgs, by = 'movieId') %>% .$b_i
RMSE_movie_effect <- RMSE(y_hat_movie_Avg,validation_new$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effect Model",
                                                   RMSE = RMSE_movie_effect))
rm(y_hat_movie_Avg)

#Movie & User effect
user_avgs <- edx_new %>% left_join(movie_avgs, by = 'movieId') %>% 
  group_by(userId) %>% summarize(b_u=mean(rating-Global_Average-b_i))
y_hat_movie_Avg_user_Avg <- validation_new %>% left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>% mutate(pred = Global_Average + b_i + b_u) %>% .$pred
RMSE_movie_user_effect <-RMSE(y_hat_movie_Avg_user_Avg, validation_new$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie & User Effect Model",
                                                   RMSE = RMSE_movie_user_effect))
rm(y_hat_movie_Avg_user_Avg)

#Movie, User & Year effect
year_avgs <- edx_new %>% left_join(movie_avgs, by = 'movieId') %>% left_join(user_avgs, by = 'userId') %>%
  group_by(year) %>% summarize(b_y=mean(rating-Global_Average-b_i-b_u))
y_hat_movie_Avg_user_Avg_year_Avg <- validation_new %>% left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% left_join(year_avgs, by = 'year') %>% 
  mutate(pred=Global_Average+b_i+b_u+b_y) %>%.$pred
RMSE_movie_user_year_effect <- RMSE(y_hat_movie_Avg_user_Avg_year_Avg, validation_new$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie, User & Year Effect Model",
                                                   RMSE = RMSE_movie_user_year_effect))
rm(y_hat_movie_Avg_user_Avg_year_Avg)

#Movie, User, Year and Drama effect
#Looking at the impact of Drama / not Drama
edx_new %>% group_by(Drama) %>% summarize(average=mean(rating))
#Adding the effect
Drama_avg <- edx_new %>% left_join(movie_avgs, by = 'movieId') %>% left_join(user_avgs, by = 'userId') %>%
  left_join(year_avgs, by = 'year') %>% group_by(Drama) %>% summarize(b_d=mean(rating-Global_Average-b_i-b_u-b_y))
y_hat_movie_Avg_user_Avg_year_Avg_Drama <- validation_new %>% left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by = 'userId') %>% left_join(year_avgs, by = 'year') %>% left_join(Drama_avg, by = 'Drama') %>%
  mutate(pred=Global_Average+b_i+b_u+b_y+b_d) %>%.$pred
RMSE_movie_user_year_drama_effect <- RMSE(y_hat_movie_Avg_user_Avg_year_Avg_Drama, validation_new$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie, User, Year and Drama Effect Model",
                                                   RMSE = RMSE_movie_user_year_drama_effect))
rm(y_hat_movie_Avg_user_Avg_year_Avg_Drama)

#Regularization
lambdas <- seq(0,10,0.5)
rmses <- sapply(lambdas, function(l){
  b_i <- edx_new %>% group_by(movieId) %>%
    summarize(b_i = sum(rating - Global_Average)/(n()+l))
  b_u <- edx_new %>% left_join(b_i, by="movieId") %>%
    group_by(userId) %>% summarize(b_u = sum(rating - b_i -Global_Average)/(n()+l))
  b_y <- edx_new %>% left_join(b_i, by="movieId") %>% left_join(b_u, by = 'userId') %>%
    group_by(year) %>% summarize(b_y = sum(rating - b_i - b_u -Global_Average)/(n()+l))
  b_d <- edx_new %>% left_join(b_i, by="movieId") %>% left_join(b_u, by = 'userId') %>%
    left_join(b_y, by = 'year') %>% group_by(Drama) %>% 
    summarize(b_d = sum(rating - b_i - b_u - b_y -Global_Average)/(n()+l))
  predicated_ratings <- validation_new %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'year') %>%
    left_join(b_d, by = 'Drama') %>%
    mutate(pred = Global_Average + b_i + b_u + b_y + b_d) %>%
    .$pred
  return(c(l,RMSE(predicated_ratings, validation_new$rating)))
})
Best_lambda <- rmses[1,which.min(rmses[2,])]
rmses[,which.min(rmses[2,])]


#Running the above code with the best found lambda:
b_i <- edx_new %>% group_by(movieId) %>%
  summarize(b_i = sum(rating - Global_Average)/(n()+Best_lambda))
b_u <- edx_new %>% left_join(b_i, by="movieId") %>%
  group_by(userId) %>% summarize(b_u = sum(rating - b_i -Global_Average)/(n()+Best_lambda))
b_y <- edx_new %>% left_join(b_i, by="movieId") %>% left_join(b_u, by = 'userId') %>%
  group_by(year) %>% summarize(b_y = sum(rating - b_i - b_u -Global_Average)/(n()+Best_lambda))
b_d <- edx_new %>% left_join(b_i, by="movieId") %>% left_join(b_u, by = 'userId') %>%
  left_join(b_y, by = 'year') %>% group_by(Drama) %>% 
  summarize(b_d = sum(rating - b_i - b_u - b_y -Global_Average)/(n()+Best_lambda))
y_hat_Regularization <- validation_new %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_y, by = 'year') %>%
  left_join(b_d, by = 'Drama') %>%
  mutate(pred = Global_Average + b_i + b_u + b_y + b_d) %>%
  .$pred
RMSE_Regularization <- RMSE(y_hat_Regularization, validation_new$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Model",
                                                   RMSE = RMSE_Regularization))

#Rounding the obtained ratings to the nearest half
y_hat_rounded <- round(y_hat_Regularization*2)/2
RMSE_Rounded <- RMSE(y_hat_rounded, validation_new$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Final Model",
                                                   RMSE = RMSE_Rounded))


rmse_results %>% knitr::kable()
