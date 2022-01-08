#### Letterboxd Data Analysis ####
###  Author: Nick Wibert      ###

## Description: With the final data set, I will perform exploratory
## data analysis to get a general feel for the data set. I will then
## decide on various models/approaches to fit to this data and
## compare predictive performance using classification error rates.

library(dplyr)

# project directory
project_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Coursework/STA4241 - Statistical Learning in R/Final Project"

# load in final data set
data <- read.csv(file.path(project_dir, "full-diary.csv"))

# remove duplicates
data <- distinct(data)

# remove columns of people + studio
data <- data[,-c(9:16)]

# remove movies which I did not rate (Rating = NA)
data <- data[-which(is.na(data$Rating)),]

# convert 'Watched.Date' from full date to just year
data$Watched.Date <- as.numeric(substring(data$Watched.Date, 1, 4))

# remove rows with blank tags
data <- data[!data$Tags=="",]

data$Tags <- as.factor(data$Tags)
data$Genre <- as.factor(data$Genre)
data$Country <- as.factor(data$Country)
data$Language <- as.factor(data$Language)

# store numerical covariates as its own data set
data.num <- data[,c(1,3,2,5,6,7)]

# store rating as dependent variable
y <- data[,3]


#### Linear regression ####
mod <- lm(Rating ~ as.factor(Language), data = data)
summary(mod)


#### Logistic regression ####
mod.log <- glm(Rating ~ Year + Watched.Date + Average.Rating
                      + Runtime + as.factor(Tags) + as.factor(Genre)
                      + as.factor(Country) + as.factor(Language),
               data = train)
summary(mod.log)

library(VGAM)

# fit baseline category logit model
nonparallel <- vglm(as.factor(Rating) ~ Watched.Date + Average.Rating + as.factor(Tags),
               family=multinomial, data=data)
parallel <- vglm(as.factor(Rating) ~ Watched.Date + Average.Rating,
                    family=multinomial(parallel=TRUE), data=data)
lrtest(parallel, nonparallel)

#### PCA ####
pca <- prcomp(data.num[,-c(1,2)])
plot(pca, main="Variation explained by each PC")
axis(1, at=seq(0.7, 12.5, by=1.2), labels=paste("PC", 1:10), las=2)











library(MASS)
library(e1071)


#### SIMULATION STUDY WITH NUMERICAL DATA ONLY ####
error <- matrix(NA, 100, 6) # store MSE
colnames(error) <- c("GLM.Ordered", "LDA", "QDA", "KNN", "SVMp", "SVMr")

for (i in 1:100)
{
  print(i)
  # randomly select 75 subjects to be test data
  i.test <- sample(1:nrow(data.num), 75)
  
  # split up data
  train <- data.num[-i.test,]
  test <- data.num[i.test,]
  
  x.train <- data.num[-i.test, -c(1,2)]
  x.test <- data.num[i.test, -c(1,2)]
  y.train <- data.num[-i.test, 2]
  y.test <- data.num[i.test, 2]
  
  ## Ordered logistic regression
  mod.log <- polr(as.factor(Rating) ~ Year + Watched.Date + Average.Rating + Runtime,
                 data = train)

  y.hat.log <- predict(mod.log, test)
  error[i,1] <- mean(y.test != y.hat.log)

  ## LDA
  mod.lda <- lda(Rating ~ Year + Watched.Date + Average.Rating + Runtime,
                 data = train)
  y.hat.lda <- as.numeric(predict(mod.lda, test)$class) / 2
  error[i,2] <- mean(y.test != y.hat.lda)

  # ## QDA
  # mod.qda <- qda(Rating ~ Average.Rating,
  #                data = train)
  # y.hat.qda <- as.numeric(predict(mod.qda, test)$class) / 2
  # error[i,3] <- mean(y.test != y.hat.qda)
  #
  ## KNN
  tune.knn <- tune.knn(x.train, as.factor(y.train), k=1:50)
  y.hat.knn <- knn(x.train, x.test, y.train, k = tune.knn$best.parameters)
  error[i,4] <- mean(y.test != y.hat.knn)

  ## SVM with polynomial kernel
  tune.svm.p <- tune.svm(as.factor(Rating) ~  Year + Watched.Date +
                           Average.Rating + Runtime,
                         data = train, kernel = "polynomial",
                         degree = c(1:7))
  svm.p <- tune.svm.p$best.model

  y.hat.svm.p <- predict(svm.p, test)
  error[i,5] <- mean(y.test != y.hat.svm.p)

  ## SVM with radial kernel
  gamma <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  tune.svm.r <- tune.svm(as.factor(Rating) ~  Year + Watched.Date +
                           Average.Rating + Runtime,
                         data = train, kernel = "radial",
                         gamma = gamma)
  svm.r <- tune.svm.r$best.model

  y.hat.svm.r <- predict(svm.r, test)
  error[i,6] <- mean(y.test != y.hat.svm.r)
}

# get mean error rate from each column in the error matrix
# (i.e. the mean error rate for each classification algorithm)
error.mean <- colMeans(error)
error.mean

# GLM.Ordered         LDA         QDA         KNN        SVMp        SVMr 
# 0.642       0.6602667          NA   0.6214667   0.6754667   0.6492000 

# random guessing would be correct 10% of the time
# these models seem to be right about 35% of the time on average




library(class)
library(caret)
#### SIMULATION STUDY WITH ONLY ONE PREDICTOR (AVG RATING) ####
#### (update to include response as a continuous variable) ####
error <- matrix(NA, 100, 6) # store MSE
colnames(error) <- c("GLM", "LDA", "QDA", "KNN", "SVMp", "SVMr")

for (i in 1:100)
{
  print(i)
  # split dataset into 80% training and 30% test, attempting
  # to preserve proportions of each class across the two groups
  i.train <- createDataPartition(data$Rating, p = .8, list = FALSE)
  
  # split up data
  train <- data[i.train,]
  test <- data[-i.train,]
  
  x.train <- data[i.train, 6]
  x.test <- data[-i.train, 6]
  y.train <- data[i.train, 3]
  y.test <- data[-i.train, 3]
  
  ##  logistic regression
  mod.log <- glm(Rating ~ Average.Rating,
                  data = train)

  y.hat.log <- predict(mod.log, test)
  error[i,1] <- mean((y.test - y.hat.log)^2)

  ## LDA
  mod.lda <- lda(Rating ~ Average.Rating,
                 data = train)
  y.hat.lda <- as.numeric(predict(mod.lda, test)$class) / 2
  error[i,2] <- mean((y.test - y.hat.lda)^2)

  ## KNN
  # tune.knn <- tune.knn(x.train, y.train, k=1:50)
  # y.hat.knn <- knn(as.data.frame(x.train), as.data.frame(x.test),
  #                  y.train, k = tune.knn$best.parameters)
  # error[i,4] <- mean((y.test - y.hat.knn)^2)

  ## SVM with polynomial kernel
  tune.svm.p <- tune.svm(Rating ~ Average.Rating,
                         data = train, kernel = "polynomial",
                         degree = c(1:4))
  svm.p <- tune.svm.p$best.model
  
  y.hat.svm.p <- predict(svm.p, test)
  error[i,5] <- mean((y.test - y.hat.svm.p)^2)
  
  ## SVM with radial kernel
  gamma <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  tune.svm.r <- tune.svm(Rating ~ Average.Rating,
                         data = train, kernel = "radial",
                         gamma = gamma)
  svm.r <- tune.svm.r$best.model

  y.hat.svm.r <- predict(svm.r, test)
  error[i,6] <- mean((y.test - y.hat.svm.r)^2)
}

# get mean error rate from each column in the error matrix
# (i.e. the mean error rate for each classification algorithm)
error.mean <- colMeans(error)
error.mean

# GLM       LDA       QDA       KNN      SVMp      SVMr 
# 0.2665430 0.2999836        NA        NA 0.2673123 0.2689024 

## MSE of 0.26-0.3 when response is treated as continuous variable

## Very good on 5 star scale; this tells us that the models, on average,
## are within one "level" of the true level in the context of Rating 
## as a factor (since each level is increments of 0.5)




# create dummy variables for KNN
data$Theatre <- ifelse(data$Tags == "theatre", 1, 0)
data$BluRay <- ifelse(data$Tags == "blu-ray", 1, 0)
data$DVD <- ifelse(data$Tags == "dvd", 1, 0)
data$Streaming <- ifelse(data$Tags == "streaming", 1, 0)

data$Drama <- ifelse(data$Genre == "drama", 1, 0)
data$SciFi <- ifelse(data$Genre == "science-fiction", 1, 0)
data$Action <- ifelse(data$Genre == "action", 1, 0)
data$Mystery <- ifelse(data$Genre == "mystery", 1, 0)
data$Adventure <- ifelse(data$Genre == "adventure", 1, 0)
data$GenreFilms <- ifelse(data$Genre == "Genre Films", 1, 0)
data$Thriller <- ifelse(data$Genre == "thriller", 1, 0)
data$Music <- ifelse(data$Genre == "music", 1, 0)
data$Comedy <- ifelse(data$Genre == "comedy", 1, 0)
data$Horror <- ifelse(data$Genre == "horror", 1, 0)
data$Crime <- ifelse(data$Genre == "crime", 1, 0)
data$Family <- ifelse(data$Genre == "family", 1, 0)
data$Animation <- ifelse(data$Genre == "animation", 1, 0)
data$Romance <- ifelse(data$Genre == "romance", 1, 0)
data$War <- ifelse(data$Genre == "war", 1, 0)
data$History <- ifelse(data$Genre == "history", 1, 0)
data$Fantasy <- ifelse(data$Genre == "fantasy", 1, 0)
data$Western <- ifelse(data$Genre == "western", 1, 0)
data$Documentary <- ifelse(data$Genre == "documentary", 1, 0)






#### SIMULATION STUDY WITH ALL DATA ####
error.all <- matrix(NA, 100, 5) # store classification error
colnames(error.all) <- c("GLM.Ordered", "LDA", "KNN", "SVMp", "SVMr")

for (i in 1:100)
{
  print(i)
  # split dataset into 80% training and 20% test, attempting
  # to preserve proportions of each class across the two groups
  i.train <- createDataPartition(data$Rating, p = .8, list = FALSE)
  
  # split up data
  train <- data[i.train,]
  test <- data[-i.train,]
  
  x.train <- data[i.train, -c(1:4,7:10)]
  x.test <- data[-i.train, -c(1:4,7:10)]
  y.train <- data[i.train, 3]
  y.test <- data[-i.train, 3]
  
  ## Ordered logistic regression
  mod.log <- polr(as.factor(Rating) ~  Watched.Date + Average.Rating
                  + as.factor(Genre) + as.factor(Tags),
                  data = train)
  
  y.hat.log <- predict(mod.log, test)
  error.all[i,1] <- mean(y.test != y.hat.log)
  
  ## LDA
  mod.lda <- lda(as.factor(Rating) ~ Watched.Date + Average.Rating
                 + as.factor(Genre) + as.factor(Tags),
                 data = train)
  y.hat.lda <- as.numeric(predict(mod.lda, test)$class) / 2
  error.all[i,2] <- mean(y.test != y.hat.lda)
  
  # ## QDA
  # mod.qda <- qda(Rating ~ Average.Rating,
  #                data = train)
  # y.hat.qda <- as.numeric(predict(mod.qda, test)$class) / 2
  # error[i,3] <- mean(y.test != y.hat.qda)
  #
  
  # x.train$Tags <- as.factor(x.train$Tags)
  # x.train$Genre <- as.factor(x.train$Genre)
  # x.train$Country <- as.factor(x.train$Country)
  # x.train$Language <- as.factor(x.train$Language)
  # 
  ## KNN
  tune.knn <- tune.knn(x.train, as.factor(y.train), k=1:50)
  y.hat.knn <- knn(x.train, x.test, y.train, k = tune.knn$best.parameters)
  error.all[i,3] <- mean(y.test != y.hat.knn)
  print(tune.knn$best.parameters)
  
  ## SVM with polynomial kernel
  tune.svm.p <- tune.svm(as.factor(Rating) ~ Watched.Date + Average.Rating
                         + as.factor(Genre) + as.factor(Tags),
                         data = train, kernel = "polynomial",
                         degree = c(1:7))
  svm.p <- tune.svm.p$best.model
  
  y.hat.svm.p <- predict(svm.p, test)
  error.all[i,4] <- mean(y.test != y.hat.svm.p)
  
  ## SVM with radial kernel
  gamma <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  tune.svm.r <- tune.svm(as.factor(Rating) ~ Watched.Date + Average.Rating
                         + as.factor(Genre) + as.factor(Tags),
                         data = train, kernel = "radial",
                         gamma = gamma)
  svm.r <- tune.svm.r$best.model
  
  y.hat.svm.r <- predict(svm.r, test)
  error.all[i,5] <- mean(y.test != y.hat.svm.r)
}

# get mean error rate from each column in the error matrix
# (i.e. the mean error rate for each classification algorithm)
colMeans(error.all)

# GLM.Ordered         LDA         QDA         KNN        SVMp        SVMr 
# 0.8364      0.6652          NA          NA      0.6950      0.5886 






#### CHOOSING PREDICTORS WITH STEPWISE REGRESSION FOR GLMS ####

# baseline category logistic regression
mod.log <- polr(as.factor(Rating) ~ Year + Average.Rating +
                  Runtime + as.factor(Genre) + Watched.Date
                + as.factor(Tags) + as.factor(Language),
                data = data)
summary(mod.log)

step(mod.log, direction="forward", nvmax = 10)

fwd <- regsubsets(as.factor(Rating) ~ Year + Average.Rating +
                    Runtime + as.factor(Genre) + Watched.Date
                  + as.factor(Tags),
                  data = train, nvmax = 20, method="forward")
fwdSummary <- summary(fwd)

# see which subset maximizes the adjusted R^2
which.max(fwdSummary$adjr2) # 14

# get indices of covariates which should be included
terms <- names(fwdSummary$which[14,-1])
include <- as.numeric(fwdSummary$which[14,-1])
ind <- which(include %in% 1)

terms[ind]



## model chosen: Tags, Watched.Date, Average.Rating
## let's run a simulation using just those predictors
library(class)

#### SIMULATION STUDY WITH PREDICTORS CHOSEN BY STEPWISE REGRESSION ####
error.stepwise <- matrix(NA, 100, 5) # store classification error
colnames(error.stepwise) <- c("Ord.GLM", "LDA", "KNN", "SVMp", "SVMr")

for (i in 1:100)
{
  print(i)
  # split dataset into 80% training and 30% test, attempting
  # to preserve proportions of each class across the two groups
  i.train <- createDataPartition(data$Rating, p = .8, list = FALSE)
  
  # split up data
  train <- data[i.train,]
  test <- data[-i.train,]
  
  x.train <- data[i.train, c(5,6,19:22)]
  x.test <- data[-i.train,c(5,6,19:22)]
  y.train <- data[i.train, 3]
  y.test <- data[-i.train, 3]
  
  ## Ordered logistic regression
  mod.log <- polr(as.factor(Rating) ~ as.factor(Tags) + Watched.Date
                  + Average.Rating, data = train)
  
  y.hat.log <- predict(mod.log, test)
  error.stepwise[i,1] <- mean(y.test != y.hat.log)
  
  ## LDA
  mod.lda <- lda(as.factor(Rating) ~ as.factor(Tags) + Watched.Date
                 + Average.Rating, data = train)
  y.hat.lda <- as.numeric(predict(mod.lda, test)$class) / 2
  error.stepwise[i,2] <- mean(y.test != y.hat.lda)
  

  ## KNN
  tune.knn <- tune.knn(x.train, as.factor(y.train), k=1:50)
  y.hat.knn <- knn(x.train, x.test, y.train, k = tune.knn$best.parameters)
  error.stepwise[i,3] <- mean(y.test != y.hat.knn)

  ## SVM with polynomial kernel
  tune.svm.p <- tune.svm(as.factor(Rating) ~ as.factor(Tags) + Watched.Date
                         + Average.Rating, data = train, kernel = "polynomial",
                         degree = c(1:7))
  svm.p <- tune.svm.p$best.model
  
  y.hat.svm.p <- predict(svm.p, test)
  error.stepwise[i,4] <- mean(y.test != y.hat.svm.p)
  
  ## SVM with radial kernels
  gamma <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  tune.svm.r <- tune.svm(as.factor(Rating) ~ as.factor(Tags) + Watched.Date
                         + Average.Rating, data = train, kernel = "radial",
                         gamma = gamma)
  svm.r <- tune.svm.r$best.model
  
  y.hat.svm.r <- predict(svm.r, test)
  error.stepwise[i,5] <- mean(y.test != y.hat.svm.r)
}

# get mean error rate from each column in the error matrix
# (i.e. the mean error rate for each classification algorithm)
colMeans(error.stepwise)



#### SIMULATION STUDY WITH PREDICTORS CHOSEN BY BEST SUBSET ####
error.subset <- matrix(NA, 100, 6) # store classification error
colnames(error.subset) <- c("B.Logit", "LDA", "KNN", "SVMp", "SVMr")

for (i in 1:100)
{
  print(i)
  # split dataset into 80% training and 30% test, attempting
  # to preserve proportions of each class across the two groups
  i.train <- createDataPartition(data$Rating, p = .8, list = FALSE)
  
  # split up data
  train <- data[i.train,]
  test <- data[-i.train,]
  
  x.train <- data[i.train, c(5,6,11:22)]
  x.test <- data[-i.train, c(5,6,11:22)]
  y.train <- data[i.train, 3]
  y.test <- data[-i.train, 3]
  
  ## Baseline-category logit
  mod.log <- vglm(as.factor(Rating) ~ Average.Rating + Crime + Drama
                  + Fantasy + History + Horror + Music + Thriller + War
                  + Watched.Date + BluRay + DVD + Streaming + Theatre, 
                  data = train, family = multinomial)
  
  y.hat.log <- predict(mod.log, test)
  error.subset[i,1] <- mean(y.test != y.hat.log)
  
  ## LDA
  mod.lda <- lda(as.factor(Rating) ~ Average.Rating + Crime + Drama
                 + Fantasy + History + Horror + Music + Thriller + War
                 + Watched.Date + BluRay + DVD + Streaming + Theatre,
                 data = train)
  y.hat.lda <- as.numeric(predict(mod.lda, test)$class) / 2
  error.subset[i,2] <- mean(y.test != y.hat.lda)

  ## KNN
  tune.knn <- tune.knn(x.train, as.factor(y.train), k=1:50)
  y.hat.knn <- knn(x.train, x.test, y.train, k = tune.knn$best.parameters)
  error.subset[i,3] <- mean(y.test != y.hat.knn)
  
  ## SVM with polynomial kernel
  tune.svm.p <- tune.svm(as.factor(Rating) ~ as.factor(Tags) + Watched.Date
                         + Average.Rating, data = train, kernel = "polynomial",
                         degree = c(1:7))
  svm.p <- tune.svm.p$best.model
  
  y.hat.svm.p <- predict(svm.p, test)
  error.subset[i,4] <- mean(y.test != y.hat.svm.p)
  
  ## SVM with radial kernels
  gamma <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  tune.svm.r <- tune.svm(as.factor(Rating) ~ as.factor(Tags) + Watched.Date
                         + Average.Rating, data = train, kernel = "radial",
                         gamma = gamma)
  svm.r <- tune.svm.r$best.model
  
  y.hat.svm.r <- predict(svm.r, test)
  error.subset[i,5] <- mean(y.test != y.hat.svm.r)
}

# get mean error rate from each column in the error matrix
# (i.e. the mean error rate for each classification algorithm)
colMeans(error.subset)
