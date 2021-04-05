#We'll use the iris data set, introduced in Chapter @ref(classification-in-r), 
#for predicting iris species based on 
# predictor variables Sepal.Length, Sepal.Width, Petal.Length, Petal.Width.

install.packages("tidyverse")
library("tidyverse")
install.packages("caret")
library("caret")
library("nnet")
library("dplyr")

#tidyverse for easy data manipulation
#caret for easy predictive modeling
#nnet for computing multinomial logistic regression

data("iris")

sample_n(iris,3)
sample_n(iris, 3)
set.seed(123)

training.samples <- iris$Species %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- iris[training.samples, ]
test.data <- iris[-training.samples,]

model<-nnet::multinom(species~., data =train.data)
model <- nnet::multinom(Species ~., data = train.data)
summary(model)
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == test.data$Species)
