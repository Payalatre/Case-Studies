#------------case study 3 --------------
#----------on multiple regression-------

#-----------Tidyverse for data visulization and manpulation
install.packages("tidyverse")
library("tidyverse")


install.packages("datarium")
library("datarium")

#-------We'll use the marketing data set [datarium package], which contains 
#the impact of the amount of money spent on 
#three advertising medias (youtube, facebook and newspaper) on sales.

data("marketing" , package ="datarium")
View(marketing)


setwd("D:/DATA/computer courses/R-Tutorials")
write.table(marketing , file="marketing.csv" , sep =" ,")
head(marketing , 5)


model<-lm(sales~youtube+facebook
          #+newspaper
          , data = marketing)
summary(model)


summary(model)$coefficient



# model equation = sales = 3.5 + 0.045*youtube + 0.187*facebook.

confint(model)

sigma(model)/mean(marketing$sales)



#-----------intraction model with multiple regression-----------

#The multiple linear regression equation, with interaction effects between two predictors (x1 and x2), 
#can be written as follow:

#y = b0 + b1*x1 + b2*x2 + b3*(x1*x2)    equation of  itraction model

   #sales = b0 + b1*youtube + b2*facebook + b3*(youtube*facebook)

#This can be also written as:
  
  #sales = b0 + (b1 + b3*facebook)*youtube + b2*facebook

#or as:
  
  #sales = b0 + b1*youtube + (b2 +b3*youtube)*facebook

#b3 can be interpreted as the increase in the effectiveness of youtube
#advertising for a one unit increase in facebook advertising (or vice-versa).

library("tidyverse")
install.packages("caret")
library("caret")

data("marketing" , package = "datarium")

training.samples <- marketing$sales %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- marketing[training.samples, ]
test.data <- marketing[-training.samples, ]

model1 <-lm(sales ~ facebook+youtube , data=marketing)
summary(model1)

prediction <-model1 %>% predict(test.data)

# Model performance
# (a) Prediction error, RMSE
RMSE(prediction , test.data$sales)

# R square
R2(prediction,test.data$sales)


#---------INTRACTION EFFECT----------------

#In R, you include interactions between variables using the * operator:

model2 <-lm(sales ~ facebook+youtube +facebook:youtube , data=marketing)
 #0r simplyfy
model2<-lm(sales ~ facebook*youtube , data=train.data)
summary(model2)

prediction <-model2 %>% predict(test.data)
RMSE(prediction,test.data$sales)
R2(prediction,test.data$sales)

#The prediction error RMSE of the interaction model 
#is 0.963, which is lower than the prediction error of the additive model (1.58).

#Additionally, the R-square (R2) value of the interaction model 
#is 98% compared to only 93% for the additive model.

#These results suggest that the model with the interaction term is
#better than the model that contains only main effects. So, for this specific data,
#we should go for the model with the interaction model.