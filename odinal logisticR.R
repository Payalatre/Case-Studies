#Our objective is to predict an individual's perception about government's effort 
#to reduce poverty based on factors like individual's country, gender, age etc. In 
#the given case study, individual's perception can take the following
#three values - Too Little, About Right, To

install.packages("foreign")
library("foreign")

install.packages("carData")
library("carData")
install.packages("MASS")
library("MASS")

data(WVS)
head(WVS)

summary(WVS)

install.packages("ggplot2")
library("ggplot2")

ggplot(WVS , aes(x=poverty , y=age , fill = poverty)) +
  geom_boxplot(size=.75)+ facet_grid(country ~ gender, margins = FALSE) +
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))

model<-polr(poverty~ religion+degree+country+age+gender , data= WVS , Hess = TRUE)
summary(model)

summary_table <- coef(summary(model))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

new_data<- data.frame("religion"= "yes","degree"="no","country"="Norway","age"=30,"gender"="male")
round(predict(model,new_data,type = "p"), 3)
