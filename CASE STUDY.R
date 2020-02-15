

Black_friday <- read.csv("BlackFriday.csv")
## Removed NA Values
Omitted_NA <- na.omit(Black_friday)
## Removed first variable
Omitted_NA <- Omitted_NA[c(-1)]

library(dplyr)
Purchase1 <- select(Omitted_NA,Purchase ,Gender,City_Category,Age,Stay_In_Current_City_Years)
Purchase2 <- select(Omitted_NA ,Purchase,Age)

##Areawise amount purchase
ggplot(Purchase1) + geom_col(aes(x =City_Category ,y= (Purchase/1000000), fill = Gender))  + labs(y= "Purchase in millions of dollars", title = "Areawise amount purchase")

## Agewise Amount purchase
ggplot(Purchase1) + geom_col(aes(x= Age , y= (Purchase/1000000),fill =Stay_In_Current_City_Years)) + labs(y= "Purchase in millions of dollars", title = "Agewise amount purchase")


## 3D Plot 
library(plotrix)
slices <- c(53.5,358,766,384,153,133,68.5) 
lbls <- c("0-17", "18-25", "26-35", "36-45", "46-50" ,"51-55" ,"55+")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of Diffrent age groups Participated in the Black Friday sale ")  

## plotted in between purchase amount and other variables , from the boxplot's we observed that Age, 
#Occupation, city category, Product categories and stay affects the amount of purchase 

ggplot(Omitted_NA, aes(x=Age, y= Purchase)) + geom_boxplot()+labs(title = "Boxplot Purchase vs Age")


ggplot(Omitted_NA, aes(x=City_Category, y=Purchase)) + geom_boxplot() +labs(title = "Boxplot Purchase vs City_Category ")


ggplot(Omitted_NA, aes(x=Stay_In_Current_City_Years, y=Purchase)) + geom_boxplot()+labs(title = "Boxplot Purchase vs Stay ")

ggplot(Omitted_NA, aes(x=Product_Category_1, y=Purchase)) + geom_boxplot()
ggplot(Omitted_NA, aes(x=Product_Category_2, y=Purchase)) + geom_boxplot()
ggplot(Omitted_NA, aes(x=Product_Category_3, y=Purchase)) + geom_boxplot()

## First model 
Model2 <- lm(Purchase~Product_Category_1 ,data = Omitted_NA)
summary(Model2)
## Plot of predictions for the model2
Omitted_NA %>%
  add_predictions(Model2) %>%
  ggplot(aes(x=Product_Category_1)) +
  geom_point(aes(y=Purchase)) +
  geom_point(aes(y=pred), col="red", size=2) +labs(title = "Prediction for model2 ")

## Plot of residuals for the model2

Omitted_NA %>%
  add_residuals(Model2) %>%
  ggplot(aes(x=Product_Category_1)) +
  geom_point(aes(y=resid)) +labs(title = "Residuals for model2 ")

rmse(Model2,Omitted_NA)

##Added product categories as predictor's

Model3 <- lm(Purchase~Product_Category_1 +Product_Category_2+ Product_Category_3,data = Omitted_NA)
summary(Model3)
## Plot of predictions for the model3
Omitted_NA %>%
  add_predictions(Model3) %>%
  ggplot(aes(x=Product_Category_1)) +
  geom_point(aes(y=Purchase)) +
  geom_point(aes(y=pred), col="red", size=2) +labs(title = "Prediction for model3 ")

## PLot of residuals for the model3
Omitted_NA %>%
  add_residuals(Model3) %>%
  ggplot(aes(x=Product_Category_1)) +
  geom_point(aes(y=resid)) +labs(title = "Residuals for model3 ")

rmse(Model3,Omitted_NA)

## Final model by adding all product categories

model1 <- lm(Purchase~Gender+Occupation+Age +City_Category+Stay_In_Current_City_Years+Marital_Status+
               Product_Category_1+ Product_Category_2+Product_Category_3 ,data = Omitted_NA)

summary(model1)

## RMSE for this model is very less comapred to other models.
rmse(model1,Omitted_NA)












