
# CREDIT DEFAULT - PROJEKT ZAAWANSOWANA EKONOMETRIA

setwd("C:/Users/kszaf/OneDrive/Pulpit/ekonometria") #change to your directory 
library('readxl')
library('dplyr')
library('stargazer')
library('tidyverse')
library('caret')
library('smotefamily')
library('ROSE')
library('corrplot')

dane = read_excel('dane.xls', col_names = TRUE)
names(dane) <- dane[1,]
dane <- dane[-1,]

dane <- rename(dane, "PAY_1" = "PAY_0")
dane <- rename(dane, "default" = "default payment next month")


sapply(dane, class)
dane <- as.data.frame(apply(dane, 2, as.numeric)) #zamiana typu danych na numeryczne

#...i kilka zmiennych na dyskretne
dane$SEX <- as.factor(dane$SEX) 
dane$EDUCATION <- as.factor(dane$EDUCATION)
dane$MARRIAGE<- as.factor(dane$MARRIAGE)
dane$default <- as.factor(dane$default)
dane$PAY_1 <- as.factor(dane$PAY_1)
dane$PAY_2 <- as.factor(dane$PAY_2)
dane$PAY_3 <- as.factor(dane$PAY_3)
dane$PAY_4 <- as.factor(dane$PAY_4)
dane$PAY_5 <- as.factor(dane$PAY_5)
dane$PAY_6 <- as.factor(dane$PAY_6)

#dziwne wartoœci zmiennych dyskretnych...
dane$EDUCATION[dane$EDUCATION == '0'] = '4' #wszytko wrzucam do "inne"
dane$EDUCATION[dane$EDUCATION == '5'] = '4'
dane$EDUCATION[dane$EDUCATION == '6'] = '4'

dane$MARRIAGE[dane$MARRIAGE == '0'] = '3'


dane2 = dane[-c(1, 7:12)] #bez PAY_1,...,PAY_6


#WSTÊPNA ANALIZA DANYCH
summary(dane)
sum(is.na(dane)) # = 0


sum(dane$default[dane$SEX == "2"] == "1")/sum(dane$default[dane$SEX == "2"] == "0")
sum(dane$default[dane$SEX == "1"] == "1")/sum(dane$default[dane$SEX == "1"] == "0")

sum(dane$default[dane$EDUCATION == "1"] == "1")
sum(dane$default[dane$EDUCATION == "2"] == "1")
sum(dane$default[dane$EDUCATION == "3"] == "1")
sum(dane$default[dane$EDUCATION == "4"] == "1")

M = cor(dane2[-c(2:5,18)])
corrplot(M, type = "lower", method = "number")


#LOGIT (glm, family = binomial)

model <- glm(default ~ ., data = dane2, family = binomial(link = "logit"))
summary(model) 
stargazer(model)

#dok³adnoœæ modelu
probabilities <- model %>% predict(dane2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
# Model accuracy
mean(predicted.classes == dane2$default)

#predicted <- predict(model, dane2, type="response")
#predicted.classes <- ifelse(predicted > 0.5, "1", "0")

#convert defaults from "Yes" and "No" to 1's and 0's


#find optimal cutoff probability to use to maximize accuracy
#optimal <- optimalCutoff(test$default, predicted)[1]

#create confusion matrix
confusionMatrix(as.factor(dane2$default), as.factor(predicted.classes))
unique(dane2$default)
unique(predicted.classes) #przewiduje zawsze "0", mo¿e trzeba zrebalansowaæ?

#oversampling

dane3 = ovun.sample(default ~., data = dane2, method = "over")$data
summary(dane3)

model2 = glm(default ~ ., data = dane3, family = binomial(link = "logit"))
summary(model2)

probabilities2 <- model2 %>% predict(dane3, type = "response")
predicted.classes2 <- ifelse(probabilities2 > 0.5, "1", "0")

# Model accuracy
mean(predicted.classes2 == dane3$default)
confusionMatrix(as.factor(dane3$default), as.factor(predicted.classes2))
unique(predicted.classes2)
unique(dane3$default)

16964/(16964+6353)
