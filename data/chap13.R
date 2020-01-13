install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

# 1. dataset : iris

idx <- sample(nrow(iris),nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]

model <- rpart(Species ~ . ,data = train)
model


# 분류 모델 시각화
rpart.plot(model)


y_pred <- predict(model,test)
y_pred

y_pred <- predict(model,test,type = 'class')
y_pred


table(y_pred, test$Species)

# 중요변수

Titanic3 <- read.csv("c://Rwork/data/titanic3.csv")
Titanic3$survived <- factor(Titanic3$survived)
table(Titanic3$survived)

titanic <- Titanic3[-c(3,8,10,12:14)]
str(titanic)

# train / test set


install.packages("randomForest")
library(randomForest)
names(iris)

model <- randomForest(Species ~ . ,data =iris)

model2 <- randomForest(Species ~ . ,data =iris, ntree=400,ntry =2 ,importandc= True ,na.action = na.omit)
model2
importance(model2) # gini : 노드 불순도(불확실성) 개선에 기여하는 변수
varImpPlot(model2)

mtry = floor(1/3*14)

