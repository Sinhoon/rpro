# chap13_2_RandomForest

##################################################
#randomForest
##################################################
# 결정트리(Decision tree)에서 파생된 모델 
# 랜덤포레스트는 앙상블 학습기법을 사용한 모델
# 앙상블 학습 : 새로운 데이터에 대해서 여러 개의 Tree로 학습한 다음, 
# 학습 결과들을 종합해서 예측하는 모델(PPT 참고)
# DT보다 성능 향상, 과적합 문제를 해결


# 랜덤포레스트 구성방법(2가지)
# 1. 결정 트리를 만들 때 데이터의 일부만을 복원 추출하여 트리 생성 
#  -> 데이터 일부만을 사용해 포레스트 구성 
# 2. 트리의 자식 노드를 나눌때 일부 변수만 적용하여 노드 분류
#  -> 변수 일부만을 사용해 포레스트 구성 
# [해설] 위 2가지 방법을 혼용하여 랜덤하게 Tree(학습데이터)를 구성한다.

# 새로운 데이터 예측 방법
# - 여러 개의 결정트리가 내놓은 예측 결과를 투표방식(voting) 방식으로 선택 


install.packages('randomForest')
library(randomForest) # randomForest()함수 제공 

data(iris) # 범주형 

# 1. 랜덤 포레스트 모델 생성 
?randomForest
# 형식) randomForest(y ~ x, data, ntree, mtry)
model = randomForest(Species~., data=iris)  
model
names(model)

con <- model$confusion
con
(50+47+47) / nrow(iris) # 0.96(4%)

err <- model$err.rate
err
str(err) # num [1:500, 1:4]
# "OOB" "setosa" "versicolor" "virginica"
err[1,]
x <- c(0.00000000, 0.11764706, 0.05882353)

model$importance
#             MeanDecreaseGini
#Sepal.Length        10.084740
#Sepal.Width          2.323503
#Petal.Length        42.476515
#Petal.Width         44.326669


# ntree = 500
# mtry =2 : 설명변수 선택 
p <- 4
sqrt(p) # 2


# 2. 파라미터 조정 300개의 Tree와 4개의 변수 적용 모델 생성 
model = randomForest(Species~., data=iris, 
                     ntree=300, mtry=4, na.action=na.omit )
model
# Number of trees: 300
# No. of variables tried at each split: 4

# OOB estimate of  error rate: 4.67%


# 3. 최적의 파리미터(ntree, mtry) 찾기
# - 최적의 분류모델 생성을 위한 파라미터 찾기

ntree <- c(400, 500, 600) # data set size 
mtry <- c(2:4) # x size 

# 2개 vector이용 data frame 생성 
param <- data.frame(n=ntree, m=mtry)
param

for(i in param$n){ # 400,500,600
  cat('ntree = ', i, '\n')
  for(j in param$m){ # 2,3,4
    cat('mtry = ', j, '\n')
    model = randomForest(Species~., data=iris, 
                         ntree=i, mtry=j, 
                         na.action=na.omit )    
    print(model)
  }
}


# 4. 중요 변수 생성  
model3 = randomForest(Species ~ ., data=iris, 
                      ntree=500, mtry=2, 
                      importance = T,
                      na.action=na.omit )
model3 

importance(model3)
# MeanDecreaseAccuracy : 분류정확도 개선에 기여하는 변수 
# MeanDecreaseGini : 노드 불순도(불확실성) 개선에 기여하는 변수 
# Petal.Length, Petal.Width : 중요변수  

varImpPlot(model3)

################################
## 회귀트리(y변수 : 비율척도)
################################
library(MASS)
data("Boston")
str(Boston)
#crim : 도시 1인당 범죄율 
#zn : 25,000 평방피트를 초과하는 거주지역 비율
#indus : 비상업지역이 점유하고 있는 토지 비율  
#chas : 찰스강에 대한 더미변수(1:강의 경계 위치, 0:아닌 경우)
#nox : 10ppm 당 농축 일산화질소 
#rm : 주택 1가구당 평균 방의 개수 
#age : 1940년 이전에 건축된 소유주택 비율 
#dis : 5개 보스턴 직업센터까지의 접근성 지수  
#rad : 고속도로 접근성 지수 
#tax : 10,000 달러 당 재산세율 
#ptratio : 도시별 학생/교사 비율 
#black : 자치 도시별 흑인 비율 
#lstat : 하위계층 비율 
#medv(y) : 소유 주택가격 중앙값 (단위 : $1,000)

# 'data.frame':	506 obs. of  14 variables:
# medv(주택 가격) : y, 13 : x(주택 가격에 영향)

# mtree = 400 ~ 500
# mtry 결정 수식 
# 범주형 : sqrt(p)
# 연속형 : p/3
p <- 13
p / 3  # 4.33 -> 4 or 5
#(1/3) * p

model4 <- randomForest(medv ~ ., data = Boston,
                       mtree = 500, mtry = 5,
                       importance = T)

model4
# Mean of squared residuals: 9.792578

pred <- model4$predicted  # model 예측치 
real_value <- model4$y # 정답 
err <- pred - real_value # 오차(residuals) 
s_err <- err^2 # (squared)
mse <- mean(s_err)
mse # 9.792578

# model 평가 방법 
# y 범주형 - confusion matrix 
# y 연속형 - MSE( Mean of squared Error) 

importance(model4)
varImpPlot(model4)


################################
## 분류트리(y변수 : 범주형)
################################
titanic = read.csv(file.choose()) #
str(titanic) 
# titanic3.csv 변수 설명
#'data.frame': 1309 obs. of 14 variables:
#1.pclass : 1, 2, 3등석 정보를 각각 1, 2, 3으로 저장
#2.survived : 생존 여부. survived(생존=1), dead(사망=0)
#3.name : 이름(제외)
#4.sex : 성별. female(여성), male(남성)
#5.age : 나이
#6.sibsp : 함께 탑승한 형제 또는 배우자의 수
#7.parch : 함께 탑승한 부모 또는 자녀의 수
#8.ticket : 티켓 번호(제외)
#9.fare : 티켓 요금
#10.cabin : 선실 번호(제외)
#11.embarked : 탑승한 곳. C(Cherbourg), Q(Queenstown), S(Southampton)
#12.boat     : (제외)Factor w/ 28 levels "","1","10","11",..: 13 4 1 1 1 14 3 1 28 1 ...
#13.body     : (제외)int  NA NA NA 135 NA NA NA NA NA 22 ...
#14.home.dest: (제외)


# 삭제 칼럼 : 3, 8, 10, 12~14
titanic_df <- titanic[, -c(3, 8, 10, 12:14)]
dim(titanic_df)  # 1309    8  

# survived : y변수(더미변수) 
class(titanic_df$survived) # integer(0, 1) : 숫자 
titanic_df$survived <- factor(titanic_df$survived)
class(titanic_df$survived) # factor(0, 1) : 집단변수 

# 1. randomForest 모델 생성 
# 설명변수 선택 : p = 7
# tree = 500, mtry = ?
p <- 7
mtry <- sqrt(p) # 2.6

# y변수 : factor형 
titanic_model <- randomForest(survived ~ ., 
                              data = titanic_df,
                              ntree = 500, mtry=3,
                              importance = T,
                              na.action = na.omit)
titanic_model
100-19.33 # 80.67

# 2. 중요변수 시각화 
varImpPlot(titanic_model)
# 중요변수 - sex > pclass > age > fare(티켓요금)
