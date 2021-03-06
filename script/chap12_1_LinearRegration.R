# chap12_1_LinearRegration

# 1. 단순선형회귀분석

# - 독립변수(1) -> 종속변수(1) 영향 미치는 영향 분석

setwd("c:/dev/Rwork/data")
product <- read.csv("product.csv")
str(product)

# 1) x,y 변수 선택
x <- product$"제품_적절성" # 독립변수
y <- product$"제품_만족도" # 종속변수

df <- data.frame(x, y)
df

# 2) 회귀모델 생성
model <- lm(y ~ x , data = df)
model

# Coefficients: 회귀계수(기울기, 절편)
# 0.7789(절편)       0.7393(기울기)

# 회귀 방정식(y) = 0.7393x + 0.7789
head(df)
# x=4, y=3
x = 4
Y = 3
y = 0.7393 * x + 0.7789
y # 3.7361
 
# 오차 = 관측치(정답) - 예측치
err <- Y - y 
err # -0.7361
abs(err)
mse = mean(err^2) #평균제곱오차

mse # 제곱(부호+, 패널티)

names(model)
# "coefficients"  : 계수
# "residuals"   : 오차(잔차)
# "effects"   : 적합치(예측치)

model$coefficients
model$residuals[1]
model$fitted.values[1] # 3.735963

# 3) 회귀모델 분석
summary(model)

# <회귀모델 분석 순서>
# 1. F검정 통계량 : 모델의 유의성 검정(p < 0.05)
# 2. 모델의 설명력 : Adjusted R-squared(0.5865)
# 3. x변수 유의성 검정 : 영향력 판단(p < 0.05)


# R-squared = R^2
R <- sqrt(0.5865)
R

# 4) 회귀선 : 회귀방정식에 의해서 구해진 직선(예측치)

# X, Y 산점도
plot(df$x, df$y)
# 회귀선(직선)
abline(model, col="red")


# 2. 다중선형회귀분석
# - 독립변수(n) -> 종속변수(1) 미치는 영향 분석

install.packages("car")
library(car)

Prestige
str(Prestige)
# 'data.frame':	102 obs. of  6 variables:

# 102 직업군 대상 : 교육수준, 수입, 여성비율, 평판, 직원수, 유형
# 'data.frame':	102 obs. of  6 variables

row.names(Prestige)

# subset
newdata <- Prestige[c(1:4)]
str(newdata)

# 상관분석
cor(newdata)

model <- lm(income ~ education + women + prestige , data=newdata)
model

head(newdata)

y_pred <- 177.2*newdata$education + -50.9*newdata$women + 141.4*newdata$prestige -253.8
y_pred[1]
newdata$income[1]
err <- y_pred[1] - newdata$income[1]
err # 1121.432
mse = mean(err^2)
mse

summary(model)
# 모델 유의성 검정 : 2.2e-16 유의하다.
# Adjusted R-squared:  0.5865  설명력 

# (Intercept) -253.850   1086.157  -0.234    0.816    
# education    177.199    187.632   0.944    0.347      --- 영향 없음
# women        -50.896      8.556  -5.948 4.19e-08 ***
# prestige     141.435     29.910   4.729 7.58e-06 ***




####################
# 3) 변수 선택법
##################

newdata2 <- Prestige[c(1:5)]
dim(newdata2)

library(MASS)
model2 <- lm(income ~ . ,data=newdata2)


stepAIC(model2,direction = "both")


###############
# 4. 기계학습
#################
iris_data <- iris[-5]
idx <- sample(x=nrow(iris_data),size=nrow(iris_data)*0.7,replace=FALSE)

idx
train <- iris_data[idx,]
test <- iris_data[-idx,]

dim(train)
dim(test)

model <- lm(Sepal.Length ~ . ,data=train)
model

y_pred <- predict(model , test)
y_true <- test$Sepal.Length

# 평가 : mse
mse <- mean((y_pred - y_true)^2)
mse
cor(y_pred,y_true)

#############################3

##########################################
##  5. 선형회귀분석 잔차검정과 모형진단
##########################################

# 1. 변수 모델링  
# 2. 회귀모델 생성 
# 3. 모형의 잔차검정 
#   1) 잔차의 등분산성 검정
#   2) 잔차의 정규성 검정 
#   3) 잔차의 독립성(자기상관) 검정 
# 4. 다중공선성 검사 
# 5. 회귀모델 생성/ 평가 


names(iris)

# 1. 변수 모델링 : y:Sepal.Length <- x:Sepal.Width,Petal.Length,Petal.Width
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width


# 2. 회귀모델 생성 
model <- lm(formula = formula,  data=iris)
model
names(model)


# 3. 모형의 잔차검정
plot(model)
#Hit <Return> to see next plot: 잔차 vs 적합값 -> 패턴없이 무작위 분포(포물선 분포 좋지않은 적합) 
#Hit <Return> to see next plot: Normal Q-Q -> 정규분포 : 대각선이면 잔차의 정규성 
#Hit <Return> to see next plot: 척도 vs 위치 -> 중심을 기준으로 고루 분포 
#Hit <Return> to see next plot: 잔차 vs 지렛대값 -> 중심을 기준으로 고루 분포 

# (1) 등분산성 검정 
plot(model, which =  1) 
methods('plot') # plot()에서 제공되는 객체 보기 

# (2) 잔차 정규성 검정
attributes(model) # coefficients(계수), residuals(잔차), fitted.values(적합값)
res <- residuals(model) # 잔차 추출 = model$residuals
shapiro.test(res) # 정규성 검정 - p-value = 0.9349 >= 0.05
# 귀무가설 : 정규성과 차이가 없다.

# 정규성 시각화  
hist(res, freq = F) 
qqnorm(res)

# (4) 잔차의 독립성(자기상관 검정 : Durbin-Watson)  # 독립변수 간의 강한 상관관계
install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model) # 더빈 왓슨 값


# 4. 다중공선성 검사 
library(car)
sqrt(vif(model)) > 2 # TRUE 

# 5. 모델 생성/평가 
formula = Sepal.Length ~ Sepal.Width + Petal.Length 
model <- lm(formula = formula,  data=iris)
summary(model) # 모델 평가
