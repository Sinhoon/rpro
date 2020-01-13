#################################
## <제12장_2 연습문제>
################################# 

# 01.  admit 객체를 대상으로 다음과 같이 로지스틱 회귀분석을 수행하시오.
# <조건1> 변수 모델링 : y변수 : admit, x변수 : gre, gpa, rank 
# <조건2> 7:3비율로 데이터셋을 구성하여 모델과 예측치 생성 
# <조건3> 분류 정확도 구하기 

# 파일 불러오기
setwd("c:/Rwork/data")
admit <- read.csv("admit.csv")
str(admit) # 'data.frame':	400 obs. of  4 variables:
#$ admit: 입학여부 - int  0 1 1 1 0 1 1 0 1 0 ...
#$ gre  : 시험점수 - int  380 660 800 640 520 760 560 400 540 700 ...
#$ gpa  : 시험점수 - num  3.61 3.67 4 3.19 2.93 3 2.98 3.08 3.39 3.92 ...
#$ rank : 학교등급 - int  3 3 1 4 4 2 1 2 3 2 ...

# 1. train/test data 구성 
idx <- sample(nrow(admit),nrow(admit)*0.7,replace = FALSE)
train <- admit[idx,]
test <- admit[-idx,]
# 2. model 생성 
model <- glm(admit~ .,data=train ,family = 'binomial')

# 3. predict 생성 
pred <- predict(model, newdata=test, type="response")  
pred
# 4. 모델 평가(분류정확도) : 혼돈 matrix 이용/ROC Curve 이용
cpred <- ifelse(pred >= 0.5,"yes","no")


# 1) 혼돈 matrix 이용
t <- table(cpred,test$admit)
acc <-  (69+4) / sum(t)
acc

# 2) ROCR 패키지 제공 함수 : prediction() -> performance
# f1 socre 
F1_Score(y_pred = pred, y_true = test$admit, positive = "0")
F1_Score(y_pred = pred, y_true = test$admit, positive = "1")

install.packages("MLmetrics")
library(MLmetrics)

pr <- prediction(pred,test$admit)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# 다항형 로지스틱 회귀분석
install.packages("nnet")
library(nnet)
# 이항 분류 vs 다항 분류  sigmoid(0~1) cutoff 0.5  vs softmax(0~1) 확률 합 1
names(iris)
idx <- sample(nrow(iris), nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]
model <- multinom(Species ~ . , data=train)

# 모델 평가
y_pred <- predict(model,test,type="probs")
apply(y_pred,1,sum)
range(y_pred)
y_preds <- predict(model,test,type="class")
t <- table(y_preds,test$Species)
acc <- (t[1,1] + t[2,2] + t[3,3]) /sum(t)
acc
