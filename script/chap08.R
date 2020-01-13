# chap08_hypothesis

# 가설(hypothesis)  어떤 사건 설명하기위한 가정 - 가설(hypothesis)
# 검정(Test)        검정통계량(표본)으로 가설 채택 or 기각과정
# 추정              표본을 통해서 모집단을 확률적으로 추정 
# 신뢰구간          모수를 포함하는 구간(채택역), 벗어나면(기각역)
# 유의수준          알파 표시 , 오차범위 기준
# 구간추정          신뢰구간과 검정통계량 비교해서 가설 기각 유무 결정



# 귀무가설(HO) : 중2 남학생 평균키는 165.2cm 차이가 없다.
# 대립가설(H1) : 중2 남학생 평균키는 165.2cm 차이가 있다.

#----------------------------------------------------------------
# 1. 모집단에서 표본 추출(1000학생)

x <- rnorm(1000,mean=165.2,sd=1) 
hist(x)

# 정규성 검정
# 귀무가설 : 정규분포와 차이가없다.
shapiro.test(x)
# W = 0.99779       - 검정통계량으로 유의확률 확인
# p-value = 0.2059  - 0.05보다 p값이 크므로 귀무가설 채택



#----------------------------------------------------------------
# 2.평균차이 검정 : 평균 : 165.2cm
t.test(x,mu=165.2)

# t값 , df(자유도) : 검정통계량   //  p-value : 유의확률 

# data:  x
# t = 0.046705, df = 4999, p-value = 0.9628
# alternative hypothesis: true mean is not equal to 165.2
# 95 percent confidence interval:
#   165.1729 165.2284
# sample estimates:
#   mean of x 
# 165.2007    -> 실제평균 (mean(x)) - 표본의 통계

# [해설] 검정통계량이 신뢰구간에 포함되므로 모수의 평균키 165.2cm와 차이가 없다고 볼수있다.


#----------------------------------------------------------------
# 3. 기각역의 평균검정
t.test(x,mu=165.09,conf.level = 0.95) # 신뢰수준 95

#----------------------------------------------------------------
# 4. 신뢰수준 변경
t.test(x,mu=165.2,conf.level = 0.99)
# 신뢰수준 향상 -> 신뢰구간 넓어짐(귀무가설 기각 더 어려워짐)

#----------------------------------------------------------------
# 표준화 vs 정규화

# 1 .표준화 : 정규분포 -> 표준 정규분포(0,1)
# 정규분포
n <- 2000
x <- rnorm(n,mean=100,sd=10)
shapiro.test(x)

# 표준화 공식 (z) = (x-mu)/sd(x)
z  <- (x - mean(x) )/ sd(x)
hist(z)

# scale() 함수 표준화 함수
z2 <- as.data.frame(scale(x)) # matrix -> data.frame
str(z2)
hist(z2$V1)

# 2.정규화 
# - 특정변수값을 일정한 범위(0~1)로 일치시키는 과정
str(iris)
summary(iris[-5])

iris_nor <- as.data.frame(scale(iris[-5]))
summary(iris_nor)

# 2)정규화 함수정의(0~1)

nor <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

iris_nor2 <- apply(iris[-5],2,nor)
summary(iris_nor2)


# 3. 데이터셋 샘플링 

#sample(x,100,replace= FALSE) # 비복원 추출

no <- 1: 100
score <- runif(100,min=40,max=100)
score

df <- data.frame(no,score)
nrow(df)
idx <- sample (x = nrow(df),size=15)
idx
sam <- df[idx,]
sam


# train/test 70% 30% dataset
idx <- sample(x = nrow(df),size= x*0.7)
idx

train <- df[idx,]
test  <- df[-idx,]
dim(test)


# 50% vs 50% 
idx <- sample(nrow(iris),nrow(iris)*0.5)
train <- iris[idx,]
test <- iris[-idx,]

head(iris)
# speal.length : y (종속변수)
# petal.length : x (독립변수)
# model : x - > y

model <- lm(Sepal.Length ~Petal.Length ,data = train)
pred  <- model$fitted.values # 예측치 (y 예측)
pred

model2 <- lm(Sepal.Length ~Petal.Length ,data = test)
pred2 <- model2$fitted.values
pred2
predict(model, newdata=test$Pepal.Length, interval="confidence")

# train_y 
train_y <- train$Sepal.Length # 정답

# train_y 
test_y <- test$Sepal.Length # 정답

# 정답 vs 예측치
plot(train$Petal.Length,train_y , col="blue")
points(train$Petal.Length,pred , col="red")

# 범례
legend("topleft" ,legend = c("예측","실제") ,col =c('red','blue') ,pch=c(18,19))
