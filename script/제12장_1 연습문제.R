#################################
## <제12장 연습문제>
################################# 

# 01. mpg의 엔진크기(displ)가 고속도록주행마일(hwy)에 어떤 영향을 미치는가?    
# <조건1> 단순선형회귀모델 생성 
# <조건2> 회귀선 시각화 
# <조건3> 회귀분석 결과 해석 : 모델 유의성검정, 설명력, x변수 유의성 검정  


library(ggplot2)
data(mpg)
mpg

x <- mpg$'displ'
y <- mpg$'hwy'

df <- data.frame(x, y)

mpg_model <- lm(y~ x, data = df)
mpg_model


plot(df$x, df$y)

# text 추가
text(df$x, df$y, labels = df$y,
     cex=0.7, pos=2, col = 'red')

abline(mpg_model, col='blue')
summary(mpg_model)


# 02. product 데이터셋을 이용하여 다음과 같은 단계로 다중회귀분석을 수행하시오.
setwd("c:/dev/Rwork/data")
product <- read.csv("product.csv", header=TRUE)
product

#  단계1 : 학습데이터(train),검정데이터(test)를 7 : 3 비율로 샘플링
idx <- sample(nrow(product), nrow(product)*0.7 ,replace =FALSE)
train <- product[idx,]
test <- product[-idx,]
#  단계2 : 학습데이터 이용 회귀모델 생성 
#           변수 모델링) y변수 : 제품_만족도, x변수 : 제품_적절성, 제품_친밀도

model <- lm(제품_만족도 ~ . ,data =train)
summary(model)

#  단계3 : 검정데이터 이용 모델 예측치 생성 
py <- predict(model,test)
pt <- test$제품_만족도

#  단계4 : 모델 평가 : cor()함수 이용  
mean((py-pt)^2)
cor(py,pt)

plot(pt, col="blue" ,type="o" , pch=18)
points(py ,col="red" ,type="o" , pch=18)
legend("topleft",legend = c('real','pred'),col=c('blue','red'),pch = c(18,19))
# 03. ggplot2패키지에서 제공하는 diamonds 데이터 셋을 대상으로 
# carat, table, depth 변수 중 다이아몬드의 가격(price)에 영향을 
# 미치는 관계를 다중회귀 분석을 이용하여 예측하시오.
#조건1) 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
#조건2) 다중회귀 분석 결과를 정(+)과 부(-) 관계로 해설

library(ggplot2)
data(diamonds)

