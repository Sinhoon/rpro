# chap10 anova t-test
getwd()
setwd("./data")
data <- read.csv("one_sample.csv")
head(data)


# 2)빈도 /통계량 
x <- data$survey99
summary(x)
table(x)
length(x)


#3) 가설검정
          #성공횟수 , 시행횟수 , 확률 p 
binom.test(14,length(x),0.2)
# p-value = 0.0006735 < 0.05 : 귀무가설 기각 따라서 차이가 있다.

binom.test(14,150,0.2,alternative = "two.sided" , conf.level = 0.95) 
# 양측검정 95%신뢰수준 


# 대립가설 채택 : 방향성이 있는 가설
# 2014(20%) > < 2015(14)  => 단측검정 이용

# 귀무가설 : 앞쪽의 오른쪽 이랑 차이가 없다 - 귀무가설 채택 
binom.test(14,150,p=0.2,alternative = "greater")
# p-value = 0.9999 > 0.05  

# 귀무가설 : 앞쪽의 왼쪽 이랑 차이가 없다  - 귀무가설 기각 
binom.test(14,150,p=0.2,alternative = "less")
# p-value = 0.0003179 < 0.05 


# 1-2 . 단일집단 평균차이 검정  (정규분포여부에 따른 모수 비모수 검정)

# 1) data 가져오기
data <- read.csv("one_sample.csv")
str(data)

# 2) 데이터 전처리  이상치 결측치 제거 
time <- data$time
mean(time, na.rm=T)
x <- na.omit(time)
length(x)

# 3) 전제조건 
shapiro.test(x)
# p-value >= 0.05 정규분포와 차이가없다  => 모수검정

# 4) 단일지단 평균차이 검정
t.test(x,mu=5.2)
# p-value = 0.0001417 <  0.05 평균차이가 있다.
# t : -1.96 ~ 1.96

# 5) 평군차이가 있으므로 방향성 검정

t.test(x,mu=5.2,alternative = "greater")
# p-value = 7.083e-05 < 0.05  x의 오른쪽이랑 차이가 있다. 

t.test(x,mu=5.2,alternative = "less")
# p-value = 0.9999 > 0.05  x의 왼쪽이랑 차이가 없다. 

#따라서 국내 노트북  사용시간보다  a회사 노트북 사용시간이 더 길다.


# 2-1 두집단 비율차이 검정
# 1) 실습데이터 가져오기 
getwd()
data <- read.csv("two_sample.csv", header=TRUE)
data
head(data) # 변수명 확인

# 2) 두집단 SUBSET 작성 
data$method # 1, 2 -> 노이즈 없음
data$survey # 1(만족), 0(불만족)
# 데이터 정체/전처리
x<- data$method # 교육방법(1, 2) -> 노이즈 없음
y<- data$survey # 만족도(1: 만족, 0:불만족)
table(x,y)

# 3) 두집단 비율차이검정 - prop.test()

help(prop.test) # prop.test(x,n,p, alternative, conf.level, correct)
# 양측검정
prop.test(c(110,135),c(150,150)) # 방법A 만족도와 방법B 만족도 차이 검정
# p-value = 0.0003422
#sample estimates: 집단 간 비율
# prop 1 prop 2
#0.7333333 0.9000000
prop.test(c(110,135),c(150,150), alternative="two.sided", conf.level=0.95)
# 해설) p-value = 0.0003422 - 두 집단간의 만족도에 차이가 있다.

# 단측검정
prop.test(c(110,135),c(150,150), alter="greater", conf.level=0.95)
# 해설) p-value=0.9998 : 방법A가 방법B에 비해 만족도가 낮은 것으로 파악


# 4) 두집단 평균차이검정 

#1. 실습파일 가져오기
data <- read.csv("c:/Rwork/Part-III/two_sample.csv", header=TRUE)
data
print(data)
head(data) #4개 변수 확인
summary(data) # score - NA's : 73개
#2. 두 집단 subset 작성(데이터 정제,전처리)
result <- subset(data, !is.na(score), c(method, score))
# c(method, score) : data의 전체 변수 중 두 변수만 추출
# !is.na(score) : na가 아닌 것만 추출
# 위에서 정제된 데이터를 대상으로 subset 생성
result # 방법1과 방법2 혼합됨
length(result$score) # 227

# 데이터 분리
#1) 교육방법 별로 분리
a <- subset(result,method==1)
b <- subset(result,method==2)
#2) 교육방법에서 점수 추출
a1 <- a$score
b1 <- b$score
# 기술통계량 -> 평균값 적용 -> 정규성 검정 필요
length(a1); # 109
length(b1); # 118

#3. 분포모양 검정 : 두 집단의 분포모양 일치 여부 검정
# 귀무가설 : 두 집단 간 분포의 모양이 동질적이다.
# 두 집단간 동질성 비교(분포모양 분석)
var.test(a1, b1) # p-value = 0.3002 -> 차이가 없다.
# 동질성 분포 : t.test()
# 비동질성 분포 : wilcox.test()
#4. 가설검정 – 두 집단 평균 차이검정
t.test(a1, b1)
t.test(a1, b1, alter="two.sided", conf.int=TRUE, conf.level=0.95)
# p-value = 0.0411 - 두 집단간 평균에 차이가 있다.
t.test(a1, b1, alter="greater", conf.int=TRUE, conf.level=0.95)
# p-value = 0.9794 : a1을 기준으로 비교 -> a1이 b1보다 크지 않다.
t.test(a1, b1, alter="less", conf.int=TRUE, conf.level=0.95)
# p-value = 0.02055 : a1이 b1보다 작다.
