# 1. 기본 내장함수 
seq(-2, 2, by=.2) # 0.2씩 증가
seq(length=10, from=-5, by=.2) # -5부터 10개 생성 
rnorm(20, mean = 0, sd = 1) # 정규분포를 따르는 20개 데이터 생성
runif(20, min=0, max=100) # 0~100사이의 20개 난수 생성
sample(0:100, 20) # 0~100사이의 20개 sample 생성
vec<-1:10
min(vec)
max(vec)
range(vec)
mean(vec) # 평균
median(vec) # 중위수
sum(vec) 
prod(vec) # 데이터의 곱

factorial(5) # 팩토리얼=120
abs(-5)  # 절대값
sd(rnorm(10)) # 표준편차 구하기 
table(vec) # 빈도수 
sqrt(16) # 4 
4^2 # 16
# 나머지 구하기
5%%3 # 2
6%%2 # 0

user_fun1 <- function(){
  cat("user_fuc")
}
user_fun1()

# 결측치

na <- function(data){
  # 결측치 제거
  print(mean(data,na.rm=T))
  
  # 결측치  0
  tmp1 <- ifelse(is.na(data),0,data)
  print(mean(tmp1))
  
  
  # 결측치 평균
  tmp2 <- ifelse(is.na(data),mean(data,na.rm=T),data)
  print(mean(tmp2))
}

data <- c(2,10,NA,20)
na(data)

# 특수문자 처리함수

library(stringr)
library(help="stringr")  

string <- "홍길동35 이순신45 유관순 25"
name <- str_extract_all(string,"[가-힣]{3}")
name # 홍길동 이순신 유관순 list

names <- unlist(name) # list -> vector
names
s = "1,2$3)%"
as.numeric(str_replace_all(s,"\\$|\\,|\\)|\\%",""))

data_pro <- function(data){
  library(stringr)
  return (as.numeric(str_replace_all(data,"\\$|\\,|\\)|\\%","")))
}

data_pro(c("$14%2","%3"))


stock <- read.csv("c://rwork/data/stock.csv")
stock

# subset : 1~15
stock_df <- stock[c(1:15)]
str(stock_df)

# 숫자 : 특수문자 제저
stock_df[c(7:15)]
stock_num <- apply(stock_df[c(7:15)],2,data_pro)

new_stock <- cbind(stock_df[c(1:6)],stock_num)
head(new_stock)

# 1)기본 내장함수
data <- runif(21,min=0,max=100)
data
min(data)
range(data)
median(data)

sort(data)[length(data)/2]
sort(data)[11]
(sort(data)[10] + sort(data)[11]) /2

summary(data)

sum(data)

# sort / order

data(iris)
str(iris)

# $ Sepal.Length: 꽃받침 num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: 꽃잎 num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 
# 연속형 범주형(문자형)

head(iris$Sepal.Length)

# colunm 단위 
sort(head(iris$Sepal.Length)) # 내용 반환
sort(iris$Sepal.Length,decreasing=T)

# row 단위
dim()
idx <- order(head(iris$Sepal.Length)) # index 반환
iris[idx,]

summary(iris)

# 로그 지수
log10(10) # 1 -> 10^1 = 10

# 자연 로그
log(exp(2)) # e = 2.718
log(10) # 2.302585 -> e^2.302585 = 10

# 지수 
exp(2) # e^x


#로그 vs 지수

x <- c(0.12,1,12,99,999)
exp(x) # 너무 커지므로 정규화는 log 함수로 편향제거 
log(x)
# 로그하수 - 정규화 x증가하면 y 완만
# 지수함수 - 활성함수(sigmoid, softmax) x증가하면 y증가


# 3)난수 생성과 확률분포


# 형식nrom(n,mean,sd)
# 정규분포 (실수형태)
hist(rnorm(1000,0,1))


# 균등 분포 (실수형태 )
r2 <- runif(1000,min=0,max=2)
hist(r2)



# 이항분포를 따르는 난수 생성
# rbinom(n,size,prob)
# size :sample size , prob : 확률
r3 <- rbinom(10,size=1,prob= 0.5)
r4 <- rbinom(10,size=1,prob= 0.25)
r5 <- rbinom(10,size=3,prob= 0.25)
r5

# 종자값 (seed)
set.seed(124)
r <- rnorm(10)
r
