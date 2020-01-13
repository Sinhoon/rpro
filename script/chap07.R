# chap07

# 실습데이터 파일 가져오기
getwd()
dataset <- read.csv("../dataset.csv")
str(dataset)

# 1. 결측치 NA 처리

# 1) 결측치 확인 
summary(dataset)
# 2) 칼럼 기준 결측치 제거  : subset()
dataset2 <- subset(dataset,!(is.na(price))) #(dataset, 조건식)
summary(dataset2)

# 3) 전체 칼럼 기준 결측치 제거 : na.omit()
dataset2 <- na.omit(dataset)
summary(dataset2)

# 4) 결측치 처리 (0으로 대체)
dataset$price2 <- ifelse(is.na(dataset$price),0,dataset$price)
head(dataset,30)
dataset[c("price","price2")]

#5) 결측치 처리 (평균으로 대체)
dataset$price3 <- ifelse(is.na(dataset$price),mean(dataset$price,na.rm=T),dataset$price)
dataset[c("price","price3")]

#6) 통계적 방법 : 고객 유형별 NA 처리
# type : 1. 우수 2. 보통 3. 저조
# 1. 우수 : 8.75*1.5
# 2. 보통 : 8.75
# 3. 저조 : 8.75*0.5
dim(dataset)
type <- rep(1:3,100)
type
dataset$type <- type 
head(dataset)

avg= mean(dataset$price)
price4 <-0 
for(i in 1:nrow(dataset)){
  if(is.na(dataset$price[i])){
  if(dataset$type[i] == 1){
    price4[i] = avg * 1.5
  }else if (dataset$type[i] == 2){
    price4[i] = avg 
  }else{
    price4[i] = avg * 0.5
  }
  }else {
    price4[i] <- dataset$price[i]
  }
}
dim(price4)  



# 2. 이상치 발견과 정제
#(1) 범주형(명목 , 서열) 극단치 처리
gender <- dataset$gender
gender

table(gender)
pie(table(gender))

dataset <- subset(dataset,gender==1 | gender==2) 

#(2) 비율척도 연속형 이상치 처리
price <- dataset$price 
plot(price)
# 이상치 발견
boxplot(price)$stats
# 이상치 정제
dataset2 <- dataset

dataset2 <- subset(dataset2 , price >=2.1 & price <=7.9)
plot(dataset2$price)
boxplot(dataset2$price)$stats


# age 변수 이상치 처리 
summary(dataset2$age)
dataset2 <- subset(dataset2, age>=20 & age <=69)


# 1) 데이터 가독성 
table(dataset2$resident)
# 1   2   3   4   5 
# 102  46  22  13  34 
dataset2$resident2[dataset2$resident==1]<- "1.서울시"
dataset2$resident2[dataset2$resident==2]<- "2.인천시"
dataset2$resident2[dataset2$resident==3]<- "3.대전시"
dataset2$resident2[dataset2$resident==4]<- "4.대구시"
dataset2$resident2[dataset2$resident==5]<- "5.광주시"


# 2) 척도변경 (연속형 -> 범주형)
dataset2$age2[dataset2$age <=30] <-"청년층"


# 4. 탐색적 분석을 위한 시각화 

setwd("c:/Rwork/data")
new_data <- read.csv("new_data.csv", header=TRUE)

# 1) 명목척도(범주/서열) vs 명목척도(범주/서열) 
# - 거주지역과 성별 칼럼 시각화 
resident_gender <- table(new_data$resident2, new_data$gender2)
resident_gender
gender_resident <- table(new_data$gender2, new_data$resident2)
gender_resident

# 성별에 따른 거주지역 분포 현황 
barplot(resident_gender, beside=T, horiz=T,
        col = rainbow(5),
        legend = row.names(resident_gender),
        main = '성별에 따른 거주지역 분포 현황') 
# row.names(resident_gender) # 행 이름 

# 거주지역에 따른 성별 분포 현황 
barplot(gender_resident, beside=T, 
        col=rep(c(2, 4),5), horiz=T,
        legend=c("남자","여자"),
        main = '거주지역별 성별 분포 현황')  

# 2) 비율척도(연속) vs 명목척도(범주/서열)
# - 나이와 직업유형에 따른 시각화 
install.packages("lattice")  # chap08
library(lattice)

colnames(new_data)
# 직업유형에 따른 나이 분포 현황   
densityplot( ~ age, data=new_data, groups = job2,
             plot.points=T, auto.key = T)
# plot.points=T : 밀도, auto.key = T : 범례 

# 3) 비율(연속) vs 명목(범주/서열) vs 명목(범주/서열)
# - 구매비용(연속):x칼럼 , 성별(명목):조건, 직급(서열):그룹   

# (1) 성별에 따른 직급별 구매비용 분석  
densityplot(~ price | factor(gender2), data=new_data, 
            groups = position2, plot.points=T, auto.key = T) 
# | 격자 : 범주형(성별), groups(그룹) : 직급 

# (2) 직급에 따른 성별 구매비용 분석  
densityplot(~ price | factor(position2), data=new_data, 
            groups = gender2, plot.points=T, auto.key = T) 
# 조건 : 직급(격자), 그룹 : 성별



# 4) 연속형 변수간의 상관분석
str(new_data)
new_data2 <- na.omit(new_data)
age <- new_data2$age
price <- new_data2$price
df <- data.frame(age,price)
cor(df)
plot(age ,price)
  