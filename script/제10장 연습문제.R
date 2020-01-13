#################################
## <제13장 연습문제>
################################# 

# 01. 중소기업에서 생산한 HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 
#  기존 구매비율  15% 보다 향상되었는지를 각 단계별로 분석을 수행하여 검정하시오.
#귀무가설(H0) : 기존 구매비율과 차이가 없다.
#연구가설(H1) : 기존 구매비율과 차이가 있다.

#조건) 구매여부 변수 : buy (1: 구매하지 않음, 2: 구매)

# 단계1 : 데이터셋 가져오기
hdtv <- read.csv("hdtv.csv")

# 단계2 :  빈도수와 비율 계산
summary(hdtv) 
length(hdtv$buy) # 50개

table(hdtv$buy)
table(hdtv$buy, useNA="ifany") # NA 빈도수 표시

hdtv[hdtv$buy==2,]


# 단계3 : 가설검정
binom.test(length(hdtv[hdtv$buy==2,]),length(hdtv$buy),0.15)
# p-value= 0.321 > 0.05 귀무가설 채택

# 단계4 : 검정결과 해설 
# 기존 구매비율과 차이가 없다.

# 02. 우리나라 전체 중학교 2학년 여학생 평균 키가 148.5cm로 알려져 있는 상태에서  
# A중학교 2학년 전체 500명을 대상으로 10%인 50명을 표본으로 선정하여 표본평균신장을 
# 계산하고 모집단의 평균과 차이가 있는지를 각 단계별로 분석을 수행하여 검정하시오.

# 단계1 : 데이터셋 가져오기
stheight<- read.csv("student_height.csv")
stheight
height <- stheight$height
head(height)

# 단계2 : 기술통계량/결측치 확인
length(height) #50
summary(height) # 149.4 

x1 <- na.omit(height)
x1 # 정제 데이터 
mean(x1) # 149.4 -> 평균신장

# 단계3 : 정규성 검정
shapiro.test(x1)
#p-value = 0.0001853 < 0.05 정규분포와 차이가 있다.

# 단계4 : 가설검정 - 양측검정
wilcox.test(x1,mu=148.5) # 비정규분포
# p-value = 0.067 >= 0.05 기존집단과 차이가 없다. 

# 03. 대학에 진학한 남학생과 여학생을 대상으로 진학한 대학에 대해서
# 만족도에 차이가 있는가를 검정하시오.

# 힌트) 두 집단 비율 차이 검정
#  조건) 파일명 : two_sample.csv, 변수명 : gender(1,2), survey(0,1)
# gender : 남학생(1), 여학생(2)
# survey : 불만(0), 만족(1)
# prop.test('성공횟수', '시행횟수')

# 단계1 : 실습데이터 가져오기
getwd()
data <- read.csv("two_sample.csv")
data
head(data) # 변수명 확인

# 단계2 : 두 집단 subset 작성
table(data$gender ,data$survey)

# 단계3 : 두집단 비율차이검증 : prop.test()
prop.test(c(138,107),c(36+138,19+107))
# 두집단 차이가 없다다

# 04. 교육방법에 따라 시험성적에 차이가 있는지 검정하시오.


#힌트) 두 집단 평균 차이 검정
#조건1) 파일 : twomethod.csv
#조건2) 변수 : method : 교육방법, score : 시험성적
#조건3) 모델 : 교육방법(명목)  ->  시험성적(비율)
#조건4) 전처리 : 결측치 제거 : 평균으로 대체 

#단계1. 실습파일 가져오기
Data <- read.csv("twomethod.csv", header=TRUE)
head(Data) #3개 변수 확인 -> id method score

#단계2. 두 집단 subset 작성
# subset 생성 
Data <- Data[c('method', 'score')] 
# score 결측치 처리 
Data$score <- ifelse(is.na(Data$score), mean(Data$score, na.rm=T), Data$score)
summary(Data)


#단계3. 데이터 분리
# 1) 집단(교육방법)으로 분리
a <- subset(Data,method==1)
b <- subset(Data,method==2)

# 2) 교육방법에서 시험성적 추출
a1 <- a$score
b1 <- b$score

#단계4 : 분포모양 검정
var.test(a1,b1)
#  p-value = 0.7302 차이가 없다

#단계5: 가설검정
# 양측 검정
t.test(a1,b1)
# p-value = 1.859e-06 < 0.05 차이가 있다.
t.test(a1,b1,alternative = "greater")
# p-value = 1 > 0.05  a1 집단 보다 b1 집단이 높은 성적을 가졋다.



# 3. 분산분석
# 두집단 이상 평균차이 검정(집단 분산 차이 검정)
# 일원배치 분산 분석 : 독립변수(x) , 종속변수 (y)
# cf) 이원배치 분산 분석 : y ~ x1 + x2  

# anova
# aov(y ~ x , data = dataset)
# 독립변수 : 집단 변수 (범주형)
# 종속변수 : 연속형 변수(비율 , 등간 척도)
# ex ) 고객별 연령대별(이산 x) 구매금액 ( 연속 y)

# 귀무가설 : 집단별 평균(분산의차이)가 없다.
# 대립가설 : 적어도 한 집에 평균차이가 있다.

#####################
# iris
#################

# 1. 변수선택 
x <- iris$Species
y <- iris$Sepal.Width
colnames(y)

# 2.데이터 전처리

# 3. 동질성 검정정
bartlett.test(Sepal.Width ~ Species, data = iris)
# 0.3515 > 0.05 차이가 없으므로 동질성 확인 aov()

# 4. 분산분석 : aov(y~x .data)
model <- aov(Sepal.Width ~ Species, data = iris)
model

# 5. 분산분석 해석
summary(model)

# F value 통해 집단간의 차이 확인
#               Df Sum Sq Mean Sq F value Pr(>F)    
# Species       2  11.35   5.672   49.16 <2e-16 ***
#   Residuals   147  16.96   0.115         

# 6. 사후검정 어떤 집단이 차이가있는지 .
TukeyHSD(model)

                        #차이정도 신뢰구간(하한가  상한가)  p-value (0.05미만은 유의미한 차이가있다.)
#                       diff         lwr        upr     p adj
# versicolor-setosa    -0.658 -0.81885528 -0.4971447 0.0000000
# virginica-setosa     -0.454 -0.61485528 -0.2931447 0.0000000
# virginica-versicolor  0.204  0.04314472  0.3648553 0.0087802

plot(TukeyHSD(model))
#신뢰구간이 0 을 포함하면 차이가 없다.

# 통계검정 : 각 집단의 평균차이
library(dplyr) # dataset %>% function
iris %>% group_by(Species) %>% summarise(avg=mean(Sepal.Width))

#   <집단>      <평균>
# 1 setosa      3.43
# 2 versicolor  2.77
# 3 virginica   2.97

a <- subset(iris,Species=='setosa',Sepal.Width)
mean(a$Sepal.Width)
# 2.77-3.43 = 사후검정 diff

#############
# 비모수 검정
############

names(iris)
x <- iris$Species
y <- iris$Sepal.Length
bartlett.test(y ~ x)
# p-value = 0.0003345 < 0.05:비모수 검정 

# 3. 분산분석 (비모수 검정 ): 평균 -> 중위수
kruskal.test(y ~ x , data=iris)

# 2.2e-16 < 0.05 집단별 차이가 있다.

iris %>% group_by(Species) %>% summarise(med=median(Sepal.Width))
# Species      med
# <fct>      <dbl>
#   1 setosa       3.4
# 2 versicolor   2.8
# 3 virginica    3 


# airquality 를 대상으로 월별 오존량에 차이가 있는지 검정
airquality
str(airquality)
#$ ozone -> 연속형 변수
#$ month ->  이산형 변수 
summary(airquality)
air <- subset(airquality,!is.na(Ozone),c(Month,Ozone)) # na.omit(air)
summary(air)
x <- air$Month
y <- air$Ozone

bartlett.test(y ~ x)
#  p-value = 0.00927 < 0.05 : 비모수검정

# 비모수검정 
md <- kruskal.test(y~x)
#  p-value =  6.901e-06 < 0.05 이므로 월별 오존량에 차이가있다
air %>% group_by(Month) %>% summarise(med=median(Ozone))

# Month   med
# <int> <dbl>
#   1     5    18
# 2     6    23
# 3     7    60
# 4     8    52
# 5     9    23


########
## quakes
#########

x <- # 집단변수
y <- quakes $ mag # 연속형

range(quakes$depth)
680 - 40 
div <- round(640/3)

quakes$depth2[quakes$depth <= (40+div)] <- "low"
quakes$depth2[quakes$depth < 253 &  quakes$depth > (40+div)] <- "low"
quakes$depth2[quakes$depth >= (253)] <- "low"

x <- quakes $ depth2
