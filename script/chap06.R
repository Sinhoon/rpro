#chap06_DataVisualization

#1.이산변수 (discrete quantitative data) 시각화
# - 정수단위로 나누어 측정 

# 1)막대차트 시각화
# 차트 데이터 생성
chart_data <- c(305,450, 320, 460, 330, 480, 380, 520) 
names(chart_data) <- c("2016 1","2017 1","2016 2","2017 2","2016 3","2017 3","2016 4","2017 4")
str(chart_data)
chart_data


# 세로
barplot(chart_data, ylim=c(0,600), col=c("green","red"),main="분기별 매출")
# 가로
barplot(chart_data, Xlim=c(0,600), col=rainbow(8),horiz=TRUE,main="분기별 매출")


par(mfrow=c(1,2))
# 개별 
barplot(VADeaths, beside=T,col=rainbow(5),main="미국 버지니아주 하위계층 사망비율")
# 누적
barplot(VADeaths, beside=F,col=rainbow(5),main="미국 버지니아주 하위계층 사망비율") 
legend(19, 71, c("50-54","55-59","60-64","65-69","70-74"), cex=0.8,fill=rainbow(5))


# 2) 점차트 시각화
par(mfrow=c(1,1))
dotchart(chart_data,color=c("green","red"),labels=names(chart_data),xlab ="매출액",main="분기별매출출")


# 3) 파이 차트 시각화
pie(chart_data , labels = names(chart_data),color=c("green","red"), cex = 1.5)
title("2016 vs 2017 판매현황")



# 연속변수
# 1) boxplot
VADeaths
str(VADeaths)
summary(VADeaths)
boxplot(VADeaths,range=1000)

# 2) histogram
iris
range(iris$Sepal.Length)
hist(iris$Sepal.Length , xlab ="s leng" , col= "magenta" , main ="길이" , xlim = c(3,8))

# 기본 가설 : 정규분포와 차이가 없다.
# 정규성 검정
shapiro.test(iris$Sepal.Length)
#  p-value = 0.01018 유의수준 < 알파(0.05) 기준 : x  기각 
shapiro.test(iris$Sepal.Width)
# p-value = 0.1012  유의수준 > 알파(0.05) 기준 : 0 채택


# 3) 선점도 시각화
price <- runif(10,min=1,max=100)
plot(price)

plot(price, type="o")
plot(price, type="l")
plot(price, type="h")
plot(price, type="s")

data()
AirPassengers
plot(AirPassengers)

# 회귀모델 -> 회귀모델 시각화 자료
install.packages("HistData")
library(HistData)
data(Galton)
str(Galton)
HistData::Galton

model <- lm(child ~ parent, data=Galton) 
model 
# 계수 
# (Intercept)절편       parent 기울기  
# 23.9415               0.6463  

plot(model)
methods(plot)

# 3. 변수간의 비교 시각화
pairs(iris[1:4])

# 꽃의 종별 변수 비교
unique(iris$Species)

iris
pairs(iris[iris$Species=='virginica',1:4])

# 4.차트 파일 저장 
setwd("c:/Rwork/data/output")
jpeg("iris.jpg",width=720,height=480)
plot(iris$Sepal.Length , iris$Sepal.Width)
title("iris data")
dev.off()



#########################
### 3차원 산점도 
#########################
install.packages('scatterplot3d')
library(scatterplot3d)


# 꽃의 종류별 분류 
iris_setosa = iris[iris$Species == 'setosa',]
iris_versicolor = iris[iris$Species == 'versicolor',]
iris_virginica = iris[iris$Species == 'virginica',]

# scatterplot3d(밑변, 오른쪽변, 왼쪽변, type='n') # type='n' : 기본 산점도 제외 
d3 <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length, iris$Sepal.Width, type='n')

d3$points3d(iris_setosa$Petal.Length, iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, bg='orange', pch=21)

d3$points3d(iris_versicolor$Petal.Length, iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width, bg='blue', pch=23)

d3$points3d(iris_virginica$Petal.Length, iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, bg='green', pch=25)
