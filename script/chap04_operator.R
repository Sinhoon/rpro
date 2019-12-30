# chap04_operator
# <실습> 산술연산자 
num1 <- 100 # 피연산자1
num2 <- 20  # 피연산자2
result <- num1 + num2 # 덧셈
result # 120
result <- num1 - num2 # 뺄셈
result # 80
result <- num1 * num2 # 곱셈
result # 2000
result <- num1 / num2 # 나눗셈
result # 5

result <- num1 %% num2 # 나머지 계산
result # 0

result <- num1^2 # 제곱 계산(num1 ** 2)
result # 10000
result <- num1^num2 # 100의 20승
result # 1e+40 -> 1 * 10의 40승과 동일한 결과


# <실습> 관계연산자 
# (1) 동등비교 
boolean <- num1 == num2 # 두 변수의 값이 같은지 비교
boolean # FALSE
boolean <- num1 != num2 # 두 변수의 값이 다른지 비교
boolean # TRUE

# (2) 크기비교 
boolean <- num1 > num2 # num1값이 큰지 비교
boolean # TRUE
boolean <- num1 >= num2 # num1값이 크거나 같은지 비교 
boolean # TRUE
boolean <- num1 < num2 # num2 이 큰지 비교
boolean # FALSE
boolean <- num1 <= num2 # num2 이 크거나 같은지 비교
boolean # FALSE

# <실습> 논리연산자(and, or, not, xor)
logical <- num1 >= 50 & num2 <=10 # 두 관계식이 같은지 판단 
logical # FALSE
logical <- num1 >= 50 | num2 <=10 # 두 관계식 중 하나라도 같은지 판단
logical # TRUE

logical <- num1 >= 50 # 관계식 판단
logical # TRUE
logical <- !(num1 >= 50) # 괄호 안의 관계식 판단 결과에 대한 부정
logical # FALSE

x <- TRUE; y <- FALSE
xor(x,y) # [1] TRUE
x <- TRUE; y <- TRUE
xor(x,y) # FALSE


x <-10
y <-5
z <- x * y
z 
if(score >=50 ){
  cat (score,"=50이상입니다") 
}else{
  cat (score,"=50미만입니다")   
} 


score <- scan() #키보드입력

if(score >=50 ){
  cat (score,"=50이상입니다") 
}else{
  cat (score,"=50미만입니다")   
}

grade <- ""
if(score >=90){
  grade <-"A"
}else if(score >=80){
  grade <-"B"
}else{
  grade <-'D'
}
print(grade)

score <- c(70,90,NA,40,50)
result <- ifelse(score>70 ,"합","불합")
result

result2 <- ifelse(is.na(score),0,score)
result2
mean(result2)
mean(score,na.rm = T)

# which() 조건에 해당하는위치() index 반환
x <- seq(1,10,3)
x # 1 4 7 10
which(x==7)

emp <- read.csv("c:/Rwork/data/emp.csv")
emp[1,2]
emp$name[1] ="유관순"

index <- which(emp$name == "유관순")
index

emp_sub <- emp[index,]
emp_sub

library("MASS")
data("Boston")
str(Boston)
# medv 종속변수 
# 1~13 독립변수
cols <- names(Boston)
idx <- which(cols=="medv")
idx  

y <- Boston[,idx]
x <- Boston[,-idx]
plot(x$crim,y)
dim(x) # data frame
y # vector


# 반복문

x <- rnorm(n=1000,mean=0,sd=1)
idx <- 1 # index
for(v in x){
  cat("v=",v,"\n")
  y[idx] <- v^2
  idx <- idx + 1
}
plot(x,y)

num <- scan()
num

st <- read.table("c://rwork/data/student.txt")
st
colnames(st) <- c("id","name","height","weight")
st

# if else for
height2 <- ifelse(st$height >= 180 , "high","low")
st$height2 <- height2
weight2 <- ifelse(st$weight >= 70 , "high","low")
st$weight2 <- weight2
st
