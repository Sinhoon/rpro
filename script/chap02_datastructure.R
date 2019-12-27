#chap02_DataStructure

# 1. vector 자료구조
# - 1차원 배열구조 
# - 동일 자료형을 갖는 1차원 배열 구조 
# - c , seq , rep

# c()함수
x <- c(1:5)
x
y <- c(1,3,5)

# seq() 함수
seq(1,9,by=2) #(start,end,step)
seq(9,1,by=-2) #(start,end,step)

# rep() 
rep(1:3,each=3) #(var,num,each=(각각))

# index

a <-c(1:100)
a[50:60]
length(a[50:60])
a[length(a)-5:length(a)]

a[a>10 & a<40]


#2. matirx 자료구조 
# 동일한 자료형을 갖는 2차원(행,열) 배열
# matrix(), rbind(), cbind()

m1 = matrix(c(1:31),nrow = 3)
dim(m1)

x1 <- c(1:5)
x2 <- 5:1

rbind(x1,x2)

cbind(x1,x2)

m1
m1[,1]
m1[1,]
m1[c(2:3),4:7]

x <- matrix(1:9,nrow =3 , ncol =3 ,byrow =TRUE)
x

colnames(x) <- c("one","two","three")
x

# broadcast 연산
# scaler vs vector
x <- 1:10
x * 0.5  # scaler * vector = vector

# vector vs matrix
x <- 1:3
y <- matrix(rep(1,6),nrow=2,byrow=FALSE)
y
dim(y)
z <- x*y
z

# apply() : 처리
apply(z,2,sum) # 1 행단위 2 열단위
apply(z,2,max) 
apply(z,2,mean) 

# 3. array

arr <- array(1:12,c(3,2,2))
arr

arr[,,1]


# 4. data frame 
# 행렬 구조갖는 자료구조 col단위 자료형일치(table 비슷)

no <- 1:3
name <- c("a","b","c")
age <- c(35,45,20)
pay <- c(200,300,150)

df <- data.frame(No=no,Name=name,Age=age,Pay=pay)

str(df['Name'])
epay <- df$Pay
str(epay)

# 산포도 : 분산과 표준편차
var(epay)
sqrt(var((epay)))
sd(epay)

score <- c(90,85,93)
var(score)

# 분산 =  sum((x-산술평균)^2) / n-1(표본)  n(모집단)

avg <- mean(score)
diff <- (score - avg)^2
sum(diff)/(length(score)-1)

# list 서로다른 자료형과 자료구조를 갖는 자료구조(1,2,3)이다
# key value 구조
lst1<- list('lee',"이순신",34)
lst1
# [[1]] -> key
# [1] lee->value
lst1[[1]]  # key를 가지고  value 접근
lst1[1]  # index를 가지고 key value

lst2<- list('a'=c(1,2,3),"이순신",34)
lst2
lst2$a[1]

install.packages(("stringr"))
library(stringr)

str <- "김 12 이 13 디 14"
names <- str_extract_all(str,"[가-힣]{1}")
class(names)
names[[1]][1]

# key [[1]]
# [[1]]
# [1] "김" "이" "디"
  

# 2) key = value
member = list(name= c("홍길동","이순신"), age =c(12,13))
member$name[1]
a = data.frame(member)
class(a)
a
a$age

# list$key  data.frame$column
