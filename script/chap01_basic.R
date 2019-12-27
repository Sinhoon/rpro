#chap01_basic

# 수업 내용 
#-----------------------------------------------------
# 1. session 정보 확인
# session : R을 실행환경
sessionInfo()
# R.ver , 다국어 , basic package

#-----------------------------------------------------
# 2. 실행 방법
# 실행     : ctrl + enter  / ctrl + r
# 자동완성 : ctrl + space
# 여러줄 주석 : ctrl + shift + c(토글)

# 줄단위 실행
a <- rnorm(n=1000)
mean(a)

# 블럭단위 실행
getwd() # 현재 작업경로
pdf("test.pdf") #open 
hist(a)         #히스토그램 
dev.off()       #close

#-----------------------------------------------------
# 3. 패키지와 데이터셋

# 1) 패키지 = function + [dataset]
# 사용가능한 패키지
dim(available.packages())
# 15328 row 17 col  약152328개 package

# 패키지 설치와 사용
install.packages("stringr")
# 패키지 in memory
library(stringr)
# 사용 가능한 패키지 
search()
# 설치 위치
.libPaths()

# 패키지 활용 
str <- "홍길동 35 이순신 45 신훈 10"
# 이름 추출
a <- str_extract_all(str,"[가-힣]{2,3}") # 정규 표현식
a[1]
b <- str_count(str,"[0-9]{2}")
b[1]

#패키지 삭제
remove.packages("stringr")

#-----------------------------------------------------
# 2) 기본 데이터 셋

data()
data("Nile") # in memory
Nile
typeof(Nile)
length(Nile)
mode(Nile)
plot(Nile)
mean(Nile)



#-----------------------------------------------------
# 4. 변수 자료형

# 1)variable : memory address store
# - r의 모든 변수는 object(reference variable)
# - 변수 선언시 타입은 없음
a <- c(1:10)


# 2)변수 작성 규치
# - 첫자는 영문자
# - 점(.)을 사용 (lr.model) , 예약어 사용 불가 ,대소문자 구분
var2 <- 10
var3 <- 2
var2 ;var3

memeber.id <- "홍"
memeber.name <- "길동"

typeof(memeber.id)


# vector vs scala
name <- c("홍길동 ", "이순신")
name[1]
ls()

#tensor : scala(0), vector(1), matrix(2)


# 3)자료형  /숫자형 논리형 문자형
int <- 100 
string <-"abc"
bool <- TRUE
mode(int) # "numeric" ,"character" , "logical"

is.numeric(int) # "true"

x<- c(100,90,11,NA,55)
is.na(x)
is.null(x[3])


# 4) 자료형 변환(casting)

# 문자 - > 숫자
num <-"123"
mode(strtoi(num))
mode(intToUtf8(123))

nu <- c(10,20,40,"50") # char로 autocasting
mode(nu)
plot(nu)

#mean(nu) 오류
mean(as.numeric(nu))

# 요인형 (factor)
# - 동일한 값을 범주로 갖는 집단변수 생성
# ex) 성별) 남(0), 여(1) -> dummy 

gender <- c("m","f","m","f","m")
mode(gender)

fgender <- as.factor(gender)
# [1] m f m f
# Levels: f m //영문자 순으로 f부터터
str(fgender)
# Factor w/ 2 levels "f","m": 2 1 2 1 2    
plot(fgender)

x <- c("m","f")
fgender2 <- factor(gender,levels = x)
str(fgender)

# mode()자료형 반환  /  class() 자료구조 반환
mode(fgender)  # numeric
class(fgender) # factor
a <- c(1,2,3)
class(a)

# factor 고려사항  // 숫자형 - > 요인형
num <- c(4,2,2,4)
fnum <- as.factor(num)
str(fnum)

# 요인형 -> 숫자형
num2 <- as.numeric(fnum) # 요인형이 숫자로 반환됨
num2

# 요인형  -> 문자형 -> 숫자형
num3 <-as.numeric(as.character(fnum))
num3

#-----------------------------------------------------
# 5. 도움말  

# 1)기본함수
mean(c(10,20,30,NA) , na.rm =TRUE) # 평균 
help(mean)
?mean

# 2)작업공간
getwd()
setwd("C:/Rwork/data")
getwd()

test <-read.csv("test.csv")
str

# obj : 402 행 - 관측치   variable : 5열 -변수 변인
# 'data.frame':	402 obs. of  5 variables:
# $ A: int  2 1 2 3 3 4 3 4 4 4 ...
# $ B: int  4 2 3 5 2 3 4 2 4 2 ...
# $ C: int  4 2 4 5 4 3 4 4 5 4 ...
# $ D: int  2 2 3 3 4 4 2 4 3 2 ...
# $ E: int  2 2 3 3 4 2 2 4 4 3 ...
