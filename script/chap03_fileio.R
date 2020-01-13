setwd("C:/Rwork/data")

# -------------------------------------------------------
# 파일 자료 읽기
# -------------------------------------------------------
# (1) File

student = read.table("student.txt",header=FALSE)
str(student)
student

student1 = read.table("student1.txt",header=TRUE,sep="")
student1

student2 = read.table("student2.txt",header=TRUE,sep=";")
student2

# -------------------------------------------------------
# (2)csv File

bmi <- read.csv("bmi.csv")
str(bmi)
bmi

# 'data.frame':	20000 obs. of  3 variables:
# $ height: int(숫자형)  184 189 183 143 187 161 186 144 184 165 ...
# $ weight: int(숫자형)  61 56 79 40 66 52 54 57 55 76 ...
# $ label : Factor(범주형/문자형) w/ 3 levels "fat","normal",..: 3 3 2 2 2 2 3 1 3 1 

h <- bmi$height
mean(h)

# 범주형 빈도
table(bmi$label)


# 문자형 -> 문자형  범주형변환 x
bmi2 <- read.csv("bmi.csv",stringsAsFactors = FALSE)
bmi2

# 파일 탐색기 이용
test <- read.csv(file.choose())
test

# -------------------------------------------------------
# (3)xlsx File
install.packages("xlsx")
library(xlsx)

kospi <- read.xlsx("sam_kospi.xlsx", sheetIndex = 1)
str(kospi)
head(kospi)

# 한글 엑셀 파일 일기: encoding 

st_excel <- read.xlsx("studentexcel.xlsx",sheetIndex = 1,encoding = "UTF-8")
st_excel


# 데이터 셋 제공 사이트 
# http://www.public.iastate.edu/~hofmann/data_in_r_sortable.html - Datasets in R packages
# https://vincentarelbundock.github.io/Rdatasets/datasets.html
# https://r-dir.com/reference/datasets.html - Dataset site
# http://www.rdatamining.com/resources/data

# 미국의 각 주별 1인당 소득자료  
url <- "http://ssti.org/blog/useful-stats-capita-personal-income-state-2010-2015"

# (4) 인터넷 파일 읽기
titanic <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv")
titanic
str(titanic)
dim(titanic)

table(titanic['sex'])

#교차 분할표  : 2개 범주형 변수 (행(범주1) / 열(범주2))
tab <- table(titanic$sex,titanic$survived) # 성별에따른 생존 여부
tab

barplot(tab,col=rainbow(2),main="생존여부")


# -------------------------------------------------------
# 파일 자료 저장
# -------------------------------------------------------

# 1) 화면 출력
a <- 10
b <- 20
c <- a*b
print(c)
cat('c=',c)

# 2) 파일 저장 
# read.csv <-> write.csv
# read.xlsx <-> write.xlsx
getwd()
setwd("c://rwork/data/output")

# (1) write.csv() : , 구분자
str(titanic)

# 1칼럼 제외 ,따옴표 제거, 행번호 제거
write.csv(titanic[-1],file="titanic.csv",quote = F, row.names = F)

titan <- read.csv("titanic.csv")
head(titan)

# (2) write.xlsx : 엑셀 파일 저장
library(xlsx)
write.xlsx(kospi, "kospi.xlsx",sheetName = "sheet1", row.names = F)
kos <- read.xlsx("kospi.xlsx",sheetIndex = 1)
kos
