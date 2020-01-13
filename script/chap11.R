
# chap11 _correlation 

# dataset 가져오기 
product <- read.csv("product.csv")

str(product)

# 1. 상관분석 두변수의 확률분포의 상관관계정도 나타내는 계수

cor(product,method="pearson")

cor(x= product$제품_친밀도, y =product$제품_만족도,method="pearson")

# 2. 시각화
install.packages("corrplot")
library(corrplot)

corrplot(corr = cor(product),method ="number")
corrplot(corr = cor(product),method ="circle")


install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(product)



# 공분산 : 크기 vs 상관계수 : 크기 , 방향 

cov(product)

# 제품_친밀도 제품_적절성 제품_만족도
# 제품_친밀도   0.9415687   0.4164218   0.3756625
# 제품_적절성   0.4164218   0.7390108   0.5463331
# 제품_만족도   0.3756625   0.5463331   0.6868159