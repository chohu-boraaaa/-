library(HSAUR2)
library(dplyr)
FM <- filter(household, gender == 'female')
M <- filter(household, gender == 'male')
library(lattice)

# 1번 문제
#평행좌표그림
parallelplot(FM[,1:4], main = 'female', col='#ff0000')
parallelplot(M[,1:4], main = 'male', col='#0000ff')

# 상자그림
par(mfrow=c(1,2))
boxplot(FM[,1:4], main = 'female')
boxplot(M[,1:4], main = 'male')

# 별그림
stars(FM[,1:4], main = 'female')
stars(M[,1:4], main = 'male')

# 버블차트
head(FM)
symbols(FM$housing, FM$food, circle = sqrt(FM$goods), 
        inches = FALSE, bg=1:nrow(FM), main='female')
symbols(M$housing, M$food, circle = sqrt(M$goods), 
        inches = FALSE, bg=1:nrow(FM), main='male')
symbols(FM$housing, FM$food, circle = sqrt(FM$service), 
        inches = FALSE, bg=1:nrow(FM), main='female')
symbols(M$housing, M$food, circle = sqrt(M$service), 
        inches = FALSE, bg=1:nrow(FM), main='male')

# 2번 문제
library(HSAUR2)
d <- heptathlon
head(heptathlon)
d
require(grDevices)
# 시각화
hep_st = stars(d[, 1:8], flip.labels = FALSE, key.loc = c(15, 2),
     draw.segments = TRUE, col.lines = NA, main = 'Seoul Olympic heptathlon')
stars(d, locations=c(0,0), radius = FALSE, key.loc = c(0,0), 
      main = 'Seoul Olympic heptathlon', lty = 2, lwd = 1.8, col.lines = 1:nrow(d))
# 상관분석
pairs(d, pch=21, bg = c("red", "orange", "yellow", "green", "blue", "purple", "pink", "gray"))
# javelin이 분산이 크다 아마 창살은 

# 다변량 이상치 식별
library(mvoutlier)
aq.plot(d)

# 다변량 정규성 검토
library(mvnormtest)
mshapiro.test(t(as.matrix(d))) # 기각할 확률 높음

# 다변량 정규성 시각화
x <- as.matrix(d)
center <- colMeans(x)
n <- nrow(x)
p <- ncol(x)
cov <- cov(x)
dis <- mahalanobis(x, center, cov)
qqplot(qchisq(ppoints(n), df=p), dis, main = 'normality')
abline(a=0, b=1)

# 3번
library(HSAUR2)
# 지역별로 여성과 배우자 소득 자료 시각화
str(CHFLS)
# boxplot 이용
boxplot(R_income~ R_region, data = CHFLS)
boxplot(A_income~ R_region, data = CHFLS)

# 지역별로 여성과 배우자 소득의 자료의 산포 동일성 검정
# 여성은 비동일
fligner.test(R_income~ R_region, data = CHFLS)
# 등-산포변환
library(car)
library(MASS)
spreadLevelPlot(R_income+1 ~ R_region, CHFLS)
boxplot(((R_income+1)^0.5-1)/0.5 ~ R_region, data=CHFLS)
fligner.test(((R_income+1)^0.5-1)/0.5 ~ R_region, data = CHFLS)
fligner.test(((R_income+1)^0.5-1)/0.5 ~ R_region, data = CHFLS)

# 남성은 지역별로 비동일
fligner.test(A_income~ R_region, data = CHFLS)
spreadLevelPlot(A_income+1 ~ R_region, CHFLS)
boxplot(((A_income+1)^0.5-1)/0.5 ~ R_region, data=CHFLS)
fligner.test(((A_income+1)^0.5-1)/0.5 ~ R_region, data = CHFLS)

# (c)
# 지역별 남성과 여성의 수입에 따른 행복 상태 파악
attach(CHFLS)
symbols(x=CHFLS$R_income, y=CHFLS$A_income, squares=CHFLS$R_happy, fg=CHFLS$R_region)
install.packages('rgl')
library('rgl')
plot3d(CHFLS$R_income, CHFLS$A_income, CHFLS$R_happy)


# 4번
library(HSAUR2)
# (a)
head(heptathlon)

# 산점도
plot(heptathlon$longjump, heptathlon$hurdles, col="blue", pch=20)
x <- heptathlon$longjump
y <- heptathlon$hurdles
f<-function(x,y){dmvnorm(cbind(x,y),mean=c(mean(heptathlon$longjump),
                                           mean(heptathlon$hurdles)),
                         sigma=matrix(c(s1,s12,s12,s2),ncol=2))}

# 이변량 정규분포 등고선 그림추가
x<-seq(min(heptathlon$longjump),max(heptathlon$longjump),length=100)
y<-seq(min(heptathlon$hurdles),max(heptathlon$hurdles),length=100)
s1 = var(heptathlon$longjump)
s2 = var(heptathlon$hurdles)
s12 = cov(heptathlon$longjump, heptathlon$hurdles)
f<-function(x,y){dmvnorm(cbind(x,y),mean=c(mean(heptathlon$longjump),
                        mean(heptathlon$hurdles)),
                        sigma=matrix(c(s1,s12,s12,s2),ncol=2))}
par(new=T)
contour(x, y, outer(x,y,f), col="gray")

# 이변량 상자그림
library(asbio)
library(MVA)
bvbox(heptathlon[, c("longjump", "hurdles")])


# 밀도함수
persp(x,y,outer(x, y, f), theta=20, phi=30)

