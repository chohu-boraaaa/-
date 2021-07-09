setwd("C:\\Users\\home\\Desktop\\김지희\\3학년1학기\\다변량 자료분석")
weather <- read.csv("weather.csv", header=T)
weather

# 평균벡터
colMeans(weather[, c(2:6)])
# 공분산행렬
cov(weather[, c(2:6)])
# 상관행렬
cor(weather[, c(2:6)])

# (b) 처음 4개의 변수(x1~x4)의 공분산행렬을 이용하여 주성분분석 실시
# 처음 4개의 변수의 데이터 담은 변수 생성
w_cov <- weather[, c(2:5)]
head(w_cov)
# 로그함수 이용한 박스콕스 변환을 이용하여 왜도 안정화
nw_cov <- log(w_cov)
# pca
w_cov.pca <- prcomp(nw_cov, center=TRUE)
print(w_cov.pca)
plot(w_cov.pca, type="l")
summary(w_cov.pca)
barplot(w_cov.pca$rotation[,1], col=rainbow(4), ylim=c(-1,1))
biplot(w_cov.pca)
# 첫 번째 주성분만으로도 자료 분산의 약 95% 설명 가능
# 중심화만 해주어 길이가 제각각

# (c) 처음 4개의 변수(x1~x3)의 상관행렬을 이용하여 주성분분석 실시
# 처음 4개의 변수의 데이터 담은 변수 생성
w_cor <- weather[, c(2:5)]
# 로그변환을 이용하여 분산안정화 및 정규화를 위한 변환 수행
nw_cor <- log(w_cor)
# pca
w_cor.pca <- prcomp(nw_cov, center=TRUE, scale=TRUE)
print(w_cor.pca)
plot(w_cor.pca, type="l")
summary(w_cor.pca)
# 두 번째 주성분으로 자료 분산의 약 87%를 설명 가능
barplot(w_cor.pca$rotation[,1], col=rainbow(4), ylim=c(-1,1), main="PC1")
barplot(w_cor.pca$rotation[,2], col=rainbow(4), ylim=c(-1,1), main="PC2")
biplot(w_cor.pca)
# 표준화해주어 길이가 거의 비슷

# 13장 2번
flea_data <- read.csv("flea.csv", header=T)
flea_data

# 선형판별분석 수행
require(MASS)
a <- lda(formula = species ~., data=flea_data)
# 자료의 판별 차원 점수
plot(a) 
a$prior
a$counts
a$means
a$scaling
a$svd
prop <- a$svd^2/sum(a$svd^2)
prop 

# LD1으로 다 설명 가능

# 교차타당성 방법을 통한 선형판별분석
a2 <- lda(formula = species ~., data=flea_data, CV=TRUE)
# 어떻게 분류되었는지 확인
head(a2$class)
# 사후확률
a2$posterior
# 예측 정확도, 정오분류표(행:실제, 열:예측)
accuracy <- table(flea_data$species, a2$class)
accuracy
# 각 범주별 정분류 비율
diag(prop.table(accuracy))
# 정분류율
sum(diag(prop.table(accuracy)))


# lda에 기초한 관측값의 분류 결과 시각화
library(klaR)
str(flea_data)
partimat(as.factor(species) ~ x2+x1, data=flea_data, method="lda")
par(new=T)
# points함수를 이용하여 (190,125) 점을 그래프에 찍어 판별
points(190,125,pch=18,col="blue",cex=2)

# 따라서 종 A에 속함

# 선형판별분석 결과 등고선 그림으로 시각화
plot(flea_data$x1[1:18], flea_data$x2[1:18], pch=17, col="red", xlim=c(160,240), ylim=c(110, 140), xlab="x1", ylab="x2")
par(new=T)
plot(flea_data$x1[19:36], flea_data$x2[19:36], pch=15, col="blue", xlim=c(160,240), ylim=c(110, 140), xlab="", ylab="")
legend("topleft",legend=c("A","B"),pch=c(17,15),col=c("red","blue"))
x<-seq(160, 240)
y<-seq(110, 140)
s1 = var(flea_data$x1[1:18])
s2 = var(flea_data$x2[1:18])
s12 = cov(flea_data$x1[1:18], flea_data$x2[1:18])
library(mvtnorm)
f<-function(x,y){dmvnorm(cbind(x,y),mean=c(mean(flea_data$x1[1:18]),
                                           mean(flea_data$x2[1:18])),
                         sigma=matrix(c(s1,s12,s12,s2),ncol=2))}
par(new=T)
contour(x, y, outer(x,y,f), col="gray", lwd=2, lty=2)

s1 = var(flea_data$x1[19:36])
s2 = var(flea_data$x2[19:36])
s12 = cov(flea_data$x1[19:36], flea_data$x2[19:36])
f<-function(x,y){dmvnorm(cbind(x,y),mean=c(mean(flea_data$x1[19:36]),
                                           mean(flea_data$x2[19:36])),
                         sigma=matrix(c(s1,s12,s12,s2),ncol=2))}
par(new=T)
contour(x, y, outer(x,y,f), col="pink", lwd=2, lty=2)
mu1 <- a$means[1,]
mu2 <- a$means[2,]
library(dplyr)
A <- filter(flea_data, species=="A")
B <- filter(flea_data, species=="B")
var1 <- var(A[2:3])
var2 <- var(B[2:3])

# 합동추정량 계산하여 공통의 공분산
var <- ((18-1)*var1+(18-1)*var2)/(18+18-2)
# X의 계수
rank <- (mu1-mu2)%*%solve(var)
# 상수항
constant <- 0.5%*%(mu1-mu2)%*%solve(var)%*%(mu1+mu2)
# 선형판별함수 그리기
abline(a=constant/rank[2] ,b=rank[1]/(-rank[2]))
# points함수를 이용하여 (190,125) 점을 그래프에 찍어 판별
points(190,124,pch=19,col="green",cex=2)


# 10장 1번
result <- matrix(c(302.3, 125.8, 100.4, 105.1, 116.1, 125.8, 170.9, 84.2,
                   93.6, 97.9, 100.4, 84.2, 111.6, 110.8, 120.5, 105.1,
                   93.6, 110.8, 217.9, 153.8, 116.1, 97.9, 120.5, 153.8,
                   294.4), ncol=5)
result
# 공분산행렬을 상관행렬으로 변환환
cor <- matrix(rep(0,25), 5, 5)
for(i in 1:5) {
  for(j in 1:5){
    cor[i,j] = result[i,j]/sqrt((result[i,i]*result[j,j]))
  }
}
colnames(cor) <- c("MECH", "VEC", "ALG", "ANA", "STAT")
rownames(cor) <- c("MECH", "VEC", "ALG", "ANA", "STAT")
cor

# 인자의 수 결정
library(psych)
# 상관행렬 고유값에 대한 scree plot
VSS.scree(cor)
abline(h=0.7, col="red", lty=2)

# 사교회전
library(GPArotation)
# 잠재 과목 간에 다소의 상관 있을 것으로 예상
FA <- fa(r=cor, nfactors=2, rotate="oblimin", fm="pa")
FA

# 인자 부하의 시각화
ls(FA)
print(FA$loadings, cutoff=0)
load <- FA$loadings[,1:2]
plot(load, type="n")
text(load, labels=c("MECH", "VEC", "ALG", "ANA", "STAT"))

# 부하가 모두 0.5 이하인 점을 색상으로 구분
factor.plot(FA, cut=0.5, labels=c("MECH", "VEC", "ALG", "ANA", "STAT"))
fa.diagram(FA)

# 시각화
plot(load, main="Biplot of the first 2 factors", col="red", pch=16)
text(load[,1], load[,2], c("MECH", "VEC", "ALG", "ANA", "STAT"), col="blue", cex=0.7)
for(i in 1:5){
  segments(0,0,load[i,1], load[i,2])
}
fa.diagram(FA)
