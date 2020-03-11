library(tidyverse)
library(MASS)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(scatterplot3d)
library(rgl)

#Happiness 2015-2017 3년 모두 있는 나라
Happiness_ <- read.csv("C:/Project/data/Happiness_Join_.csv", header = T, sep = ",")
#Happiness 2015-2017 3년 모두 없는 나라
Happiness_f <- read.csv("C:/Project/data/Happiness_Join_f.csv", header = T, sep = ",")

Hap2015 <- Happiness_ %>% filter(Year == "2015")
Hap2016 <- Happiness_ %>% filter(Year == "2016")
Hap2017 <- Happiness_ %>% filter(Year == "2017")

#EDA
#연도별 차이 없음을 보임



#2017년 데이터에 Continent 추가
#Continent - Africa, Asia, Oceania, Europe, North America, South America
Hap2017$Continent <- ifelse(Hap2017$Region %in% c("Central and Eastern Europe", "Western Europe"), "Europe", 
                            ifelse(Hap2017$Region %in% c("Eastern Asia", "Southeastern Asia", "Southern Asia"), "Asia",
                                   ifelse(Hap2017$Region %in% c("Middle East and Northern Africa", "Sub-Saharan Africa", "South Africa"), "Africa",
                                          ifelse(Hap2017$Region %in% c("North America"), "North America",
                                                 ifelse(Hap2017$Region %in% c("Latin America and Caribbean"), "South America",
                                                        ifelse(Hap2017$Region %in% c("Australia and New Zealand"), "Oceania", FALSE))))))

head(Hap2017)

#PCA - 2017년 데이터를 바탕으로
S <- var(Hap2017[,c(6:12)])
round(S, 2)
R <- cor(Hap2017[,c(6:12)])
round(R,2)

PC.result <- princomp(Hap2017[,c(6:12)], cor = TRUE)
summary(PC.result)
PC.result$loadings
screeplot(PC.result, type="lines")
biplot(PC.result)
#첫번째 Prin comp는 Dystopia_Residual을 제외한 overall을, 두 번째 Prin comp는 (Economy, Family, Health, Dystopia_Residual)와 (Freedom, Trust, Generosity)의 대비
#평균 고유값 1을 넘는 comp1과 comp2를 선택

#CLUSTERING
#Kmeans 2
prin <- PC.result$scores[,c(1:2)]
K <- kmeans(prin, centers = 2)
CLUS <- data.frame(Rank = Hap2017$Happiness_Rank, Continent = Hap2017$Continent, prin, kmeans = factor(K$cluster))
ggplot(CLUS, aes(Comp.1, Comp.2)) + geom_text(aes(label = Rank), size = 4) + geom_point(aes(color = kmeans), size =7, alpha=0.3)

#Kmeans 4
prin <- PC.result$scores[,c(1:2)]
K <- kmeans(prin, centers = 4)
CLUS <- data.frame(Continent = Hap2017$Continent, prin, kmeans = factor(K$cluster))
ggplot(CLUS, aes(Comp.1, Comp.2)) + geom_text(aes(label = Continent), size = 4) + geom_point(aes(color = kmeans), size =7, alpha=0.3)

KA <- data.frame(Continent = Hap2017$Continent, Cluster = K$cluster)

#KA$Cluster <- ifelse(KA$Cluster == 1, "Asia", KA$Cluster)
#KA$Cluster <- ifelse(KA$Cluster == 4, "Europe", KA$Cluster)
#KA$Cluster <- ifelse(KA$Cluster == 3, "Africa", KA$Cluster)
#KA$Cluster <- ifelse(KA$Cluster == 4, "Other", KA$Cluster)

KA$Continent <- ifelse(KA$Continent %in% c("Oceania", "South America", "North America"), "Other", Hap2017$Continent)
KA %>% group_by(Cluster, Continent) %>% summarise(count=n())

correct.rate.K <- mean(KA$Continent == KA$Cluster)
error.rate.K <- mean(KA$Continent != KA$Cluster)
correct.rate.K
error.rate.K

#Kmeans 5
prin <- PC.result$scores[,c(1:2)]
K <- kmeans(prin, centers = 5)
CLUS <- data.frame(Continent = Hap2017$Continent, prin, kmeans = factor(K$cluster))
ggplot(CLUS, aes(Comp.1, Comp.2)) + geom_text(aes(label = Continent), size = 4) + geom_point(aes(color = kmeans), size =7, alpha=0.3)

KA <- data.frame(Continent = Hap2017$Continent, Cluster = K$cluster)
KKK<- KA %>% group_by(Cluster, Continent) %>% summarise(count=n())
KKK

KA$Cluster <- ifelse(KA$Cluster == 1, "Europe", KA$Cluster)
KA$Cluster <- ifelse(KA$Cluster == 2, "Asia", KA$Cluster)
KA$Cluster <- ifelse(KA$Cluster == 3, "South America", KA$Cluster)
KA$Cluster <- ifelse(KA$Cluster == 4, "Africa", KA$Cluster)
KA$Cluster <- ifelse(KA$Cluster == 5, "Other", KA$Cluster)

KA$Continent <- ifelse(KA$Continent %in% c("Oceania", "North America"), "Other", Hap2017$Continent)
KA
correct.rate.K <- mean(KA$Continent == KA$Cluster)
error.rate.K <- mean(KA$Continent != KA$Cluster)
correct.rate.K
error.rate.K

#linear regression - (X)
prin <- PC.result.R$scores[,c(1:3)]
Happiness_l <- cbind(Happiness[,c(2,5)], happy.prin)
Happiness_lm <- lm(Happiness_l$Happiness_Score ~ Happiness_l$Comp.1 + Happiness_l$Comp.2 + Happiness_l$Comp.3)
summary(Happiness_lm)

str(Happiness_f)
Happiness_f
Happiness_f_ <- Happiness_f[c(11, 13, 15, 16, 19), ]
Happiness_f_

Hap_test <- data.frame(Country = Happiness_f_$Country,
                       Region = Happiness_f_$Region,
                       Comp.1 = c(0.492*Happiness_f_$Economy + 0.425*Happiness_f_$Family + 0.476*Happiness_f_$Health + 0.433*Happiness_f_$Freedom + 0.361*Happiness_f_$Trust + 0.180*Happiness_f_$Generosity),
                       Comp.2 = c(0.325*Happiness_f_$Economy + 0.139*Happiness_f_$Family + 0.289*Happiness_f_$Health - 0.300*Happiness_f_$Freedom - 0.329*Happiness_f_$Trust - 0.654*Happiness_f_$Generosity + 0.407*Happiness_f_$Dystopia_Residual),
                       Comp.3 = c(0.128*Happiness_f_$Economy + 0.333*Happiness_f_$Family - 0.176*Happiness_f_$Freedom - 0.274*Happiness_f_$Trust - 0.162*Happiness_f_$Generosity - 0.859*Happiness_f_$Dystopia_Residual))
Hap_test

predict.lm(Happiness_lm, newdata = Hap_test[,c(3:5)])

Hap_test$Happiness_score <- c(5.370728 + 0.587609*Hap_test$Comp.1 + 0.211149*Hap_test$Comp.2 + 0.211149*Hap_test$Comp.3)
Hap_test[,c(1:2, 6)]

#원래 score값과 비교
Happiness_f_[,c(2:3, 5)]

#40 47
set.seed(-7053)
#Discrimination Analysis - 전체 데이터셋 이용, 관측치수를 비슷하도록 Asia / Western Europe / Central and Easthern Europe / Latin America 만 비교
Hap_Asia <- Happiness_[grep("Asia", Happiness_$Region),]
Hap_Asia$index <- c(1:63)
Hap_Asia$Region <- c("Asia")
Hap_CE_Europe <- Happiness_[grep("Central and Eastern Europe", Happiness_$Region),]
Hap_CE_Europe$index <- c(1:87)
Hap_W_Europe <- Happiness_[grep("Western Europe", Happiness_$Region),]
Hap_W_Europe$index <- c(1:63)
Hap_L_America <- Happiness_[grep("Latin America", Happiness_$Region),]
Hap_L_America$index <- c(1:63)

Asia_r <- data.frame(index = c(sample(c(1:63), 10, replace = FALSE)))
CE_Europe_r  <- data.frame(index = c(sample(c(1:87), 10, replace = FALSE)))
W_Europe_r <- data.frame(index = c(sample(c(1:63), 10, replace = FALSE)))
L_America_r <- data.frame(index = c(sample(c(1:63), 10, replace = FALSE)))

Hap_Asia_Tr <- Hap_Asia[Hap_Asia$index[-c(Asia_r$index)], ]
Hap_Asia_T <- inner_join(Hap_Asia, Asia_r, by = c("index" = "index"))

Hap_CE_Europe_Tr <- Hap_CE_Europe[Hap_CE_Europe$index[-c(CE_Europe_r$index)], ]
Hap_CE_Europe_T <- inner_join(Hap_CE_Europe, CE_Europe_r, by = c("index" = "index"))

Hap_W_Europe_Tr <- Hap_W_Europe[Hap_W_Europe$index[-c(W_Europe_r$index)], ]
Hap_W_Europe_T <- inner_join(Hap_W_Europe, W_Europe_r, by = c("index" = "index"))

Hap_L_America_Tr <- Hap_L_America[Hap_L_America$index[-c(L_America_r$index)], ]
Hap_L_America_T <- inner_join(Hap_L_America, L_America_r, by = c("index" = "index"))

Hap_Tr <- rbind(Hap_Asia_Tr, Hap_CE_Europe_Tr, Hap_W_Europe_Tr, Hap_L_America_Tr)
str(Hap_Tr)

Hap_T <- rbind(Hap_Asia_T, Hap_CE_Europe_T, Hap_W_Europe_T, Hap_L_America_T)
str(Hap_T)

LDA.Result <- lda(Hap_Tr[,c(6:12)], Hap_Tr$Region)
LDA.Predict <- predict(LDA.Result, Hap_Tr[,c(6:12)])
P_Region <- LDA.Predict$class
correct.rate <- mean(Hap_Tr$Region == P_Region) 
error.rate <- mean(Hap_Tr$Region != P_Region)

correct.rate
error.rate


#3D plot
#Asia - 핑크 / CE europe - 하늘색 / W europe - 연두색 / Latin America and Caribbean - 노란색
LDA.col <- ifelse(Hap_Tr$Region == P_Region & Hap_Tr$Region == "Asia", "#E287AA", 
                  ifelse(Hap_Tr$Region == P_Region & Hap_Tr$Region == "Central and Eastern Europe", "#56B4E9", 
                         ifelse(Hap_Tr$Region == P_Region & Hap_Tr$Region == "Western Europe", "#81C147",
                                ifelse(Hap_Tr$Region == P_Region & Hap_Tr$Region == "Latin America and Caribbean", "#E69F00", "#000000"))))

plot3d(LDA.Predict$x, pch=18, size = 5, col = LDA.col)

#Test 데이터 제대로 나누는지 보기
LDA.Predict.T <- predict(LDA.Result, Hap_T[,c(6:12)])
PT_Region <- LDA.Predict.T$class
correct.rate.T <- mean(Hap_T$Region == PT_Region)
error.rate.T <- mean(Hap_T$Region != PT_Region)
correct.rate.T
error.rate.T

LDA.col.T <- ifelse(Hap_T$Region == PT_Region & Hap_T$Region == "Asia", "#E287AA", 
                  ifelse(Hap_T$Region == PT_Region & Hap_T$Region == "Central and Eastern Europe", "#56B4E9", 
                         ifelse(Hap_T$Region == PT_Region & Hap_T$Region == "Western Europe", "#81C147",
                                ifelse(Hap_T$Region == PT_Region & Hap_T$Region == "Latin America and Caribbean", "#E69F00", "#000000"))))

plot3d(LDA.Predict.T$x, pch=18, size = 5, col = LDA.col.T)


####
str(Hap2017)
Hap2017 <- Happiness %>% filter(Year == "2017")
T <- kmeans(Hap2017[,c(6:12)], 4, nstart = 25)
fviz_cluster(T, data=Hap2017[,c(6:12)])

T.PCA <- PCA(Hap2017[,c(6:12)], graph = FALSE)
fviz_contrib(T.PCA, choice = "var", axes=1, top=7)
fviz_contrib(T.PCA, choice = "var", axes=2, top=7)
str(T.PCA)
summary(T.PCA)

