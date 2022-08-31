rm(list = ls())
options(digits = 2)
library(TeachingSampling)
library(dplyr)
library(sae)
library(lme4)
library(nlme)
library(ggplot2)

setwd("/Users/psirusteam/Desktop/SAE codes/Uruguay Code")
load("CensoS.RData")
load("SamS.RData")

load("CensoH.RData")
load("SamH.RData")
load("EstimacionesH.RData")

load("CensoM.RData")
load("SamM.RData")
load("EstimacionesM.RData")

##############################
##   Census Empirical Best  ##
##############################

# HOMBRES ---------------------------------------------

# Monte Carlo Census Empirical Best
# County_numeric = sec2

# big X matrix
X <- cbind.data.frame(CensoH$sec2, 1, CensoH$condacto,
                      CensoH$condactc, CensoH$condactj,
                      CensoH$alfasi)
colnames(X)[1] <- "sec2"

# mean matrix by group
means <- CensoH %>%
  group_by(sec2) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(sec2)
mean_values <- means[,-1]


# 1) producing the mixed effects regression Battese Harter Fuller 

#SamH$LogIng <- log(SamH$Ing + 1)
BHFreg <- lmer(ing ~ condacto + condactc + 
               condactj + alfasi + (1|secc), data = SamH)
betas <- as.matrix(fixed.effects(BHFreg))
var_e <- summary(BHFreg)$sigma^2
var_u <- as.numeric(VarCorr(BHFreg))
gammad<-var_u/(var_u+var_e/EstimacionesH$n.sec)


mu <- matrix(0,0,1)
# creating the mu vector
for (i in 1:nrow(EstimacionesH)){
  XB = as.matrix(X[X$sec2 == i,-1])%*%betas
  if (EstimacionesH[EstimacionesH$sec2 == i,]$n.sec != 0){
    meany <- mean(SamH[SamH$sec2 == i,]$ing)
    meanx_beta <- as.matrix(cbind(1,mean_values[i,]))%*%betas
    add_term <- rep(gammad[i]*(meany - meanx_beta),nrow(XB))
  }else{
    add_term <- rep(0,nrow(XB))
  }
  mu <- rbind(mu, XB+add_term)
}

var <- matrix(0,0,1)
# creating the variances
for (i in 1:nrow(EstimacionesH)){
  if (EstimacionesH[EstimacionesH$sec2 == i,]$n.sec != 0){
    vardi <- var_u*(1-gammad[i]) + var_e
    vard <- as.matrix(rep(vardi, nrow(CensoH[CensoH$sec2 ==i,])))
  }else{
    vard <- as.matrix(rep(var_e,nrow(CensoH[CensoH$sec2 ==i,])))
  }
  var <- rbind(var, vard)
}

sd_Census <- sqrt(var)

# 2) Monte carlo simulation 

# creating new matrix: 
CensoH_new <- CensoH[,c("sec2","condacto","condactc","condactj",
                        "alfasi")]
CensoH_new$mu <- mu
CensoH_new$sd <- sd_Census
CensoH_new <- CensoH_new[complete.cases(CensoH_new),]

A = 1000
CensusEB <- numeric(25)
for (i in 1:A){
  print(i)
  censoY <- rnorm(nrow(CensoH_new),CensoH_new$mu,CensoH_new$sd)
  censo_zone <- cbind.data.frame(CensoH_new$sec2, censoY)
  colnames(censo_zone)[1] <- "sec2"
  estimates <- censo_zone %>%
    group_by(sec2) %>%
    summarise(CensusEB_a = mean(censoY)) # na.omit
  CensusEB <- CensusEB + estimates$CensusEB_a
}

EstimacionesH$CensusEB <- (CensusEB)/A


# MUJERES ------------------------------------------

# Monte Carlo Census Empirical Best
# County_numeric = sec2

# big X matrix
X <- cbind.data.frame(CensoM$sec2, 1, CensoM$condacto,
                      CensoM$condactc, CensoM$condactj,
                      CensoM$alfasi)
colnames(X)[1] <- "sec2"

# mean matrix by group
means <- CensoM %>%
  group_by(sec2) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(sec2)
mean_values <- means[,-1]


# 1) producing the mixed effects regression Battese Harter Fuller 

#SamH$LogIng <- log(SamH$Ing + 1)
BHFreg <- lmer(ing ~ condacto + condactc + 
                 condactj + alfasi + (1|secc), data = SamM)
betas <- as.matrix(fixed.effects(BHFreg))
var_e <- summary(BHFreg)$sigma^2
var_u <- as.numeric(VarCorr(BHFreg))
gammad<-var_u/(var_u+var_e/EstimacionesM$n.sec)


mu <- matrix(0,0,1)
# creating the mu vector
for (i in 1:nrow(EstimacionesM)){
  XB = as.matrix(X[X$sec2 == i,-1])%*%betas
  if (EstimacionesM[EstimacionesM$sec2 == i,]$n.sec != 0){
    meany <- mean(SamM[SamM$sec2 == i,]$ing)
    meanx_beta <- as.matrix(cbind(1,mean_values[i,]))%*%betas
    add_term <- rep(gammad[i]*(meany - meanx_beta),nrow(XB))
  }else{
    add_term <- rep(0,nrow(XB))
  }
  mu <- rbind(mu, XB+add_term)
}

var <- matrix(0,0,1)
# creating the variances
for (i in 1:nrow(EstimacionesM)){
  if (EstimacionesM[EstimacionesM$sec2 == i,]$n.sec != 0){
    vardi <- var_u*(1-gammad[i]) + var_e
    vard <- as.matrix(rep(vardi, nrow(CensoM[CensoM$sec2 ==i,])))
  }else{
    vard <- as.matrix(rep(var_e,nrow(CensoM[CensoM$sec2 ==i,])))
  }
  var <- rbind(var, vard)
}

sd_Census <- sqrt(var)

# 2) Monte carlo simulation 

# creating new matrix: 
CensoM_new <- CensoM[,c("sec2","condacto","condactc","condactj",
                        "alfasi")]
CensoM_new$mu <- mu
CensoM_new$sd <- sd_Census
CensoM_new <- CensoM_new[complete.cases(CensoM_new),]

A = 1000
CensusEB <- numeric(25)
for (i in 1:A){
  print(i)
  censoY <- rnorm(nrow(CensoM_new),CensoM_new$mu,CensoM_new$sd)
  censo_zone <- cbind.data.frame(CensoM_new$sec2, censoY)
  colnames(censo_zone)[1] <- "sec2"
  estimates <- censo_zone %>%
    group_by(sec2) %>%
    summarise(CensusEB_a = mean(censoY)) # na.omit
  CensusEB <- CensusEB + estimates$CensusEB_a
}

EstimacionesM$CensusEB <- (CensusEB)/A


save(EstimacionesH, file = "EstimacionesH.RData")
save(EstimacionesM, file = "EstimacionesM.RData")
