rm(list = ls())
options(digits = 3)
library(TeachingSampling)
library(dplyr)
library(sae)
library(nlme)

load("Estimaciones.Rdata")
load("data.sam.Rdata")
load("BigLucy.Rdata")

total1 <- 114812
total2 <- 114812
thr <- 10000

##################################
##   Census Empirical Best BHF  ##
##################################

# Monte Carlo Census Empirical Best

# County numeric (for iteration purposes)
BigLucy$CountyNumeric <- BigLucy$ZoneLevel
NI <- length(unique(BigLucy$ZoneLevel))
levels(BigLucy$CountyNumeric) <- c(1:NI)

data.sam$CountyNumeric <- data.sam$ZoneLevel
levels(data.sam$CountyNumeric) <- c(1:NI)

Estimaciones$CountyNumeric <- Estimaciones$ZoneLevel
levels(Estimaciones$CountyNumeric) <- c(1:NI)

# big X matrix
X <- cbind.data.frame(BigLucy$CountyNumeric, 1, 
                      log(BigLucy$Employees), log(BigLucy$Taxes))
colnames(X)[1] <- "CountyNumeric"

# mean matrix by group
means <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(Employees = mean(Employees),
            Taxes = mean(Taxes)) %>%
  arrange(ZoneLevel)
mean_values <- means[,-1]


# 1) producing the mixed effects regression Battese Harter Fuller 
data.sam$LogIncome = log(data.sam$Income)

BHFreg <- lmer(LogIncome ~ log(Employees) + log(Taxes) + (1|ZoneLevel), 
               data = data.sam)
betas <- as.matrix(fixed.effects(BHFreg))
var_e <- summary(BHFreg)$sigma^2
var_u <- as.numeric(VarCorr(BHFreg))
gammad <- var_u/(var_u + (var_e/Estimaciones$n.county))

u.pred <- as.data.frame(random.effects(BHFreg))[, c(3,4)]
names(u.pred) <- c("ZoneLevel", "u.est")

data.sam <- inner_join(data.sam, u.pred)

BigLucy$mu <- matrix(NA, nrow = nrow(BigLucy), 1)
# creating the mu vector
for (i in 1:nrow(Estimaciones)){
  XB = as.matrix(X[X$CountyNumeric == i,-1]) %*% betas
  if (Estimaciones[Estimaciones$CountyNumeric == i,]$n.county != 0){
    add_term <- rep(unique(data.sam[data.sam$CountyNumeric == i,]$u.est),
                    nrow(XB))
  }else{
    add_term <- rep(0,nrow(XB))
  }
  BigLucy$mu[BigLucy$CountyNumeric == i,] <- XB + add_term
}

mu <- BigLucy$mu

var <- matrix(0,0,1)
# creating the variances
for (i in 1:nrow(Estimaciones)){
  if (Estimaciones[Estimaciones$CountyNumeric == i,]$n.county != 0){
    vardi <- 1 * (var_u*(1-gammad[i]) + var_e)
    vard <- as.matrix(rep(vardi, nrow(BigLucy[BigLucy$CountyNumeric ==i,])))
  }else{
    vard <- as.matrix(rep(var_e,nrow(BigLucy[BigLucy$CountyNumeric ==i,])))
  }
  var <- rbind(var, vard)
}

sd_Census <- sqrt(var)
summary(sd_Census)
sum(is.na(sd_Census))

# 2) Monte carlo simulation 
A = 500
CensusEB <- 0
for (i in 1:A){
  print(i)
  censoY <- rnorm(nrow(BigLucy),mu,sd_Census) 
  censoY <- exp(censoY)
  censo_ZoneLevel <- cbind.data.frame(BigLucy$ZoneLevel, censoY)
  colnames(censo_ZoneLevel)[1] <- "ZoneLevel"
  estimates <- censo_ZoneLevel %>%
    group_by(ZoneLevel) %>%
    summarise(CensusEB_a = mean(censoY))
  CensusEB <- CensusEB + estimates$CensusEB_a
}

# oe <- data.frame(BigLucy$ZoneLevel, BigLucy$Income, 
#                  BigLucy$mu, censoY)

Estimaciones$CensusEB <- CensusEB/A

sum(Estimaciones$CensusEB)

if(abs(sum(Estimaciones$CensusEB, na.rm=TRUE) - total1) > thr){
  stop("total not correct")
}


#######
## Molina y Martín 2018
#######

BigLucy$MolMar <- exp(BigLucy$mu + var/2)
EBLog <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(EBLog = mean(MolMar))

Estimaciones$EBLog <- as.numeric(as.data.frame(EBLog)$EBLog)

#####

Estimaciones$CountyNumeric <- NULL
save(Estimaciones, file = "Estimaciones.RData")
save(BigLucy, file = "BigLucy.RData")


