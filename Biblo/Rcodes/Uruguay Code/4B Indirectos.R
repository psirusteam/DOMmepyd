rm(list = ls())
options(digits = 2)
library(TeachingSampling)
library(dplyr)
library(nlme)

setwd("/Users/psirusteam/Desktop/SAE codes/Uruguay Code")
load("CensoS.RData")
load("SamS.RData")

load("CensoH.RData")
load("SamH.RData")
load("EstimacionesH.RData")

load("CensoM.RData")
load("SamM.RData")
load("EstimacionesM.RData")

#############################
# Post-stratified Snythetic #
#############################

## HOMBRE -------------------------

# Ndj estimate for Hájek estimator in the PS_SYN estimator
attach(SamH)
Ng1 <- sum(SamH[age_group == 1,]$factorex)
Ng2 <- sum(SamH[age_group == 2,]$factorex)
Ng3 <- sum(SamH[age_group == 3,]$factorex)
Ng4 <- sum(SamH[age_group == 4,]$factorex)
Ng_Hajek <- c(Ng1, Ng2, Ng3, Ng4)

#Hájek estimator for mean in group j 
HA_sum <- tapply(factorex*ing, age_group, sum)
HA_est <- c(HA_sum[1]/Ng_Hajek[1], 
            HA_sum[2]/Ng_Hajek[2], 
            HA_sum[3]/Ng_Hajek[3],
            HA_sum[4]/Ng_Hajek[4])
detach(SamH)

#sums by group in population, then weighting by HA mean
attach(CensoH)
sums_group <- as.matrix(tapply(I, list(secc, age_group), sum))
PS_SYN = as.matrix(sums_group[,1]*HA_est[1] + 
                     sums_group[,2]*HA_est[2] + 
                     sums_group[,3]*HA_est[3] + 
                     sums_group[,4]*HA_est[4])
detach(CensoH)

#creating estimator
EstimacionesH$PS_SYN <- PS_SYN/EstimacionesH$N.sec


## MUJER -------------------------

# Ndj estimate for Hájek estimator in the PS_SYN estimator
attach(SamM)
Ng1 <- sum(SamM[age_group == 1,]$factorex)
Ng2 <- sum(SamM[age_group == 2,]$factorex)
Ng3 <- sum(SamM[age_group == 3,]$factorex)
Ng4 <- sum(SamM[age_group == 4,]$factorex)
Ng_Hajek <- c(Ng1, Ng2, Ng3, Ng4)

#Hájek estimator for mean in group j 
HA_sum <- tapply(factorex*ing, age_group, sum)
HA_est <- c(HA_sum[1]/Ng_Hajek[1], 
            HA_sum[2]/Ng_Hajek[2], 
            HA_sum[3]/Ng_Hajek[3],
            HA_sum[4]/Ng_Hajek[4])
detach(SamM)

#sums by group in population, then weighting by HA mean
attach(CensoM)
sums_group <- as.matrix(tapply(I, list(secc, age_group), sum))
PS_SYN = as.matrix(sums_group[,1]*HA_est[1] + 
                     sums_group[,2]*HA_est[2] + 
                     sums_group[,3]*HA_est[3] + 
                     sums_group[,4]*HA_est[4])
detach(CensoM)

#creating estimator
EstimacionesM$PS_SYN <- PS_SYN/EstimacionesM$N.sec



#############################
## Synthetic Regression at ##
##      Level of Area      ##
#############################

## HOMBRE -----------------------------

#creating data frame for area

area_values <- CensoH %>%
  group_by(secc) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(secc)
area_values$HT <- EstimacionesH$HT

#synthetic regression by area
reg1syn <- lm(HT ~ condacto + condactc + 
                condactj + alfasi, data = area_values)
EstimacionesH$reg1.syn <- predict.lm(reg1syn, area_values)

## MUJER ----------------------------

#creating data frame for area

area_values <- CensoM %>%
  group_by(secc) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(secc)
area_values$HT <- EstimacionesM$HT


#synthetic regression by area
reg1syn <- lm(HT ~ condacto + condactc + 
                condactj + alfasi, data = area_values) #weights = 1/var.HT)
EstimacionesM$reg1.syn <- predict.lm(reg1syn, area_values)


#############################
## Synthetic Regression at ##
##   Level of Individual   ##
#############################

## HOMBRE ---------------------------------

reg2syn <- lm(ing ~ condacto + condactc + edad +
                condactj + alfasi, data = SamH)

means <- CensoH %>%
  group_by(secc) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            edad = mean(edad),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(secc)

means$reg2.pred <- predict.lm(reg2syn, means)
EstimacionesH$reg2.syn <- NA
EstimacionesH$reg2.syn <- means$reg2.pred

## MUJER ------------------------------------

reg2syn <- lm(ing ~ condacto + condactc + edad +
                condactj + alfasi, data = SamM)

means <- CensoM %>%
  group_by(secc) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            edad = mean(edad),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(secc)

means$reg2.pred <- predict.lm(reg2syn, means)
EstimacionesM$reg2.syn <- NA
EstimacionesM$reg2.syn <- means$reg2.pred


#############################
##   Compound Estimators   ##
#############################

# HOMBRE -----------------------------------

Nd <- EstimacionesH$N.sec
Nd.HT <- tapply(SamH$factorex, SamH$secc, sum)

alpha <- 2
Nd.alpha <- alpha * Nd
lambda <- ifelse(Nd.HT > Nd.alpha, 1, Nd.HT / Nd.alpha)
lambda <- as.matrix(lambda)

Income.comp <- lambda * EstimacionesH$HT +
  (1 - lambda) * EstimacionesH$reg1.syn
EstimacionesH$compuesto <- Income.comp

# MUJER -----------------------------------

Nd <- EstimacionesM$N.sec
Nd.HT <- tapply(SamM$factorex, list(SamM$secc), sum)

alpha <- 1
Nd.alpha <- alpha * Nd
lambda <- ifelse(Nd.HT > Nd.alpha, 1, Nd.HT / Nd.alpha)
lambda <- as.matrix(lambda)

Income.comp <- lambda * EstimacionesM$HT +
  (1 - lambda) * EstimacionesM$reg1.syn
EstimacionesM$compuesto <- Income.comp



save(EstimacionesH, file = "EstimacionesH.RData")
save(EstimacionesM, file = "EstimacionesM.RData")
