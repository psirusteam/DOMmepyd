rm(list = ls())
options(digits = 2)
set.seed(20150802)
library(TeachingSampling)
library(dplyr)

load("Estimaciones.Rdata")
load("data.sam.Rdata")
load("BigLucy.RData")

total1 <- 114812
total2 <- 114812
thr <- 10000

#############################
# Post-stratified Snythetic #
#############################


# Ndj estimate for Hájek estimator in the PS_SYN estimator
attach(data.sam)
Ng1 <- sum(data.sam[Level == "Big",]$FEX)
Ng2 <- sum(data.sam[Level == "Medium",]$FEX)
Ng3 <- sum(data.sam[Level == "Small",]$FEX)
Ng_Hajek <- c(Ng1, Ng2, Ng3)

#Hájek estimator for mean in group j 
HA_sum <- tapply(FEX*Income, Level, sum)
HA_est <- c(HA_sum[1]/Ng_Hajek[1], 
            HA_sum[2]/Ng_Hajek[2], 
            HA_sum[3]/Ng_Hajek[3])
detach(data.sam)

#sums by group in population, then weighting by HA mean
attach(BigLucy)
sums_group <- as.matrix(tapply(I, list(ZoneLevel, Level), sum))
sums_group[is.na(sums_group)] <- 0
PS_SYN = as.matrix(sums_group[,1]*HA_est[1] + 
         sums_group[,2]*HA_est[2] + 
         sums_group[,3]*HA_est[3])
detach(BigLucy)

#creating estimator
Estimaciones$PS_SYN <- PS_SYN/Estimaciones$N.county
if(abs(sum(Estimaciones$PS_SYN, na.rm=TRUE)-total1)>thr){
  stop("total not correct")
}


#############################
## Synthetic Regression at ##
##      Level of Area      ##
#############################

#creating data frame for area

area_values <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(Employees = sum(Employees), 
            Taxes = sum(Taxes)) %>%
  arrange(ZoneLevel)
area_values$HT <- Estimaciones$HT
area_values[area_values == 0] <- NA

#synthetic regression by area
reg1syn <- lm(HT ~ Employees + Taxes, data = area_values)
Estimaciones$reg1.syn <- predict.lm(reg1syn, area_values)

if(abs(sum(Estimaciones$reg1.syn, na.rm=TRUE)-total1)>thr){
  stop("total not correct")
}


#############################
## Synthetic Regression at ##
##   Level of Individual   ##
#############################

reg2syn <- lm(Income ~ Employees + Taxes, data = BigLucy)
means <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(Employees = mean(Employees),
            Taxes = mean(Taxes)) %>%
  arrange(ZoneLevel)

means$reg2.pred <- predict.lm(reg2syn, means)
Estimaciones$reg2.syn <- NA
Estimaciones$reg2.syn <- means$reg2.pred


if(abs(sum(Estimaciones$reg2.syn, na.rm=TRUE)-total1)>thr){
  stop("total not correct")
}


#############################
##   Compound Estimators   ##
#############################

Nd <- Estimaciones$N.county
Nd.HT <- tapply(data.sam$FEX, list(data.sam$ZoneLevel), sum)
Nd.HT[is.na(Nd.HT)] <- 0

delta <- 5
# No es una buena idea que delta sea 1, 
# porque le da más peso al estimador directo y
# por definición la suma del directo es N.hat
# si a eso se le agrega el sintético en las áreas
# no muesreadas, entonces 
# se sobreestimará siempre

Nd.delta <- delta * Nd
phi <- ifelse(Nd.HT > Nd.delta, 1, Nd.HT / Nd.delta)
phi <- as.matrix(phi)

Income.comp <- phi * Estimaciones$HT +
  (1 - phi) * Estimaciones$PS_SYN
Income.comp <- ifelse(Estimaciones$n.county == 0, 
                      Estimaciones$PS_SYN, Income.comp)
Estimaciones$compuesto <- Income.comp

if(abs(sum(Estimaciones$compuesto, na.rm=TRUE)-total1)>thr){
  stop("total not correct")
}


save(Estimaciones, file = "Estimaciones.RData")
save(BigLucy, file = "BigLucy.RData")


