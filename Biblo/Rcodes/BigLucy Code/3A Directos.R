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


###################################
## Estimador de Horvitz-Thompson ##
###################################

attach(data.sam)
Yd.HT <- as.matrix(tapply(Income * FEX, ZoneLevel, sum))
Estimaciones$HT <- Yd.HT/Estimaciones$N.county
detach(data.sam)

if(abs(sum(Estimaciones$HT, na.rm=TRUE)-total2)>thr){
   stop("total not correct")
}

save(Estimaciones, file = "Estimaciones.RData")


###################
###    Hájek    ###
###################

#Creating estimated total
attach(data.sam)
N_est <- tapply(FEX, ZoneLevel, sum)
N_est <- as.matrix(N_est)
detach(data.sam)

#Creating the estimator
Estimaciones$Hajek <- Yd.HT / N_est

if(abs(sum(Estimaciones$Hajek, na.rm=TRUE)-total2)>thr){
  stop("total not correct")
}

save(Estimaciones, file = "Estimaciones.RData")


#############################
##      GREG estimator     ##
#############################

# Constructing the individual regressions per group
library(nlme)
GREGregs <- lmList(Income ~ 0 + Employees + Taxes | ZoneLevel, BigLucy)
GREGregs <- as.data.frame(lapply(GREGregs, coef))


#constructing true X matrix
attach(BigLucy)
area_values <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(N = n(),
            Employees = sum(Employees)/sum(I), 
            Taxes = sum(Taxes)/sum(I)) %>%
  arrange(ZoneLevel)
detach(BigLucy)

#constructing HT X values and difference
attach(data.sam)
area_values$N.HT <- tapply(FEX*1, ZoneLevel, sum)
area_values$N.HT <- area_values$N.HT/Estimaciones$N.county
area_values$N.diff <- 1 - area_values$N.HT

area_values$Employees.HT <- tapply(FEX*Employees, ZoneLevel, sum)
area_values$Employees.HT <- area_values$Employees.HT/Estimaciones$N.county
area_values$Employees.diff <- area_values$Employees - area_values$Employees.HT

area_values$Taxes.HT <- tapply(FEX*Taxes, ZoneLevel, sum)
area_values$Taxes.HT <- area_values$Taxes.HT/Estimaciones$N.county
area_values$Taxes.diff <- area_values$Taxes - area_values$Taxes.HT
detach(data.sam)

D <- nrow(Estimaciones)

#for loop to construct estimator
Estimaciones$GREG <- rep(0, D)
for (i in c(1:D)){
  Estimaciones$GREG[i] =
    Estimaciones$HT[i] +
    area_values$N.diff[i]*GREGregs[1,i] +
    area_values$Employees.diff[i]*GREGregs[1,i] +
    area_values$Taxes.diff[i]*GREGregs[2,i]
}

if(abs(sum(Estimaciones$GREG, na.rm=TRUE)-total2)>thr){
  stop("total not correct")
}

save(Estimaciones, file = "Estimaciones.RData")
save(BigLucy, file = "BigLucy.RData")

