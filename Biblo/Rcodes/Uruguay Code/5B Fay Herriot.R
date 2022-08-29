rm(list = ls())
options(digits = 2)
library(TeachingSampling)
library(dplyr)
library(nlme)
library(sae)

setwd("/Users/psirusteam/Desktop/SAE codes/Uruguay Code")
load("CensoS.RData")
load("SamS.RData")

load("CensoH.RData")
load("SamH.RData")
load("EstimacionesH.RData")

load("CensoM.RData")
load("SamM.RData")
load("EstimacionesM.RData")

##########################
####    FAY HERRIOT    ###
##########################

# HOMBRE ------------------------------------------

popsize <- as.data.frame(EstimacionesH$N.sec)
popsize <- cbind(EstimacionesH$secc,
                 popsize,EstimacionesH$n.sec)
colnames(popsize) <- c("secc","N","n")
popsize$secc <- as.factor(popsize$secc)
popsize <- popsize[,-ncol(popsize)]

attach(SamH)
direct <- direct(ing, secc, factorex, 
                 domsize = popsize, replace = FALSE)
detach(SamH)


direct_ests <- direct$Direct
sd_direct <- direct$SD
var_direct <- sd_direct^2

# FH data frame
area_values <- CensoH %>%
  group_by(secc) %>%
  summarise(condacto = sum(na.omit(condacto)),
            condactc = sum(na.omit(condactc)),
            condactj = sum(na.omit(condactj)),
            alfasi = sum(na.omit(alfasi))) %>%
  arrange(secc)



FH_values <- CensoH %>%
  group_by(secc) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(secc)

FH_values$nd = EstimacionesH$n.sec
FH_values$Nd = EstimacionesH$N.sec

FH_values$HT = direct_ests
FH_values$v.dir <- var_direct

#Constructing the estimators
attach(FH_values)
FHresult <- eblupFH(direct_ests ~ condacto + 
                    condactc + condactj + alfasi, vardir = v.dir)
detach(FH_values)
EstimacionesH$FH <- NA
EstimacionesH$FH <- FHresult$eblup

FH_values$gamma <- FHresult$fit$refvar / (FHresult$fit$refvar + FH_values$v.dir)

# MUJER ------------------------------------------

popsize <- as.data.frame(EstimacionesM$N.sec)
popsize <- cbind(EstimacionesM$secc,
                 popsize,EstimacionesM$n.sec)
colnames(popsize) <- c("secc","N","n")
popsize$secc <- as.factor(popsize$secc)
popsize <- popsize[,-ncol(popsize)]

attach(SamM)
direct <- direct(ing, secc, factorex, 
                 domsize = popsize, replace = FALSE)
detach(SamM)


direct_ests <- direct$Direct
sd_direct <- direct$SD
var_direct <- sd_direct^2

# FH data frame
area_values <- CensoM %>%
  group_by(secc) %>%
  summarise(condacto = sum(na.omit(condacto)),
            condactc = sum(na.omit(condactc)),
            condactj = sum(na.omit(condactj)),
            alfasi = sum(na.omit(alfasi))) %>%
  arrange(secc)



FH_values <- CensoM %>%
  group_by(secc) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(secc)
FH_values$nd = EstimacionesM$n.sec
FH_values$Nd = EstimacionesM$N.sec

FH_values$HT = direct_ests
FH_values$v.dir <- var_direct

#constructing the estimator
attach(FH_values)
FHresult <- eblupFH(direct_ests ~ condacto + 
                      condactc + condactj + alfasi, vardir = v.dir)
detach(FH_values)
EstimacionesM$FH <- NA
EstimacionesM$FH <- FHresult$eblup

FH_values$gamma <- FHresult$fit$refvar /
  (FHresult$fit$refvar + FH_values$v.dir)


#Saving our files
save(EstimacionesH, file = "EstimacionesH.RData")
save(EstimacionesM, file = "EstimacionesM.RData")
