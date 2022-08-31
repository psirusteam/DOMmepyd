rm(list = ls())
options(digits = 3)
library(TeachingSampling)
library(dplyr)
library(sae)

load("Estimaciones.Rdata")
load("data.sam.Rdata")
load("BigLucy.Rdata")

total1 <- 114812
total2 <- 114812
thr <- 10000
# 
# 
# ##################################
# ## Varianza de Horvitz-Thompson ##
# ##################################
# #Nd <- tapply(BigLucy$I, BigLucy$Zone, sum)
# 
# Nh <- tapply(BigLucy$I, BigLucy$Level, sum)
# nh <- tapply(data.sam$I, data.sam$Level, sum)
# 
# Income.zone <- Domains(data.sam$Zone) * data.sam$Income
# est <- E.STSI(data.sam$Level, Nh, nh, Income.zone)[2,4,]
# 
# # must divide sd by Nh because originally estimated total, not mean
# sd <- est[-1] / Estimaciones$N.county
# 
# Estimaciones$sd.HT <- as.numeric(sd)
# 
# ##################
# ## Fay - Herriot #
# ##################
# FH_values <- BigLucy %>%
#   group_by(Zone) %>%
#   summarise(Employees = mean(Employees), 
#             Taxes = mean(Taxes)) %>%
#   arrange(Zone)
# FH_values$nd = Estimaciones$n.county
# FH_values$Nd = Estimaciones$N.county
# FH_values$HT = Estimaciones$HT
# FH_values$sd.HT <- Estimaciones$sd.HT*1
# FH_values$v.HT <- Estimaciones$sd.HT^2
# 
# FH_nonzero <- as.data.frame(FH_values[FH_values$nd !=0,])
# 
# # sampled areas
# attach(FH_nonzero)
# FHresult <- eblupFH(HT ~ 1 + Employees + Taxes + Nd, vardir = v.HT)
# detach(FH_nonzero)
# Estimaciones$FH <- NA
# Estimaciones[Estimaciones$n.county!=0,]$FH <- FHresult$eblup
# 
# # nonsampled areas 
# Xmatrix <- cbind(1, FH_values[FH_values$nd ==0,]$Employees, 
#                  FH_values[FH_values$nd ==0,]$Taxes,
#                  FH_values[FH_values$nd ==0,]$Nd)
# betas <- FHresult$fit$estcoef$beta
# Estimaciones[Estimaciones$n.county==0,]$FH <- Xmatrix %*% betas
# 
# 
# if(abs(sum(Estimaciones$FH, na.rm=TRUE)-total1)>thr){
#   stop("total not correct")
# }
# 
# save(Estimaciones, file = "Estimaciones.RData")
# sum(Estimaciones$FH, na.rm=TRUE)
# 
# 
# 
# #FH_nonzero$v.HT <- as.numeric(as.vector(
# #Estimaciones[Estimaciones$n.county != 0,]$sd.HT^2))
# #FH_nonzero$v.HT <- as.numeric(as.vector(Estimaciones$sd.HT^2))
# #FH_nonzero$HT <- Estimaciones[Estimaciones$n.county != 0,]$HT
# #FH_nonzero$v.HT <- as.numeric(Estimaciones[Estimaciones$n.county != 0,]$sd.HT^2)
# 
# attach(FH_nonzero)
# model2 <- lm(HT ~ 0 + Employees + Taxes + IncomeMean + Nd)
# detach(FH_nonzero)


## Method 2 

popsize <- as.data.frame(Estimaciones$N.county)
popsize <- cbind(Estimaciones$Zone,
                 popsize,Estimaciones$n.county)
colnames(popsize) <- c("ZoneLevel","N","n")
popsize$ZoneLevel <- as.factor(popsize$ZoneLevel)
popsize_nonzero <- popsize[popsize$n != 0,][,-ncol(popsize)]

attach(data.sam)
direct <- direct(Income, ZoneLevel, FEX, 
                 domsize = popsize_nonzero, replace = FALSE)
detach(data.sam)


direct_ests <- direct$Direct
sd_direct <- direct$SD
var_direct <- sd_direct^2

# FH data frame
FH_values <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(Employees = mean(Employees),
            Taxes = mean(Taxes)) %>%
  arrange(ZoneLevel)
FH_values$nd = Estimaciones$n.county
FH_values$Nd = Estimaciones$N.county

FH_nonzero <- as.data.frame(FH_values[FH_values$nd !=0,])
FH_nonzero$HT = direct_ests
FH_nonzero$v.dir <- var_direct

# sampled areas
attach(FH_nonzero)
FHresult <- eblupFH(direct_ests ~ Employees, vardir = v.dir)
detach(FH_nonzero)
Estimaciones$FH <- NA
Estimaciones[Estimaciones$n.county!=0,]$FH <- FHresult$eblup

# non sampled areas
Xmatrix <- cbind(1, FH_values[FH_values$nd ==0,]$Employees)
betas <- FHresult$fit$estcoef$beta
Estimaciones[Estimaciones$n.county==0,]$FH <- Xmatrix %*% betas


if(abs(sum(Estimaciones$FH, na.rm=TRUE)-total1)>thr){
  stop("total not correct")
}

save(Estimaciones, file = "Estimaciones.RData")
save(BigLucy, file = "BigLucy.RData")




