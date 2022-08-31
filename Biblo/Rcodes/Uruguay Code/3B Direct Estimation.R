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



###################################
## Estimador de Horvitz-Thompson ##
###################################

# Hombre

attach(SamH)
Yd.HT.H <- as.matrix(tapply(ing * factorex, secc, sum))
EstimacionesH$HT <- Yd.HT.H/EstimacionesH$N.sec
detach(SamH)

save(EstimacionesH, file = "EstimacionesM.RData")

# Mujer

attach(SamM)
Yd.HT.M <- as.matrix(tapply(ing * factorex, secc, sum))
EstimacionesM$HT <- Yd.HT.M/EstimacionesM$N.sec
detach(SamM)

save(EstimacionesM, file = "EstimacionesM.RData")


###################
###    Hájek    ###
###################

# Hombre

#Creating estimated total
attach(SamH)
N_est <- tapply(factorex, secc, sum)
N_est <- as.matrix(N_est)
detach(SamH)

#Creating the estimator
EstimacionesH$Hajek <- Yd.HT.H / N_est

# Mujer

#Creating estimated total
attach(SamM)
N_est <- tapply(factorex, secc, sum)
N_est <- as.matrix(N_est)
detach(SamM)

#Creating the estimator
EstimacionesM$Hajek <- Yd.HT.M / N_est




#############################
##      GREG estimator     ##
#############################


# Hombre 
CensoH_new <- CensoH[,c("secc","condacto","condactc","condactj",
                        "alfasi")]
SamH_new <- SamH[,c("secc","factorex","ing","condacto","condactc","condactj",
                  "alfasi")]

# Constructing the individual regressions per group
GREGregs <- lmList(ing ~ condacto + condactc + 
                   condactj + alfasi | 
                   secc, SamH_new[complete.cases(SamH_new),])
GREGregs <- as.data.frame(lapply(GREGregs, coef))
GREGregs[is.na(GREGregs)] <- 0 


#constructing true X matrix
#attach(CensoH_new[complete.cases(CensoH_new),])
area_values <- CensoH_new[complete.cases(CensoH_new),] %>%
  group_by(secc) %>%
  summarise(condacto = mean(condacto), 
            condactc = mean(condactc),
            condactj = mean(condactj),
            alfasi = mean(alfasi)) %>%
  arrange(secc)
#detach(CensoH_new[complete.cases(CensoH_new),])

#constructing HT X values and difference
attach(SamH_new[complete.cases(SamH_new),])
area_values$condacto.HT <- tapply(factorex*condacto, secc, sum)
area_values$condacto.HT <- area_values$condacto.HT/EstimacionesH$N.sec
area_values$condacto.diff <- area_values$condacto - area_values$condacto.HT

area_values$condactc.HT <- tapply(factorex*condactc, secc, sum)
area_values$condactc.HT <- area_values$condactc.HT/EstimacionesH$N.sec
area_values$condactc.diff <- area_values$condactc - area_values$condactc.HT

area_values$condactj.HT <- tapply(factorex*condactj, secc, sum)
area_values$condactj.HT <- area_values$condactj.HT/EstimacionesH$N.sec
area_values$condactj.diff <- area_values$condactj - area_values$condactj.HT

# area_values$condacti.HT <- tapply(factorex*condacti, secc, sum)
# area_values$condacti.HT <- area_values$condacti.HT/EstimacionesH$N.sec
# area_values$condacti.diff <- area_values$condacti - area_values$condacti.HT

area_values$alfasi.HT <- tapply(factorex*alfasi, secc, sum)
area_values$alfasi.HT <- area_values$alfasi.HT/EstimacionesH$N.sec
area_values$alfasi.diff <- area_values$alfasi - area_values$alfasi.HT

# area_values$alfano.HT <- tapply(factorex*alfano, secc, sum)
# area_values$alfano.HT <- area_values$alfano.HT/EstimacionesH$N.sec
# area_values$alfano.diff <- area_values$alfano - area_values$alfano.HT
detach(SamH_new[complete.cases(SamH_new),])


#for loop to construct estimator
EstimacionesH$GREG <- rep(0, 25)
for (i in c(1:25)){
  EstimacionesH$GREG[i] =
    EstimacionesH$HT[i] +
    area_values$condacto.diff[i]*GREGregs[2,i] +
    area_values$condactc.diff[i]*GREGregs[3,i] +
    area_values$condactj.diff[i]*GREGregs[4,i] +
    area_values$alfasi.diff[i]*GREGregs[5,i]
}

# Mujer
CensoM_new <- CensoM[,c("secc","condacto","condactc","condactj",
                        "alfasi")]
SamM_new <- SamM[,c("secc","factorex","ing","condacto","condactc","condactj",
                    "alfasi")]

# Constructing the individual regressions per group
SamM_new <- SamM[,c("secc","factorex","ing","condacto","condactc","condactj",
                    "alfasi")]
GREGregs <- lmList(ing ~ condacto + condactc + 
                     condactj + alfasi | 
                     secc, SamM_new[complete.cases(SamM_new),])
GREGregs <- as.data.frame(lapply(GREGregs, coef))
GREGregs[is.na(GREGregs)] <- 0 


#constructing true X matrix
#attach(CensoH_new[complete.cases(CensoH_new),])
area_values <- CensoM_new[complete.cases(CensoM_new),] %>%
  group_by(secc) %>%
  summarise(condacto = mean(condacto), 
            condactc = mean(condactc),
            condactj = mean(condactj),
            alfasi = mean(alfasi)) %>%
  arrange(secc)
#detach(CensoH_new[complete.cases(CensoH_new),])

#constructing HT X values and difference
attach(SamM_new[complete.cases(SamM_new),])
area_values$condacto.HT <- tapply(factorex*condacto, secc, sum)
area_values$condacto.HT <- area_values$condacto.HT/EstimacionesM$N.sec
area_values$condacto.diff <- area_values$condacto - area_values$condacto.HT

area_values$condactc.HT <- tapply(factorex*condactc, secc, sum)
area_values$condactc.HT <- area_values$condactc.HT/EstimacionesM$N.sec
area_values$condactc.diff <- area_values$condactc - area_values$condactc.HT

area_values$condactj.HT <- tapply(factorex*condactj, secc, sum)
area_values$condactj.HT <- area_values$condactj.HT/EstimacionesM$N.sec
area_values$condactj.diff <- area_values$condactj - area_values$condactj.HT

# area_values$condacti.HT <- tapply(factorex*condacti, secc, sum)
# area_values$condacti.HT <- area_values$condacti.HT/EstimacionesH$N.sec
# area_values$condacti.diff <- area_values$condacti - area_values$condacti.HT

area_values$alfasi.HT <- tapply(factorex*alfasi, secc, sum)
area_values$alfasi.HT <- area_values$alfasi.HT/EstimacionesM$N.sec
area_values$alfasi.diff <- area_values$alfasi - area_values$alfasi.HT

# area_values$alfano.HT <- tapply(factorex*alfano, secc, sum)
# area_values$alfano.HT <- area_values$alfano.HT/EstimacionesH$N.sec
# area_values$alfano.diff <- area_values$alfano - area_values$alfano.HT
detach(SamM_new[complete.cases(SamM_new),])


#for loop to construct estimator
EstimacionesM$GREG <- rep(0, 25)
for (i in c(1:25)){
  EstimacionesM$GREG[i] =
    EstimacionesM$HT[i] +
    area_values$condacto.diff[i]*GREGregs[2,i] +
    area_values$condactc.diff[i]*GREGregs[3,i] +
    area_values$condactj.diff[i]*GREGregs[4,i] +
    area_values$alfasi.diff[i]*GREGregs[5,i]
}


save(EstimacionesH, file = "EstimacionesH.RData")
save(EstimacionesM, file = "EstimacionesM.RData")



