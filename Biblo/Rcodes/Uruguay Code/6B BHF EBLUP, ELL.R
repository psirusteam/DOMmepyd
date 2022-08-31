rm(list = ls())
options(digits = 2)
library(TeachingSampling)
library(dplyr)
library(sae)
library(lme4)
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


#################################
## EBLUP in nested error model ##
## BHF model                   ##
#################################

# HOMBRE ------------------------

##creating the mean X matrix
means <- CensoH %>%
  group_by(secc) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(secc)
means$secc <- as.factor(means$secc)

#population size
popsize <- as.data.frame(EstimacionesH$N.sec)
popsize <- cbind(EstimacionesH$secc,popsize)
colnames(popsize) <- c("secc","N")
popsize$secc <- as.factor(popsize$secc)

# #domains
# domains <- as.factor(SamH$secc)

#regression
BHF.reg <- eblupBHF(ing ~ condacto + condactc +
                    condactj + alfasi, dom=secc,
                    meanxpop=means, popnsize=popsize, 
                    data=SamH)

# Entering estimators 
EstimacionesH$eblup_BHF = BHF.reg$eblup[order(BHF.reg$eblup$domain),]$eblup


# MUJER ------------------------

##creating the mean X matrix
means <- CensoM %>%
  group_by(secc) %>%
  summarise(condacto = mean(na.omit(condacto)),
            condactc = mean(na.omit(condactc)),
            condactj = mean(na.omit(condactj)),
            alfasi = mean(na.omit(alfasi))) %>%
  arrange(secc)
means$secc <- as.factor(means$secc)

#population size
popsize <- as.data.frame(EstimacionesM$N.sec)
popsize <- cbind(EstimacionesM$secc,popsize)
colnames(popsize) <- c("secc","N")
popsize$secc <- as.factor(popsize$secc)

#domains
domains <- as.factor(SamM$secc)

#regression
BHF.reg <- eblupBHF(ing ~ condacto + condactc +
                      condactj + alfasi, dom=secc,
                    meanxpop=means, popnsize=popsize, data=SamM)

# Entering estimators 
EstimacionesM$eblup_BHF = BHF.reg$eblup[order(BHF.reg$eblup$domain),]$eblup





###########################
####        ELL        ####
#### World Bank method ####
###########################

# HOMBRE-------------------------------

# income transformation
c <- 1000
SamH$LogIng = log(SamH$ing + c)

SamH_new <- cbind.data.frame(SamH$secc,SamH$sec2,SamH$LogIng,SamH$condact_numeric,
                             SamH$condacto,SamH$condactc, SamH$condactj,
                             SamH$alfasi, SamH$edad)
colnames(SamH_new) <- c("secc","sec2","LogIng","condacto","condact_numeric",
                        "condactc", "condactj","alfasi","edad")
#regression
attach(SamH_new[complete.cases(SamH_new),]) 
ELLreg <- lmer(LogIng ~ condacto + condactc + condactj + alfasi + (1|secc))
detach(SamH_new[complete.cases(SamH_new),]) 

coef(ELLreg)


#from output
sd_u <- sqrt(as.numeric(VarCorr(ELLreg)))
sd_e <- sqrt(summary(ELLreg)$sigma^2)

#creating X and beta matrix: 
X = cbind.data.frame(as.factor(CensoH$secc), 1, CensoH$condacto, CensoH$condactc,
                     CensoH$condactj, CensoH$alfasi)
colnames(X)[1] <- 'secc'
colnames(X)[2] <- 'one'
X_nonzero = X[complete.cases(X),]
colnames(X_nonzero) <- c('secc','one','condacto','condactc',
                         'condactj','alfasi')
#colnames(X) <- c('secc','one','condact_numeric','edad')

N.sec_complete <- as.data.frame(X_nonzero) %>%
  group_by(secc) %>%
  summarize(N.sec = sum(one)) %>%
  arrange(secc)

X_nonzero <- as.matrix(X_nonzero[,-1])
betas = as.matrix(fixed.effects(ELLreg))

EstimacionesH$ELL <- NA
ELL <- 0
for (i in 1:100){
  print(i)
  #random
  random_effects <- rnorm(25,0,sd_u)
  errors <- rnorm(nrow(X_nonzero),0,sd_e)
  
  #XB and zone
  matrix <- cbind.data.frame(X[complete.cases(X),]$secc, X_nonzero%*%betas) 
  colnames(matrix) <- c("secc","XB")
  
  # attaching random effects
  matrix$RE <- X[complete.cases(X),]$secc 
  levels(matrix$RE) <- random_effects
  matrix$RE <- as.numeric(as.character(matrix$RE))
  
  #attaching errors
  matrix$errors <- errors
  
  #summing for Yid predictions
  matrix$Yid <- matrix$XB + matrix$RE + matrix$errors
  matrix$Yid <- exp(matrix$Yid) - c
  
  # summing the Yid
  estimates <- as.data.frame(matrix) %>%
    group_by(secc) %>%
    summarize(estimates = sum(Yid)) %>%
    arrange(secc)
  
  #updating for iteration purposes
  ELL <- ELL + (estimates$estimates / N.sec_complete$N.sec)
  
}
EstimacionesH$ELL <- ELL / i



# MUJER-------------------------------

# income transformation
c <- 800
SamM$LogIng = log(SamM$ing + c)

SamM_new <- cbind.data.frame(SamM$secc,SamM$sec2,SamM$LogIng,SamM$condact_numeric,
                             SamM$condacto,SamM$condactc, SamM$condactj,
                             SamM$alfasi, SamM$edad)
colnames(SamM_new) <- c("secc","sec2","LogIng","condacto","condact_numeric",
                        "condactc", "condactj","alfasi","edad")
#regression
attach(SamM_new[complete.cases(SamM_new),]) 
ELLreg <- lmer(LogIng ~ condacto + condactc + condactj + alfasi + (1|secc))
detach(SamM_new[complete.cases(SamM_new),]) 

#from output
sd_u <- sqrt(as.numeric(VarCorr(ELLreg)))
sd_e <- sqrt(summary(ELLreg)$sigma^2)

#creating X and beta matrix: 
X = cbind.data.frame(as.factor(CensoM$secc), 1, CensoM$condacto, CensoM$condactc,
                     CensoM$condactj, CensoM$alfasi)
colnames(X)[1] <- 'secc'
colnames(X)[2] <- 'one'
X_nonzero = X[complete.cases(X),]
colnames(X_nonzero) <- c('secc','one','condacto','condactc',
                         'condactj','alfasi')
#colnames(X) <- c('secc','one','condact_numeric','edad')

N.sec_complete <- as.data.frame(X_nonzero) %>%
  group_by(secc) %>%
  summarize(N.sec = sum(one)) %>%
  arrange(secc)

X_nonzero <- as.matrix(X_nonzero[,-1])
betas = as.matrix(fixed.effects(ELLreg))

EstimacionesM$ELL <- NA
ELL <- 0
for (i in 1:100){
  print(i)
  #random
  random_effects <- rnorm(25,0,sd_u)
  errors <- rnorm(nrow(X_nonzero),0,sd_e)
  
  #XB and zone
  matrix <- cbind.data.frame(X[complete.cases(X),]$secc, X_nonzero%*%betas) 
  colnames(matrix) <- c("secc","XB")
  
  # attaching random effects
  matrix$RE <- X[complete.cases(X),]$secc 
  levels(matrix$RE) <- random_effects
  matrix$RE <- as.numeric(as.character(matrix$RE))
  
  #attaching errors
  matrix$errors <- errors
  
  #summing for Yid predictions
  matrix$Yid <- matrix$XB + matrix$RE + matrix$errors
  matrix$Yid <- exp(matrix$Yid) - c
  
  # summing the Yid
  estimates <- as.data.frame(matrix) %>%
    group_by(secc) %>%
    summarize(estimates = sum(Yid)) %>%
    arrange(secc)
  
  #updating for iteration purposes
  ELL <- ELL + (estimates$estimates / N.sec_complete$N.sec)
  
}
EstimacionesM$ELL <- ELL / i


save(EstimacionesH, file = "EstimacionesH.RData")
save(EstimacionesM, file = "EstimacionesM.RData")
