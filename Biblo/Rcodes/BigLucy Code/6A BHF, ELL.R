rm(list = ls())
options(digits = 3)
library(TeachingSampling)
library(dplyr)
library(sae)
library(lme4)

load("Estimaciones.Rdata")
load("data.sam.Rdata")
load("BigLucy.Rdata")

total1 <- 114812
total2 <- 114812
thr <- 10000

#################################
## EBLUP in nested error model ##
## Battese - Harter - Fuller   ##
#################################

#data(incomedata)
#data("Xoutsamp")

##creating the mean X matrix
means <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(Employees = mean(Employees),
            Taxes = mean(Taxes)) %>%
  arrange(ZoneLevel)
means$ZoneLevel <- as.factor(means$ZoneLevel)

#population size
popsize <- as.data.frame(Estimaciones$N.county)
popsize <- cbind(Estimaciones$ZoneLevel,popsize)
colnames(popsize) <- c("ZoneLevel","N")
popsize$ZoneLevel <- as.factor(popsize$ZoneLevel)

#domains
domains <- as.factor(data.sam$ZoneLevel)

#regression
BHF.reg <- eblupBHF(Income ~ Employees + Taxes, dom=ZoneLevel,
               meanxpop=means, popnsize=popsize, 
               data=data.sam)

# Entering estimators for sampled
Estimaciones$BHF <- NA
for(i in 1:nrow(BHF.reg$eblup)){
  for (j in 1:nrow(Estimaciones)){
    if (as.character(Estimaciones$Zone[j]) == as.character(BHF.reg$eblup[i,1])){
      Estimaciones$BHF[j] = BHF.reg$eblup$eblup[i]
    }
  }
}

# NON-SAMPLED COUNTIES

# data and betas 
nonsample <- Estimaciones[Estimaciones$n.county == 0,]$ZoneLevel
nonsampled_BigLucy <- BigLucy[BigLucy$ZoneLevel %in% nonsample,]
nonsampleX <- cbind(1,nonsampled_BigLucy$Employees, 
                    nonsampled_BigLucy$Taxes)
fix_betas <- as.matrix(BHF.reg$fit$fixed)

#constructing vector of Yid estimates
nonsampled_BigLucy$Yid <- NA
nonsampled_BigLucy$Yid <- nonsampleX %*% fix_betas

#constructing the estimated means
nonsampled_estimates <- nonsampled_BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(summed = sum(Yid), 
            estimates = summed/sum(I)) %>%
  arrange(ZoneLevel)

#placing the values in the dataset
for(i in 1:nrow(nonsampled_estimates)){
  for (j in 1:nrow(Estimaciones)){
    if (as.character(Estimaciones$ZoneLevel[j]) == 
        as.character(nonsampled_estimates$ZoneLevel[i])){
      Estimaciones$BHF[j] = nonsampled_estimates$estimates[i]
    }
  }
}

if(abs(sum(Estimaciones$BHF, na.rm=TRUE)-total2)>thr){
  stop("total not correct")
}

save(Estimaciones, file = "Estimaciones.RData")
save(BigLucy, file = "BigLucy.RData")

#####################
####     ELL     ####
#####################

# income transformation
c <- 0
BigLucy$LogIncome = log(BigLucy$Income + c)
data.sam$LogIncome = log(data.sam$Income + c)

# qqnorm(BigLucy$LogIncome)
# qqline(BigLucy$LogIncome, col = 2)
# hist(BigLucy$LogIncome)

#regression
#Las UPMs son Zone
ELLreg <- lmer(LogIncome ~ 1 + log(Employees) +
                 log(Taxes) + (1|Zone), 
               data = data.sam)
NI <- length(unique(BigLucy$Zone))

#from output
sd_model <- as.data.frame(summary(ELLreg)$varcor)$sdcor
sd_u <- sd_model[1]
sd_e <- sd_model[2]

#creating X and beta matrix: 
X = cbind(1, log(BigLucy$Employees), log(BigLucy$Taxes))
betas = as.matrix(ELLreg@beta)

#counties
counties <- as.data.frame(Estimaciones$ZoneLevel)

Estimaciones$ELL <- NA
ELL <- 0
B <- 500

for (i in 1:B){
  print(i)
  #random
  random_effects <- rnorm(NI,0,sd_u)
  errors <- rnorm(nrow(X),0,sd_e)
  
  #XB and zone
  matrix <- cbind.data.frame(BigLucy$ZoneLevel, X%*%betas)
  colnames(matrix) <- c("ZoneLevel","XB")
  
  # attaching random effects
  matrix$RE <- BigLucy$Zone
  levels(matrix$RE) <- random_effects
  matrix$RE <- as.numeric(as.character(matrix$RE))
  
  #attaching errors
  matrix$errors <- errors
  
  #summing for Yid predictions
  matrix$Yid <- matrix$XB + matrix$RE + matrix$errors
  matrix$Yid <- exp(matrix$Yid) - c
  
  # summing the Yid
  estimates <- matrix %>%
    group_by(ZoneLevel) %>%
    summarize(estimates = sum(Yid))
  
  #updating for iteration purposes
  ELL <- ELL + (estimates$estimates / Estimaciones$N.county)

}

Estimaciones$ELL <- ELL / B

if(abs(sum(Estimaciones$ELL, na.rm=TRUE)-total1)>thr){
  stop("total not correct")
}

save(Estimaciones, file = "Estimaciones.RData")
save(BigLucy, file = "BigLucy.RData")

