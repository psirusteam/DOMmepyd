rm(list = ls())
library(dplyr)
setwd("/Users/psirusteam/Desktop/SAE codes/Uruguay Code")
load("SamM.RData")
load("SamH.RData")
load("censoM.RData")
load("censoH.RData")

# Setting up our estimaciones matrix
sec_vector <- unique(CensoM$secc)
new_sec <- c(1:length(sec_vector))

# Hombre
EstimacionesH <- CensoH %>%
  group_by(secc) %>%
  summarise(N.sec = sum(I)) %>%
  arrange(secc)
EstimacionesH$sec2 <- new_sec
EstimacionesH <- EstimacionesH[,c("secc","sec2","N.sec")]

n <- SamH %>%
  group_by(secc) %>%
  summarize(n.sec = sum(I)) %>% 
  arrange(secc)
EstimacionesH$n.sec <- n$n.sec

# Mujer
EstimacionesM <- CensoM %>%
  group_by(secc) %>%
  summarise(N.sec = sum(I)) %>%
  arrange(secc)
EstimacionesM$sec2 <- new_sec
EstimacionesM <- EstimacionesM[,c("secc","sec2","N.sec")]

n <- SamM %>%
  group_by(secc) %>%
  summarize(n.sec = sum(I)) %>% 
  arrange(secc)
EstimacionesM$n.sec <- n$n.sec


save(EstimacionesH, file = "EstimacionesH.RData")
save(EstimacionesM, file = "EstimacionesM.RData")
