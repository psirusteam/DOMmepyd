rm(list = ls())
set.seed(20150802)
library(TeachingSampling)
library(dplyr)

load("Estimaciones.Rdata")
load("BigLucy.RData")

# Level is the stratifying variable
summary(BigLucy$Level)
attach(BigLucy)
# Defines the size of each stratum
N1 <- summary(Level)[[1]]
N2 <- summary(Level)[[2]]
N3 <- summary(Level)[[3]]
N1;N2;N3
(Nh <- c(N1,N2,N3))
# Defines the sample size at each stratum 

n1 <- round(N1 * 0.02)
n2 <- round(N2 * 0.02)
n3 <- round(N3 * 0.02)
(nh<-c(n1,n2,n3))
# Draws indices from stratified sample
sam <- S.STSI(Level, Nh, nh)
# The information about the units in the sample is stored in an object called data
data.sam <- BigLucy[sam,]

data.sam$FEX <- NULL
data.sam$FEX[data.sam$Level == "Big"] <- Nh[1] / nh[1]
data.sam$FEX[data.sam$Level == "Medium"] <- Nh[2] / nh[2]
data.sam$FEX[data.sam$Level == "Small"] <- Nh[3] / nh[3]

data.sam$I <- 1

dim(data.sam)
save(data.sam, file = "data.sam.RData")

############# Generate the n.sample
BigLucy$sampleIk <- 0
BigLucy$sampleIk[sam] <- 1

n <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(n = sum(sampleIk)) %>%
  arrange(ZoneLevel)

Estimaciones$n.county <- n$n
(Total2 <- sum(Estimaciones[Estimaciones$n.county != 0,]$Mean))

Estimaciones <- Estimaciones[c("ZoneLevel", "N.county", "n.county", "Total.Income","Mean")]
save(Estimaciones, file = "Estimaciones.RData")
save(BigLucy, file = "BigLucy.RData")

detach(BigLucy)


