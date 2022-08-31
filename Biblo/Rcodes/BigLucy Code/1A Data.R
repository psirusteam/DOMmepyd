rm(list = ls())
library(TeachingSampling)
library(dplyr)
data("BigLucy")

BigLucy$ZoneLevel <- as.factor(paste(BigLucy$Zone, BigLucy$Level))

summary(BigLucy$ZoneLevel)
levels(BigLucy$ZoneLevel)

Total <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(Income = sum(Income)) %>%
  arrange(ZoneLevel)

N <- BigLucy %>%
  group_by(ZoneLevel) %>%
  summarise(N.county = n()) %>%
  arrange(ZoneLevel)

plot(Total$Income)
Estimaciones <- data.frame(N, Total$Income)
Estimaciones$Mean <- Estimaciones$Total.Income/Estimaciones$N.county
save(Estimaciones, file = "Estimaciones.RData")

BigLucy$I <- 1
save(BigLucy, file = "BigLucy.RData")

(Total1 <- sum(Estimaciones$Mean))

