rm(list = ls())
library(dplyr)
setwd("/Users/psirusteam/Desktop/SAE codes/Uruguay Code")
load("encuesta11.RData")
load("censo.RData")

# Loading files
censo$I <- 1
encuesta11$I <- 1

# CENSUS -----------------------------------------------------

# Constructing the data frame (small) that we will use
CensoS <- data.frame(
  depto = censo$depto, secc = censo$secc,
  segm = censo$segm, loca = censo$locagr,
  ccz = censo$ccz, sexo = censo$sexo, alfabet = censo$alfabet,
  condact = censo$condact5, edad = censo$edad,
  anoest = censo$anoest, areageo = censo$areageo,
  I = censo$I
)
CensoS <- CensoS[CensoS$depto == "Montevideo", ]

# Age groups
CensoS$age_group <- cut(CensoS$edad, c(-2, 20, 35, 55, 120))
levels(CensoS$age_group) <- c(1, 2, 3, 4)

# 2 alfabet covariables
CensoS$alpha_numeric <- as.numeric(CensoS$alfabet)
CensoS$alfano <- ifelse(CensoS$alpha_numeric == 2, 1, 0)
CensoS$alfasi <- ifelse(CensoS$alpha_numeric == 3, 1, 0)

CensoS[CensoS$alfabet == "NA", ]$alfano <- NA
CensoS[CensoS$alfabet == "NA", ]$alfasi <- NA

# 4 condact covariables
CensoS$condact_numeric <- as.numeric(CensoS$condact)
CensoS$condacto <- ifelse(CensoS$condact_numeric == 2, 1, 0)
CensoS$condactc <- ifelse((CensoS$condact_numeric == 3) |
  (CensoS$condact_numeric == 4), 1, 0)
CensoS$condactj <- ifelse(CensoS$condact_numeric == 5, 1, 0)
CensoS$condacti <- ifelse(CensoS$condact_numeric == 6, 1, 0)

CensoS[CensoS$condact == "NA", ]$condacto <- NA
CensoS[CensoS$condact == "NA", ]$condactc <- NA
CensoS[CensoS$condact == "NA", ]$condactj <- NA
CensoS[CensoS$condact == "NA", ]$condacti <- NA

# creating new section variable for iteration purposes
sections <- unique(CensoS$secc)
new_sections <- c(1:length(sections))
CensoS$sec2 <- NA

for (i in 1:25) {
  CensoS[CensoS$secc == sections[i], ]$sec2 <- new_sections[i]
}

# Constructing H and M censos
CensoM <- CensoS[CensoS$sexo == "Mujer", ]
CensoH <- CensoS[CensoS$sexo == "Hombre", ]

# SAMPLE -----------------------------------------------------

# Constructing the sample data frame that we will use
SamS <- data.frame(
  depto = encuesta11$depto, secc = encuesta11$secc,
  segm = encuesta11$segm, loca = encuesta11$locagr,
  ccz = encuesta11$ccz, sexo = encuesta11$sexo,
  alfabet = encuesta11$alfabet, condact = encuesta11$condact5,
  edad = encuesta11$edad, anoest = encuesta11$anoest,
  areageo = encuesta11$areageo, ing = encuesta11$ingcorte,
  li = encuesta11$li, lp = encuesta11$lp,
  pobreza = encuesta11$pobreza, factorex = encuesta11$factorex,
  I = encuesta11$I
)

# New section variable for iteration purposes
SamS$sec2 <- NA
for (i in 1:25) {
  SamS[SamS$secc == sections[i], ]$sec2 <- new_sections[i]
}

# 2 alfabet covariables
SamS$alpha_numeric <- as.numeric(SamS$alfabet)
SamS$alfano <- ifelse(SamS$alpha_numeric == 2, 1, 0)
SamS$alfasi <- ifelse(SamS$alpha_numeric == 3, 1, 0)

SamS$alfano <- ifelse(SamS$alfabet == "NA", NA, SamS$alfano)
SamS$alfasi <- ifelse(SamS$alfabet == "NA", NA, SamS$alfasi)
# SamS[SamS$alfabet == "NA",]$alfano <- NA
# SamS[SamS$alfabet == "NA",]$alfasi <- NA

# 4 condact covariables
SamS$condact_numeric <- as.numeric(SamS$condact)
SamS$condacto <- ifelse(SamS$condact_numeric == 2, 1, 0)
SamS$condactc <- ifelse((SamS$condact_numeric == 3) | (SamS$condact_numeric == 4), 1, 0)
SamS$condactj <- ifelse(SamS$condact_numeric == 5, 1, 0)
SamS$condacti <- ifelse(SamS$condact_numeric == 6, 1, 0)

SamS[SamS$condact == "NA", ]$condacto <- NA
SamS[SamS$condact == "NA", ]$condactc <- NA
SamS[SamS$condact == "NA", ]$condactj <- NA
SamS[SamS$condact == "NA", ]$condacti <- NA


SamS <- SamS[SamS$depto == "Montevideo", ]
SamM <- SamS[SamS$sexo == "Mujer", ]
SamH <- SamS[SamS$sexo == "Hombre", ]

# Age group
SamM$age_group <- cut(SamM$edad, c(-2, 20, 35, 55, 120))
levels(SamM$age_group) <- c(1, 2, 3, 4)

SamH$age_group <- cut(SamH$edad, c(-2, 20, 35, 55, 120))
levels(SamH$age_group) <- c(1, 2, 3, 4)

# Saving
save(CensoS, file = "CensoS.RData")
save(SamS, file = "SamS.RData")

save(CensoM, file = "CensoM.RData")
save(SamM, file = "SamM.RData")

save(CensoH, file = "CensoH.RData")
save(SamH, file = "SamH.RData")

T1 <- SamM %>%
  group_by(secc) %>%
  summarise(n = n()) %>%
  arrange(n)

as.data.frame(T1)

T2 <- CensoM %>%
  group_by(secc) %>%
  summarise(n = n()) %>%
  arrange(n)

as.data.frame(T2)
