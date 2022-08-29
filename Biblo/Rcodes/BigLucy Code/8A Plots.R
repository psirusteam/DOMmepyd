rm(list = ls())
library(ggplot2)
ggplot2::theme_set(theme_bw())

# p + theme(axis.line=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks=element_blank())

library(reshape2)
library(gridExtra)
library(dplyr)
load("Estimaciones.Rdata")

EstimacionesMelt <- melt(Estimaciones, 
 id=c("ZoneLevel", "N.county", "n.county"),
  measure.vars = c("Mean", "HT", "Hajek", 
    "GREG", "PS_SYN", "reg1.syn", "reg2.syn", "compuesto",
    "FH", "BHF", "ELL", "CensusEB", "EBLog"))

# Direct Estimators

EM1 <- EstimacionesMelt %>%
  filter(variable %in% c("HT", "Mean", "Hajek", "GREG"))

q1 <- ggplot(EM1, aes(variable, value)) + 
  geom_boxplot() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

q2 <- ggplot(EM1, aes(value, colour = variable)) + 
  geom_density(adjust = 1.2) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

DirectQ <- gridExtra::grid.arrange(q1, q2, nrow = 2,
           top = "Estimadores Directos")
ggsave(filename = "DirectQ.pdf", plot = DirectQ, 
       device= "pdf")
  
p1 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = HT, colour = "HT"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador HT con el parámetro real",
        subtitle = "Triángulo: estimación HT - Círculo: media real",
        caption = "Fuente: elaboración propia") 

p2 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = Hajek, colour = "Hajek"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador Hajek con el parámetro real",
        subtitle = "Triángulo: estimación Hajek - Círculo: media real",
        caption = "Fuente: elaboración propia") 

p3 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = GREG, colour = "GREG"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador GREG con el parámetro real",
        subtitle = "Triángulo: estimación GREG - Círculo: media real",
        caption = "Fuente: elaboración propia") 


Direct <- gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
ggsave(filename = "Direct.pdf", plot = Direct, 
       device= "pdf")

# Indirect 

EM1 <- EstimacionesMelt %>%
  filter(variable %in% c("Mean", "PS_SYN", "reg1.syn", 
                         "reg2.syn", "compuesto"))

q1 <- ggplot(EM1, aes(variable, value)) + 
  geom_boxplot() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

q2 <- ggplot(EM1, aes(value, colour = variable)) + 
  geom_density(adjust = 1.2) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

SyntheticQ <- gridExtra::grid.arrange(q1, q2, nrow = 2,
              top = "Estimadores Sintéticos y compuestos")
ggsave(filename = "SyntheticQ.pdf", plot = SyntheticQ, 
       device= "pdf")

p1 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = PS_SYN, colour = "PS_SYN"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador PS_SYN con el parámetro real",
        subtitle = "Triángulo: estimación PS_SYN - Círculo: media real",
        caption = "Fuente: elaboración propia") 

p2 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = reg1.syn, colour = "reg1.syn"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador reg1.syn con el parámetro real",
        subtitle = "Triángulo: estimación reg1.syn - Círculo: media real",
        caption = "Fuente: elaboración propia") 

p3 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = reg2.syn, colour = "reg2.syn"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador reg2.syn con el parámetro real",
        subtitle = "Triángulo: estimación reg2.syn - Círculo: media real",
        caption = "Fuente: elaboración propia") 

p4 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = compuesto, colour = "compuesto"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador compuesto con el parámetro real",
        subtitle = "Triángulo: estimación compuesto - Círculo: media real",
        caption = "Fuente: elaboración propia") 

Synthetic <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave(filename = "Synthetic.pdf", plot = Synthetic, 
       device= "pdf")


# Model based - Fay Herriot

EM1 <- EstimacionesMelt %>%
  filter(variable %in% c("Mean", "HT", "FH"))

q1 <- ggplot(EM1, aes(variable, value)) + 
  geom_boxplot() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

q2 <- ggplot(EM1, aes(value, colour = variable)) + 
  geom_density(adjust = 1.2) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

AreaLevelQ <- gridExtra::grid.arrange(q1, q2, nrow = 2,
       top = "Estimador de Modelo de Área")
ggsave(filename = "AreaLevelQ.pdf", plot = AreaLevelQ, 
       device= "pdf")

p1 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = HT, colour = "HT"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador HT con el parámetro real",
        subtitle = "Triángulo: estimación HT - Círculo: media real",
        caption = "Fuente: elaboración propia") 

p2 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = FH, colour = "FH"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador FH con el parámetro real",
        subtitle = "Triángulo: estimación FH - Círculo: media real",
        caption = "Fuente: elaboración propia") 

AreaLevel <- gridExtra::grid.arrange(p1, p2, ncol = 2)
ggsave(filename = "AreaLevel.pdf", plot = AreaLevel, 
       device= "pdf")

# Model based - ELL BHF EBP 

EM1 <- EstimacionesMelt %>%
  filter(variable %in% c("Mean", "BHF", "ELL", "CensusEB", "EBLog"))

q1 <- ggplot(EM1, aes(variable, value)) + 
  geom_boxplot() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

q2 <- ggplot(EM1, aes(value, colour = variable)) + 
  geom_density(adjust = 1.2) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

UnitLevelQ <- gridExtra::grid.arrange(q1, q2, nrow = 2,
              top = "Estimador de Modelops de Unidad")
ggsave(filename = "UnitLevelQ.pdf", plot = UnitLevelQ, 
       device= "pdf")

p1 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = BHF, colour = "BHF"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador BHF con el parámetro real",
        subtitle = "Triángulo: estimación BHF - Círculo: media real",
        caption = "Fuente: elaboración propia") 

p2 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = ELL, colour = "ELL"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador ELL con el parámetro real",
        subtitle = "Triángulo: estimación ELL - Círculo: media real",
        caption = "Fuente: elaboración propia") 


p3 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = CensusEB, colour = "CensusEB"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador CensusEB con el parámetro real",
        subtitle = "Triángulo: estimación CensusEB - Círculo: media real",
        caption = "Fuente: elaboración propia") 


p4 <- ggplot(Estimaciones, aes(ZoneLevel, group = 1)) + 
  geom_point(aes(y = Mean, colour = "Mean"), 
             shape = 19, size = 4, col = "grey") +
  geom_point(aes(y = EBLog, colour = "EBLog"),
             shape = 2, colour = "blue4") + 
  theme(panel.grid.major = element_blank()) + 
  labs( x = "Área pequeña", y = "Estimación",
        title ="Comparación del estimador EBLog con el parámetro real",
        subtitle = "Triángulo: estimación EBLog - Círculo: media real",
        caption = "Fuente: elaboración propia") 

UnitLevel <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave(filename = "UnitLevel.pdf", plot = UnitLevel, 
        device= "pdf")
