#------------------/ Proyecto de investigación: Adaptación del VQ
options(scipen=999)
setwd("C:/Users/Angel/Desktop/Proyectos de investigación/Valuing questionnaire/Análisis")

#------------------/ Libraries
library(tidyverse)
library(haven)
library(lavaan)
library(psych)
library(openxlsx)
library(TAM)

#------------------/ Data
data <- read_sav("Bases finales/VQ.sav") 
items <- data %>% select(starts_with("vq")) 

#------------------/ Confiabilidad
#---------Obstruction
modelo.con <- "Obstruction =~ vq_01 + vq_02 + vq_06 + vq_08 + vq_10"
modelo.tau <- "Obstruction =~ v1*vq_01 + v1*vq_02 + v1*vq_06 + v1*vq_08 + v1*vq_10"
modelo.par <- "Obstruction =~ v1*vq_01 + v1*vq_02 + v1*vq_06 + v1*vq_08 + v1*vq_10
               vq_01~~v2*vq_01
               vq_02~~v2*vq_02
               vq_06~~v2*vq_06
               vq_08~~v2*vq_08
               vq_10~~v2*vq_10"

estimation.con <- cfa(model = modelo.con, data = items,estimator="MLR")	
estimation.tau <- cfa(model = modelo.tau, data = items,estimator="MLR")	
estimation.par <- cfa(model = modelo.par, data = items,estimator="MLR")	

comparisson <- cbind(fitMeasures(estimation.con, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"),
                     fitMeasures(estimation.tau, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"),
                     fitMeasures(estimation.par, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"))
comparisson <- data.frame(comparisson)
names(comparisson) <- c("congenerico","tau equivalente","paralelo")
comparisson <- round(comparisson,3)
write.csv(comparisson,"4. Reliability analysis/Obstruction measurement models.csv")
cargas <- inspect(estimation.con,what="std")$lambda
omega <-  function(cargas) { omega <- sum(cargas)^2/(sum(cargas)^2 + sum(1-(cargas^2)))
return(omega)  }
omega_obs <- omega(cargas)

#---------PRogress
modelo.con <- "Progress =~ vq_03 + vq_04 + vq_05 + vq_07 + vq_09"
modelo.tau <- "Progress =~ v1*vq_03 + v1*vq_04 + v1*vq_05 + v1*vq_07 + v1*vq_09"
modelo.par <- "Progress =~ v1*vq_03 + v1*vq_04 + v1*vq_05 + v1*vq_07 + v1*vq_09
               vq_03~~v2*vq_03
               vq_04~~v2*vq_04
               vq_05~~v2*vq_05
               vq_07~~v2*vq_07
               vq_09~~v2*vq_09"

estimation.con <- cfa(model = modelo.con, data = items,estimator="MLR")	
estimation.tau <- cfa(model = modelo.tau, data = items,estimator="MLR")	
estimation.par <- cfa(model = modelo.par, data = items,estimator="MLR")	


comparisson <- cbind(fitMeasures(estimation.con, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"),
                     fitMeasures(estimation.tau, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"),
                     fitMeasures(estimation.par, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"))
comparisson <- data.frame(comparisson)
names(comparisson) <- c("congenerico","tau equivalente","paralelo")
comparisson <- round(comparisson,3)
write.csv(comparisson,"4. Reliability analysis/Progress measurement models.csv")

summary(estimation.tau,fit.measures=T,standardized=T)

cargas <- inspect(estimation.con,what="std")$lambda
omega <-  function(cargas) { omega <- sum(cargas)^2/(sum(cargas)^2 + sum(1-(cargas^2)))
return(omega)  }
omega_pro <- omega(cargas)

#-------------------------------------------------# Confiabilidad
omega <- rbind(omega_obs,omega_pro)
alfa <- rbind(alpha(items[,c("vq_01","vq_02","vq_06","vq_08","vq_10")])$total[1],
              alpha(items[,c("vq_03","vq_04","vq_05","vq_07","vq_09")])$total[1])
alfastd <- rbind(alpha(items[,c("vq_01","vq_02","vq_06","vq_08","vq_10")])$total[2],
                 alpha(items[,c("vq_03","vq_04","vq_05","vq_07","vq_09")])$total[2])

confiabilidad <- cbind(omega,alfa,alfastd)
rownames(confiabilidad) <- c("Obstruction","Progress")
colnames(confiabilidad) <- c("omega","Alfa","Alfa estandarizado")
write.xlsx(confiabilidad,"4. Reliability analysis/Confiabilidad.xlsx",row.names=T)
