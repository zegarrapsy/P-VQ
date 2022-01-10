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
library(semTools)

#------------------/ Data
data <- read_sav("Bases finales/VQ.sav") 

#------------------/ CFA
items <- data %>% select(starts_with("vq")) 
modelo <- "Obstruction =~ vq_01 + vq_02  + vq_06 + vq_08 + vq_10
           Progress =~ vq_03 + vq_04 + vq_05 + vq_07 + vq_09"
estimation <- cfa(model = modelo, 	
                  data = data, 	
                  estimator="MLR")

#------------------/ CFA para hombres y mujeres
hombres <- data %>% dplyr::filter(sexo=="1")
modelo <- "Obstruction =~ vq_01 + vq_02  + vq_06 + vq_08 + vq_10
           Progress =~ vq_03 + vq_04 + vq_05 + vq_07 + vq_09"
estimation <- cfa(model = modelo, 	
                  data = hombres, 	
                  estimator="MLR")
fitMeasures(estimation, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix")
summary(estimation,fit.measures=T,standardized=T)

mujeres <- data %>% dplyr::filter(sexo=="2")
modelo <- "Obstruction =~ vq_01 + vq_02  + vq_06 + vq_08 + vq_10
           Progress =~ vq_03 + vq_04 + vq_05 + vq_07 + vq_09"
estimation <- cfa(model = modelo, 	
                  data = mujeres, 	
                  estimator="MLR")

summary(estimation,fit.measures=T,standardized=T)
fitMeasures(estimation, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix")

#------------------/ Global
global <- measurementInvariance(model=modelo,data=data,group="sexo",strict=T,estimator="MLR")

#------------------/ Configural
estimation.con <- cfa(model = modelo, 	
                  data = data, 	
                  estimator="MLR",
                  group = "sexo")

#------------------/ Metric
estimation.met <- cfa(model = modelo, 	
                  data = data, 	
                  estimator="MLR",
                  group = "sexo",
                  group.equal=c("loadings"))

#------------------/ Scalar
estimation.sca <- cfa(model = modelo, 	
                  data = data, 	
                  estimator="MLR",
                  group = "sexo",
                  group.equal=c("loadings", "intercepts"))

#------------------/ Strict
estimation.str <- cfa(model = modelo, 	
                  data = data, 	
                  estimator="MLR",
                  group = "sexo",
                  group.equal=c("loadings", "intercepts", "residuals"))

comparisson <- cbind(fitMeasures(estimation.con, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"),
                     fitMeasures(estimation.met, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"),
                     fitMeasures(estimation.sca, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"),
                     fitMeasures(estimation.str, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust", "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr","wrmr"),output = "matrix"))
comparisson <- data.frame(comparisson)
names(comparisson) <- c("Configural","Métrica","Scalar","Strict")
comparisson <- round(comparisson,3)

write.csv(comparisson,"5. Fairness analysis/Invarianza.csv")

#------------------/ Chi square test
anova(estimation.con,estimation.met)
anova(estimation.met,estimation.sca)
anova(estimation.sca,estimation.str)

