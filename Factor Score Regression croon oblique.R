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
library(sem)
library(semPlot)

#------------------/ Data
data <- read_sav("Bases finales/VQ.sav") 

items <- data %>% select(starts_with("vq"),
                         starts_with("spanas"),
                         starts_with("aaq"),
                         starts_with("swls"),
                         starts_with("cfq")) 


#------------------/ Regresión jerárquica con SEM: step 1
modelo <- '
    # measurement model
     AAQ  =~ aaq_01 + aaq_02 + aaq_03 + aaq_04 + aaq_05 + aaq_06 + aaq_07
     SWLS =~ swls_01 + swls_02 + swls_03 + swls_04 + swls_05

  # regressions
    SWLS ~ AAQ 
    
  # residual correlations
    aaq_01 ~~ aaq_04
'

estimation <- lavaan:::sam(model = modelo, 	
              data = as.matrix(items), 
              sam.method = "global",
              estimator="MLR")
#semPaths(estimation)
summary(estimation,fit.measures=T,standardized=T)
standardizedSolution(estimation, type = "std.all")
inspect(estimation,"r2")

#------------------/ Regresión jerárquica con SEM: step 2
modelo <- '
  # measurement model
     APOSI =~ spanas_03 + spanas_05  + spanas_07 + spanas_08 + spanas_10
     ANEGA =~ spanas_01 + spanas_02  + spanas_04 + spanas_06 + spanas_09
     AAQ =~ aaq_01 + aaq_02 + aaq_03 + aaq_04 + aaq_05 + aaq_06 + aaq_07
     SWLS =~ swls_01 + swls_02 + swls_03 + swls_04 + swls_05

  #Orthogonal
     APOSI ~~ ANEGA

  # regressions
    SWLS ~ AAQ + APOSI + ANEGA
    
  # residual correlations
    aaq_01 ~~ aaq_04
'

estimation <- lavaan:::sam(model = modelo, 	
                           data = as.matrix(items), 
                           sam.method = "global",
                           estimator="MLR")

#semPaths(estimation)
summary(estimation,fit.measures=T,standardized=T)
standardizedSolution(estimation, type = "std.all")
inspect(estimation,"r2")

#------------------/ Regresión jerárquica con SEM: step 3
modelo <- '
  # measurement model
     APOSI =~ spanas_03 + spanas_05  + spanas_07 + spanas_08 + spanas_10
     ANEGA =~ spanas_01 + spanas_02  + spanas_04 + spanas_06 + spanas_09
     AAQ =~ aaq_01 + aaq_02 + aaq_03 + aaq_04 + aaq_05 + aaq_06 + aaq_07
     SWLS =~ swls_01 + swls_02 + swls_03 + swls_04 + swls_05
     CFQ =~ cfq_01 + cfq_02 + cfq_03 + cfq_04 + cfq_05 + cfq_06 + cfq_07
           
     #Orthogonal
      APOSI ~~ ANEGA

  # regressions
    SWLS ~ AAQ + APOSI + ANEGA + CFQ
    
  # residual correlations
    aaq_01 ~~ aaq_04
    cfq_02~~cfq_03
    cfq_01~~cfq_02
'

estimation <- lavaan:::sam(model = modelo, 	
                           data = as.matrix(items), 
                           sam.method = "global",
                           estimator="MLR")

#semPaths(estimation)
summary(estimation,fit.measures=T,standardized=T)
standardizedSolution(estimation, type = "std.all")
inspect(estimation,"rsquare")

estimation <- 
  lavaan:::sam(model = modelo, data = as.matrix(items), estimator = "MLR",sam.method = "local")
summary(estimation,fit.measures=T,standardized=T)
standardizedSolution(estimation, type = "std.all")
inspect(estimation,"r2")
semPaths(estimation)



#------------------/ Regresión jerárquica con SEM: step 4
modelo <- '
  # measurement model
     Obstruction =~ vq_01 + vq_02  + vq_06 + vq_08 + vq_10
     Progress =~ vq_03 + vq_04 + vq_05 + vq_07 + vq_09
     APOSI =~ spanas_03 + spanas_05  + spanas_07 + spanas_08 + spanas_10
     ANEGA =~ spanas_01 + spanas_02  + spanas_04 + spanas_06 + spanas_09
     AAQ =~ aaq_01 + aaq_02 + aaq_03 + aaq_04 + aaq_05 + aaq_06 + aaq_07
     SWLS =~ swls_01 + swls_02 + swls_03 + swls_04 + swls_05
     CFQ =~ cfq_01 + cfq_02 + cfq_03 + cfq_04 + cfq_05 + cfq_06 + cfq_07
           
     #Orthogonal
Obstruction ~~ Progress
      APOSI ~~ ANEGA

  # regressions
    SWLS ~ AAQ + APOSI + ANEGA + CFQ + Progress + Obstruction
    
  # residual correlations
    aaq_01 ~~ aaq_04
    cfq_02~~cfq_03
    cfq_01~~cfq_02
'

estimation <- lavaan:::sam(model = modelo, 	
                           data = as.matrix(items), 
                           sam.method = "global",
                           estimator="MLR")
#semPaths(estimation)
summary(estimation,fit.measures=T,standardized=T)
standardizedSolution(estimation, type = "std.all")
inspect(estimation,"r2")

estimation <- 
  lavaan:::sam(model = modelo, data = as.matrix(items), estimator = "MLR",sam.method = "local")
summary(estimation,fit.measures=T,standardized=T)
standardizedSolution(estimation, type = "std.all")
inspect(estimation,"r2")

estimation<-  lavaan:::fsr(modelo, data = as.matrix(items), se = "standard", output = "lavaan",fsr.method = "Croon")

res <- lavPredict(estimation)
#names(res)
summary(lm("SWLS ~ AAQ",data=data.frame(final)))
summary(lm("SWLS ~ AAQ + APOSI + ANEGA",data=data.frame(final)))
summary(lm("SWLS ~ AAQ + APOSI + ANEGA + CFQ",data=data.frame(final)))
summary(lm("SWLS ~ AAQ + APOSI + ANEGA + CFQ + Progress + Obstruction",data=data.frame(final)))
