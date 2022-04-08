getwd()
setwd("C:/Users/Francisco/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/Doctorado - COLSON/Semestre 2/EACS/Prácticas R/Resultados preliminares")

library(dplyr)
library(survey)
library(foreign)
library(tidyverse)

sdem21<-read.dbf("ENOEN_SDEMT121.dbf")
sdem21

class(sdem21)
names(sdem21)

sum(sdem21$FAC_TRI)
sdem21$FAC_TRI

sdem21$EDA=as.numeric(as.character(sdem21$EDA))
class(sdem21$EDA)

sdem21 %>% filter(R_DEF=="00", C_RES==1|C_RES==3)%>% summarise(POBLACION=sum(FAC_TRI))
sdem21

#PoblaciÃ³n de 15 aÃ±os o mÃ¡s
sdem21 %>% filter(R_DEF=="00",C_RES==1|C_RES==3,EDA>=15 & EDA<=98)%>%summarise(POBLACION_15omas=sum(FAC_TRI))

#PoblaciÃ³n Economicamente activa
sdem21%>% filter(R_DEF=="00", C_RES==1|C_RES==3,EDA>=15 & EDA<=98,CLASE1==1)%>%summarise(PEA=sum(FAC_TRI))

#PEA Ocupada
sdem21%>% filter(R_DEF=="00", C_RES==1|C_RES==3,EDA>=15 & EDA<=98,CLASE1==1,CLASE2==1)%>%summarise(PEA_Ocupada=sum(FAC_TRI))

#PEA Subordinados y remunerados
sdem21%>% filter(R_DEF=="00",C_RES==1|C_RES==3,EDA>=15 & EDA<=98,CLASE1==1,CLASE2==1,POS_OCU==1)%>%summarise(PEA_Subor=sum(FAC_TRI))

#PEA EMPLEADORES
sdem21 %>% filter(R_DEF=="00",C_RES==1|C_RES==3,EDA>=15 & EDA<=98,CLASE1==1,CLASE2==1,POS_OCU==2)%>%summarise(PEA_Empleadores=sum(FAC_TRI))

#Seleccionamos solo a la poblaciÃ³n ocupada, con remuneraciÃ³n
ocupados21=sdem21 %>% 
  filter(CLASE2 == 1,
         ING_X_HRS>0,
         C_RES==1 | C_RES==3,
         EDA>=15 & EDA<=98)%>%
  select(FAC_TRI,EST_D_TRI, UPM,ING_X_HRS,INGOCUP,HRSOCUP, ANIOS_ESC, NIV_INS,EDA, SEX,ENT)



mean(ocupados21$INGOCUP)
mean(ocupados21$ING_X_HRS)
mean(ocupados21$EDA)
mean(ocupados21$HRSOCUP)
mean(ocupados21$ANIOS_ESC)
mean(ocupados21$NIV_INS)
     

rm(sdem21)
library(survey)
options(survey.lonely.psu="adjust")

#EspecificaciÃ³n de la muestra compleja (estratificada y multietÃ¡pica)
ds_ocupados21<- svydesign(id = ~UPM, strata = ~EST_D_TRI, weights = ~FAC_TRI, nest=TRUE, data=ocupados21)

#Obtener media de ingreso
svymean(~INGOCUP, ds_ocupados21)
confint(svymean(~INGOCUP, ds_ocupados21))

edos_norte <-c(2,26,8,5,19,28)
ocupados_nortenos <- ds_ocupados21$variables %>% filter(ENT %in% edos_norte)

write_csv(ocupados_nortenos,"ocupados_nortenos.csv")

mean(ocupados_nortenos$INGOCUP)
mean(ocupados_nortenos$ING_X_HRS)
mean(ocupados_nortenos$EDA)
mean(ocupados_nortenos$HRSOCUP)
mean(ocupados_nortenos$ANIOS_ESC)
mean(ocupados_nortenos$NIV_INS)


