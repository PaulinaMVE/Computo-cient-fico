library(visdat)
library(dplyr)
library(skimr)
library(insuranceData)
library(ggplot2)
?dataCar
data(dataCar)
str(dataCar) #estructura
summary(dataCar) #resumen estadístico de cada variable

#Resumen más detallado:
skim(dataCar)
head(dataCar,10) #primeras 10 filas
dim(dataCar) #dimensión del dataframe, filas y columnas

#Características del dataframe
glimpse(dataCar) #formato de cada variable
?glimpse #para consultar info de la función

#Conocer el nombre de las columnas, dos formas:
colnames(dataCar)
names(dataCar)

#Verificar si hay datos faltantes
miss <- any(is.na(dataCar))
miss
which() #para saber cuál es el NA

#Visualizar los datos
vis_dat(dataCar)
vis_miss(dataCar)

#Reporte con DataExplorer
#DataExplorer: :create_report(dataCar)

#Para conocer cuántos datos existen y las reclamaciones >=1
pol <-length(dataCar$numclaims)
claims <- sum(dataCar$numclaims>=1)
claims
(claims/pol)*100 #porcentaje

#otra opción:
sum(dataCar$clm>=1)

#Verificar si hay datos faltantes y el resumen estadistico -> anteriormente

#Verificar el top 5 de vehículos con mayor número de reclamaciones
#Contar el número de reclamaciones 
claims_tipo <- dataCar %>%
  group_by(veh_body) %>%
  summarise(totclaims = sum(numclaims)) %>%
  arrange(desc(totclaims))
#muestra solo los 5 primeros registros
top5<-head(claims_tipo,5)

ggplot(top5, aes(x= reorder(veh_body, -totclaims), y=totclaims))+
  geom_bar(stat = "identity",position = "dodge", fill="skyblue")+
  labs(title = "Top 5 tipos de vehículo con mayor no. de reclamaciones",
       x="Tipo", y="Total claims")+ theme_minimal()


#Número de pólizas por tipo de vehículo
tipo_pol <- dataCar %>%
  group_by(veh_body) %>%
  summarise(tipo_veh = n()) %>%
  arrange(desc(tipo_veh))
head(tipo_pol)


#Verificar el top 10 de vehículos con mayor monto de reclamaciones
claims_monto <- dataCar %>%
  group_by(veh_body) %>%
  summarise(totmonto = sum(claimcst0)) %>%
  arrange(desc(totmonto))
#muestra solo los 10 primeros registros
top10<-head(claims_monto,10)
top10

ggplot(top10, aes(x= reorder(veh_body, -totmonto), y=totmonto))+
  geom_bar(stat = "identity",position = "dodge", fill="skyblue")+
  labs(title = "Top 10 vehículos con mayor monto de reclamaciones",
       x="Tipo", y="Monto")+ theme_minimal()

#Realice un análisis contemplando el género
gen <- dataCar %>%
  group_by(gender)%>%
summarise(tipo_gen = n()) %>%
  arrange(desc(tipo_gen))
gen

claims_gen<- dataCar%>% 
  group_by(gender)%>% 
  summarise(totclaims=sum(numclaims))%>%
  arrange(desc(totclaims))
claims_gen

cost_gen<- dataCar%>% 
  group_by(gender)%>% 
  summarise(totcost=sum(claimcst0))%>%
  arrange(desc(totcost))
cost_gen

montos_gen<- dataCar%>%
  group_by(gender,veh_body,numclaims)%>%
  summarise(total_claims=sum(claimcst0))%>%
  arrange(desc(total_claims))
montos_gen

claims_veh_gender<- dataCar%>%
  group_by(gender,veh_body)%>%
  summarise(totclaims=sum(numclaims))%>%
  arrange(desc(totclaims))
claims_veh_gender

#Edad con género y el mayor número de reclamaciones
age_gen<- dataCar%>%
  group_by(gender,veh_body,agecat)%>%
  summarise(tot_claims=sum(numclaims))%>%
  arrange(desc(tot_claims))
age_gen

#Gráficos con ggplot2
ggplot(dataCar,aes(x=exposure, y=veh_value)) + 
  geom_point(color="darkblue") + 
  labs(title = "Gráfico dispersión Exposición vs Valor de Vehículo",x="Exposición", y="Valor vehículo")+ 
  theme_minimal()

#¿Qué vehículo tiene el máximo valor? Máximo valor del vehículo, tipo, monto de reclamación y exposición
#Vehículo con mayor valor
veh1<- dataCar%>%
  group_by(veh_body,veh_value,claimcst0,exposure)%>%
  summarise(valor_veh=max(veh_value))%>%
  arrange(desc(valor_veh))
veh1

#Con mayor monto de reclamación:
veh<- dataCar%>%
  group_by(veh_body,veh_value,claimcst0,exposure)%>%
  summarise(val_veh_c=max(claimcst0))%>%
  arrange(desc(val_veh_c))
veh

#Crear gráfico de barras del número de reclamaciones por género
ggplot(claims_veh_gender,aes(x= reorder(veh_body,-totclaims),y= totclaims,fill = gender))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Tipos de vehículos más reclamados por género",
       x="Tipo de vehículo",
       y="Numero de reclamaciones") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))