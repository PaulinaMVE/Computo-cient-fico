# instalar paqueterias

install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("visdat")
install.packages("skimr")
install.packages("tidyverse")
install.packages("zoo")
install.packages("pacman")
install.packages("modeest")
install.packages("FactoMineR")

library(factoextra)
library(reshape2)
library(pacman)
library(dplyr)
library(ggplot2)
library(readr)
library(visdat)
library(skimr)
library(tidyverse)
library(zoo)
library(modeest)
library(FactoMineR)
library(caret)
library(corrplot)

# Cargar dataset
setwd("D:/Usuarios/Orta/Documentos/Actuaria/8° semestre/Computo cientifico/Proyecto")

r <- read.csv("C:/Users/Ines Kimberly/Downloads/QUEJAS.csv", header = T, sep = ",", dec = ".", na.strings = c("-", "null"), stringsAsFactors = FALSE,fileEncoding = "latin1")

View(QUEJAS)
# r dataset original: no alterar
r <- read.csv("C:/Users/Ines Kimberly/Downloads/QUEJAS.csv", header = T, sep = ",", dec = ".", na.strings = c("-", "null"), stringsAsFactors = FALSE,fileEncoding = "latin1")
names(r)

##################### Analisis Exploratorio de Datos #########################

# Resumen estadistico de cada una de las variables
skim(r) 
# Se tienen 3 tipos de variables, 20 categoricas, 2 numericas, 1 una POSIXct que representa fecha y hora de la emision de la queja

# Caracteristicas del dataframe
glimpse(r)
head(r)
# La variable de monto recuperado se reconoce como tipo "Caracter", por o cual se debdera convertir a tipo numerica

# Dimension del dataframe
dim(r) #filas, columnas

# Conocer los nombres de las columnas
colnames(r)

### Datos duplicados
sum(duplicated(r))

# verificar si hay datos faltantes
sum(is.na(r))
colSums(is.na(r))



###################### Preparacion del dataset #################################

# Crear variable excluyendo columnas innecesarias
r1 <- r %>% select(-EXPEDIENTE, -PROVEEDOR, -SECTOR, -ODECO, -MODALIDAD.DE.COMPRA, -TIPO.PRODUCTO)

# Convertir las columnas de fecha al formato Date
r1 <- r1 %>% 
  mutate(
    # Convertir a formato Date
    FECHA_DE_INGRESO = suppressWarnings(dmy(FECHA.DE.INGRESO)),
    FECHA_DE_CIERRE = suppressWarnings(dmy(FECHA.DE.CIERRE)),
    
    # Asignar el numero de dia del anho, colocando 0 si no hay fecha
    DIA_INGRESO = ifelse(is.na(FECHA_DE_INGRESO), 0, yday(FECHA_DE_INGRESO)),
    DIA_CIERRE = ifelse(is.na(FECHA_DE_CIERRE), 0, yday(FECHA_DE_CIERRE))
  )
# Mostrar los cambios
head(r1[,c("FECHA.DE.INGRESO","DIA_INGRESO", "FECHA.DE.CIERRE", "DIA_CIERRE")])

# Borrar las variables FECHA.DE.INGRESO Y FECHA.DE.CIERRE (Se duplicaron para cambiar el formato y no alterar la base)
r1 <- subset(r1, select = -c(FECHA.DE.INGRESO, FECHA.DE.CIERRE))
View(r1)

# Para tener el mismo formato en el nombre de las variables
r1 <- r1 %>% rename(FECHA.DE.INGRESO = FECHA_DE_INGRESO,FECHA.DE.CIERRE = FECHA_DE_CIERRE, DIA.INGRESO = DIA_INGRESO, DIA.CIERRE = DIA_CIERRE)

# Reducir la categorigozacion de motivo de reclamacion (MOTIVO.DE.RECLAMACION) a 10 categorias
r1 <- r1 %>%
  mutate(
    MOTIVO.DE.RECLAMACIÓN = case_when(
      MOTIVO.DE.RECLAMACIÓN %in% c("Negativa a la rescisión del contrato", "Negativa a la renovación", "Modificación o recisión sin aviso ni autorización", 
                                   "No respetó acuerdo previo", "Contrato de comercialización de bienes no determinados o no determinables", "Contiene cláusulas abusivas") ~ "Incumplimiento de contrato o condiciones",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("Negativa a la entrega del bien o servicio", "Negativa a cambio o devolución", "Negativa a la devolución de depósito", 
                                   "Negativa a bonificación", "Negativa a pagar costos adicionales incurridos por el consumidor", "Cambios, devoluciones o bonificaciones", 
                                   "Política de cambios y devoluciones") ~ "Negativa a entrega, cambios o devoluciones",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("Negativa a corregir errores", "Producto o servicio equivocado", "Defectos de fabricación", "Deficiencia en la reparación", 
                                   "Descripción del producto o servicio", "Descripción del producto o servicio-Info no clara", "Negativa a pago por deterioro del producto", 
                                   "Negativa a pago por pérdidas o deterioro a consecuencia del uso del producto") ~ "Errores o problemas con el producto o servicio",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("Error de cálculo en el cobro", "Cuota extraordinaria", "Comisiones", "Intereses", "Estados de cuenta", "Periodicidad de pagos", 
                                   "Condiciones de pago", "Forma de pago", "Comprobantes") ~ "Problemas de facturación y cobros incorrectos",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("No respeta descuentos", "No respeta precios anunciados", "Modificación del precio convenido o presupuestado", 
                                   "Precio o tarifa", "Por alteración de precio o tarifa máximo u oficial", "No respeta garantía de precio bajo") ~ "Incumplimiento de precios, tarifas y descuentos",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("Negativa a hacer efectiva la garantía", "Negativa a pago por deterioro del producto", 
                                   "Negativa a pago por pérdidas o deterioro a consecuencia del uso del producto") ~ "Problemas con la garantía",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("Publicidad engañosa", "Aviso en medio masivo para hacer efectivo un cobro o cumplimiento de un contrato", 
                                   "Envío de información, promociones u ofertas no solicitadas por teléfono", "Envío de información, promociones u ofertas", 
                                   "Información insuficiente") ~ "Publicidad y promociones engañosas",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("Penalización por causa imputable al proveedor", "Responsabilidad del proveedor por actos de sus dependientes", 
                                   "Plazos, cantidades, condiciones", "Incumplimiento de plazos") ~ "Penalizaciones y abusos del proveedor",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("Suspensión de la provisión del servicio", "Negativa a la venta de un bien o a la prestación de un servicio") ~ "Suspensión o negación del servicio o venta",
      
      MOTIVO.DE.RECLAMACIÓN %in% c("Uso fraudulento", "Producto o servicio no solicitado o autorizado") ~ "Uso indebido o fraude",
      
      TRUE ~ "Otra"
    )
  )

# Varias ciudades son del mismo estado
r1 <- r1 %>%
  mutate(
    ESTADO = case_when(
      ESTADO %in% c("Toluca", "Tlalnepantla", "Nezahualcóyotl") ~ "Edomex",
      
      ESTADO %in% c("Centro","Poniente","Oriente","DIR. GRAL. CONS. TELECOM") ~ "Cdmx",
      
      TRUE ~ ESTADO
    )
  )

# Varios giros son el mismo con distintas palabras 
r1 <- r1 %>%
  mutate(
    GIRO = case_when(
      GIRO %in% c("Empresa de telefonía celular", "Empresa de telefonía",
                  "EMPRESA DE TELEFONÍA CELULAR", "Distribuidor de servicio de telefonía celular"
                  ) ~ "Empresa de telefonia celular",
      GIRO %in% c("TV VIA SATELITE", "TV SATELITAL", "Empresa de TV de paga (de TV restringida)",
                  "Television por cable", "EMPRESA DE TV DE PAGA",
                  "Televisión por cable") ~ "TELEVISION VIA SATELITAL",
      GIRO %in% c("Proveedor de acceso a Internet y hospedaje de páginas Web"
                  ) ~ "Prestación de Servicios de Internet",
      GIRO %in% c("Fabricaci¢n de electrodom¿sticos y l¡nea blanca", 
                  "Taller de reparación de equipo de cómputo y bienes relacionados",
                  "Taller de reparación y mantenimiento de equipo electrónico"
                  ) ~ "Tienda especializada en productos electrónicos",
      GIRO %in% c("Empresa de telefonía local") ~ "Casetas telefónicas (Incluye mensajería telefónica)",
      TRUE ~ GIRO
    )
  )

# Estado procesal
r1 <- r1 %>%
  mutate(
    ESTADO.PROCESAL = case_when(
      ESTADO.PROCESAL %in% c("No conciliada", "No Conciliada") ~ "No Conciliada",
      TRUE ~ ESTADO.PROCESAL
    )
  )

# El tipo de reclamaciones presenta algunas reclamaciones iguales pero con mas/menos palabras
r1 <- r1 %>%
  mutate(
    TIPO.DE.RECLAMACIÓN = case_when(
      TIPO.DE.RECLAMACIÓN %in% c("Información no clara al consumidor sobre el producto o servicio",
                                 "Información Incorrecta al consumidor sobre el producto o servicio",
                                 "Ausencia de Información al consumidor sobre el producto o servicio"
                                 ) ~ "Información incompleta al consumidor sobre el producto o servicio",
      TIPO.DE.RECLAMACIÓN %in% c("Información incorrecta al consumidor sobre el pago",
                                 "Información no clara o ilegible al consumidor sobre el pago",
                                 "Negativa de entrega de Información al consumidor sobre el pago"
                                 ) ~ "Información incompleta al consumidor sobre el pago",
      TIPO.DE.RECLAMACIÓN %in% c("PORTABILIDAD", "DESBLOQUEO", "45378.43125") ~ "Portabilidad",
      TRUE ~ TIPO.DE.RECLAMACIÓN
    )
  )

# Se homogeniza el problema especial (PROBLEMA.ESPECIAL)
r1 <- r1 %>%
  mutate(
    PROBLEMA.ESPECIAL = case_when(
      PROBLEMA.ESPECIAL %in% c("CABLEMAS ¿ CABLEVISION MONTERR", "IZZI Telecom") ~ NA,
      PROBLEMA.ESPECIAL %in% c("Uso de internet/datos") ~ "Internet",
      PROBLEMA.ESPECIAL %in% c("La empresa elimino canales de Televisa") ~ "Television",
      PROBLEMA.ESPECIAL %in% c("Telefonia") ~ "Telefonia Movil",
      TRUE ~ PROBLEMA.ESPECIAL
    )
  )

# NA'S DE MODALIDAD DE PAGO
r1 <- r1 %>%
  mutate(
    MODALIDAD.PAGO = case_when(
      MODALIDAD.PAGO == "N/A" ~ NA_character_,
      TRUE ~ MODALIDAD.PAGO  
    )
  )

# Descripcion de las categorias de cada variable
print(lapply(c("TIPO.DE.CONCILIACIÓN", "ESTADO.PROCESAL", "NOMBRE.COMERCIAL", 
               "GIRO", "ESTADO", "TIPO.DE.RECLAMACIÓN", "MOTIVO.DE.RECLAMACIÓN", 
               "PROCEDIMIENTO", "MEDIO.DE.INGRESO", "BIEN.O.SERVICIO", 
               "MODALIDAD.PAGO", "PROBLEMA.ESPECIAL"), 
             function(var) unique(r1[[var]])) %>%
        setNames(c("TIPO.DE.CONCILIACIÓN", "ESTADO.PROCESAL", "NOMBRE.COMERCIAL", 
                   "GIRO", "ESTADO", "TIPO.DE.RECLAMACIÓN", "MOTIVO.DE.RECLAMACIÓN", 
                   "PROCEDIMIENTO", "MEDIO.DE.INGRESO", "BIEN.O.SERVICIO",
                   "MODALIDAD.PAGO", "PROBLEMA.ESPECIAL")))

num_var <- c("FECHA.DE.INGRESO", "FECHA.DE.CIERRE", "DIA.INGRESO", "DIA.CIERRE",
"COSTO.BIEN.O.SERVICIO", "MONTO.RECLAMADO", "MONTO.RECUPERADO")
r1 <- r1 %>% 
  mutate(across(
    .cols = setdiff(names(r1), c(num_var, sapply(r1, is.numeric))),
    .fns = ~ as.numeric(as.factor(.))
  ))

# Verificar que se realizó la conversión
head(r1)

print('Total de datos faltantes en dataset:')
sum(is.na(r1))
colSums(is.na(r1))

# Se detecto que COSTO.BIEN.O.SERVICIO y MONTO.RECLAMADO son numeros registrados como
# caracter, se realiza la conversion a v. numerica por lo tanto
r1 <- r1 %>% 
  mutate(
    MONTO.RECLAMADO = as.numeric(as.character(MONTO.RECLAMADO)),
    COSTO.BIEN.O.SERVICIO = as.numeric(as.character(COSTO.BIEN.O.SERVICIO))
  )
skim(r1)

# Resumen estadistico de cada una de las variables de la nueva base

moda <- function(x) {
  uniq_vals <- unique(x) 
  freqs <- tabulate(match(x, uniq_vals))  
  uniq_vals[which.max(freqs)]  
}

modas <- sapply(r1_inter[, c("TIPO.DE.CONCILIACIÓN", "ESTADO.PROCESAL", "NOMBRE.COMERCIAL", 
                           "GIRO", "ESTADO", "TIPO.DE.RECLAMACIÓN", "MOTIVO.DE.RECLAMACIÓN", 
                           "PROCEDIMIENTO", "MEDIO.DE.INGRESO", "BIEN.O.SERVICIO", 
                           "MODALIDAD.PAGO", "PROBLEMA.ESPECIAL","FECHA.DE.INGRESO", "DIA.INGRESO", "DIA.CIERRE")], moda)
modas

# Para las variables que si son de caracter numerico

summary(r1_inter[, c("COSTO.BIEN.O.SERVICIO","MONTO.RECLAMADO","MONTO.RECUPERADO")])

consulta <- r1_inter %>%
  filter(COSTO.BIEN.O.SERVICIO == "2683872")
consulta

######################## Reemplazar los datos vacios ###########################

# Excluir columnas de FECHA.DE.INGRESO y FECHA.DE.CIERRE porque se tiene su equivalente:
# DIA.INGRESO y DIA.CIERRE
r1 <- r1 %>%
  select(-FECHA.DE.INGRESO, -FECHA.DE.CIERRE)

# Dado que se tienen 3 variables numericas y otras 14 originalmente categoricas,
# se opta por separar el reemplazo con media y moda respectivamente, junto con 
# hacer un tercer reemplazo para los dias basado en el tiempo de cierre promedio 
# de una queja

# Variables categoricas
moda <- function(x) {
  x <- na.omit(x) # Quitar NA's
  if (length(x) == 0) return(NA) # Condicion para verificar existencia de datos
  as.numeric(mlv(x, method = "mfv")[[1]])
}
r1 <- r1 %>% 
  mutate(across(c("TIPO.DE.CONCILIACIÓN", "ESTADO.PROCESAL", "NOMBRE.COMERCIAL", 
                  "GIRO", "ESTADO", "TIPO.DE.RECLAMACIÓN", "MOTIVO.DE.RECLAMACIÓN", 
                  "PROCEDIMIENTO", "MEDIO.DE.INGRESO", "BIEN.O.SERVICIO", 
                  "MODALIDAD.PAGO", "PROBLEMA.ESPECIAL"),
                ~ replace(., is.na(.),moda(.))))

# Variables numericas
r1 <- r1 %>% 
  mutate(across(c("COSTO.BIEN.O.SERVICIO","MONTO.RECLAMADO","MONTO.RECUPERADO"),
                ~ replace(., is.na(.), mean(., na.rm = TRUE))))

# Variables de fecha
dif_prom_fecha <- mean(r1$DIA.CIERRE - r1$DIA.INGRESO, na.rm = TRUE)

r1 <- r1 %>% 
  mutate(DIA.CIERRE = ifelse(is.na(DIA.CIERRE), DIA.INGRESO + dif_prom_fecha, DIA.CIERRE))

# Comprobacion
summary(r1)
sum(is.na(r1))

# Analisis de los metodos de reeemplazamiento empleados
summary(r1)

# 2. Usando la base con datos imputados
# (a) Histograma

# Fecha de ingreso
ggplot(r1, aes(x = DIA.INGRESO)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "black", alpha = 0.7) +
  labs(title = "Distribucion de quejas por Ingreso de fecha", x = "Fecha", y = "Frecuencia") +
  theme_minimal()

# Estado Procesal
ggplot(r1, aes(x = ESTADO.PROCESAL)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Estado Procesal", x = "Estado procesal", y = "Frecuencia") +
  theme_minimal()

# Proveedor Comercial
ggplot(r1, aes(x = NOMBRE.COMERCIAL)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "black", alpha = 0.7) +
  labs(title = "Distribucion de quejas por Proveedores", x = "Proveedor", y = "Frecuencia") +
  theme_minimal()

# Estado
ggplot(r1, aes(x = ESTADO)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "black", alpha = 0.7) +
  labs(title = "Distribucion de quejas por Estado", x = "Estado", y = "Frecuencia") +
  theme_minimal()

# Giro
ggplot(r1, aes(x = GIRO)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "black", alpha = 0.7) +
  labs(title = "Distribucion de quejas por Giro", x = "Giro", y = "Frecuencia") +
  theme_minimal()

# Tipo de reclamacion
ggplot(r1, aes(x = TIPO.DE.RECLAMACIÓN)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "black", alpha = 0.7) +
  labs(title = "Distribucion de quejas por Tipo de Reclamacion", x = "Tipo de reclamacion", y = "Frecuencia") +
  theme_minimal()

# Montos de reclamacion

boxplot(r1$MONTO.RECLAMADO, 
        main = "Montos Reclamados", 
        ylab = "Pesos mexicanos", 
        col = "navy")
# Montos de recuperacion

boxplot(r1$MONTO.RECUPERADO, 
        main = "Montos Recuperado", 
        ylab = "Pesos mexicanos", 
        col = "navy")
# costos
boxplot(r1$COSTO.BIEN.O.SERVICIO, 
        main = "Costo del bien o servicio", 
        ylab = "Pesos mexicanos", 
        col = "navy")


# Procedimiento
ggplot(r1, aes(x = PROCEDIMIENTO)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "black", alpha = 0.7) +
  labs(title = "Distribucion de quejas por procedimiento", x = "Procedimiento", y = "Frecuencia") +
  theme_minimal()


# Análisis bivariado
# -- Correlación entre variables numéricas
cor(r1[,c("TIPO.DE.CONCILIACIÓN", "ESTADO.PROCESAL", "NOMBRE.COMERCIAL", 
          "GIRO", "ESTADO", "TIPO.DE.RECLAMACIÓN", "MOTIVO.DE.RECLAMACIÓN", 
          "PROCEDIMIENTO", "MEDIO.DE.INGRESO", "BIEN.O.SERVICIO", 
          "MODALIDAD.PAGO", "PROBLEMA.ESPECIAL", "DIA.INGRESO", "DIA.CIERRE",
          "COSTO.BIEN.O.SERVICIO","MONTO.RECLAMADO","MONTO.RECUPERADO")])
# Indica el grado de dependencia que hay entre 2 variables, 

#################### Analisis de componentes principales #######################
# Estandarizar r1
r1_escala <- scale(na.omit(r1))
acp <- prcomp(r1_escala, center = TRUE, scale = TRUE)
summary(acp)
screeplot(acp, type = "lines", main = "Scree Plot")
# Ver variables que explican el 85% de la informacion
var_exp <- cumsum(acp$sdev^2 / sum(acp$sdev^2))
num_comp <- ifelse(any(var_exp >= 0.85), which(var_exp >= 0.85)[1], length(var_exp))
cat("Numero de CP necesarios:", num_comp, "\n")
# Visualizar
plot(var_exp, type = "b", xlab = "Numero de CP", 
     ylab = " Varianza explicativa acumulada",
     main = "Varianza acumulativa explicativa")
abline(h = 0.85, col = "red", lty = 2)

################################ Heat Map ######################################
temp_r1 <- na.omit(r1)
matriz_cor <- cor(temp_r1)

melted_cor <- melt(matriz_cor)
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +  # Dibujar el heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black"), 
        axis.title = element_blank(),  
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Mapa de calor de la correlación entre variables numéricas", 
       x = "Variables", y = "Variables") 

alta_cor <- findCorrelation(matriz_cor, cutoff = 0.9, verbose = FALSE)
if (length(alta_cor) > 0) {
  cat("Variables eliminadas por alta correlacion:", colnames(R1)[alta_cor], "\n")
  r1_filtrado <- temp_r1[, -alta_cor] # filtrar variables seleccionadas
} else {
  cat("No se eliminaron variables por alta correlacion.\n")
  r1_filtrado <- temp_r1
}

########################### Eigenvalores ######################################
str(r1)
psych::KMO(r1) # Prueba Kaiser-Meyer-Olkin, medir si las variables son susceptibles de pca

pca<-prcomp(r1_escala,center = TRUE, scale = TRUE) # proceso de reduccion de dimensionalidad
summary(pca)

fviz_eig(pca, addlabels = TRUE, ncp = 10) # grafico de eigenvalores que uestra varianza
# explicada por los componentes principales (cuantos componentes principales tomar)
get_eigenvalue(pca)
pca$rotation[,1:8] # para ver las cargas de los 8 componentes principales sugeridos

###############################################################################
# Eleccion del modelo
