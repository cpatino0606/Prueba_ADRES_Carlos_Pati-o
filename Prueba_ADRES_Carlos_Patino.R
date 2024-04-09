## inicio

## prueba vacante ADRES - CARLOS ANDRÉS PATIÑO LÓPEZ #######





#### ---------------Intalación y cargue de librerias----------------###########

#install.packages("DBI")
#install.packages("RSQLite")
#install.packages("tidyverse")
#install.packages("writexl")

# se cargan las librerias
library(DBI)
library(RSQLite)
library(tidyverse)
library(writexl)

#establecer path

setwd("C:/Users/carlo/Desktop/Prueba_ADRES/Base_SQLite")


#### ----------------Conexión con SQLite----------------###########
#conexión a base de datos SQLite

base_datos <- dbConnect(SQLite(), dbname = "C:/Users/carlo/Desktop/Prueba_ADRES/Base_SQLite/Base_ADRES.db")

#validación de conexión a base de datos de SQLite
print(base_datos)
#generamos un listado de las tablas que contiene la base de datos
dbListTables(base_datos)
# contamos con 3 tablas, 1. Municipios, 2. Prestadores y 3.Municipios y departamentos. 
# la tercera es una tabla del DANE con los nombres y códigos de los municipios y departamentos. 


#### ---------------Cargue y asignación de data----------------###########

#asignamos las tablas a una variable para comenzar a gestionarlas en R

mun <- dbGetQuery(base_datos, "SELECT * FROM Municipios")

Prs <- dbGetQuery(base_datos, "SELECT * FROM Prestadores")

nom_mun <- dbGetQuery(base_datos, "SELECT * FROM municipios_y_departamentos")

#al cargar la base de prestadores, genera una alerta en la que las columnas "codigo_habitacion 
# y nits_nit presentan una mezcla de tipos de datos. Para corregir ese error, corremos los siguientes scripts

consulta1 <- "UPDATE Prestadores SET codigo_habilitacion = CAST(codigo_habilitacion AS TEXT)"
consulta2 <- "UPDATE Prestadores SET nits_nit = CAST(nits_nit AS TEXT)"

dbExecute(base_datos, consulta1)
dbExecute(base_datos, consulta2)


#### ----------------Merge 1-------------------#####

# se realiza el primer merge entre mun y nom_mun para corregir los nombres de los municipios
# la llave utilizada para realizar el merge es el código del municipio. 

munfull <- merge(mun, nom_mun, by.x = "Depmun", by.y = "CODIGO_MUNICIPIO", all = TRUE ) 

#revisando los datos, se evidencia que hay un municipio que se encontraba en la base, por lo que generaba datos NA. 
# para corregir esto, se decide generar los datos de este municipio faltante para que en los cálculos siguientes
# no se encuentren missing values. 

#nueva fila para el municipio GUACHENÉ

datos_guachene <- data.frame(
  Depmun = "19300",
  Departamento = "Cauca",
  Dep = 19,
  Municipio = "GUACHENÉ",
  Superficie =  0,
  Poblacion = 19815,
  Irural = 0,
  Region = "Región Pacífico",
  NOMBRE_DEPTO = "Cauca",
  NOMBRE_MPIO = "GUACHENÉ",
  Nombre = "GUACHENÉ"
)

#se agrega la fila nueva con los datos del municipio GUACHENÉ
munfull <- rbind(munfull,datos_guachene)

#### ----------------Merge 2-------------------#####

#merge para llevar la info a prestadores y tener la base completa
#con la especificación de all.x = TRUE mantenemos los datos que concidan con la tabla de prestadores de servicios
base_completa <- merge(Prs,munfull, by.x = "muni_nombre", by.y = "NOMBRE_MPIO", all.x = TRUE)

#con el fin de facilitar el procesamiento y mantener la fuente de la base intacta, pasamos la base final a un DF
df_base_completa <- data.frame(base_completa)


#### ----------------Gráfico #1 total prestadores de servicio por región -------------------#####

# Convertimos la columna clpr_nombre a factor para que puedan ser contados sin generar error 
df_base_completa$clpr_nombre <- factor(df_base_completa$clpr_nombre)

# creamos el primer gráfico de barras
grafica_1 <- ggplot(df_base_completa, aes(x = Region, fill = clpr_nombre)) +
  geom_bar(position = "stack") +
  geom_text(stat = 'count', aes(label = after_stat(count)), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución de clpr_nombre por región",
       x = "Región",
       y = "Conteo") +
  theme_minimal()

# Exportar la gráfica como una imagen PNG
ggsave("grafica_1.png", plot = grafica_1, width = 10, height = 6, dpi = 300)


#### ----------------Gráfico #2 total municipios por región-------------------######

# Calculamos el conteo de muni_nombre por región
conteo_por_region <- df_base_completa %>%
  group_by(Region) %>%
  summarize(Conteo_muni_nombre = n_distinct(muni_nombre))

# Creamos la gráfica 2 de barras
grafica_2 <- ggplot(conteo_por_region, aes(x = Region, y = Conteo_muni_nombre)) +
  geom_bar(stat = 'identity', fill = "skyblue") +
  geom_text(aes(label = Conteo_muni_nombre), vjust = -0.5, size = 3) +
  labs(title = "Total de muni_nombre por región",
       x = "Región",
       y = "Total de muni_nombre") +
  theme_minimal()

# Exportar la gráfica como una imagen PNG
ggsave("grafica_2.png", plot = grafica_2, width = 10, height = 6, dpi = 300)


#### ----------------Gráfico #3 total poblacional por región -------------------######

# Calculamos el total de población por región
poblacion_por_region <- df_base_completa %>%
  group_by(Region) %>%
  summarize(Total_Poblacion = sum(Poblacion))

# Crear la gráfica de barras
grafica_3 <- ggplot(poblacion_por_region, aes(x = Region, y = Total_Poblacion)) +
  geom_bar(stat = 'identity', fill = "skyblue") +
  geom_text(aes(label = Total_Poblacion), vjust = -0.5, size = 3) +
  labs(title = "Total de Población por región",
       x = "Región",
       y = "Total de Población") +
  theme_minimal()

# Exportar la gráfica como una imagen PNG
ggsave("grafica_3.png", plot = grafica_3, width = 10, height = 6, dpi = 300)

#### ----------------Gráfico #4 naturaleza jurídica de prestadores de servicio por región-------------------######

# Calculamos el conteo de clpr_nombre (tipos de prestadores de servicio) y naju_nombre(naturaleza jurídica) por región
conteo_por_region <- df_base_completa %>%
  group_by(Region, clpr_nombre, naju_nombre) %>%
  count()

# Calculamos la suma de los valores por región y tipo de prestador
suma_por_region <- df_conteo_por_region %>%
  group_by(Region, naju_nombre) %>%
  summarise(sum_n = sum(n))

# Creamos un gráfico de barras apiladas
grafica_4 <- ggplot(suma_por_region, aes(x = Region, y = sum_n)) +
  geom_bar(stat = "identity", aes(fill = naju_nombre, Position="stack")) +
  # Agregamos etiquetas con la suma de valores
  geom_text(aes(label = sum_n), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  labs(title = "Distribución de tipos de prestadores de servicio por región",
       x = "Región",
       y = "Conteo") +
  theme_minimal()

# Exportar la gráfica como una imagen PNG
ggsave("grafica_4.png", plot = grafica_4, width = 10, height = 6, dpi = 500)


#### ----------------Cierre de conexión con SQLite-------------------######
dbDisconnect(base_datos)



