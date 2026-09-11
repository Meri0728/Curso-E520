
# TAREA 05: IMPORTAR DATOS CON READR

# 1. Cargar las librerías necesarias
library(readr)
library(dplyr)

# 2. Cargar el dataset de ANAC 2025 usando la ruta exacta
vuelos_2025 <- read_csv2("C:/Users/marti/OneDrive/Documentos/Curso-E520/econ-520/data/informe_2025_ANAC.csv")

# 3. Explorar los datos importados
# Muestra la estructura, tipos de variables y los primeros registros
glimpse(vuelos_2025)

head(vuelos_2025)  # Ver las primeras 6 filas de la tabla
dim(vuelos_2025)   # Ver la cantidad total de filas y columnas (dimensiones)

# FILTRADO Y RESUMEN DE DATOS

# 1. Renombrar o limpiar nombres de columnas si es necesario
colnames(vuelos_2025)  # Ver los nombres exactos de las variables
# 2. Filtrar vuelos domésticos / regulares (Ejemplo de manipulación de datos)
vuelos_domesticos <- vuelos_2025 %>% 
  filter(`Clasificación Vuelo` == "Doméstico")
# 3. Calcular el total de pasajeros por Aerolínea
resumen_pasajeros <- vuelos_2025 %>% 
  group_by(`Aerolinea Nombre`) %>% 
  summarise(total_pasajeros = sum(Pasajeros, na.rm = TRUE)) %>% 
  arrange(desc(total_pasajeros))

# Ver los primeros resultados del resumen
head(resumen_pasajeros)
