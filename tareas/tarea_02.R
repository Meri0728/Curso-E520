# A1. Tipos de datos básicos
encuestado_id <- 1045
ingreso <- 350000.50       # numeric
miembros_hogar <- 4L       # integer
estado <- "Ocupado"        # character
busca_trabajo <- FALSE     # logical

# Verificación de tipos
class(ingreso)
class(busca_trabajo)

# A2. Números y Cadenas de Texto
horas_trabajadas <- 40.5
edad_anios <- 28L

sector_actividad <- "Comercio"
categoria_ocupacional <- 'Cuenta propia'

# Funciones de texto
paste("Sector:", sector_actividad, "-", categoria_ocupacional)
grepl("propia", "Cuenta propia con local")

# A3. Booleanos y Operadores
salario_mensual <- 450000
salario_anual <- salario_mensual * 13

es_mayor_edad <- edad_anios >= 18
es_desocupado <- estado == "Desocupado"

# Operador lógico combinado 
es_pea <- (estado == "Ocupado" | estado == "Desocupado") & edad_anios >= 16

# A4. Toma de Decisiones 
if (salario_mensual < 200000) {
  print("Por debajo del salario mínimo")
}

# Clasificacon por decil
if (salario_mensual > 800000) {
  decil <- "Alto"
} else if (salario_mensual >= 300000) {
  decil <- "Medio"
} else {
  decil <- "Bajo"
}

# A5. Bucles - While Loop
meses_busqueda <- 0
while (meses_busqueda < 3) {
  print(paste("Mes", meses_busqueda, ": Buscando empleo..."))
  meses_busqueda <- meses_busqueda + 1
}

# Uso de break
meses_busqueda <- 0
while (TRUE) {
  meses_busqueda <- meses_busqueda + 1
  if (meses_busqueda == 2) {
    print("¡Empleo encontrado!")
    break
  }
}

# A6. Bucles 
salarios_hora <- c(1500, 2200, 1800, 3100)

for (salario in salarios_hora) {
  print(salario * 8)
}

# Modificación mediante índices (aumento del 10%)
for (i in 1:length(salarios_hora)) {
  salarios_hora[i] <- salarios_hora[i] * 1.10
}

# A7. Vectores y Listas
edades_hogar <- c(45, 42, 16, 12)
promedio_edad <- mean(edades_hogar)

jefe_hogar <- list(
  id = 101,
  nombre = "Carlos",
  edades_familia = edades_hogar,
  es_propietario = TRUE
)

# A8. Matrices y Arrays
datos_transicion <- c(80, 20, 15, 85)
matriz_transicion <- matrix(datos_transicion, nrow = 2, byrow = TRUE)

panel_laboral <- array(1:12, dim = c(2, 2, 3))

# A9. Data Frames
microdatos <- data.frame(
  id_persona = c(1, 2, 3),
  edad = c(34, 19, 52),
  ingreso = c(450000, 0, 780000),
  trabajo_semana_pasada = c(TRUE, FALSE, TRUE)
)

str(microdatos)
summary(microdatos)
microdatos$ingreso


# A10. Factores 
vector_estados <- c("Ocupado", "Desocupado", "Inactivo", "Ocupado")
estado_factor <- factor(vector_estados)
levels(estado_factor)

# Factor ordinal 
nivel_edu <- factor(c("Secundario", "Universitario", "Primario"),
                    levels = c("Primario", "Secundario", "Universitario"),
                    ordered = TRUE)
