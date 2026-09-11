# ==============================================================================
# TAREA 04 - SECCIÓN 19.2.4: EJERCICIOS DE CLAVES

library(tidyverse)
library(nycflights13)


# RESPUESTA 1: Relación entre 'weather' y 'airports'

# Explicación: 'weather' se conecta con 'airports' uniendo la clave foránea weather$origin con la clave primaria airports$faa.
# En el diagrama se representaría con una flecha que conecta weather (origin) -> airports (faa).

weather |> 
  distinct(origin) |> 
  anti_join(airports, by = c("origin" = "faa")) 
# Retorna 0 filas, lo que demuestra que todos los orígenes existen en 'airports'.


# RESPUESTA 2: Conexión adicional de 'weather' con 'flights' si tuviera todos los aeropuertos

# Explicación: Si 'weather' incluyera el clima de todo EE.UU., se conectaría con 'flights' no solo mediante 'origin', sino también mediante 'dest' (aeropuerto de destino).
# Esto permitiría conocer las condiciones meteorológicas tanto al despegar como al aterrizar.
vuelos_con_clima_origen_y_destino <- flights |> 
  # Clima en el aeropuerto de origen al despegar
  left_join(weather, by = c("year", "month", "day", "hour", "origin")) |> 
  # Clima en el aeropuerto de destino (requeriría arr_time/hour_arr y dest)
  left_join(weather, by = c("year", "month", "day", "hour", "dest" = "origin"),
            suffix = c("_origen", "_destino"))

# RESPUESTA 3: Hora con observaciones duplicadas en 'weather'
# Código para averiguar la hora especial con duplicados:
weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1)

# Ocurre el 3 de noviembre de 2013 a las 1:00 AM. 
# Ese día finalizó el horario de verano (Daylight Saving Time), por lo que el reloj se retrasó una hora y la 1:00 AM ocurrió dos veces.


# RESPUESTA 4: Marco de datos para días especiales / festivos
# Creación del marco de datos de días especiales:
dias_especiales <- tibble(
  year = 2013,
  month = c(12, 12, 1),
  day = c(24, 25, 1),
  nombre_festivo = c("Nochebuena", "Navidad", "Año Nuevo")
)

# Clave primaria: La combinación compuesta de (year, month, day).
# Conexión: Se conectaría mediante un join con la tabla 'flights' utilizando las columnas coincidentes (year, month, day).


# Para el ejercicio 5 es necesario instalar y cargar el paquete Lahman:
install.packages("Lahman")
library(Lahman)

# DIAGRAMAS Y RELACIONES EN LAHMAN
library(Lahman)
library(tidyverse)

# DIAGRAMA 1: Batting, People y Salaries
# Esquema / Diagrama conceptual:

#   [ Batting ] ──( playerID )──> [ People ] <──( playerID )── [ Salaries ]

# Explicación:
# 'People' actúa como la tabla principal de entidades (demografía de los jugadores).
#  Su clave primaria es 'playerID'.
# 'Batting' y 'Salaries' utilizan 'playerID' como Foreign Key para conectarse a 'People'.
# La relación es de 1 a Muchos (Un jugador en 'People' puede tener múltiples registros de bateo en 'Batting' y múltiples salarios en 'Salaries').

# Verificación de claves en R:
People |> count(playerID) |> filter(n > 1)   # clave primaria (0 duplicados)

# DIAGRAMA 2: People, Managers y AwardsManagers

# Esquema 
#   [ People ] ──( playerID )──> [ Managers ] ──( playerID, yearID )──> [ AwardsManagers ]

# Explicación:
# 'People' se conecta con 'Managers' mediante la clave 'playerID'.
# 'Managers' se conecta con 'AwardsManagers' mediante la clave compuesta 'playerID' y 'yearID' (ya que los premios a mánagers se otorgan por año).

# CARACTERIZACIÓN: Batting, Pitching y Fielding
# ¿Cómo se caracterizan estas tablas?
# Tienen una relación PARALELA / HOMÓLOGA.
# Las tres tablas contienen estadísticas de rendimiento deportivo a nivel de jugador-temporada-equipo.
# Comparten la misma Clave Compuesta (Primary Key equivalente): c("playerID", "yearID", "stint")

# Tipo de relación:
# Un jugador ('playerID') en un mismo año ('yearID') y período ('stint') puede aparecer en las tres tablas si bateó, lanzó (pitcheó) y fildeó.
# Se unen mediante un FULL JOIN o INNER JOIN usando: by = c("playerID", "yearID", "teamID", "lgID", "stint")



# ==============================================================================
# SECCIÓN 19.3.4: EJERCICIOS DE JOINS (R4DS)

#Más arriba ya abrimos las librerias necesarias
# EJERCICIO 1: Identificar las 48 horas con mayores retrasos y comparar con 'weather'
top_48_horas <- flights |> 
  group_by(year, month, day, hour) |> 
  summarize(retraso_promedio = mean(dep_delay, na.rm = TRUE), .groups = "drop") |> 
  slice_max(order_by = retraso_promedio, n = 48) |> 
  inner_join(weather, by = c("year", "month", "day", "hour"))

# Respuesta: Se observa que las horas con mayores retrasos coinciden frecuentemente con malas condiciones meteorológicas (precipitaciones altas, baja visibilidad y rachas de viento).

# EJERCICIO 2: Encontrar todos los vuelos hacia los 10 destinos más populares
top_dest <- flights |> 
  count(dest, sort = TRUE) |> 
  head(10)

# Para filtrar los vuelos hacia esos destinos usamos semi_join:
vuelos_top_dest <- flights |> 
  semi_join(top_dest, by = "dest")

# EJERCICIO 3: ¿Cada vuelo de salida dispone de datos meteorológicos?
vuelos_sin_clima <- flights |> 
  anti_join(weather, by = c("year", "month", "day", "hour", "origin"))

# Respuesta: No todos disponen de clima. Existen registros de vuelos cuyo horario u origen no tiene la medición horaria correspondiente en la tabla 'weather'.

# EJERCICIO 4: Números de cola (tailnum) que no coinciden en 'planes'
aviones_sin_registro <- flights |> 
  distinct(tailnum, carrier) |> 
  anti_join(planes, by = "tailnum")

# Respuesta: La gran mayoría de los aviones sin registro en 'planes' pertenecen 
# a las aerolíneas American Airlines (AA) y Envoy Air (MQ), las cuales no reportan los datos de sus flota a esta base de datos.


# EJERCICIO 5: Aerolíneas por avión y verificación de la hipótesis
aviones_aerolineas <- flights |> 
  filter(!is.na(tailnum)) |> 
  group_by(tailnum) |> 
  summarize(
    aerolineas = paste(unique(carrier), collapse = ", "),
    n_carriers = n_distinct(carrier)
  )

# Verificación: ¿Un avión es operado por más de una aerolínea?
aviones_multiaerolinea <- aviones_aerolineas |> 
  filter(n_carriers > 1)
# Respuesta: Se refuta la hipótesis de que cada avión es operado por una sola aerolínea, ya que existen aviones que registran operaciones con más de un 'carrier'.

# EJERCICIO 6: Agregar latitud y longitud de origen y destino a 'flights'
vuelos_lat_lon <- flights |> 
  left_join(airports |> select(faa, lat, lon), by = c("origin" = "faa")) |> 
  left_join(airports |> select(faa, lat, lon), by = c("dest" = "faa"), suffix = c("_origen", "_destino"))

# Respuesta: Es más fácil seleccionar/renombrar las columnas ANTES de la unión o usar el argumento 'suffix' para evitar ambigüedades en las columnas lat y lon.

# EJERCICIO 7: Mapa de distribución espacial de retrasos por destino
retrasos_destino <- flights |> 
  group_by(dest) |> 
  summarize(retraso_prom = mean(arr_delay, na.rm = TRUE)) |> 
  inner_join(airports, by = c("dest" = "faa"))

# Gráfico del mapa:
retrasos_destino |> 
  ggplot(aes(x = lon, y = lat, size = retraso_prom, color = retraso_prom)) +
  borders("state") +
  geom_point(alpha = 0.7) +
  coord_quickmap() +
  scale_color_viridis_c() +
  labs(title = "Distribución Espacial de Retrasos por Destino",
       x = "Longitud", y = "Latitud", size = "Retraso (min)", color = "Retraso (min)")



# EJERCICIO 8: ¿Qué ocurrió el 13 de junio de 2013?
retrasos_13_junio <- flights |> 
  filter(year == 2013, month == 6, day == 13) |> 
  group_by(dest) |> 
  summarize(retraso_prom = mean(arr_delay, na.rm = TRUE)) |> 
  inner_join(airports, by = c("dest" = "faa"))

retrasos_13_junio |> 
  ggplot(aes(x = lon, y = lat, size = retraso_prom, color = retraso_prom)) +
  borders("state") +
  geom_point(alpha = 0.7) +
  coord_quickmap() +
  scale_color_viridis_c() +
  labs(title = "Retrasos de vuelos el 13 de Junio de 2013")

# Rta: El 13 de junio de 2013 ocurrió una serie de tormentas severas y derecho meteorológicos en el sureste y costa este de EE.UU., provocando retrasos masivos concentrados en esa región.



# ==============================================================================
# SECCIÓN 3.2.5: EJERCICIOS DE FILTRADO Y ORDENAMIENTO (R4DS)

library(tidyverse)
library(nycflights13)


# EJERCICIO 1: Filtrar vuelos según condiciones específicas
# a) Retraso en la llegada de 2 o más horas (120 minutos)
flights |> 
  filter(arr_delay >= 120)

# b) Voló a Houston (IAH o HOU)
flights |> 
  filter(dest %in% c("IAH", "HOU"))

# c) Fueron operados por United (UA), American (AA) o Delta (DL)
flights |> 
  filter(carrier %in% c("UA", "AA", "DL"))

# d) Partió en verano (julio, agosto y septiembre)
flights |> 
  filter(month %in% c(7, 8, 9))

# e) Llegó con más de 2 horas de retraso, pero salió a tiempo o antes (dep_delay <= 0)
flights |> 
  filter(arr_delay > 120, dep_delay <= 0)

# f) Se retrasó al menos 1 hora (60 min) en salir, pero recuperó más de 30 min en vuelo
flights |> 
  filter(dep_delay >= 60, (dep_delay - arr_delay) > 30)


# EJERCICIO 2: Ordenar vuelos por retrasos y horario de salida

# Mayores retrasos en la salida:
flights |> 
  arrange(desc(dep_delay))

# Vuelos que salieron más temprano por la mañana:
flights |> 
  arrange(dep_time)


# EJERCICIO 3: Encontrar los vuelos más rápidos (mayor velocidad)
# Calculamos la velocidad en millas por hora (distance / (air_time / 60))
flights |> 
  arrange(desc(distance / air_time))

# EJERCICIO 4: ¿Hubo vuelos todos los días de 2013?
vuelos_por_dia <- flights |> 
  distinct(year, month, day) |> 
  count()
# Respuesta: Sí, hubo vuelos los 365 días del año de 2013.

# EJERCICIO 5: Vuelos con mayor y menor distancia
# Mayor distancia (JFK a HNL - Honolulu):
flights |> 
  arrange(desc(distance))

# Menor distancia (EWR a PHL - Philadelphia):
flights |> 
  arrange(distance)

# EJERCICIO 6: ¿Importa el orden en que se usan filter() y arrange()?
# 1. En el RESULTADO FINAL: No cambia. El conjunto de datos filtrados y ordenados será exactamente el mismo.
# 2. En la EFICIENCIA/CANTIDAD DE TRABAJO: Sí importa mucho. Es mejor usar filter() PRIMERO 
# y arrange() DESPUÉS. De esta forma, R ordena únicamente el subconjunto de filas que pasaron el filtro, evitando el costo computacional de ordenar millones de filas innecesarias.


# ==============================================================================
# SECCIÓN 3.3.5: EJERCICIOS DE SELECCIÓN Y RENOMBRADO (R4DS)

library(tidyverse)
library(nycflights13)

# EJERCICIO 1: Relación entre dep_time, sched_dep_time y dep_delay
# La relación matemática esperada es: dep_delay = dep_time - sched_dep_time. (Teniendo en cuenta que los horarios están en formato HHMM, no en minutos continuos).
flights |> 
  select(dep_time, sched_dep_time, dep_delay)


# EJERCICIO 2: Distintas formas de seleccionar dep_time, dep_delay, arr_time, arr_delay

# Forma 1: Por nombres explícitos
flights |> select(dep_time, dep_delay, arr_time, arr_delay)

# Forma 2: Usando helpers start_with / contains
flights |> select(starts_with("dep_"), starts_with("arr_"))

# Forma 3: Especificando vector de cadenas con any_of()
flights |> select(any_of(c("dep_time", "dep_delay", "arr_time", "arr_delay")))

# Forma 4: Por expresiones regulares con matches()
flights |> select(matches("^(dep|arr)_(time|delay)$"))


# EJERCICIO 3: ¿Qué ocurre si especificas la misma variable varias veces en select()?
# Respuesta: R ignora los duplicados y la muestra una sola vez. No genera error.
flights |> select(dep_time, dep_time, dep_time)


# EJERCICIO 4: ¿Qué hace any_of()?
# Respuesta: Selecciona cualquier variable que coincida con los nombres en el vector.
# Es muy útil porque NO da error si alguna variable del vector no existe en el dataset.
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |> select(any_of(variables))


# EJERCICIO 5: Manejo de mayúsculas/minúsculas en contains()
# Código provisto:
flights |> select(contains("TIME"))

# 1. Por defecto, las funciones auxiliares de selección ignoran Mayúsculas y Minúsculas (case_insensitive = TRUE), por lo que selecciona "dep_time", "arr_time", etc.
# 2. Para cambiar ese comportamiento y hacer que respete exacto las mayúsculas:
flights |> select(contains("TIME", ignore.case = FALSE))


# EJERCICIO 6: Renombrar air_time a air_time_min y moverlo al principio
flights |> 
  rename(air_time_min = air_time) |> 
  relocate(air_time_min)


# EJERCICIO 7: ¿Por qué no funciona el código y qué significa el error?
# Código que falla:
 flights |> 
   select(tailnum) |> 
   arrange(arr_delay)
# No funciona porque `select(tailnum)` descuarta todas las demás columnas del dataframe dejando únicamente `tailnum`. Cuando el pipeline pasa a `arrange(arr_delay)`, 
# la columna `arr_delay` ya no existe en el conjunto de datos, provocando el error 'object arr_delay not found'.

