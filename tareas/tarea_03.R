# === Tarea 3: Visualización de Datos con ggplot2 y palmerpenguins ===

# Carga de librerías esenciales 
# Carga el ecosistema tidyverse (incluye ggplot2 para gráficos y dplyr para manipulación de datos)
library(tidyverse)
# Carga la base de datos de los pingüinos de Palmer
library(palmerpenguins)

# Exploración inicial del dataset 
# Abre la tabla de datos completa en una pestaña del visor de RStudio
View(penguins)
# Muestra la estructura de la base de datos: número de filas, columnas y tipos de variables
glimpse(penguins)

#Inicializa el lienzo en blanco indicando la base de datos que se va a utilizar
ggplot(data = penguins)

#Define las estéticas principales mapeando el largo de la aleta al eje X y el peso al eje Y
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)
#Agrega la capa geom_point para crear el gráfico de dispersión
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()

#Incorpora color según la especie del pingüino (species) para diferenciar los grupos visualmente
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()

#Agrega una línea de tendencia o regresión lineal sobre los puntos para analizar la relación
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")

#Distingue las especies usando distintos COLORES en los puntos
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")

#Distingue las especies usando COLOR y FORMA (círculo, triángulo, etc.) en los puntos
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

#Carga la librería ggthemes para acceder a paletas de colores accesibles
library(ggthemes)

#Gráfico completo y final con títulos
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()  # Aplica la paleta de colores amigable para personas con daltonismo

# Guarda el último gráfico generado dentro de la carpeta 'tareas' con el nombre 'tarea03_grafico.png'
ggsave("Curso-E520/tareas/tarea03_grafico.png", width = 8, height = 6, dpi = 300)

# Ejercicio 1: Filas y columnas del dataset
nrow(penguins)
ncol(penguins)
#Respuesta: 344 filas y 8 columnas

# Ejercicio 2: Significado de bill_depth_mm
?penguins
# Respuesta: Mide la profundidad del pico del pingüino en milímetros.

# Ejercicio 3: Diagrama bill_depth_mm vs bill_length_mm y describir relación
ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(na.rm = TRUE)
# Respuesta: Los puntos se agrupan en cúmulos según la especie. 
# Dentro de cada especie la relación es positiva, pero al juntarlas parece negativa (Paradoja de Simpson).

# Ejercicio 4: species vs bill_depth_mm
ggplot(data = penguins, mapping = aes(x = species, y = bill_depth_mm, fill = species)) +
  geom_boxplot(na.rm = TRUE)
# Respuesta: No es ideal geom_point() porque 'species' es categórica y los puntos se amontonan.
# Es mejor usar geom_boxplot() o geom_violin().

# Ejercicio 5: ¿Por qué se produce el error en ggplot(data=penguins) + geom_point()?
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
# Respuesta: Falla porque geom_point() requiere obligatoriamente definir los ejes x e y dentro de aes().

# Ejercicio 6: Argumento na.rm en geom_point()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE)
# Respuesta: na.rm = TRUE remueve los valores faltantes (NA) silenciosamente 
# sin mostrar la advertencia (warning). Su valor por defecto es FALSE.

# Ejercicio 7: Título con labs()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE) +
  labs(caption = "Data come from the palmerpenguins package.")

# Ejercicio 8: Recrear visualización con bill_depth_mm en color continuo
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(color = bill_depth_mm), na.rm = TRUE) +
  geom_smooth(se = FALSE, na.rm = TRUE)
# Respuesta: Se asigna 'color = bill_depth_mm' únicamente dentro de geom_point() a nivel local para que el color afecte a los puntos y no divida la línea de geom_smooth().

# Ejercicio 9: Predicción del código con color = island
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point(na.rm = TRUE) +
  geom_smooth(se = FALSE, na.rm = TRUE)

# Código A
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth()

# Código B
ggplot() +
  geom_point(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_smooth(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g))
# Respuesta: Muestra puntos coloreados por isla y una línea de tendencia por cada isla sin intervalo de confianza.

# 10. ¿Se verán diferentes estos dos gráficos?
# Respuesta: NO, se ven exactamente iguales. 
# El Código A define los datos y mapeos de forma global en ggplot(), 
# mientras que el Código B los define localmente en cada geom, logrando el mismo resultado

ggplot(penguins, aes(y = species, fill = species)) +
  geom_bar()
ggplot(penguins, aes(x = species, fill = species)) +
  geom_bar()
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.05)
 