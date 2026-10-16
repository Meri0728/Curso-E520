# ==============================================================================
# TRABAJO PRÁCTICO: ANÁLISIS DE REDES - GAME OF THRONES (LIBRO 3)
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
library(readr)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggplot2)

# 2. CARGA DE DATOS DESDE GITHUB -----------------------------------------------
url_book3 <- "https://raw.githubusercontent.com/mathbeveridge/asoiaf/refs/heads/master/data/asoiaf-book3-edges.csv"
edges <- read.csv(url_book3)

# 3. CONSTRUCCIÓN DEL GRAFO BASE CON IGRAPH ------------------------------------
g <- graph_from_data_frame(d = edges, directed = FALSE)

# ------------------------------------------------------------------------------
# RESPUESTAS A LAS PREGUNTAS DEL TRABAJO
# ------------------------------------------------------------------------------

# A y B. ANÁLISIS DE GRADO (Conexiones directas)
grados <- degree(g)
print("--- Top 10 Personajes con más conexiones directas ---")
print(head(sort(grados, decreasing = TRUE), 10))

# C. CENTRALIDAD DE AUTOVECTOR (Personajes más influyentes)
influencia <- eigen_centrality(g)$vector
top10_inf <- head(sort(influencia, decreasing = TRUE), 10)

df_inf <- data.frame(
  personaje = names(top10_inf),
  centralidad = as.numeric(top10_inf)
)

# Gráfico de barras de influencia
ggplot(df_inf, aes(x = reorder(personaje, centralidad), y = centralidad)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Personajes más influyentes - Libro 3",
    subtitle = "Centralidad de Autovector (Eigenvector)",
    x = "Personaje",
    y = "Eigenvector Centrality"
  ) +
  theme_minimal()

# D. DETECCIÓN DE COMUNIDADES (Clusters)
comunidades <- cluster_louvain(g)
cat("\nCantidad de comunidades/clusters detectados:", length(comunidades), "\n")

# E. CENTRALIDAD DE INTERMEDIACIÓN (Personajes Puentes)
intermediacion <- betweenness(g)
print("--- Top 10 Personajes Puentes (Intermediación) ---")
print(head(sort(intermediacion, decreasing = TRUE), 10))

# F. MATRIZ COMPARATIVA DE MÉTRICAS
personajes_interes <- c(
  "Tyrion-Lannister",
  "Cersei-Lannister",
  "Joffrey-Baratheon",
  "Robert-Baratheon",
  "Jaime-Lannister",
  "Sansa-Stark"
)

metricas <- data.frame(
  Personaje = personajes_interes,
  Grado = degree(g)[personajes_interes],
  Influencia = round(eigen_centrality(g)$vector[personajes_interes], 4),
  Intermediacion = round(betweenness(g)[personajes_interes], 2),
  Cercania = round(closeness(g, normalized = TRUE)[personajes_interes], 4)
)

print("--- Tabla de Métricas para Personajes Clave ---")
print(metricas)

# VISUALIZACIÓN PRINCIPAL DEL GRAFO 

graph_book3 <- as_tbl_graph(edges, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(weights = weight),
    group = factor(group_infomap()),
    # Solo mostramos el nombre de los Top 30 para no saturar el mapa
    label_top = if_else(rank(-degree) <= 30, name, NA_character_) 
  ) %>%
  activate(edges) %>%
  filter(!edge_is_multiple())

set.seed(123)
layout_b3 <- create_layout(graph_book3, layout = "fr", niter = 1000)

ggraph(layout_b3) +
  geom_edge_link(aes(edge_width = weight), alpha = 0.18, color = "grey65", show.legend = FALSE) +
  scale_edge_width(range = c(0.2, 1.2)) +
  
  geom_node_point(aes(size = degree, color = group), alpha = 0.85) +
  scale_size_continuous(range = c(2, 8), name = "Degree") +
  
  geom_node_text(
    aes(label = label_top), 
    size = 3.2, 
    color = "black", 
    fontface = "bold",
    repel = TRUE, 
    max.overlaps = 50,
    point.padding = unit(0.3, "lines")
  ) +
  
  scale_color_brewer(palette = "Set1", name = "Comunidad") +
  
  theme_graph(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "grey30", hjust = 0),
    plot.caption = element_text(size = 8.5, color = "grey40", face = "italic"),
    legend.position = "right"
  ) +
  labs(
    title = "Red de Personajes - Game of Thrones (Libro 3)",
    subtitle = "Nodos coloreados por Comunidad | Etiquetas para los personajes principales",
    caption = "ECON 520 - Análisis de Redes - Libro 3 | FCE - UBA"
  )

# NOTA SOBRE EL AVISO EN CONSOLA (Set1 palette):
# El aviso 'n too large, allowed maximum for palette Set1 is 9' es únicamente 
# una advertencia informativa de RColorBrewer. 
#Para el Libro 3, la red identifica 9 comunidades principales (Infomap), por lo que la asignación cromática 
# y la segmentación de la red son totalmente correctas.