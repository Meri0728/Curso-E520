
# TAREA 05 - TEXT MINING: MCKINSEY MIND-THE-GAP

library(tidyverse)    # Manipulación de datos y visualización (dplyr, ggplot2, readr)
library(tidytext)     # Herramientas para text mining bajo el enfoque tidy
library(topicmodels)  # Modelos probabilísticos de tópicos (LDA)

# 1. IMPORTACIÓN DE DATOS
# Descargamos el dataset oficial de artículos de McKinsey 
url_mckinsey <- "https://gitlab.com/uploads/-/system/personal_snippet/4897254/d83847870c9577a22a20063379f91120/DATA-T9-mckinsey-mind-the-gap-articles-20251020.csv"
mckinsey_raw <- read_csv(url_mckinsey)

#para que no aparezca un cartel largo en la consola puedo usar --> "show_col_types = FALSE" dentro del comando.

# 2. TOKENIZACIÓN Y LIMPIEZA DE TEXTO
# Transformamos el texto a estructura "un token por fila" (one-token-per-row)
# a partir de la columna 'article_text' y filtramos palabras vacías (stopwords)
mckinsey_words <- mckinsey_raw %>%
  unnest_tokens(output = word, input = article_text) %>%
  anti_join(stop_words, by = "word")

# Exploración rápida: Palabras más frecuentes en el corpus general
frecuencia_palabras <- mckinsey_words %>%
  count(word, sort = TRUE)

# 3. ANÁLISIS DE FRECUENCIA PONDERADA (TF-IDF)
# Calculamos TF-IDF para identificar las palabras clave más representativas de cada artículo
mckinsey_tf_idf <- mckinsey_words %>%
  count(title, word) %>%
  bind_tf_idf(term = word, document = title, n = n) %>%
  arrange(desc(tf_idf))

# 4. ANÁLISIS DE SENTIMIENTOS
# A. Clasificación Binaria (Lexicón 'bing': positivo vs. negativo)
# Agrupamos por artículo para calcular volumen de palabras positivas/negativas y puntaje neto
sent_binario <- mckinsey_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(title, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(puntaje_neto = positive - negative)

# B. Clasificación Graduada (Lexicón 'afinn': escala numérica de -5 a +5)
# Evaluamos la intensidad o carga de sentimiento promedio por artículo
sent_graduado <- mckinsey_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(title) %>%
  summarise(
    puntaje_afinn_promedio = mean(value),
    palabras_evaluadas = n()
  )

# 5. TOPIC MODELING (MODELADO DE TÓPICOS CON LDA)
# Conversión del formato 'tidy' a la matriz Document-Term Matrix (DTM) requerida por LDA
dtm_mckinsey <- mckinsey_words %>%
  count(title, word) %>%
  cast_dtm(document = title, term = word, value = n)

# A. Modelo Latent Dirichlet Allocation (LDA) para k = 10 tópicos
lda_k10 <- LDA(dtm_mckinsey, k = 10, control = list(seed = 1234))

# B. Modelo Latent Dirichlet Allocation (LDA) para k = 15 tópicos
lda_k15 <- LDA(dtm_mckinsey, k = 15, control = list(seed = 1234))

# Extracción de la distribución palabra-tópico (beta) para el modelo k = 10
top_terminos_k10 <- tidy(lda_k10, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Extracción de la distribución palabra-tópico (beta) para el modelo k = 15
top_terminos_k15 <- tidy(lda_k15, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 6. VISUALIZACIÓN DE RESULTADOS
print(top_terminos_k10, n = 50)

