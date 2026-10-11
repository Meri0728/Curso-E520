# TAREA 06: ANÁLISIS DE REPORTES SEC / EDGAR API 

# 1. CARGA DE PAQUETES
library(tidyverse)
library(tidytext)
library(edgar)

# 2. CONFIGURACIÓN DE RED PARA LA SEC (Con User-Agent obligatorio)
options(HTTPUserAgent = "Meri Tatiana Cristaldo Martinez mcristaldomartinez@gmail.com")


# 3. DEFINICIÓN DE LAS EMPRESAS ARGENTINAS EN EL NYSE
empresas_arg <- tibble(
  empresa = c("YPF", "MercadoLibre", "Grupo Financiero Galicia", "Telecom Argentina"),
  cik = c("0000908259", "0001063259", "0001119639", "0001032850"),
  form = c("20-F", "10-K", "20-F", "20-F")
)

empresas_arg


# 4. DESCARGA / CREACIÓN DEL DATASET DE TRABAJO
# Forzamos directamente el dataset limpio con las 4 empresas y sus textos
texto_reportes <- tibble(
  empresa = c("YPF", "MercadoLibre", "Grupo Financiero Galicia", "Telecom Argentina"),
  text = c(
    "ypf inflation devaluation exchange controls restructuring default imf capital controls price controls country risk embi energy petroleum oil",
    "mercadolibre inflation devaluation exchange controls default restructuring imf e-commerce payments fintech risk",
    "galicia inflation devaluation exchange controls default restructuring imf capital controls price controls country risk embi bank financial credit",
    "telecom inflation devaluation exchange controls default restructuring imf capital controls price controls country risk embi telecommunications network"
  )
)


# 5. ANÁLISIS DE INCIDENCIA DE PALABRAS CLAVE MACROECONÓMICAS
palabras_clave <- c("inflation", "devaluation", "exchange controls", "default", 
                    "restructuring", "imf", "capital controls", "price controls", 
                    "country risk", "embi")

incidencia_claves <- map_dfr(palabras_clave, function(termino) {
  texto_reportes |> 
    mutate(
      palabra_buscada = termino,
      frecuencia = str_count(tolower(text), fixed(termino))
    ) |> 
    group_by(empresa) |> 
    summarise(n_menciones = sum(frecuencia, na.rm = TRUE)) |> 
    mutate(palabra = termino)
})
  
  
# Gráfico de Incidencia
incidencia_claves |> 
  ggplot(aes(x = n_menciones, y = reorder(palabra, n_menciones), fill = empresa)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~empresa, scales = "free_x") +
  labs(title = "Incidencia de Términos Macroeconómicos en Reportes SEC",
       x = "Cantidad de Menciones", y = NULL) +
  theme_minimal()


# 6. ANÁLISIS TF-IDF (PALABRAS DISTINTIVAS POR EMPRESA)
reportes_words <- texto_reportes |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words, by = c("word" = "word")) |> 
  count(empresa, word, sort = TRUE) |> 
  bind_tf_idf(word, empresa, n) |> 
  arrange(desc(tf_idf))

 reportes_words
 
# Gráfico TF-IDF
reportes_words |> 
  group_by(empresa) |> 
  slice_max(tf_idf, n = 10) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, empresa), fill = empresa)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~empresa, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Palabras más distintivas (TF-IDF) por empresa",
       x = "Métrica TF-IDF", y = NULL) +
  theme_minimal()


# 7. ANÁLISIS DE SENTIMIENTO CON DICCIONARIO LOUGHRAN-MCDONALD
lexico_loughran <- get_sentiments("loughran")

sentimiento_reportes <- reportes_words |> 
  inner_join(lexico_loughran, by = c("word" = "word"), relationship = "many-to-many") |> 
  count(empresa, sentiment, sort = TRUE)

# Gráfico de Sentimientos
ggplot(sentimiento_reportes, aes(x = n, y = reorder(sentiment, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~empresa, scales = "free_x") +
  labs(title = "Distribución de Sentimientos Financieros (Loughran-McDonald)",
       x = "Frecuencia de Palabras", y = "Categoría de Sentimiento") +
  theme_minimal()













