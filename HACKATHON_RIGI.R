# ==============================================================================
# HACKATÓN ECON 520 - ANÁLISIS DE DESCALCE TERRITORIAL (RIGI vs. REDEPRO)
# ==============================================================================

# 1. Cargar librerías necesarias
library(tidyverse) 
#para la limpieza, manipulación, armonización y visualización de datos.
library(readxl)
#para la lectura de los archivos de datos en formato Excel.
install.packages("ggrepel")
library(ggrepel) # Para etiquetar los puntos del gráfico sin que se superpongan

# 2. Cargar datasets 
#Importamos las bases descargadas directamente desde los portales oficiales.
rigi <- read_excel("C:/Users/marti/Downloads/base_completa (1).xlsx")
redepro <- read_excel("C:/Users/marti/Downloads/proveedores-nacionales-sectores-estrategicos.xlsx")

# FUENTES DE INFORMACIÓN UTILIZADAS:
# 1. Base RIGI (Proyectos de Inversión): Portal de Datos Abiertos del Ministerio de Economía de la Nación
# 2. Base REDEPRO (Registro de Proveedores de Sectores Estratégicos): Registro de Proveedores Nacionales de la Secretaría de Industria y Desarrollo Productivo
#https://www.argentina.gob.ar/economia/rigi
#https://datos.gob.ar/dataset/registro-nacional-de-desarrollo-de-proveedores-redepro
#https://monitor-rigi-desip-iiep.github.io/RIGI/base-datos.html


# 3. Limpieza y preparación de la base RIGI
# - Se limpian los valores numéricos de inversión eliminando caracteres no numéricos.
# - Se utiliza separate_rows() para desglosar proyectos compartidos entre múltiples provincias.
# - Se estandarizan los nombres de las jurisdicciones (ej: CABA).
rigi_clean <- rigi %>%
  mutate(
    # parse_number extrae el valor numérico respetando puntos/comas adecuadamente
    inversion_num = readr::parse_number(as.character(inversion_total_mill_usd)),
    empleo_num = readr::parse_number(as.character(empleos_directos_indirectos_informados)),
    inversion_num = replace_na(inversion_num, 0),
    empleo_num = replace_na(empleo_num, 0)
  ) %>%
  # Contamos cuántas provincias tiene cada proyecto para no duplicar montos
  mutate(cant_provincias = str_count(provincia, ";") + 1) %>%
  separate_rows(provincia, sep = ";") %>%
  mutate(
    provincia = trimws(provincia),
    provincia = ifelse(provincia == "CABA", "Ciudad Autónoma de Buenos Aires", provincia),
    # Dividimos la inversión en partes iguales entre las provincias involucradas
    inversion_asignada = inversion_num / cant_provincias
  )

# 4. Limpieza y preparación de la base REDEPRO
# - Se estandarizan los nombres provinciales con case_when().
# - Se usa pivot_longer() para llevar la estructura ancha a formato largo y listar todos los sectores atendidos por cada empresa.
redepro_clean <- redepro %>%
  mutate(
    provincia_nombre = case_when(
      provincia_nombre == "Neuquen" ~ "Neuquén",
      provincia_nombre == "Rio Negro" ~ "Río Negro",
      provincia_nombre == "Cordoba" ~ "Córdoba",
      provincia_nombre == "CABA" ~ "Ciudad Autónoma de Buenos Aires",
      TRUE ~ trimws(provincia_nombre)
    )
  )

# Convertir a formato largo para mapear todos los sectores informados
redepro_largo <- redepro_clean %>%
  pivot_longer(
    cols = starts_with("sector_abastecido_"),
    names_to = "col_sector",
    values_to = "sector_redepro"
  ) %>%
  filter(!is.na(sector_redepro))

# Crosswalk: Mapeo de sectores REDEPRO a la taxonomía del RIGI
crosswalk <- tribble(
  ~sector_redepro,           ~sector_rigi,
  "Petróleo y Gas",           "Petróleo y Gas",
  "Industria Petroquímica",   "Petróleo y Gas",
  "Minería",                  "Minería",
  "Energias Renovables",      "Energía",
  "Nuclear",                  "Energía",
  "Industria Siderúrgica",    "Siderurgia",
  "Industria Metalúrgica",    "Siderurgia",
  "Construcción",             "Infraestructura",
  "Industria Ferroviaria",    "Infraestructura",
  "Agua y Saneamiento",       "Infraestructura"
)
# Unimos la oferta de proveedores con la tabla de equivalencias mediante inner_join()
redepro_traducido <- redepro_largo %>%
  inner_join(crosswalk, by = "sector_redepro")

# 5. Agregación a nivel provincial (Demanda RIGI vs. Oferta REDEPRO)
# Agrupamos por provincia para obtener totales de inversión y cantidad de proveedores únicos por jurisdicción.
demanda_prov <- rigi_clean %>%
  group_by(provincia) %>%
  summarise(
    inversion_total = sum(inversion_asignada, na.rm = TRUE),
    empleos_totales = sum(empleo_num, na.rm = TRUE),
    .groups = "drop"
  )

oferta_prov <- redepro_traducido %>%
  group_by(provincia_nombre) %>%
  summarise(
    cant_proveedores = n_distinct(razon_social),
    .groups = "drop"
  )

# 6. Cruce de datos y cálculo de porcentajes
# Unimos demanda y oferta con full_join() y calculamos la participación relativa (%) de cada provincia.
comparacion_prov <- full_join(demanda_prov, oferta_prov, by = c("provincia" = "provincia_nombre")) %>%
  mutate(
    cant_proveedores = replace_na(cant_proveedores, 0),
    inversion_total = replace_na(inversion_total, 0)
  ) %>%
  mutate(
    pct_inversion = (inversion_total / sum(inversion_total)) * 100,
    pct_proveedores = (cant_proveedores / sum(cant_proveedores)) * 100
  )

# 7. Filtrar Top 10 y preparar datos para el gráfico
# Seleccionamos las Top 10 provincias con mayor volumen de inversión y adaptamos la estructura para ggplot2.
top10 <- comparacion_prov %>%
  arrange(desc(inversion_total)) %>%
  slice(1:10) %>%
  pivot_longer(
    cols = c(pct_inversion, pct_proveedores),
    names_to = "metrica",
    values_to = "porcentaje"
  ) %>%
  mutate(
    metrica = case_when(
      metrica == "pct_inversion" ~ "% de inversión RIGI",
      metrica == "pct_proveedores" ~ "% de proveedores REDEPRO"
    )
  )

# 8. GENERACIÓN DE LA VISUALIZACIÓN CON ggplot2
# El gráfico de barras agrupadas (geom_col con position = "dodge") compara la distribución geográfica de la demanda de capital (RIGI) frente a la densidad de la oferta proveedora (REDEPRO). 
# Evidencia un claro "descalce territorial": provincias como Neuquén y Río Negro capturan casi la totalidad de la inversión proyectada 
#pero tienen una escasa participación de proveedores locales registrados, mientras que centros industriales como Buenos Aires o Santa Fe concentran la oferta proveedora pero casi nula inversión RIGI.
dev.new() # Abre el gráfico en una ventana limpia

ggplot(top10, aes(x = reorder(provincia, -porcentaje), y = porcentaje, fill = metrica)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    title = "Descalce territorial: inversión RIGI vs proveedores REDEPRO",
    subtitle = "Participación de cada provincia sobre el total (top 10 provincias con más inversión)",
    x = "Provincia",
    y = "% del total",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13)
  )
