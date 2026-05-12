# ==============================================================================
# PROPÓSITO: Carga, limpieza, armonización y filtrado de censos de aves acuáticas
# PROYECTO: Respuesta de las aves acuáticas a la dinámica de inundación (Proyecto Cerceta)
# ARCHIVOS DE ENTRADA:
#   - data/2005_2026_series/terrestrial_counts_data.csv (Censos brutos)
#   - data/comparacion_aligned.csv (Traductor para armonizar nombres de parcelas)
#   - data/parcelas_censo_terrestre/parcelas.shp (Capa SIG de parcelas oficiales)
# ARCHIVOS DE SALIDA:
#   - outputs/01_censos_invernales_limpios.csv (Tabla para análisis de tendencias)
#   - outputs/01_parcelas_filtradas.shp (Capa SIG armonizada con los censos)
# ==============================================================================


# 1. Cargar librerías ----------------------------------------------------------
library(tidyverse) # manipulación de datos (dplyr, ggplot2, etc.)
library(here) #gestión de rutas relativas para asegurar la portabilidad
library(lubridate) #manejo eficiente de objetos tipo fecha
library(sf) #procesamiento de datos espaciales (vectoriales)
library(readr)


# 2. Carga de datos ------------------------------------------------------------
# Se utilizan rutas relativas mediante here() para que el script funcione en cualquier PC
localities <- read_csv("data/2005_2026_series/terrestrial_counts_localties.csv")
counts_raw <- read_csv("data/2005_2026_series/terrestrial_counts_data.csv")

# Cargamos el shapefile de las parcelas de censo terrestre
parcelas_shp <- st_read(here("data/parcelas_censo_terrestre/parcelas.shp"))

# Cargamos el archivo de alineación para corregir discrepancias de nombres entre el censo y el mapa
traductor_nombres <- read_csv2(here("data/comparacion_aligned.csv")) |>  
  distinct(`presentes en censos invierno (2005-2026)`, .keep_all = TRUE)

# 3. Limpieza y Filtro Temporal (Invernada) ------------------------------------
# El objetivo es filtrar solo los meses de invernada (Noviembre, Diciembre y Enero)
counts_clean <- counts_raw |> 
  mutate(
    # Transformamos el código de censo en una fecha real
    date = ym(str_remove(Censo, "CT_")),
    year = year(date),
    month = month(date),
    # Definición de año hidrológico: Nov y Dic pertenecen al ciclo del año siguiente
    hydro_year = if_else(month %in% c(11, 12), year, year - 1)
  ) |> 
  # Filtramos solo el periodo clave de invernada (Nov-Ene)
  filter(month %in% c(11, 12, 1))

# 4. Armonización de nombres y unión de datos ---------------------------------
# Unimos los censos con el traductor para asegurar que las parcelas se llamen igual que en el archivo de polígonos (shapefile).
counts_aligned <- counts_clean |> 
  inner_join(traductor_nombres, by = c("Localidad nivel 4" = "presentes en censos invierno (2005-2026)"))

# 5. Filtro de Representatividad Histórica (Umbral 80%) -------------------------
# Para evitar sesgos por falta de datos, solo seleccionamos parcelas que hayan sido censadas en al menos el 50% de los años del periodo 2005-2025 (16 de 20 años).
parcelas_validas <- counts_aligned |> 
  filter(!is.na(`parcelas protocolo`)) |> 
  group_by(`parcelas protocolo`) |> 
  summarize(n_anios = n_distinct(hydro_year)) |> 
  filter(n_anios >= 10) |>   #para cumplir el 50%
  pull(`parcelas protocolo`)

counts_final <- counts_aligned |> 
  filter(`parcelas protocolo` %in% parcelas_validas)

# 6. Filtrado de la Capa Espacial ----------------------------------------------
# Mantenemos solo los polígonos que tienen representación de aves válida.Pasamos de 87 a 68
parcelas_filtradas <- parcelas_shp |> 
  filter(parcela %in% parcelas_validas)


# 7. Exportación de Resultados Depurados ---------------------------------------
# Exportar tabla de censos limpia
write_csv(counts_final, here("outputs/01_censos_invernales_limpios.csv"))

# Exportar shapefile filtrado
st_write(parcelas_filtradas, 
         here("outputs/01_parcelas_filtradas.shp"), 
         delete_dsn = TRUE) # delete_dsn permite sobrescribir si ya existe

message(">>> SCRIPT 01 COMPLETADO")