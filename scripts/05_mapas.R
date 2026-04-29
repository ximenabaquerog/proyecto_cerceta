# ==============================================================================
# PROPÓSITO: Representación cartográfica de resultados espaciales y tendencias
# PROYECTO: Respuesta de las aves acuáticas a la dinámica de inundación (Proyecto Cerceta)
# ARCHIVOS DE ENTRADA:
#   - outputs/01_parcelas_filtradas.shp (Vectores de las parcelas de estudio)
#   - outputs/03_test_tendencias_agua.csv (Resultados estadísticos de significancia)
#   - outputs/GEE/2_MNDWI_Medio_Donana.tif (Ráster de inundación media histórica)
#   - outputs/GEE/5_Tendencia_Tau_Donana_TIF.tif (Ráster de tendencia de Sen)
#   - outputs/GEE/MNDWI_anuales/ (Carpeta con los 20 rásters anuales)
# ARCHIVOS DE SALIDA:
#   - outputs/05_mapa_global_significancia.png (Mapa de situación y resultados)
#   - outputs/05_mapa_tendencia_tau.png (Mapa de degradación/recuperación)
#   - outputs/05_mapa_mosaico_anual.png (Mosaico temporal de 20 años)
# ==============================================================================

# 1. Cargar librerías ----------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(ggspatial) # Para flecha norte y escala
library(here)

# 2. Configuración de Proyección y Carga de Datos ------------------------------
# Definimos el CRS estándar para Doñana (UTM 30N - ETRS89)
crs_proyecto <- "EPSG:25830"

# Carga de vectores
parcelas_estudio <- st_read(here("outputs/01_parcelas_filtradas.shp")) %>% 
  st_transform(crs_proyecto)

# Carga de resultados estadísticos del Script 03 para marcar parcelas significativas
resultados_agua <- read_csv(here("outputs/03_test_tendencias_agua.csv"))

# Unimos la estadística con la geometría
mapa_datos <- parcelas_estudio %>%
  left_join(resultados_agua, by = "parcela")

# Carga de Rásters
mndwi_medio <- rast(here("data/GEE/MNDWI_Medio_Donana_2005_2025.tif")) %>% 
  project(crs_proyecto)

raster_tau <- rast(here("data/GEE/Tendencia_Tau.tif")) %>% 
  project(crs_proyecto)


# 3. Mapa A: Inundación Media y Significancia ---------------------------------
# Visualiza el promedio histórico resaltando en rojo las lagunas que se secan.

mapa_global <- ggplot() +
  # Fondo: Ráster de MNDWI medio
  layer_spatial(data = mndwi_medio, aes(fill = after_stat(band1))) +
  scale_fill_gradientn(colors = c("white", "#ebf3fb", "#084594"), 
                       name = "MNDWI Medio", na.value = "transparent") +
  
  # Capa: Todas las parcelas de estudio (Contorno amarillo)
  geom_sf(data = mapa_datos, fill = NA, color = "yellow", linewidth = 0.4) +
  
  # CORRECCIÓN AQUÍ: Cambiamos 'p_valor' por 'p_val' según la Fuente [1]
  geom_sf(data = filter(mapa_datos, p_val < 0.05), 
          fill = NA, color = "red", linewidth = 0.8) +
  
  # Elementos cartográficos
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
  labs(title = "Inundación Media y Áreas Críticas",
       subtitle = "Rojo: Tendencias significativas (p < 0.05) | Amarillo: Parcelas",
       caption = "Datos: Landsat / GEE") +
  theme_minimal()

print(mapa_global)

# 4. Mapa B: Mapa de Tendencia Tau (Degradación vs Recuperación) ---------------
# Visualiza la pendiente de Sen: Rojo indica desecación, Verde recuperación.

mapa_tau <- ggplot() +
  # Fondo: Ráster de la tendencia Tau de Mann-Kendall
  layer_spatial(data = raster_tau, aes(fill = after_stat(band1))) +
  scale_fill_gradient2(low = "#e31a1c", mid = "white", high = "#33a02c", 
                       midpoint = 0, name = "Valor Tau", na.value = "transparent") +
  
  # Capa: Contornos de parcelas significativas para dar contexto geográfico
  # CORRECCIÓN AQUÍ: Se cambia 'p_valor' por 'p_val' según los resultados del Script 03
  geom_sf(data = filter(mapa_datos, p_val < 0.05), 
          fill = NA, color = "black", linetype = "dashed", linewidth = 0.3) +
  
  # Elementos cartográficos
  annotation_scale(location = "bl") +
  labs(title = "Mapa de Tendencia Hidrológica (Índice Tau)",
       subtitle = "Valores negativos (rojo) indican pérdida de agua persistente 2005-2025",
       caption = "Datos: Landsat / GEE | Método: Test de Mann-Kendall") +
  theme_minimal()

print(mapa_tau)


# 5. Mapa C: Mosaico Temporal (20 paneles anuales) -----------------------------
# Genera un stack de todos los inviernos para ver la evolución visual año a año.

# Cargamos los archivos desde 'data/GEE/' que es donde se almacenan los insumos [7, 8].
ruta_anuales <- here("outputs/GEE/MNDWI_anuales/")
archivos <- list.files(ruta_anuales, pattern = ".tif$", full.names = TRUE)

if (length(archivos) > 0) {
  # 1. Cargamos el stack y proyectamos al sistema oficial del proyecto [9, 10].
  stack_raw <- rast(archivos) %>% project(crs_proyecto)
  
  # 2. Extraemos el año del nombre de cada archivo para las etiquetas [11].
  # Esto asegura que debajo de cada mapa aparezca el año correcto (2005, 2006...).
  nombres_años <- archivos %>% basename() %>% str_extract("\\d{4}")
  names(stack_raw) <- nombres_años
  
  # 3. TRANSFORMACIÓN CRÍTICA: Convertimos el ráster a tabla tabular [4, 12].
  # 'xy = TRUE' mantiene la ubicación de los píxeles.
  # 'na.rm = TRUE' elimina los píxeles vacíos para que el script sea rápido.
  mosaico_df <- as.data.frame(stack_raw, xy = TRUE, na.rm = TRUE) %>%
    pivot_longer(cols = -c(x, y), names_to = "año", values_to = "valor")
  
  # 4. Creación del gráfico mediante el motor estándar de ggplot2 [2, 13].
  mapa_mosaico <- ggplot() +
    # Dibujamos los píxeles de inundación
    geom_raster(data = mosaico_df, aes(x = x, y = y, fill = valor)) +
    # Superponemos los límites de las parcelas de estudio en cada panel [14].
    geom_sf(data = parcelas_estudio, fill = NA, color = "yellow", linewidth = 0.05, alpha = 0.4) +
    # CREACIÓN DEL MOSAICO: Ahora 'año' es una columna válida para facetar [15].
    # 'strip.position = bottom' pone el año debajo de cada foto como pediste.
    facet_wrap(~año, ncol = 5, strip.position = "bottom") +
    # Escala de color: de Blanco (seco) a Azul Intenso (agua) [16].
    scale_fill_gradient(low = "white", high = "#084594", 
                        na.value = "transparent", name = "MNDWI") +
    labs(title = "Evolución Anual de la Lámina de Agua en Doñana (2005-2025)",
         subtitle = "Serie histórica de inviernos (Periodo Noviembre-Enero)") +
    # Limpiamos el gráfico para un acabado profesional [17].
    theme_void() + 
    theme(strip.text = element_text(size = 9, face = "bold", margin = margin(t = 5)),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          legend.position = "right")
  
  # Visualización
  print(mapa_mosaico)
}

# Guardar la imagen final en alta resolución para la memoria
ggsave(here("outputs/05_mosaico_anual_2005_2025.png"), 
       plot = mapa_mosaico, width = 14, height = 11, dpi = 300, bg = "white")


# 6. Exportación de Productos Cartográficos ------------------------------------

ggsave(here("figs/05_mapa_media_MNDWI.png"), mapa_global, 
       width = 10, height = 8, dpi = 300)

ggsave(here("figs/05_mapa_tendencia_tau.png"), mapa_tau, 
       width = 10, height = 8, dpi = 300)

ggsave(here("figs/05_mapa_mosaico_anual.png"), mapa_mosaico, 
       width = 14, height = 10, dpi = 300)

message(">>> SCRIPT 05 COMPLETADO")


