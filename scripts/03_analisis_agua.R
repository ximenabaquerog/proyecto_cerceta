# ==============================================================================
# PROPÓSITO: Análisis de la dinámica hídrica y cálculo de extensión de lámina de agua
# PROYECTO: Respuesta de las aves acuáticas a la dinámica de inundación (Proyecto Cerceta)
# ARCHIVOS DE ENTRADA:
#   - outputs/01_parcelas_filtradas.shp (Capa SIG con las geometrías oficiales)
#   - data/GEE/MNDWI_detallado_parcelas.csv (Datos con la columna 'mean' de MNDWI)
# ARCHIVOS DE SALIDA:
#   - outputs/03_datos_agua_final.csv (Tabla Master de Agua para el Script 04)
#   - outputs/03_test_tendencias_agua.csv (Resultados de Mann-Kendall por parcela)
#   - outputs/03_grafico_tendencia_agua.png (Visualización de la serie temporal)
# ==============================================================================


# 1. Cargar librerías ----------------------------------------------------------
library(tidyverse) # Manipulación de datos y visualización 
library(sf)        # Manejo de capas vectoriales 
library(here)      # Gestión de rutas relativas 
library(Kendall)   # Test de tendencia de Mann-Kendall 
library(mgcv)      # Suavizado GAM para visualización 

# 2. Carga y Depuración de Datos -----------------------------------------------
# Cargamos el shapefile filtrado del Script 01 
parcelas_shp <- st_read(here("outputs/01_parcelas_filtradas.shp")) |>  
  mutate(parcela = str_trim(parcela)) 

# Cargamos MNDWI detallado de GEE filtrando registros vacíos (NAs) 
datos_gee <- read_csv(here("data/GEE/MNDWI_detallado_parcelas.csv")) |> 
  filter(!is.na(mean)) |> 
  mutate(parcela = str_trim(parcela))

# 3. Cálculo: Inundación Relativa Estandarizada (0 a 1) ----------------------
# para comparar lagunas de distinto tamaño y evitar ceros,escalamos el índice MNDWI respecto a su propio rango histórico 

analisis_agua <- datos_gee |> 
  group_by(parcela) |> 
  mutate(
    # Identificamos el rango histórico de inundacion para cada parcela individual
    mndwi_min = min(mean, na.rm = TRUE),
    mndwi_max = max(mean, na.rm = TRUE),
    
    # Escalado Min-Max (Inundacion Relativa)
    # 0 = Año más seco de la serie | 1 = Año de máxima inundación histórica
    inundacion_relativa = (mean - mndwi_min) / (mndwi_max - mndwi_min)
  ) |> 
  ungroup() |> 
  # Limpieza técnica: asegurar que el rango esté estrictamente entre 0 y 1 
  mutate(inundacion_relativa = pmax(0, pmin(1, inundacion_relativa)))

print(summary(analisis_agua$inundacion_relativa)) #comprobacion

# 4. Análisis de Tendencia Mann-Kendall ---------------------------
tendencias_inundacion <- analisis_agua |> 
  group_by(parcela) |> 
  summarise(
    # Test sobre la variable continua 
    tau_val = MannKendall(inundacion_relativa)$tau,
    p_val   = MannKendall(inundacion_relativa)$sl,
    .groups = "drop"
  ) |> 
  mutate(color_tendencia = case_when(
    tau_val < 0 & p_val < 0.05 ~ "Pérdida Significativa",  # Desecación
    tau_val > 0 & p_val < 0.05 ~ "Ganancia Significativa", # Recuperación
    TRUE                       ~ "Sin Tendencia Clara"
  ))

# Unimos los resultados estadísticos a la tabla principal
data_final <- analisis_agua |> 
  left_join(tendencias_inundacion, by = "parcela")

# 5. Visualización A: Tendencia Global de inundacion ------------------
resumen_global <- data_final |> 
  group_by(year) |> 
  summarize(media_regional = mean(inundacion_relativa, na.rm = TRUE))

grafico_global <- ggplot(data_final, aes(x = year, y = inundacion_relativa)) +
  # Fondo: Nube de líneas grises con todas las lagunas [
  geom_line(aes(group = parcela), color = "grey85", alpha = 0.3, linewidth = 0.2) +
  # Media regional anual (Línea Azul Gruesa) 
  geom_line(data = resumen_global, aes(x = year, y = media_regional), 
            color = "#1f78b4", linewidth = 1.5) +
  # Tendencia Lineal Roja para demostrar el declive histórico 
  geom_smooth(aes(group = 1), method = "lm", color = "#e31a1c", 
              linetype = "dashed", se = FALSE) +
  # coord_cartesian evita que geom_smooth elimine filas fuera de rango 
  coord_cartesian(ylim = c(0, 1)) + 
  labs(title = "Evolución de la inundacion Relativa en Doñana (2005-2025)",
       subtitle = "Escala: 0 = Punto más seco histórico | 1 = Punto más húmedo histórico",
       y = "Índice de inundacion Relativa (0-1)", x = "Año Hidrológico") +
  theme_minimal()

print(grafico_global) 

# 6. Visualización B: Comparativa de Tendencias Significativas -----------------
data_sig <- data_final |> 
  filter(p_val < 0.05) |> 
  arrange(tau_val) |>  # Ordenar de mayor desecación a mayor ganancia 
  mutate(parcela = factor(parcela, levels = unique(parcela)))

grafico_comparativo <- ggplot(data_sig, aes(x = year, y = inundacion_relativa)) +
  geom_area(aes(fill = color_tendencia), alpha = 0.2) +
  geom_line(aes(color = color_tendencia), linewidth = 1) +
  # Suavizado GAM para captar ciclos interanuales 
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), color = "black", 
              linetype = "dashed", linewidth = 0.5, se = FALSE) +
  facet_wrap(~parcela, scales = "fixed") + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(values = c("Pérdida Significativa" = "#e31a1c", 
                                "Ganancia Significativa" = "#33a02c")) +
  scale_fill_manual(values = c("Pérdida Significativa" = "#e31a1c", 
                               "Ganancia Significativa" = "#33a02c")) +
  labs(title = "Lagunas con Cambios Significativos en inundacion",
       y = "inundacion Relativa (0-1)", x = "Año") +
  theme_minimal(base_size = 7) + theme(legend.position = "bottom")

print(grafico_comparativo)

# 7. Exportación de Productos -------------------------------------------

# Tabla preparada para el Script 04 
tabla_exportar <- data_final  |> 
  dplyr::select(parcela, year, mean, inundacion_relativa) |> 
  rename(año = year, 
         valor_medio_MNDWI = mean, 
         inundacion_Relativa_Std = inundacion_relativa)

# Exportar tabla master de agua
write_csv(tabla_exportar, here("outputs/03_datos_agua_final.csv")) 

# Exportar resultados estadísticos
write_csv(tendencias_inundacion, here("outputs/03_test_tendencias_agua.csv"))

# Exportar gráficas
ggsave(here("figs/03_tendencia_inundacion_global.png"), grafico_global, width = 10, height = 6, dpi = 300)
ggsave(here("figs/03_comparativa_inundacion.png"), grafico_comparativo, width = 16, height = 12, dpi = 300)

message(">>> SCRIPT 03 COMPLETADO")












