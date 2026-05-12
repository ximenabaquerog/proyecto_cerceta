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
library(tidyverse) 
library(sf)        
library(terra)     
library(here)      
library(Kendall)   
library(mgcv)      

# 2. Configuración de Entradas ------------------------------------------------
# Cargamos parcelas y creamos tabla puente para los IDs de extracción 
parcelas_vect <- vect(here("outputs/01_parcelas_filtradas.shp"))

nombres_parcelas <- as.data.frame(parcelas_vect) |>
  mutate(id_extraido = row_number(), 
         parcela = str_trim(parcela)) |>
  dplyr::select(id_extraido, parcela)

mndwi_archivos <- list.files(path = here("data/GEE/MNDWI_anuales"), 
                             pattern = ".tif$", full.names = TRUE)

# 3. Procesamiento y Reclasificación coberturas ----------------------------
func_reclass <- function(img) {
  classify(img, rbind(c(-1, 0.07, 1), 
                      c(0.07, 0.335, 2), 
                      c(0.335, 0.659, 3), 
                      c(0.659, 1, 4)))
}

lista_anual <- list()
for (i in seq_along(mndwi_archivos)) {
  anio_actual <- as.numeric(str_extract(basename(mndwi_archivos[i]), "\\d{4}"))
  r <- rast(mndwi_archivos[i])
  r_clase <- func_reclass(r)
  
  ext_clases <- terra::extract(r_clase, parcelas_vect, fun = "table", na.rm = TRUE)
  
  tabla_anio <- as.data.frame(ext_clases) |>
    rename(id_extraido = ID) |>
    mutate(across(everything(), as.numeric)) |>
    left_join(nombres_parcelas, by = "id_extraido") |>
    mutate(año = anio_actual)
  
  lista_anual[[i]] <- tabla_anio
}

df_clases_raw <- bind_rows(lista_anual)

# 4. Cálculo de Ratios de Hábitat ---------------------------------------------
df_clases_final <- df_clases_raw |>
  rename(count_seco = `1`, count_vegetal = `2`, count_verde = `3`, count_azul = `4`) |>
  mutate(
    pixeles_totales = count_seco + count_vegetal + count_verde + count_azul,
    ratio_inundacion = (count_verde + count_azul) / pixeles_totales,
    ratio_seco = count_seco / pixeles_totales,
    ratio_vegetal = count_vegetal / pixeles_totales,
    ratio_verde = count_verde / pixeles_totales,
    ratio_azul = count_azul / pixeles_totales
  )

# 5. Integración con MNDWI Medio ----------------------------------------------
datos_gee_mean <- read_csv(here("data/GEE/MNDWI_detallado_parcelas.csv")) |>
  filter(!is.na(mean)) |>
  mutate(parcela = str_trim(parcela)) |>
  dplyr::select(parcela, year, mean)

data_analisis <- df_clases_final |>
  left_join(datos_gee_mean, by = c("parcela", "año" = "year")) |>
  filter(!is.na(ratio_inundacion))

# 6. Análisis de Tendencia Mann-Kendall ----------------------------------------
tendencias <- data_analisis |>
  group_by(parcela) |>
  summarise(
    tau_val = if(n() >= 3 && sd(ratio_inundacion) != 0) MannKendall(ratio_inundacion)$tau else NA,
    p_val   = if(n() >= 3 && sd(ratio_inundacion) != 0) MannKendall(ratio_inundacion)$sl else NA,
    .groups = "drop"
  ) |>
  mutate(color_tendencia = case_when(
    tau_val < 0 & p_val < 0.05 ~ "Pérdida Significativa",
    tau_val > 0 & p_val < 0.05 ~ "Ganancia Significativa",
    TRUE                       ~ "Sin Tendencia Clara"
  ))

data_final <- data_analisis |> left_join(tendencias, by = "parcela")


# 7. Visualización  ---------------------------------------

# --- GRAFICO 1: TENDENCIA GLOBAL AGUA  ---
resumen_global <- data_final |>
  group_by(año) |>
  summarize(media_regional = mean(ratio_inundacion, na.rm = TRUE))

grafico_tendencia_global <- ggplot(data_final, aes(x = año, y = ratio_inundacion)) +
  geom_line(aes(group = parcela), color = "grey90", alpha = 0.4, linewidth = 0.2) +
  geom_line(data = resumen_global, aes(y = media_regional), color = "#1f78b4", linewidth = 1.5) +
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, color = "#e31a1c", linetype = "dashed", se = FALSE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Evolución Temporal del Ratio de Inundación Relativa en las Parcelas de Censo Terrestre (2005-2025)", subtitle = "Evolución del promedio de inundación invernal (línea azul) frente a la tendencia histórica de la serie temporal (línea roja discontinua)",
       y = "Ratio de Inundación", x = "Año") +
  theme_minimal()
print(grafico_tendencia_global)

# --- GRAFICO 2: RESUMEN MEDIAS COBERTURA ---
resumen_coberturas <- data_final |>
  group_by(año) |>
  summarize(across(starts_with("ratio_"), mean, na.rm = TRUE)) |>
  pivot_longer(cols = starts_with("ratio_"), names_to = "clase", values_to = "media")


# Definimos los colores exactos vinculados a cada ratio
colores_cobertura <- c(
  "ratio_seco"    = "#8c510a", # Marrón
  "ratio_vegetal" = "#7fbc41", # Verde
  "ratio_verde"   = "#92c5de", # Azul claro 
  "ratio_azul"    = "#0571b0"  # Azul oscuro 
)

# Definimos las etiquetas que queremos ver en la leyenda
etiquetas_cobertura <- c(
  "ratio_seco"    = "Seco",
  "ratio_vegetal" = "Vegetación",
  "ratio_verde"   = "Inundación Somera",
  "ratio_azul"    = "Agua Profunda"
)

grafico_resumen_coberturas <- ggplot() +
  # Capa 1: Líneas de colores para las 4 clases de cobertura
  geom_line(data = resumen_coberturas |> filter(clase != "ratio_inundacion"),
            aes(x = año, y = media, color = clase), linewidth = 1.2) +
  
  # Capa 2: Línea gris punteada para el ratio de inundación total
  geom_line(data = resumen_coberturas |> filter(clase == "ratio_inundacion"),
            aes(x = año, y = media), color = "grey60", linewidth = 0.6, linetype = "dotted") +
  
  # Configuración manual de colores y nombres de leyenda
  scale_color_manual(values = colores_cobertura, 
                     labels = etiquetas_cobertura) +
  
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Dinámica Interanual de las Coberturas Superficiales en Doñana durante el Periodo Invernal", 
       subtitle = "Línea punteada: Inundación Total (Somera + Profunda)",
       y = "MNDWI Medio estandarizado", 
       x = "Año",
       color = "Tipo de Cobertura") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))

print(grafico_resumen_coberturas)



# --- GRAFICO 3: TOP 10 CAMBIOS EXTREMOS  ---
top_10_list <- bind_rows(
  tendencias |> filter(p_val < 0.05) |> arrange(tau_val) |> head(5),
  tendencias |> filter(p_val < 0.05) |> arrange(desc(tau_val)) |> head(5)
)

grafico_top10_cambios <- data_final |>
  filter(parcela %in% top_10_list$parcela) |>
  mutate(parcela = factor(parcela, levels = top_10_list$parcela)) |>
  ggplot(aes(x = año, y = ratio_inundacion)) +
  geom_area(aes(fill = color_tendencia), alpha = 0.2) +
  geom_line(aes(color = color_tendencia), linewidth = 1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", linewidth = 0.5, se = FALSE) +
  facet_wrap(~parcela, scales = "fixed", nrow = 2) +
  scale_color_manual(values = c("Pérdida Significativa" = "#e31a1c", "Ganancia Significativa" = "#33a02c")) +
  scale_fill_manual(values = c("Pérdida Significativa" = "#e31a1c", "Ganancia Significativa" = "#33a02c")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = " Series Temporales de Inundación en Localidades con Tendencias Hidrológicas Extremas", 
       subtitle = "Evolución del ratio de inundación en las 5 lagunas con mayor pérdida hídrica (rojo) y las 5 con mayor recuperación (paneles verdes) identificadas mediante el Test de Mann-Kendall",
       y = "Ratio de Inundación", 
       x = "Año") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
print(grafico_top10_cambios)

# 8. Exportación de productos---------------------------------------------------------------

# Tabla preparada para el Script 04 
tabla_master <- data_final  |> 
  dplyr::select(parcela, año, mean, ratio_inundacion, count_seco, count_vegetal, count_verde, count_azul,
                ratio_seco, ratio_vegetal, ratio_verde, ratio_azul) |>
  rename(valor_medio_MNDWI = mean, Ratio_Inundacion_Total = ratio_inundacion)

# Exportar tabla master de agua
write_csv(tabla_master, here("outputs/03_datos_agua_final.csv")) 

# Exportar resultados estadísticos
write_csv(tendencias, here("outputs/03_test_tendencias_agua.csv"))

# Exportar gráficas
ggsave(here("figs/03_tendencia_global_agua.png"), grafico_tendencia_global, width = 10, height = 6)
ggsave(here("figs/03_resumen_medias_cobertura.png"), grafico_resumen_coberturas, width = 10, height = 6)
ggsave(here("figs/03_top10_cambios_extremos.png"), grafico_top10_cambios, width = 14, height = 8)

message(">>> SCRIPT 03 COMPLETADO")

