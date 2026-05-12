# ==============================================================================
# PROPÓSITO: Análisis de correlación mediante Modelos Lineales Mixtos (GLMM) 
#            entre la abundancia de aves y la dinámica de la lámina de agua.
# PROYECTO: Respuesta de las aves acuáticas a la dinámica de inundación (Proyecto Cerceta)
# ARCHIVOS DE ENTRADA:
#   - outputs/02_datos_aves.csv (Abundancia por especie/parcela/año)
#   - outputs/03_datos_agua.csv (Humedad y extensión por parcela/año)
# ARCHIVOS DE SALIDA:
#   - outputs/04_TABLA_MASTER_PROYECTO.csv (Base de datos final integrada)
#   - outputs/04_resumen_funcional_anual.csv (Medianas de abundancia por gremio)
#   - outputs/04_resumen_modelo_glmm.txt (Resultados estadísticos del modelo)
#   - outputs/04_correlacion_TOTAL.png (Visualización de la respuesta)
#   - outputs/04_correlacion_SOMERA.png
#   - outputs/04_correlacion_PROFUNDA.png
# ==============================================================================

# 1. Cargar librerías ----------------------------------------------------------
library(tidyverse)
library(here)
library(lme4)      # Para modelos mixtos 
library(patchwork) # Para combinar las 3 gráficas en una sola imagen

# 2. Carga y preparación de datos ----------------------------------------------
aves <- read_csv(here("outputs/02_datos_aves.csv"))
agua <- read_csv(here("outputs/03_datos_agua.csv"))


# Calculamos las 3 variables de superficie solicitadas (en Hectáreas) 
# Resolución pixel = 30m -> 900 m2 [42, 61]
agua_superficie <- agua |>
  mutate(
    area_total_ha    = (count_verde + count_azul) * 900 / 10000,
    area_somera_ha   = count_verde * 900 / 10000,
    area_profunda_ha = count_azul * 900 / 10000
  ) |>
  dplyr::select(parcela, año, area_total_ha, area_somera_ha, area_profunda_ha)

# Unión Master 
tabla_master <- aves |>
  inner_join(agua_superficie, by = c("parcela", "año")) |>
  filter(!is.na(abundancia))

# 3. Función de Visualización  ---------------------------
crear_grafica_correlacion <- function(datos, variable_x, titulo_eje, color_base) {
  
  limite_y <- quantile(datos$abundancia, 0.95, na.rm = TRUE)
  
  ggplot(datos, aes(x = .data[[variable_x]], y = abundancia)) +
    geom_point(aes(color = fun_group), alpha = 0.15, size = 1) +
    
    geom_smooth(aes(color = fun_group), 
                fill = "grey80",   # Franja gris fija
                method = "glm", 
                method.args = list(family = "poisson"), 
                formula = y ~ x, 
                linewidth = 1.1, 
                alpha = 0.4) +     
    
    facet_wrap(~fun_group, scales = "free_y", ncol = 3) +
    coord_cartesian(ylim = c(0, limite_y)) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank()) +
    labs(x = titulo_eje, y = "Abundancia Máxima Invernal (Nº Individuos)")
}

# 4. Generación de las 3 Gráficas Comparativas ---------------------------------

# Gráfica 1: Área de Inundación Total (Somera + Profunda)
g1 <- crear_grafica_correlacion(tabla_master, "area_total_ha", 
                                "Superficie Inundada Total de la Parcela (Ha)", "Spectral") +
  labs(title = "Influencia de la Superficie Inundada Total sobre la Estructura de la Comunidad de Aves Invernantes")
print(g1)

# Gráfica 2: Área de Inundación Somera (Píxeles verdes) 
g2 <- crear_grafica_correlacion(tabla_master, "area_somera_ha", 
                                "Superficie de Agua Somera (Ha)", "Blues") +
  labs(title = "Relación entre la Abundancia de Aves Acuáticas y la Extensión de Superficies de Inundación Somera")
print(g2)

# Gráfica 3: Área de Agua Profunda (Píxeles azules) 
g3 <- crear_grafica_correlacion(tabla_master, "area_profunda_ha", 
                                "Superficie de Agua Profunda (Ha)", "YlGnBu") +
  labs(title = "Respuesta de la Abundancia de Gremios Funcionales a la Disponibilidad de Hábitats de Agua Profunda")
print(g3)

# 5. Generación de Resumen Anual por Gremio ------------------------------------
# Creamos una tabla resumen que muestre la tendencia central anual de cada 
# grupo funcional en todo Doñana, útil para comparar con la serie de inundación.
metricas_anuales_grupo <- tabla_master |>
  group_by(año, fun_group) |>
  summarize(
    mediana_abundancia_anual = median(abundancia_std, na.rm = TRUE),
    humedad_media_regional = mean(area_total_ha, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Exportación ---------------------------------------------------------------

# Exportar la Tabla Master integrada (columna vertebral del proyecto)
write_csv(tabla_master, here("outputs/04_TABLA_MASTER_PROYECTO.csv"))

# Exportar el resumen de métricas por grupo para vuestras tablas del informe
write_csv(metricas_anuales_grupo, here("outputs/04_resumen_funcional_anual.csv"))

# Combinamos y guardamos (cada una por separado para vuestra memoria)
ggsave(here("figs/04_correlacion_TOTAL.png"), g1, width = 12, height = 8, dpi = 300)
ggsave(here("figs/04_correlacion_SOMERA.png"), g2, width = 12, height = 8, dpi = 300)
ggsave(here("figs/04_correlacion_PROFUNDA.png"), g3, width = 12, height = 8, dpi = 300)

message(">>> SCRIPT 04 COMPLETADO")







