# ==============================================================================
# PROPÓSITO: Análisis de correlación mediante Modelos Lineales Mixtos (GLMM) 
#            entre la abundancia de aves y la dinámica de la lámina de agua.
# PROYECTO: Respuesta de las aves acuáticas a la dinámica de inundación (Proyecto Cerceta)
# ARCHIVOS DE ENTRADA:
#   - outputs/02_abundancias_aves_final.csv (Abundancia por especie/parcela/año)
#   - outputs/03_datos_agua_final.csv (Humedad y extensión por parcela/año)
# ARCHIVOS DE SALIDA:
#   - outputs/04_TABLA_MASTER_PROYECTO.csv (Base de datos final integrada)
#   - outputs/04_resumen_funcional_anual.csv (Medianas de abundancia por gremio)
#   - outputs/04_resumen_modelo_glmm.txt (Resultados estadísticos del modelo)
#   - outputs/04_grafico_correlacion_final.png (Visualización de la respuesta)
# ==============================================================================

# 1. Cargar librerías ----------------------------------------------------------
# tidyverse: integración, manipulación y visualización de datos
# here: gestión de rutas relativas para asegurar la reproducibilidad
# lme4: ejecución de Modelos Lineales Mixtos (GLMM)
# sjPlot: generación de resúmenes de modelos de forma estética
library(tidyverse)
library(here)
library(lme4)
library(sjPlot)

# 2. Carga de datos procesados -------------------------------------------------
# Cargamos los archivos generados en los scripts 02 (Aves) y 03 (Agua)
aves <- read_csv(here("outputs/02_datos_aves_final.csv"))
agua <- read_csv(here("outputs/03_datos_agua_final.csv"))

# 3. Integración de Datos (Creación de la Tabla Master) -----------------------
# Unimos ambas tablas mediante las claves comunes: 'parcela' y 'año'.
# El uso de inner_join garantiza que solo analizamos observaciones donde 
# existen datos tanto de aves como de satélite.
tabla_master_raw <- aves %>%
  inner_join(agua, by = c("parcela", "año"))

# 4. Cálculo de la Mediana por Grupo Funcional ---------------------------------
# Según lo solicitado, calculamos la mediana de las abundancias estandarizadas 
# de todas las especies pertenecientes a un mismo grupo funcional para cada 
# combinación de parcela y año. Esto permite captar la respuesta colectiva 
# del gremio, reduciendo el ruido de especies individuales.
tabla_master <- tabla_master_raw %>%
  group_by(año, parcela, fun_group) %>%
  mutate(
    mediana_abundancia_grupo = median(abundancia_std, na.rm = TRUE)
  ) %>%
  ungroup()

# 5. Generación de Resumen Anual por Gremio ------------------------------------
# Creamos una tabla resumen que muestre la tendencia central anual de cada 
# grupo funcional en todo Doñana, útil para comparar con la serie de inundación.
metricas_anuales_grupo <- tabla_master %>%
  group_by(año, fun_group) %>%
  summarize(
    mediana_abundancia_anual = median(abundancia_std, na.rm = TRUE),
    humedad_media_regional = mean(inundacion_Relativa_Std, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Modelización Estadística: GLMM (Familia Poisson) --------------------------
# Ejecutamos el modelo principal: la abundancia (conteo) depende del estado 
# de inundación relativo de la parcela.
# - Efectos fijos: inundacion_Relativa_Std
# - Efectos aleatorios: (1|parcela) para corregir la autocorrelación espacial 
#   y (1|año) para corregir la variabilidad climática interanual.
modelo_aves_agua <- glmer(
  abundancia ~ inundacion_Relativa_Std + (1 | parcela) + (1 | año),
  data = tabla_master,
  family = poisson(link = "log")
)

# 7. Resumen y Diagnóstico del Modelo -----------------------------------------
resumen_modelo <- summary(modelo_aves_agua)
print(resumen_modelo)

# Exportamos el resumen a texto para la memoria
capture.output(resumen_modelo, file = here("outputs/04_resumen_modelo_glmm.txt"))

# 8. Visualización de la Correlación Final -------------------------------------
# Generamos un gráfico que muestra la relación entre agua y abundancia de aves
# facetado por los 5 grupos funcionales.
grafico_final <- ggplot(tabla_master, aes(x = inundacion_Relativa_Std, y = abundancia_std)) +
  # Nube de puntos de especies individuales (difuminada para ver densidad)
  geom_point(aes(color = fun_group), alpha = 0.15, size = 1) +
  # Línea de regresión que representa la respuesta del grupo funcional
  geom_smooth(aes(color = fun_group), method = "lm", formula = y ~ x, linewidth = 1.2) +
  facet_wrap(~fun_group, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold", size = 10),
        plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Respuesta de la Comunidad de Aves a la Lámina de Agua (2005-2025)",
       subtitle = "Relación entre Humedad Relativa (0-1) y Abundancia Máxima Anual",
       x = "Humedad Relativa de la Parcela (0 = Mín. Histórico, 1 = Máx. Histórico)",
       y = "Abundancia Máxima Estandarizada",
       caption = "Modelo: GLMM Poisson | Datos: Proyecto Cerceta (Doñana)")

# Visualización en consola
print(grafico_final)

# 9. Exportación de Resultados -------------------------------------------------

# Exportar la Tabla Master integrada (columna vertebral del proyecto)
write_csv(tabla_master, here("outputs/04_TABLA_MASTER_PROYECTO.csv"))

# Exportar el resumen de métricas por grupo para vuestras tablas del informe
write_csv(metricas_anuales_grupo, here("outputs/04_resumen_funcional_anual.csv"))

# Exportar el gráfico en alta resolución
ggsave(here("figs/04_grafico_correlacion_final.png"), 
       plot = grafico_final, width = 12, height = 8, dpi = 300)

message(">>> SCRIPT 04 COMPLETADO")