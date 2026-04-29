# ==============================================================================
# PROPÓSITO: Análisis de tendencias poblacionales de aves acuáticas por gremios
# PROYECTO: Respuesta de las aves acuáticas a la dinámica de inundación (Proyecto Cerceta)
# ARCHIVOS DE ENTRADA:
#   - outputs/01_censos_invernales_limpios.csv (Censos depurados)
#   - data/functional_groups_unbalanced.csv (Clasificación de gremios funcionales)
# ARCHIVOS DE SALIDA:
#   - outputs/02_datos_aves_final.csv (Tabla para correlación en Script 04)
#   - outputs/02_test_tendencias_aves.csv (Resultados del test Mann-Kendall)
#   - outputs/02_grafico_tendencia_aves.png (Visualización de tendencias)
# ==============================================================================

# 1. Cargar librerías ----------------------------------------------------------
library(tidyverse)
library(here)
library(mgcv)      # Para suavizado GAM en gráficas
library(Kendall)   # Para test de Mann-Kendall

# 2. Carga e Integración de Datos ----------------------------------------------
# Cargamos los censos limpios del Script 01
aves_raw <- read_csv(here("outputs/01_censos_invernales_limpios.csv"))

# Cargamos el diccionario de grupos funcionales
grupos_funcionales <- read_csv(here("data/functional_groups_unbalanced.csv"))

# Unimos ambas tablas para asignar a cada registro de conteo su grupo funcional
# Usamos inner_join para asegurar que solo analizamos especies con grupo asignado
aves_clean <- aves_raw |> 
  inner_join(grupos_funcionales, by = c("Especie" = "species"))

# 3. Cálculo del Máximo Invernal Anual -----------------------------------------
# Calculamos el máximo por especie y año para evitar sesgos por esfuerzo de muestreo
aves_anual <- aves_clean |> 
  group_by(hydro_year, Especie, fun_group) |> 
  summarize(winter_max = max(Cantidad, na.rm = TRUE), .groups = "drop")

# 4. Estandarización de Abundancias (0-1) --------------------------------------
# Escalamos las abundancias para que todas las especies pesen igual en el análisis
# de gremios, independientemente de si son muy masivas o escasas.
aves_std <- aves_anual |> 
  group_by(Especie) |> 
  mutate(winter_max_std = winter_max / max(winter_max, na.rm = TRUE)) |> 
  ungroup()

# 5. Tendencia por Gremios Funcionales -----------------------------------------
# Calculamos la mediana anual de abundancia estandarizada para cada gremio
tendencia_gremios <- aves_std |> 
  group_by(hydro_year, fun_group) |> 
  summarize(mediana_gremio = median(winter_max_std, na.rm = TRUE), .groups = "drop")

# 6. Análisis de Tendencia: Test de Mann-Kendall --------------------------------
# Verificamos la significación estadística de la evolución de cada grupo
analisis_mk <- tendencia_gremios |> 
  group_by(fun_group) |> 
  summarize(
    tau = MannKendall(mediana_gremio)$tau,
    p_valor = MannKendall(mediana_gremio)$sl
  ) |> 
  mutate(estado_tendencia = case_when(
    p_valor < 0.05 & tau > 0 ~ "Incremento significativo",
    p_valor < 0.05 & tau < 0 ~ "Descenso significativo",
    TRUE ~ "Sin tendencia clara"
  ))

# 7. Visualización de Tendencias (GAM) -----------------------------------------
# Generamos una gráfica facetada por grupo funcional
grafico_tendencia <- ggplot() +
  geom_point(data = aves_std, aes(x = hydro_year, y = winter_max_std, color = fun_group), 
             alpha = 0.2, size = 1) +
  geom_smooth(data = tendencia_gremios, aes(x = hydro_year, y = mediana_gremio, color = fun_group), 
              method = "gam", formula = y ~ s(x, k = 5), linewidth = 1.2) +
  facet_wrap(~fun_group, scales = "free_y") +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold")) +
  labs(title = "Tendencia de Aves Acuáticas por Gremio Funcional (2005-2025)",
       subtitle = "Puntos: especies individuales estandarizadas | Línea: tendencia del gremio (GAM)",
       x = "Año Hidrológico", 
       y = "Abundancia Máxima Estandarizada (0-1)")

print(grafico_tendencia)

# 8. Exportación de Resultados -------------------------------------------------

# Tabla preparada para el Script 04 (mantenemos resolución de parcela)
tabla_abundancia_parcela <- aves_raw |> 
  inner_join(grupos_funcionales, by = c("Especie" = "species")) |> 
  group_by(`parcelas protocolo`, hydro_year, Especie, fun_group) |> 
  summarize(abundancia = max(Cantidad, na.rm = TRUE), .groups = "drop") |> 
  group_by(Especie) |> 
  mutate(abundancia_std = abundancia / max(abundancia, na.rm = TRUE)) |> 
  ungroup() |> 
  rename(parcela = `parcelas protocolo`, año = hydro_year)

# Exportar tabla master de aves
write_csv(tabla_abundancia_parcela, here("outputs/02_datos_aves_final.csv"))

# Exportar resultados estadísticos
write_csv(analisis_mk, here("outputs/02_test_tendencias_aves.csv"))

# Exportar gráfica
ggsave(here("figs/02_grafico_tendencia_aves.png"), 
       plot = grafico_tendencia, width = 12, height = 8, dpi = 300)

message(">>> SCRIPT 02 COMPLETADO")
