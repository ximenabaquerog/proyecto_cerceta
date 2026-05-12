# ==============================================================================
# PROPÓSITO: Análisis de tendencias poblacionales de aves acuáticas por grupos funcionales
# PROYECTO: Respuesta de las aves acuáticas a la dinámica de inundación (Proyecto Cerceta)
# ARCHIVOS DE ENTRADA:
#   - outputs/01_censos_invernales_limpios.csv (Censos depurados)
#   - data/functional_groups_unbalanced.csv (Clasificación de gremios funcionales)
# ARCHIVOS DE SALIDA:
#   - outputs/02_datos_aves_final.csv (Tabla para correlación en Script 04)
#   - outputs/02_test_tendencias_aves.csv (Resultados del test Mann-Kendall)
#   - outputs/02_grafico_tendencia_aves.png (Visualización de tendencias)
# ==============================================================================

# Cargar paquetes necesarios:
library(readr)
library(tidyverse)
library(Kendall)
library(here)

# Importar abundancias por año, especie y localidad para meses invernales
species_abundance <- read_csv("outputs/01_censos_invernales_limpios.csv")

# Importar clasificación en grupos funcionales y añadir a las abundancias
grupos_funcionales <- read_csv(here("data/functional_groups_unbalanced.csv"))

abundancia_grupos_fun <- species_abundance|> 
  inner_join(grupos_funcionales |> select(-notes), by = c("Especie" = "species"))

# Calcular la abundancia máxima por especie, parcela y año.
maximos_parcela <- abundancia_grupos_fun |> 
  group_by(Especie, `Localidad nivel 4`, hydro_year, fun_group) |> 
  summarize(locality_max = max(Cantidad), .groups = "drop") |> 
  arrange(hydro_year)

# Calcular la abundancia como máximos invernales para cada especie, entre todas las parcelas
maximos_invernales_sp <- maximos_parcela |> 
  group_by(hydro_year, Especie, fun_group) |> 
  summarize(winter_max = max(locality_max, na.rm = TRUE), .groups = "drop") 

# Estandarizar la abundancia entre 0-1, para dar el mismo peso a las especies comunes
# y las raras.
aves_std <- maximos_invernales_sp |> 
  group_by(Especie) |> 
  mutate(winter_max_std = winter_max / max(winter_max))

# Calcular la abundancia anual de cada grupo funcional como la mediana de las abundancias
# de cada especie que lo integra.
tendencia_grupos <- aves_std |> 
  group_by(hydro_year, fun_group) |> 
  summarize(mediana = median(winter_max_std, na.rm = TRUE), .groups = "drop")

# Análisis Mann-Kendall:
analisis_mk <- tendencia_grupos |>
  arrange(hydro_year) |>
  group_by(fun_group) |>
  summarize(mk = list(MannKendall(mediana))) |>
  mutate(
    tau     = map_dbl(mk, "tau"),
    p_valor = round(map_dbl(mk, "sl"), digits = 5)
  ) |>
  select(-mk) |>
  mutate(estado_tendencia = case_when(
    p_valor < 0.05 & tau > 0 ~ "Incremento significativo",
    p_valor < 0.05 & tau < 0 ~ "Descenso significativo",
    TRUE ~ "Sin tendencia clara"
  ))
# Dar formato al p-value y tau de cada grupo funcional como una etiqueta que añadir
# al gráfico
analisis_mk <- analisis_mk |>
  mutate(label = paste0("τ = ", round(tau, 2), "\np = ", ifelse(p_valor < 0.001, "< 0.001", round(p_valor, 3)))
  )

# Gráfico sencillo con suavizado lineal, descartamos GAM que era más confuso de interpretar.
grafico_tendencia_aves <- ggplot(tendencia_grupos, aes(x = hydro_year, y = mediana, colour = fun_group)) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_line(linewidth = 0.8) +
  geom_smooth(data = filter(tendencia_grupos, fun_group %in% filter(analisis_mk, p_valor >= 0.05)$fun_group),
              method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7, colour = "grey40") +
  geom_smooth(data = filter(tendencia_grupos, fun_group %in% filter(analisis_mk, p_valor < 0.05)$fun_group),
              method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7, colour = "red3") +
  geom_text(data = filter(analisis_mk, p_valor >= 0.05),
            aes(label = label),
            colour = "black", fontface = "plain",
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3.5) +
  geom_text(data = filter(analisis_mk, p_valor < 0.05),
            aes(label = label),
            colour = "red3", fontface = "bold",
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3.5) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~ fun_group, axes = "all_x", labeller = as_labeller(c(
    "duck_dabb"  = "Patos no buceadores",
    "duck_dive"  = "Patos buceadores",
    "grazer"     = "Ánsar común",
    "wader_deep" = "Limícolas grandes",
    "wader_mid"  = "Limícolas medianos",
    "wader_shall"= "Limícolas pequeños"
  ))) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", size = 10.5),
    legend.position  = "none",
    axis.title       = element_text(face = "bold")
  ) +
  labs(x = "Año hidrológico",
       y = "Mediana de la abundancia")

# Exportar figura 
ggsave(here("figs/02_grafico_tendencia_aves.png"), 
       plot = grafico_tendencia_aves, width = 12, height = 8, dpi = 300)
