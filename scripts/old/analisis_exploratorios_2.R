################################################################################
#                       IMPORTAR Y LIMPIAR DATOS                              #
################################################################################

# Cargar paquetes
library(tidyverse)
library(janitor)

# Se puede hacer la selección de especies ANTES de descargar los datos. Aquí voy a 
# utilizar un archivo con todas las especies posibles para hacernos una idea rápida.

raw_2005_2025_aero <- read_csv("data/old/2005_2025_series/aerial_counts_data.csv")

raw_2005_2025_aero <- clean_names(raw_2005_2025_aero)

# CÓDIGO DE CHATGPT
# Primero calcular (por especies) el número total de bichos para cada mes de cada año,
# sumando los datos de todas las localidades en el mismo mes.
winter_monthly_aero <- raw_2005_2025_aero  |> 
  filter(mes %in% c(11, 12, 1))  |> 
  mutate(
    invernada = ifelse(mes %in% c(11, 12), ano + 1, ano) #Invernada se refiere a ano
  )  |> 
  group_by(especie, invernada, mes)  |> 
  summarise(
    abund_mes = sum(individuos, na.rm = TRUE),
    .groups = "drop"
  )

# Ahora agrupamos las 3 filas que pertenecen a la misma especie en cada año (una
# por mes), y calculamos media, desviación y nº de meses que componen la media.
winter_summary_aero <- winter_monthly_aero %>%
  group_by(especie, invernada) %>%
  summarise(
    abund_media = round(mean(abund_mes), 1),
    sd   = sd(abund_mes),
    n_meses       = n_distinct(mes),
    .groups = "drop"
  ) |> 
  arrange(invernada)

# Ahora, filtramos las especies que no llegan a abundancia de 100 en ninguna invernada.
winter_summary_aero_filter <- winter_summary_aero %>%
  group_by(especie) %>%
  filter(max(abund_media, na.rm = TRUE) >= 100) %>%
  ungroup()

# Qué especies nos quedamos en cómputo general con censos aéreos.
lista_aero_full <- sort(unique(winter_summary_aero_filter$especie))
lista_aero_full #37 spp

#--------------------------------------------------------------------------------
# Hacemos lo mismo con censos terrestres para ver la diferencia:
raw_2005_2025_terr <- read_csv("data/old/2005_2025_series/terrestrial_counts_data.csv")

raw_2005_2025_terr <- clean_names(raw_2005_2025_terr)

# Sacamos una columna de mes y año:
raw_2005_2025_terr <- raw_2005_2025_terr |> 
  mutate(
    fecha = ym(str_remove(censo, "CT_")), # lubridate::ym() convierte a formato ymd, asumiendo que el día es 1.
    ano = year(fecha),
    mes = month(fecha)
  ) |> 
  relocate(fecha, ano, mes, .after = censo)

# Primero por meses
winter_monthly_terr <- raw_2005_2025_terr  |> 
  filter(mes %in% c(11, 12, 1))  |> 
  mutate(
    invernada = ifelse(mes %in% c(11, 12), ano + 1, ano)
  )  |> 
  group_by(especie, invernada, mes)  |> 
  summarise(
    abund_mes = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# Ahora agrupamos las 3 filas que pertenecen a la misma especie en cada año (una
# por mes), y calculamos media, desviación y nº de meses que componen la media.
winter_summary_terr <- winter_monthly_terr %>%
  group_by(especie, invernada) %>%
  summarise(
    abund_media = round(mean(abund_mes), 1),
    sd   = sd(abund_mes),
    n_meses       = n_distinct(mes),
    .groups = "drop"
  ) |> 
  arrange(invernada)

# Ahora, filtramos las especies que no llegan a abundancia de 100 en ninguna invernada.
# Además filtro también las que sean sp/sp y solo el género, porque aquí hay datos para cada
# especie individualmente.
winter_summary_terr_filter <- winter_summary_terr %>%
  group_by(especie) %>%
  filter(max(abund_media, na.rm = TRUE) >= 100) %>%
  filter(!str_detect(especie, "spp.|/")) %>%
  ungroup()

# Qué especies nos quedamos en cómputo general con censos aéreos.
lista_terr_full <- sort(unique(winter_summary_terr_filter$especie))
lista_terr_full #70 spp
#---------------------------------------------------------------------------------
# Ahora unimos las listas. Primero pasar a data frame:
df_terr <- data.frame(sp_terr = lista_terr_full, stringsAsFactors = FALSE)

df_aero <- data.frame(sp_aero = unique(lista_aero_full))

# Unirlas conservando todas las columnas de ambas y añadir columna de invernante.
sp_terr_aero <- full_join(
  df_terr,
  df_aero,
  by = c("sp_terr" = "sp_aero"),
  keep = TRUE
) |> 
  mutate(
    n_parejas = NA,
    n_invernantes = NA,
    inver_eur = NA,
    inver_ibe = NA,
  ) # Rellenar a a mano en excel con datos de bibliografía.
# Podemos considerar invernantes europeas las que no tienen poblaciones reproductoras
# en España o no superan las 100 parejas.

#--------------------------------------------------------------------------------
# Sacar valores medios anuales globales de abundancia por especie:
winter_vs_breeding <- winter_summary_terr_filter %>%
  group_by(especie) %>%
  summarise(
    mean_winter_abundance = mean(abund_media, na.rm = TRUE),
    sd_winter_abundance   = sd(abund_media, na.rm = TRUE),
    n_winters             = n(),
    .groups = "drop"
  )

# Comprobar de qué valores salen las medias, que son un poco raras. Parece que no salen
# mal.
winter_nested <- winter_summary_terr_filter %>%
  arrange(invernada) %>%
  group_by(especie) %>%
  summarise(
    yearly_values = list(
      tibble(
        invernada = invernada,
        abund_media = abund_media,
        n_meses = n_meses
      )
    ),
    mean_winter_abundance = mean(abund_media, na.rm = TRUE),
    sd_winter_abundance   = sd(abund_media, na.rm = TRUE),
    n_winters             = n(),
    .groups = "drop"
  )

# Añadir los valores de tamaño medio de invernada al csv de sp_terr_aero:
sp_terr_aero$n_invernantes <- ifelse(
  is.na(sp_terr_aero$sp_terr),
  NA, 
  winter_nested$mean_winter_abundance
  ) 

# Añadir los valores de nº parejas: sp_breed
read_csv()

sp_terr_aero <- write_csv(sp_terr_aero, "outputs/sp_terr_aero.csv")
