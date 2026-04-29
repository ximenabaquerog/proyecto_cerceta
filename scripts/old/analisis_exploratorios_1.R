#####################################################################################
#                         ANÁLISIS EXPLORATORIOS INICIALES                              #
#####################################################################################

# Instalar paquetes
install.packages("tidyverse")
install.packages("janitor")
# ...

# Cargar paquetes
library(tidyverse)
library(janitor)

# Datos se descargan de la web como bird_counts.zip.
# Renombro las carpetas: YYYY_whole_year

# 1º prueba, tomar datos anuales de riqueza y abundancia de aves acuáticas en 2025
# y 2005 (por ejemplo, para ver a ojo si hay algo de diferencia).

# Cargar datos 2005, tanso censo aéreo como terrestre:
terrestrial_counts_2005 <- read_csv("data/old/2005_whole_year/terrestrial_counts_data.csv")
aerial_counts_2005 <- read_csv("data/old/2005_whole_year/aerial_counts_data.csv")

terrestrial_counts_2025 <- read_csv("data/old/2025_whole_year/terrestrial_counts_data.csv")
aerial_counts_2025 <- read_csv("data/old/2025_whole_year/aerial_counts_data.csv")

# Estandarizar nombres de variables con janitor
terrestrial_counts_2005 <- clean_names(terrestrial_counts_2005)
terrestrial_counts_2025 <- clean_names(terrestrial_counts_2025)
aerial_counts_2005 <- clean_names(aerial_counts_2005)
aerial_counts_2025 <- clean_names(aerial_counts_2025)

# Ver qué especies aparecen en cada uno de los censos:

# CENSOS TERRESTRES
# 1. Desglosar código del censo en año y mes:

unique(terrestrial_counts_2005$censo) #Para ver que el formato del código de censo
unique(terrestrial_counts_2025$censo) # es siempre igual. Lo es, incluso en los de 2025
unique(aerial_counts_2025$fecha) #Hubo censos en enero, febrero, marzo, noviembre y diciembre
unique(aerial_counts_2005$fecha) #Aquí los hubo en julio, agosto, septiembre, octubre, noviembre y diciembre.

terrestrial_counts_2005 <- terrestrial_counts_2005 |> 
  mutate(
    date = ym(str_remove(censo, "CT_")), # lubridate::ym() convierte a formato ymd, asumiendo que el día es 1.
    year = year(date),
    month = month(date)
    ) |> 
  relocate(date, year, month, .after = censo) |> 
  select(-censo, -date)

terrestrial_counts_2025 <- terrestrial_counts_2025 |> 
  mutate(
    date = ym(str_remove(censo, "CT_")), # lubridate::ym() convierte a formato ymd, asumiendo que el día es 1.
    year = year(date),
    month = month(date)
  ) |> 
  relocate(date, year, month, .after = censo) |> 
  select(-censo, -date)

# Los áereos ya tienen fecha, lo puedo sacar de ahí directamente.

# 2. Filtrar aves no acuáticas:
sp_terr_2005 <- terrestrial_counts_2005 |> 
  filter(month %in% c(11, 12)) |> 
  filter(!str_detect(especie, "Falco|Circus|Circaetus|Buteo|Accipiter|Corvus")) |> 
  group_by(especie) |> 
  summarize(n = sum(cantidad)) |> 
  filter(n > 100)

sp_terr_2025 <- terrestrial_counts_2025|> 
  filter(month %in% c(11, 12)) |> 
  filter(!str_detect(especie, "Falco|Circus|Circaetus|Buteo|Accipiter|Corvus")) |> 
  group_by(especie) |> 
  summarize(n = sum(cantidad)) |> 
  filter(n > 100)

# 3. Juntar tablas para comparar:
ab_05_25_terr <- full_join(sp_terr_2005, sp_terr_2025, by = "especie") |> 
  rename(n_2005 = n.x, n_2025 = n.y)
# Esta tabla filtrarla bien quitando reproductores, y nos centramos en 3 grupos funcionales:
# limícolas, dabbling ducks y diving ducks. 

#--------------------------------------------------------------------------------

# CENSOS AÉREOS
# Como son salteados y no mensuales, vamos a tomar datos de noviembre, diciembre y enero,
# y hacer una media. 

# Filtrar censos validos.
aerial_counts_2005 <- aerial_counts_2005 |> 
  filter (censo_valido == TRUE) # Todos son censos válidos

# 1. Los áereos ya tienen fecha, mes y año puedo sacarlo de ahí directamente.


# 2. Filtrar para noviembre, diciembre o enero, quitar rapaces y calcular media entre los 3 meses.
sp_aero_2005 <- aerial_counts_2005 |> 
  filter(mes %in% c(11, 12, 13)) |> 
  filter(!str_detect(especie, "Falco|Circus|Circaetus|Buteo|Accipiter|Corvus")) |> 
  group_by(especie) |> 
  summarize(mean_abund = round(mean(individuos), 1)) |> 
  filter(mean_abund > 100)

sp_aero_2005 <- aerial_counts_2005 |> 
  filter(mes %in% c(11, 12, 13)) |> 
  filter(!str_detect(especie, "Falco|Circus|Circaetus|Buteo|Accipiter|Corvus")) |> 
  group_by(especie) |> 
  summarize(mean_abund = sum(individuos)/3) |> 
  filter(mean_abund > 100)

sp_aero_2025 <- aerial_counts_2025 |> 
  filter(mes %in% c(11, 12, 13)) |> 
  filter(!str_detect(especie, "Falco|Circus|Circaetus|Buteo|Accipiter|Corvus")) |> 
  group_by(especie) |> 
  summarize(mean_abund = sum(individuos)/3) |> 
  filter(mean_abund > 100)

# 3. Juntar tablas para comparar:
ab_05_25_aero <- full_join(sp_aero_2005, sp_aero_2025, by = "especie") |> 
  rename(mean_abund_2005 = mean_abund.x, mean_abund_2025 = mean_abund.y)




