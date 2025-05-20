## ========================================
## CARGA DE LIBRERÍAS
## ========================================

library(tidyverse)   # Manipulación y visualización de datos
library(ggplot2)    # Sistema de gráficos basado en capas
library(scales)     # Para formatos como porcentaje
library(forcats)    # Para manejo de factores (si es necesario)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)

## ========================================
##  Cargar base de datos
## ========================================

bbdd <- read_excel("clases/clase3/practico/bbdd/bbdd.xlsx")


bbdd |> glimpse()
world |> glimpse()


## ========================================
bbdd <-bbdd |> rename(name = country)

bbdd2 <- bbdd |> full_join(world, by = "name") |> glimpse()

bbdd3 <- bbdd2 |> 
  group_by(name, geometry) |> 
  summarise(sum = sum(fempresvictory, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(sum = replace_na(sum, 0))

ggplot(data = bbdd3) +
  geom_sf(aes(
    geometry = geometry,
    fill = sum))

ggplot(data = bbdd3) +
  geom_sf(aes(
    geometry = geometry,
    fill = sum))

# Si sum es NA que tome el valor 0 en tidyverse


bbdd4 <- bbdd2 |> 
  group_by(name, geometry) |> 
  summarise(sum = sum(fempresvictory, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(sum = replace_na(sum, 0),
         sum = factor(sum))

ggplot(data = bbdd4) +
  geom_sf(aes(geometry = geometry, fill = sum)) +
  scale_fill_manual(
    values = c(
      "0" = "gray80",      # gris para 0
      "1" = "#a6cee3",     # azul claro para 1
      "2" = "#1f78b4"      # azul más oscuro para 2
    ),
    name = "Presidentas electas"
  ) +
  theme_minimal()


bbdd4 |> filter(name != "Antarctica") |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = sum)) +
  scale_fill_manual(
    values = c(
      "0" = "gray80",      # gris para 0
      "1" = "#a6cee3",     # azul claro para 1
      "2" = "#1f78b4"      # azul más oscuro para 2
    )) +
  labs(
    title = "Presidentas electas en el mundo",
    subtitle = "A lo largo de la democracia",
    caption = "Fuente: bbdd",
    fill = "Presidentas electas"
  ) +
  theme_minimal() 

getwd()

ggsave("clases/clase3/practico/images/presidentas_mapa.png",
       plot = last_plot(),, 
       width = 29.21, 
       height = 12.09, 
       units = "cm", dpi = 300)
1






