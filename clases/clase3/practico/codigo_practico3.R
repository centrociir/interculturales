## ========================================
## CARGA DE LIBRERÍAS
## ========================================
rm(list = ls()) # Limpia el entorno

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
world <- ne_countries(scale = "medium", returnclass = "sf")

bbdd |> glimpse()
world |> glimpse()


## ========================================
bbdd <-bbdd |> rename(name = country)

bbdd2 <- bbdd |> full_join(world, by = "name") |> glimpse() #
#bbdd2 <- bbdd |> inner_join(world, by = "name") |> glimpse() #

paises <- 324 + 242 #La variable bbdd tiene 510, pues había repetidos

# Hagamos una suma de presidentas mujeres

bbdd3 <- bbdd2 |> 
  group_by(name, geometry) |> 
  summarise(sum = sum(fempresvictory, na.rm = TRUE)) |> 
  ungroup() 

ggplot(data = bbdd3) +
  geom_sf(aes(
    geometry = geometry,
    fill = sum))

bbdd4 <- bbdd2 |> 
  group_by(name, geometry) |> 
  summarise(sum = sum(fempresvictory, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(sum = factor(sum))

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

#
# Filtrar por espacios geográfcos que alteran el mapa
# y embellecerlo
#

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

# ¿Dudas hasta acá?

# ========================================
# ChileMapas
# ========================================

# Cargar la base de datos de Chile
library(chilemapas)

chilemapas::mapa_zonas
chilemapas::mapa_comunas
chilemapas::generar_provincias()
chilemapas::generar_regiones()

mapa <- chilemapas::mapa_comunas |> 
  left_join(
    chilemapas::codigos_territoriales |> 
      select(matches("comuna")), 
    by = "codigo_comuna") 

mapa

# Cargar la base de datos de PAES:

library(readr)

data <- read_delim(
  "https://raw.githubusercontent.com/centrociir/interculturales/refs/heads/main/clases/clase3/practico/bbdd/data.csv",
  delim = ";"
)

data |> head(4)

data_paes <-data |> select(CODIGO_REGION, CODIGO_COMUNA, MATE1_REG_ACTUAL, PTJE_NEM, CLEC_REG_ACTUAL) |> glimpse()

summary(data_paes)

tabla <- data_paes |> 
  group_by(CODIGO_COMUNA) |> 
  summarise(promedio_nem = round(mean(PTJE_NEM, na.rm = TRUE), 0),
            promedio_mate = round(mean(MATE1_REG_ACTUAL, na.rm = TRUE), 0),
            promedio_lenguaje = round(mean(CLEC_REG_ACTUAL, na.rm = TRUE), 0)) |> 
  mutate(promedio_ambas = round((promedio_lenguaje + promedio_mate) / 2, 0))|> 
  drop_na(CODIGO_COMUNA)  |> 
  rename(codigo_comuna = CODIGO_COMUNA)

# Tabla: N 327. Hay comunas que no tienen PAES.


mapa |> left_join(tabla, by = "codigo_comuna") 

class(mapa$codigo_comuna)
class(tabla$codigo_comuna)

library(dplyr)

# Asegurar que ambas columnas son del mismo tipo (character)
mapa <- mapa |> mutate(codigo_comuna = as.character(codigo_comuna))
tabla <- tabla |> mutate(codigo_comuna = as.character(codigo_comuna))

data_consolidada <-mapa |> full_join(tabla, by = "codigo_comuna") 

# https://i.pinimg.com/1200x/53/54/ed/5354ed7b70c79c47dfeeee5a9fadba39.jpg


# Para `mapa`
mapa |> 
  mutate(largo = nchar(codigo_comuna)) |> 
  count(largo)

# Para `tabla`
tabla |> 
  mutate(largo = nchar(codigo_comuna)) |> 
  count(largo)



library(dplyr)
library(stringr)

tabla <- tabla |> 
  mutate(
    codigo_comuna = as.character(codigo_comuna),
    codigo_comuna = if_else(
      nchar(codigo_comuna) == 4,
      str_pad(codigo_comuna, width = 5, pad = "0"),
      codigo_comuna
    )
  )


data_consolidada <-mapa |> full_join(tabla, by = "codigo_comuna") 



data_consolidada |> 
  #filter(codigo_region == 13) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = promedio_ambas),
          col = "white")  +
  #facet_wrap(~codigo_region) +
  theme_void() +
  labs(fill = "Puntajes PAES") +
  coord_sf(datum = TRUE)  # Fija la escala de todas las facetas



colors <- colorRampPalette(c("#FF0000", "#00679E"))(3)  # Gradiente de 5 colores

# Gráfico
# Crear el gráfico con ajustes
g1 <- data_consolidada |> 
  filter(nombre_comuna != "Isla de Pascua",
         nombre_comuna != "Juan Fernandez") |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = promedio_ambas),
          col = "white") +
  scale_fill_gradientn(
    colours = colors,  # Gradiente personalizado
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  theme_classic()
g1


g1 <- data_consolidada |> 
  filter(codigo_region == 13) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = promedio_ambas),
          col = "white") +
  scale_fill_gradientn(
    colours = colors,  # Gradiente personalizado
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) 
g1


# vector con geocódigos que deseamor remover


g1 <- data_consolidada |> 
  filter(codigo_region == 13) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = promedio_ambas),
          col = "white") +
  scale_fill_gradientn(
    colours = colors,  # Gradiente personalizado
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) 
g1

# Ejercicio 3: Pueblos Indígenas.

data_indi <- read_delim(
  "https://raw.githubusercontent.com/centrociir/interculturales/refs/heads/main/clases/clase3/practico/bbdd/pueblos_indigenas_chile.csv",
  delim = ";"
)

data_indi  |> mutate(largo = nchar(codigo_comuna)) |> 
  count(largo)


data_indi <-data_indi |>   mutate(
  codigo_comuna = as.character(codigo_comuna),
  codigo_comuna = if_else(
    nchar(codigo_comuna) == 4,
    str_pad(codigo_comuna, width = 5, pad = "0"),
    codigo_comuna
  )
)

data_indi  |> mutate(largo = nchar(codigo_comuna)) |> 
  count(largo)


tabla_indi <-data_indi |> group_by(codigo_comuna, pueblo, poblacion_total)  |> 
  summarise(sum = sum(n, na.rm = TRUE)) 

indigenas_por_comuna <- tabla_indi |> 
  group_by(codigo_comuna, poblacion_total) |> 
  summarise(total_indigenas = sum(sum, na.rm = TRUE), .groups = "drop")


p <-indigenas_por_comuna |> inner_join(mapa, by = "codigo_comuna") 

p <- p |> 
  mutate(proporcion = round((total_indigenas / poblacion_total) * 100, 2))


colors <- colorRampPalette(c("brown", "#00679E"))(3)  # Gradiente de 5 colores


p |> 
  filter(proporcion > 0) |> 
  #filter(codigo_region == "09") |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = proporcion),
          col = "white") +
  scale_fill_gradientn(
    colours = colors,  # Gradiente personalizado
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(fill = "Proporción de indígenas (%)") +
  theme_void() +
  coord_sf(datum = TRUE)



#library(spdep)
#
#data_consolidada |> glimpse()
#
## Crear vecinos espaciales
#vecinos <- poly2nb(data_consolidada)
#
## Crear matriz de pesos
#pesos <- nb2listw(vecinos, style = "W")
#
## Calcular índice de Moran global para la variable promedio_ambas
#moran.test(data_consolidada$promedio_ambas, pesos)
#
#
#
#library(sf)
#library(spdep)
#
## Asegúrate que `data_consolidada` es un objeto sf
#data_consolidada <- data_consolidada |> drop_na ()
#
#data_consolidada <-st_as_sf(data_consolidada) 
#
## Crear vecinos espaciales
#vecinos <- poly2nb(data_consolidada, snap = 1e-6)
#pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
#moran.test(data_consolidada$promedio_ambas, pesos, zero.policy = TRUE)
#
#
#local_moran <- localmoran(data_consolidada$promedio_ambas, pesos)
#
## Puedes mapear los valores de local_moran[,1] (Ii) o p-values
#data_consolidada$lisa <- local_moran[,1]
#
#ggplot(data_consolidada) +
#  geom_sf(aes(fill = lisa), color = "white") +
#  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
#  theme_void() +
#  labs(fill = "Moran Local")
#
