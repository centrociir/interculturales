## ========================================
## CARGA DE LIBRERÍAS
## ========================================

rm(list = ls())  # Limpia el entorno de R (borra todos los objetos existentes)

# Carga de paquetes
library(tidyverse)        # Manipulación y visualización de datos
library(ggplot2)          # Sistema de gráficos basado en capas (también parte de tidyverse)
library(scales)           # Formatos para gráficos (porcentaje, coma decimal, etc.)
library(forcats)          # Manipulación avanzada de factores
library(sf)               # Manejo de datos espaciales (Simple Features)
library(rnaturalearth)    # Descarga geometrías mundiales
library(rnaturalearthdata)# Base de datos auxiliar de mapas
library(readxl)           # Lectura de archivos Excel

## ========================================
## CARGA DE BASE DE DATOS
## ========================================

bbdd <- read_excel("clases/clase3/practico/bbdd/bbdd.xlsx")  # Base con datos de presidentas
world <- ne_countries(scale = "medium", returnclass = "sf")   # Mapa mundial con geometría en formato sf

bbdd |> glimpse()
world |> glimpse()

## ========================================
## LIMPIEZA Y UNIÓN DE DATOS
## ========================================

bbdd <- bbdd |> rename(name = country)  # Asegura coincidencia en nombres de país

# Se realiza la unión entre datos de presidentas y geometría mundial
bbdd2 <- bbdd |> full_join(world, by = "name") |> glimpse()

# Nota: puede haber países duplicados por diferencias en nombres

## ========================================
## ANÁLISIS INICIAL: SUMA DE PRESIDENTAS
## ========================================

# Agrupa por país y calcula total de presidentas mujeres electas
bbdd3 <- bbdd2 |> 
  group_by(name, geometry) |> 
  summarise(sum = sum(fempresvictory, na.rm = TRUE)) |> 
  ungroup() 

# Mapa básico con número total de presidentas
ggplot(data = bbdd3) +
  geom_sf(aes(geometry = geometry, fill = sum))

## ========================================
## MAPA CON PALETA MANUAL Y ETIQUETAS
## ========================================

bbdd4 <- bbdd2 |> 
  group_by(name, geometry) |> 
  summarise(sum = sum(fempresvictory, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(sum = factor(sum))

ggplot(data = bbdd4) +
  geom_sf(aes(geometry = geometry, fill = sum)) +
  scale_fill_manual(
    values = c("0" = "gray80", "1" = "#a6cee3", "2" = "#1f78b4"),
    name = "Presidentas electas"
  ) +
  theme_minimal()

## ========================================
## MAPA SIN ANTÁRTICA + ETIQUETAS
## ========================================

bbdd4 |> filter(name != "Antarctica") |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = sum)) +
  scale_fill_manual(
    values = c("0" = "gray80", "1" = "#a6cee3", "2" = "#1f78b4")
  ) +
  labs(
    title = "Presidentas electas en el mundo",
    subtitle = "A lo largo de la democracia",
    caption = "Fuente: bbdd",
    fill = "Presidentas electas"
  ) +
  theme_minimal()

# Guarda imagen del gráfico
ggsave("clases/clase3/practico/images/presidentas_mapa.png",
       plot = last_plot(), width = 29.21, height = 12.09, units = "cm", dpi = 300)

## ========================================
## CHILEMAPAS: CARGA Y UNIÓN
## ========================================

library(chilemapas)

mapa <- chilemapas::mapa_comunas |> 
  left_join(
    chilemapas::codigos_territoriales |> select(matches("comuna")), 
    by = "codigo_comuna"
  )

## ========================================
## CARGA DE DATOS PAES
## ========================================

library(readr)

data <- read_delim(
  "https://raw.githubusercontent.com/centrociir/interculturales/refs/heads/main/clases/clase3/practico/bbdd/data.csv",
  delim = ";"
)

data_paes <- data |> 
  select(CODIGO_REGION, CODIGO_COMUNA, MATE1_REG_ACTUAL, PTJE_NEM, CLEC_REG_ACTUAL) |> 
  glimpse()

# Resumen estadístico de puntajes
summary(data_paes)

## ========================================
## CÁLCULO DE PROMEDIOS Y UNIÓN CON MAPA
## ========================================

tabla <- data_paes |> 
  group_by(CODIGO_COMUNA) |> 
  summarise(
    promedio_nem = round(mean(PTJE_NEM, na.rm = TRUE), 0),
    promedio_mate = round(mean(MATE1_REG_ACTUAL, na.rm = TRUE), 0),
    promedio_lenguaje = round(mean(CLEC_REG_ACTUAL, na.rm = TRUE), 0)
  ) |> 
  mutate(promedio_ambas = round((promedio_lenguaje + promedio_mate) / 2, 0)) |> 
  drop_na(CODIGO_COMUNA) |> 
  rename(codigo_comuna = CODIGO_COMUNA)

# Homogeniza largo de códigos para que coincidan
tabla <- tabla |> mutate(
  codigo_comuna = as.character(codigo_comuna),
  codigo_comuna = if_else(
    nchar(codigo_comuna) == 4,
    str_pad(codigo_comuna, width = 5, pad = "0"),
    codigo_comuna
  )
)

mapa <- mapa |> mutate(codigo_comuna = as.character(codigo_comuna))

data_consolidada <- mapa |> full_join(tabla, by = "codigo_comuna")

## ========================================
## MAPA DE PUNTAJES PAES Comuna + Nacional
## ========================================

colors <- colorRampPalette(c("#FF0000", "#00679E"))(10)

g1 <- data_consolidada |> 
  filter(nombre_comuna != "Isla de Pascua", nombre_comuna != "Juan Fernandez") |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = promedio_ambas), col = "white") +
  scale_fill_gradientn(
    colours = colors
  ) +
  theme_classic()

g1

## ========================================
## MAPA DE PUNTAJES POR COMUNA
## ========================================

data_consolidada |> 
  filter(codigo_region == 13) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = promedio_ambas), col = "white") +
  scale_fill_gradientn(
    colours = colors
  ) +
  theme_classic()


data_consolidada |>   filter(codigo_region == 13) |> select(nombre_comuna)


data_rm <-data_consolidada |> filter( nombre_comuna != "Paine",
                            nombre_comuna != "Buin",
                            nombre_comuna != "Alhue",
                             nombre_comuna != "Tiltil",
                             nombre_comuna != "Lampa",
                              nombre_comuna != "Colina",
                             nombre_comuna != "Maria Pinto",
                             nombre_comuna != "Curacavi",
                             nombre_comuna != "Isla de Maipo",
                             nombre_comuna != "Melipilla",
                             nombre_comuna != "Pirque",
                          #   nombre_comuna != "Penaflor",
                            nombre_comuna != "Lo Barnechea",
                       #      nombre_comuna != "Calera de Tango",
                      #       nombre_comuna != "San Bernardo",
                             nombre_comuna != "El Monte",
                            nombre_comuna != "San Pedro",
                         #   nombre_comuna != "Talagante",
                            nombre_comuna != "San Jose de Maipo"
                            ) |>  filter(codigo_region == 13) 




data_rm |> ggplot() +
  geom_sf(aes(geometry = geometry, fill = promedio_ambas), col = "white") +
  scale_fill_gradientn(
    colours = colors
  ) +
  theme_classic()

## ========================================
## PUEBLOS INDÍGENAS
## ========================================

data_indi <- read_delim(
  "https://raw.githubusercontent.com/centrociir/interculturales/refs/heads/main/clases/clase3/practico/bbdd/pueblos_indigenas_chile.csv",
  delim = ";"
)

mapa_regiones <- mapa_comunas |> 
  group_by(codigo_region) |> 
  summarize(geometry = st_union(geometry)) # resumir los datos agrupados uniéndolos


## Homogeniza código comuna
#data_indi <- data_indi |> mutate(
#  codigo_comuna = as.character(codigo_comuna),
#  codigo_comuna = if_else(
#    nchar(codigo_comuna) == 4,
#    str_pad(codigo_comuna, width = 5, pad = "0"),
#    codigo_comuna
#  )
#)


# Homogeniza código comuna
data_indi <- data_indi |> mutate(
  codigo_region = as.character(codigo_region)
)

## Suma población indígena por comuna
#tabla_indi <- data_indi |> 
#  group_by(codigo_comuna, pueblo, poblacion_total) |> 
#  summarise(sum = sum(n, na.rm = TRUE), .groups = "drop")

# Suma población indígena por comuna
tabla_indi <- data_indi |> 
  group_by(codigo_region, pueblo, poblacion_total) |> 
  summarise(sum = sum(n, na.rm = TRUE), .groups = "drop")

#indigenas_por_comuna <- tabla_indi |> 
#  group_by(codigo_comuna, poblacion_total) |> 
#  summarise(total_indigenas = sum(sum, na.rm = TRUE), .groups = "drop")

# Agrupar por código de región
indigenas_por_region <- indigenas_por_region |> 
  group_by(codigo_region) |>  # Agrupa todos los registros por región
  
  # Sumar población total e indígena en cada región
  summarise(
    poblacion_total = sum(poblacion_total, na.rm = TRUE),      # Suma la población total por región
    total_indigenas = sum(total_indigenas, na.rm = TRUE),      # Suma el total de indígenas por región
    .groups = "drop"                                            # Elimina la estructura de agrupamiento después del resumen
  ) |> 
  
  # Calcular proporción de indígenas
  mutate(
    proporcion_indigena = round((total_indigenas / poblacion_total) * 100, 2)  # Calcula el porcentaje de población indígena
  )



# Unión con geometría
mapa_indi <- indigenas_por_region |> inner_join(mapa_regiones, by = "codigo_region") |> 
  mutate(proporcion = round((total_indigenas / poblacion_total) * 100, 2))


indigenas_por_region 

colors <- colorRampPalette(c("grey", "blue"))(2)

# Mapa final: proporción indígena
mapa_indi |> 
  filter(proporcion > 0) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = proporcion), col = "white") +
  scale_fill_gradientn(
    colours = colors,
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(fill = "Proporción de indígenas (%)") +
  theme_void() +
  coord_sf(datum = TRUE)


# De nuevo....



indigenas_por_region <- indigenas_por_region |> 
  mutate(
    codigo_region = str_pad(codigo_region, width = 2, pad = "0")  # Rellena con 0 a la izquierda si es necesario
  )


# Unión con geometría
mapa_indi <- indigenas_por_region |> inner_join(mapa_regiones, by = "codigo_region") |> 
  mutate(proporcion = round((total_indigenas / poblacion_total) * 100, 2))

colors <- colorRampPalette(c("#F3FAFD", "#2087AC"))(10)

# Mapa final: proporción indígena
mapa_indi |> 
  filter(proporcion > 0) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = proporcion), col = "white") +
  scale_fill_gradientn(
    colours = colors,
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(fill = "Proporción de indígenas (%)") +
  theme_classic() 





