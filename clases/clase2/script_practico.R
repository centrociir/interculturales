## ========================================
## CARGA DE LIBRERÍAS
## ========================================

library(tidyverse)  # Manipulación y visualización de datos
library(ggplot2)    # Sistema de gráficos basado en capas
library(scales)     # Para formatos como porcentaje
library(forcats)    # Para manejo de factores (si es necesario)

## ========================================
## DESCARGA Y CARGA DE DATOS CASEN
## ========================================

url <- "https://raw.githubusercontent.com/centrociir/interculturales/refs/heads/main/clases/clase1/bbdd/casen2022_sample.csv"
destfile <- "casen2022_sample.csv"
download.file(url, destfile, mode = "wb")  # Descarga
casen <- read.csv(destfile, encoding = "UTF-8")  # Lectura

## ========================================
## EXPLORACIÓN BÁSICA
## ========================================

head(casen)  # Ver los primeros registros
as_tibble(casen) 
names(casen)

# Histograma de edad
ggplot(casen, aes(edad)) +
  geom_histogram(color = "white")



# Densidad por edad y pertenencia indígena

casen$pueblos_indigenas2 <- factor(casen$pueblos_indigenas, 
                                  levels = c(0, 1), 
                                  labels = c("No Indígena", "Indígena"))

casen |> ggplot(aes(edad, colour = pueblos_indigenas2)) +
  geom_density()

## ========================================
## DENSIDAD DE INGRESO AUTÓNOMO POR PERTENENCIA INDÍGENA
## ========================================

casen |> 
  mutate(pueblos_indigenas = factor(pueblos_indigenas,
                                    levels = c(0, 1), 
                                    labels = c("No Indígena", "Indígena"))) |>
  filter(yautcor < 1500000) |>  # Eliminar outliers
  ggplot(aes(x = yautcor, fill = pueblos_indigenas, color = pueblos_indigenas)) +
  geom_density(alpha = 0.6) +
  #facet_grid(~ sexo) +  # Facet por región
  labs(title = "Distribución del ingreso autónomo según pertenencia indígena",
       x = "Ingreso autónomo (yautcor)", y = "Densidad") +
  theme_classic()

## ========================================
## GRÁFICOS DE DISPERSIÓN ENTRE EDAD E INGRESO
## ========================================

# Dispersión básica

casen |> ggplot(aes(x = yautcor, y = edad)) +
  geom_point()

# Dispersión con filtro y color por etnicidad
casen |> 
  filter(yautcor < 1000000) |>
  ggplot(aes(x = yautcor, y = edad, colour = as.factor(pueblos_indigenas))) +
  geom_point(alpha = 0.3) +
  labs(title = "Ingreso autónomo por edad (sin extremos)",
       x = "Edad", y = "Ingreso autónomo", colour = "Pueblos indígenas") +
  theme_minimal()

# Con línea de tendencia
casen |> 
  filter(yautcor < 1000000) |>
  ggplot(aes(x = yautcor, y = edad)) +
  geom_smooth() +
  labs(title = "Edad según ingreso autónomo (sin extremos)",
       x = "Ingreso autónomo", y = "Edad") +
  theme_minimal()

## ========================================
## GRÁFICO DE BARRAS DE PUEBLOS INDÍGENAS
## ========================================

casen |> ggplot(aes(x = factor(pueblos_indigenas))) +
  geom_bar() +
  labs(title = "Distribución por pertenencia indígena",
       x = "Pertenencia", y = "Cantidad") +
  scale_x_discrete(labels = c("0" = "No Indígena", "1" = "Indígena")) +
  theme_minimal()

## ========================================
## CATEGORIZACIÓN DE NIVEL EDUCATIVO
## ========================================

casen$educ

casen <- casen |> 
  mutate(
    educ_nivel = case_when(
      educ %in% 0:1   ~ "Sin educación",
      educ %in% 2:4   ~ "Básica",
      educ %in% 5:6   ~ "Media",
      educ %in% 7:8   ~ "Técnico nivel superior",
      educ %in% 9:11  ~ "Profesional",
      educ == 12      ~ "Postgrado",
      TRUE            ~ NA_character_
    )
  )

casen |> select(educ, educ_nivel)

## ========================================
## TABLA DE EDUCACIÓN POR PUEBLO INDÍGENA
## ========================================

tabla_educ <- casen |>
  group_by(pueblos_indigenas, educ_nivel) |>
  count()

tabla_educ

## ========================================
## VISUALIZACIÓN: EDUCACIÓN POR PERTENENCIA INDÍGENA
## ========================================

# Gráfico de barras con barras lado a lado
tabla_educ |> ggplot(aes(x = educ_nivel, y = n, fill = factor(pueblos_indigenas))) +
  #geom_col(position = "dodge") +
  #geom_col(position = "fill") +
  geom_col(position = "dodge") +
  labs(title = "Nivel educativo por pertenencia indígena",
       x = "Nivel educativo", y = "Cantidad de personas") +
  scale_fill_manual(values = c("0" = "#999999", "1" = "#D95F02"),
                    labels = c("No Indígena", "Indígena")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tabla_educ

# Añadir proporciones
tabla_educ_prop <- tabla_educ |>
  group_by(pueblos_indigenas) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

tabla_educ_prop

ggplot(tabla_educ_prop, aes(x = educ_nivel, y = prop, fill = educ_nivel)) +
  geom_col() +
  geom_text(aes(label = percent(prop, accuracy = 1)), vjust = -0.5) +
  facet_grid(~ pueblos_indigenas, labeller = as_labeller(c("0" = "No Indígena", "1" = "Indígena"))) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribución educativa dentro de cada grupo",
       x = "Nivel educativo", 
       y = "Proporción dentro del grupo",
       caption = "casen") +
  theme_minimal()

## ========================================
## GSS: TENDENCIA DE IDENTIFICACIÓN POLÍTICA POR RAZA
## ========================================

gss_cat


gss_party_race <- gss_cat |> 
  filter(!is.na(partyid), !is.na(race)) |> 
  group_by(year, race, partyid) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(year, race) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()

voto_duro <- gss_party_race |> filter(partyid == "Strong democrat")

voto_duro

ggplot(voto_duro, aes(x = year, y = prop, color = race)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Identificación como 'Strong Democrat' por raza",
       x = "Año", y = "Proporción", color = "Raza") +
  theme_minimal()


g1 <- ggplot(voto_duro, aes(x = year, y = prop, color = race)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Voto duro demócrata en EE.UU.",
    subtitle = "Proporción que se identifica como 'Strong Democrat', por raza y año",
    caption = "Fuente: GSS (General Social Survey)",
    x = "Año",
    y = "Proporción",
    color = "Raza"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.caption = element_text(color = "gray40", size = 9),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

casen
g1
## ========================================
## GUARDAR GRÁFICO EN ARCHIVO
## ========================================

getwd()
#setwd()

ggsave("clases/clase2/images/voto_duro_race_no.png",
       plot = g1,  # último gráfico generado
       width = 8, height = 5, dpi = 300)


ggsave("clases/clase2/images/grafico_final_ppt.png",
       plot = g1,           # o el nombre del gráfico que generaste
       width = 29.21,       # en centímetros
       height = 12.09,
       units = "cm",
       dpi = 300)           # resolución alta para impresión o presentación



# Extras---


casen |>
  filter(yautcor < 500000) |>
  ggplot(aes(x = factor(pueblos_indigenas), y = yautcor, fill = factor(pueblos_indigenas))) +
  geom_boxplot(alpha = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución del ingreso autónomo por pertenencia indígena",
    x = "Pertenencia a pueblo indígena",
    y = "Ingreso autónomo",
    fill = "Pertenencia"
  ) +
  theme_minimal()



casen |>
  filter(yautcor < 500000) |>
  ggplot(aes(x = factor(pueblos_indigenas), y = yautcor, fill = factor(pueblos_indigenas))) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  labs(
    title = "Distribución de ingresos (forma y densidad)",
    x = "Pertenencia indígena",
    y = "Ingreso autónomo"
  ) +
  scale_fill_manual(values = c("0" = "#999999", "1" = "#D95F02")) +
  theme_minimal()


casen |>
  filter(yautcor < 300000) |>
  ggplot(aes(x = yautcor, fill = factor(pueblos_indigenas))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Distribución del ingreso por región y pertenencia indígena",
    x = "Ingreso autónomo",
    y = "Frecuencia"
  ) +
  theme_minimal()


tabla_educ_por_grupo |>
  ggplot(aes(x = pueblos_indigenas, y = prop, fill = educ_nivel)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.2
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Composición educativa dentro de cada grupo",
    x = "Pertenencia indígena",
    y = "Proporción",
    fill = "Nivel educativo"
  ) +
  theme_minimal()



tabla_educ_por_grupo |>
  ggplot(aes(x = pueblos_indigenas, y = prop, fill = educ_nivel)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.2
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Composición educativa dentro de cada grupo",
    x = "Pertenencia indígena",
    y = "Proporción",
    fill = "Nivel educativo"
  ) +
  theme_minimal()


