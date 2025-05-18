
# Numéricos
1
2
3.1

# Caracter o texto
"hola"
hola

# Lógicos
TRUE
FALSE


2 + 2        # suma
50 * 100     # multiplicación
4556 - 1000  # resta
6565 / 89    # división


1 == 1       # igualdad
1 == 2
40 != 30     # desigualdad
40 != 40
31 > 18      # mayor que
40 < 80      # menor que
40 >= 40     # mayor o igual
50 >= 40
4 <= 5       # menor o igual


# Crear objeto
año <- 1993
año

# Operaciones con objetos
año + 10
2024 - año

# Crear nuevo objeto a partir de un cálculo
edad <- 2024 - año
edad



url <- "https://raw.githubusercontent.com/centrociir/interculturales/refs/heads/main/clases/clase1/bbdd/casen2022_sample.csv"
destfile <- "casen2022_sample.csv"

# Descargar el archivo
download.file(url, destfile, mode = "wb")

# Leer el archivo CSV
casen <- read.csv(destfile, encoding = "UTF-8")

# Ver los primeros datos
head(casen)

#install.packages(tidyverse) En caso de no estar instalado
library(tidyverse)
library(dplyr)


casen |> glimpse() 

summary(casen) #Función de R base


casen <- casen |>  #Renombramos el objeto
  dplyr::mutate(
    educ_nivel = dplyr::case_when(
      educ %in% 0:1   ~ "Sin educación",
      educ %in% 2:4   ~ "Básica",
      educ %in% 5:6   ~ "Media",
      educ %in% 7:8   ~ "Técnico nivel superior",
      educ %in% 9:11  ~ "Profesional",
      educ == 12      ~ "Postgrado",
      TRUE            ~ NA_character_
    ),
    educ_nivel = factor(educ_nivel, levels = c(
      "Sin educación", "Básica", "Media", 
      "Técnico nivel superior", "Profesional", "Postgrado"
    ), ordered = TRUE)
  )

casen |> select(educ, educ_nivel) |> head(5)


quantile(casen$yautcor, na.rm = TRUE)

quantile(casen$yautcor, probs = seq(0, 1, 0.1), na.rm = TRUE)

casen <- casen |> 
  mutate(
    decil_yautcor = cut(
      yautcor,
      breaks = quantile(yautcor, probs = seq(0, 1, 0.1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste0("Decil ", 1:10)
    )
  )


casen <- casen %>%
  mutate(decil_yautcor2 = case_when(
    yautcor <= 120000 ~ "decil1",
    yautcor <= 200000 ~ "decil2",
    yautcor <= 300000 ~ "decil3",
    yautcor <= 400000 ~ "decil4",
    yautcor <= 439167 ~ "decil5",
    yautcor <= 500000 ~ "decil6",
    yautcor <= 600000 ~ "decil7",
    yautcor <= 804666.4 ~ "decil8",
    yautcor <= 1205499.8 ~ "decil9",
    yautcor <= 11518333 ~ "decil10",
    TRUE ~ NA_character_
  ))

casen |> select(yautcor, decil_yautcor, decil_yautcor2) |> 
  head(5)

casen |> group_by(pueblos_indigenas) |> 
  summarise(salario = mean(yautcor, na.rm = TRUE)) #Borrar valores perdidos.

casen |> group_by(pueblos_indigenas, sexo) |>  # Veámoslo con sexo. 
  summarise(salario_mean = mean(yautcor, na.rm = TRUE), 
            salaria_median = median(yautcor, na.rm = TRUE)) # Agregamos otro estadístico


casen |> group_by(pueblos_indigenas, sexo) |>  # Veámoslo con sexo. 
  summarise(salario_mean = mean(yautcor, na.rm = TRUE), 
            salario_median = median(yautcor, na.rm = TRUE)) |> # Agregamos otro estadístico
  mutate(ratio = salario_mean/salario_median) #un ratio alto puede sugerir que dentro del grupo hay mucha dispersión o desigualdad


library(DT) #Tabla dinámica

casen_tabla <- casen |> 
  filter(!is.na(educ_nivel)) |>
  count(pueblos_indigenas, educ_nivel, name = "n") |> 
  group_by(pueblos_indigenas) |>
  mutate(
    total = sum(n),
    porcentaje = round(100 * n / total, 1)
  ) |>
  arrange(pueblos_indigenas, educ_nivel)

DT::datatable(casen_tabla)


casen_tabla2 <- casen |> 
  filter(!is.na(educ_nivel)) |>
  count(pueblos_indigenas, educ_nivel, name = "n") |> 
  complete(pueblos_indigenas, educ_nivel, fill = list(n = 0)) |> # Completar el postgrado
  group_by(pueblos_indigenas) |>
  mutate(
    total = sum(n),
    porcentaje = round(100 * n / total, 1)
  ) |>
  arrange(pueblos_indigenas, educ_nivel)

DT::datatable(casen_tabla2)


# Seleccionamos solo las variables necesarias para el análisis:
# - pueblos_indigenas: para identificar si una persona se reconoce indígena
# - sexo: para distinguir entre hombres y mujeres
# - yautcor: ingreso autónomo corregido, nuestra variable de interés numérica
casen_tabla3 <- casen |>
  select(pueblos_indigenas, sexo, yautcor) |> 
  
  # Reemplazamos los valores codificados por etiquetas legibles.
  # 0 será "No indígena" y 1 será "Indígena"
  mutate(
    pueblos_indigenas = case_when(
      pueblos_indigenas == 0 ~ "No indígena",
      pueblos_indigenas == 1 ~ "Indígena",
      TRUE ~ NA_character_ # Por si hubiera algún valor extraño o NA
    ),
    
    # Hacemos lo mismo con la variable sexo:
    # 1 será "Hombre", 2 será "Mujer"
    sexo = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE ~ NA_character_
    ),
    
    # Creamos una nueva variable que combine ambos grupos:
    # Por ejemplo: "Indígena - Mujer", "No indígena - Hombre", etc.
    colapsa = paste(pueblos_indigenas, sexo, sep = " - ")
  ) |>
  
  # Agrupamos por esta nueva variable combinada (colapsa)
  group_by(colapsa) |>
  
  # Calculamos los dos estadísticos principales:
  # - El ingreso promedio (mean)
  # - El ingreso mediano (median)
  # Ignoramos los NA con `na.rm = TRUE`
  summarise(
    salario_mean = mean(yautcor, na.rm = TRUE),
    salaria_median = median(yautcor, na.rm = TRUE),
    .groups = "drop" # Para evitar que el resultado quede agrupado
  )



DT::datatable(casen_tabla3)


# Cargamos las librerías necesarias
library(scales)
library(writexl)

# Usamos mutate() para formatear las columnas numéricas:
# 'comma()' agrega separadores de miles y elimina notación científica
# 'accuracy = 1' indica que redondearemos al número entero más cercano
casen_tabla_excel <- casen_tabla3 |>
  mutate(
    salario_mean = comma(salario_mean, accuracy = 1),      # Ej: 1.234.567
    salaria_median = comma(salaria_median, accuracy = 1)   # Ej: 1.100.000
  )

# Finalmente, guardamos la tabla en un archivo Excel
# Este se guarda dentro de la carpeta 'bbdd' con el nombre 'casen_tabla.xlsx'
write_xlsx(casen_tabla_excel, "bbdd/casen_tabla.xlsx")

