library(haven)
library(tidyverse)
library(sjmisc)

casen2022 <- read_dta("clases/clase1/bbdd/casen2022.dta")

casen2022_sample <- casen2022 %>%
  sample_n(10000) |> select(region, 
                           nse, 
                           sexo, 
                           ecivil,
                           educ,
                          pueblos_indigenas,
                          pobreza,
                          p9, 
                          edad, 
                          yautcor, 
                          ind_hacina)


# Paso 1: calcular proporciones por grupo
proporciones <- casen2022 |>
  count(pueblos_indigenas) |>
  mutate(prop = n / sum(n),
         n_muestra = round(prop * 10000))


library(dplyr)


# Paso 2: hacer muestreo estratificado proporcional
casen2022_sample <- casen2022 |> 
  group_by(pueblos_indigenas) |> 
  sample_frac(size = 10000 / nrow(casen2022)) |> 
  ungroup() |> 
  select(region, 
         nse, 
         sexo, 
         ecivil,
         educ,
         pueblos_indigenas,
         pobreza,
         p9, 
         edad, 
         yautcor, 
         ind_hacina)



getwd()
write_rds(casen2022_sample, "clases/clase1/bbdd/casen2022_sample.rds")
write_rds(casen2022_sample, "clases/clase1/pres/data/casen2022_sample.rds")
write_csv(casen2022_sample, "clases/clase1/bbdd/casen2022_sample.csv")
