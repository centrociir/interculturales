library(haven)

casen2022_sample <- casen2022 %>%
  sample_n(2000) |> select(region, nse, sexo, ecivil, educ, pueblos_indigenas,
                          pobreza, yautcor)

getwd()
write_rds(casen2022_sample, "clases/clase1/bbdd/casen2022_sample.rds")
write_rds(casen2022_sample, "clases/clase1/pres/data/casen2022_sample.rds")

