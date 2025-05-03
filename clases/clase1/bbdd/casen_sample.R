library(haven)
library(tidyverse)
library(sjmisc)

casen2022 <- read_dta("clases/clase1/bbdd/casen2022.dta")

casen2022_sample <- casen2022 %>%
  sample_n(2000) |> select(region, 
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
