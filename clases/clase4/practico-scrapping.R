#Paquetes a autilizar -----
library(tidyverse)
#install.packages(rvest)
library(rvest)
library(dplyr)
library(lubridate)
library(janitor)
library(readr)
library(stringr)
#install.packages(httr)
library(httr)
#Texto El Rincón del Vago ------
prueba <- read_html("https://html.rincondelvago.com/100-preguntas-sobre-el-nuevo-desorden_carlos-taibo.html")
titular <- prueba %>% html_element(".entry-title") %>% html_text2()
titular2 <- prueba %>% html_element(".entry-title") %>% html_text2() %>% make_clean_names(sep_out="-")
cuerpo <- prueba %>% html_element(".contenido-pagina")
texto_base <- cuerpo %>% html_elements("p") %>% html_text2()

setwd("/Users/betto")
write_lines(texto_base, paste0("rincon_","-", titular2, ".txt"))

#Tabla Wikipedia Peliculas LEGO -----
html <- read_html("https://en.wikipedia.org/w/index.php?title=The_Lego_Movie&oldid=998422565")
tabla <- html %>% html_element(".tracklist") %>% html_table()

#Fórmula 1 -----
f1 <- read_html("https://www.espn.cl/deporte-motor/f1/posiciones")
f1_stats <- f1 %>% html_elements("section")
f1_stats_0 <- f1_stats %>% html_elements("table") %>% html_table()

setwd("/Users/betto")
write.csv(f1_stats_0, paste0("estadisticas_f1", ".csv"))

#Discurso Cuenta Pública -----
url <- "https://prensa.presidencia.cl/discurso.aspx?id=278977"

pagina <- httr::GET(
  url,
  httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"),
  config(ssl_verifypeer = 0L)
)

html_cp <- read_html(pagina)

texto_discurso <- html_cp %>% html_elements("#main_ltContenido") %>% html_text2()

fecha <- html_cp %>% html_element("#main_ltFEcha") %>% html_text2() %>% str_replace_all(" ", "-"); fecha

fecha2 <- html_cp %>% html_element("#main_ltFEcha") %>% html_text2() %>% dmy(locale = "ES_es") #error.

titulo <- html_cp %>% html_element("#main_ltTitulo") %>% html_text2()

titulo2 <- html_cp %>% html_element("#main_ltTitulo") %>% html_text2() %>% make_clean_names(sep_out = "-")


paste0("boric_", fecha, "-", titulo2, ".txt")

setwd("/Users/betto")

write_lines(texto_discurso, paste0("cl_boric", fecha, titulo))

