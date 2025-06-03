#===============================================================================
# 0. Cargar paquetes -----------------------------------------------------------
#===============================================================================

library(officer)      # Leer archivos Word
library(tidyverse)    # Manipulación de datos y visualización
library(tidytext)     # Tokenización y análisis de texto
library(stopwords)    # Stopwords en español



#===============================================================================
# 1. Lectura y preprocesamiento del texto --------------------------------------
#===============================================================================

# Descargar archivo desde GitHub
url <- "https://github.com/centrociir/interculturales/raw/main/clases/clase5/bbdd/cuentapublica_25.docx"
temp_file <- tempfile(fileext = ".docx")
download.file(url, temp_file, mode = "wb")

# Leer el documento y extraer solo los párrafos
texto_df <- read_docx(temp_file) %>%
  docx_summary() %>%
  filter(content_type == "paragraph") %>%
  transmute(parrafo = text) %>%
  rowid_to_column("id")

# Unir todo el texto en una sola cadena
texto_completo <- paste(texto_df$parrafo, collapse = " ")

texto_completo

#===============================================================================
# 2. Análisis de contexto: palabra "estallido" ---------------------------------
#===============================================================================

# Extraer contextos +/- 10 palabras alrededor de "estallido"
patron <- "(?:\\S+\\s+){0,10}estallido(?:\\s+\\S+){0,10}"
contextos <- str_extract_all(texto_completo, regex(patron, ignore_case = TRUE))[[1]]

contextos

# Guardar en tibble y contar palabras por contexto
contexto_df <- tibble(
  contexto = contextos,
  palabras = str_count(contextos, "\\S+")
)

print(contexto_df)

#===============================================================================
# 3. Tokenización --------------------------------------------------------------
#===============================================================================

# Tokenizar por palabra
tokens <- texto_df %>%
  unnest_tokens(palabra, parrafo)

# Remover stopwords en español
stopwords_es <- stopwords("es")
tokens_limpios <- tokens %>%
  filter(!palabra %in% stopwords_es)

# Frecuencia de palabras limpias
frecuencias_palabra <- tokens_limpios %>%
  count(palabra, sort = TRUE)

print(frecuencias_palabra)


# Seleccionar las 20 palabras más frecuentes
top_palabras <- frecuencias_palabra %>%
  slice_max(n, n = 20)

# Graficar con ggplot2
ggplot(top_palabras, aes(x = reorder(palabra, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 palabras más frecuentes",
    x = "Palabra",
    y = "Frecuencia"
  ) +
  theme_minimal()


#install.packages("wordcloud")

library(wordcloud)

# 1. Define nombre del archivo de salida
png("practico/image/wordcloud_desigualdad.png", width = 1200, height = 800, res = 150)

# 2. Ejecuta el wordcloud
wordcloud(
  words = frecuencias_palabra$palabra,
  freq = frecuencias_palabra$n,
  min.freq = 2,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

# 3. Cierra el dispositivo gráfico
dev.off()


# Fijar la ruta donde quieres guardar el gráfico
ruta_salida <- "practico/image"



# Guardar último gráfico generado
ggsave(
  filename = "practico/image/wordcloud.png",  # nombre y extensión incluidos
  plot = last_plot(),
  width = 14,
  height = 8,
  dpi = 300
)




#===============================================================================
# 4. Bigrama: creación, limpieza y conteo --------------------------------------
#===============================================================================

# Crear bigramas desde los párrafos
bigrams <- texto_df %>%
  unnest_tokens(bigrama, parrafo, token = "ngrams", n = 2)

# Separar cada bigrama en palabra1 y palabra2
bigrams_separados <- bigrams %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ")

# Filtrar bigramas que no contengan stopwords ni números
bigrams_limpios <- bigrams_separados %>%
  filter(
    !palabra1 %in% stopwords_es,
    !palabra2 %in% stopwords_es,
    !str_detect(palabra1, "^[0-9]+$"),
    !str_detect(palabra2, "^[0-9]+$")
  ) 


bigrams_limpios <- bigrams_limpios %>%
  mutate(
    bigrama = str_c(coalesce(palabra1, ""), coalesce(palabra2, ""), sep = " ")
  )


# Contar bigramas más frecuentes
frecuencias_bigrama <- bigrams_limpios %>%
  count(bigrama, sort = TRUE) %>%
  slice_max(n, n = 20)  # Muestra los 20 más frecuentes

# Graficar
frecuencias_bigrama %>%
  ggplot(aes(x = n, y = reorder(bigrama, n))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Bigramas más frecuentes",
    x = "Frecuencia",
    y = "Bigrama"
  ) +
  theme_minimal()



#===============================================================================
# 5- Quanteda --------
#===============================================================================

library(quanteda)
#install.packages("quanteda.textplots")
library(quanteda.textplots)
#install.packages("quanteda.textstats")


# Crear corpus a partir del texto completo
corpus_cuenta <- corpus(texto_completo, docnames = "cuenta_25")

# Tokenizar
tokens_cuenta <- tokens(corpus_cuenta, 
                        remove_punct = TRUE, 
                        remove_numbers = TRUE) %>%
  tokens_tolower() %>%  # Convertir a minúsculas
  tokens_remove(pattern = stopwords("spanish"))  # Eliminar stopwords en español

dfm_cuenta <- dfm(tokens_cuenta)

# Ver las palabras más frecuentes
topfeatures(dfm_cuenta, 20)

  
textplot_wordcloud(
  dfm_cuenta,
  max_words = 100,
  color = RColorBrewer::brewer.pal(8, "Dark2")
)


# Obtener las 50 palabras más frecuentes
top_terms <- topfeatures(dfm_cuenta, 10)
top_df <- data.frame(
  word = names(top_terms),
  freq = as.numeric(top_terms)
)

#install.packages("treemapify")  # si no lo tienes
library(treemapify)
library(ggplot2)


ggplot(top_df, aes(area = freq, fill = freq, label = word)) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "center", grow = TRUE, reflow = TRUE) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Treemap de las 50 palabras más frecuentes en cuenta pública") +
  theme_minimal()




#===============================================================================
# 6.  Quantenada avazando ------------------------------------------------------
#===============================================================================


library(readtext) # importar texto como data frame


# Importar texto con readtext y creación de data frame “presidentes” 
presi_df <- readtext ("bbdd/discursos/*.txt",
                      encoding = "UTF-8") 


# Explorar el data frame creado
class(presi_df)
names(presi_df)
glimpse (presi_df)
#presi_df[1,2]

# Crea corpus “presi_c” desde data frame presidentes. El corpus es el texto en 
#el que se basará nuestro análisis.

presi_c <- corpus(presi_df)

summary(presi_c)

#Tokens, conjunto de caracteres de texto. 

resumen <- as_tibble (summary(presi_c))
resumen
#View(resumen)

# Agrega "docvars" como metadata (variables o atributos del texto)
# Ordena información contenida en resumen
metadata <- resumen %>% 
  separate(col = Text, #Sepamos la variable "Text"
           into = c("Año", "Presidente", "Formato"))

# Crea vardocs con información anterior
docvars(presi_c, "Presidente") <- metadata$Presidente
docvars(presi_c, "Año")        <- metadata$Año
docvars(presi_c, "Formato")    <- metadata$Formato

summary(presi_c)
#View(summary(presi_c))

# Grafica
resumen <- arrange(resumen,Tokens)
dotchart(resumen$Tokens, resumen$Text)

# Asegúrate de que Año y Tokens son numéricos
metadata <- metadata %>%
  mutate(Año = as.numeric(Año),
         Tokens = as.numeric(Tokens))

# Gráfico
metadata %>%
  ggplot(aes(x = Año, y = Tokens)) +
  # Áreas por períodos presidenciales
  annotate("rect", xmin = 1990, xmax = 1994, ymin = -Inf, ymax = Inf, fill = "#dba0a0", alpha = 0.3) +  # Aylwin
  annotate("rect", xmin = 1994, xmax = 2000, ymin = -Inf, ymax = Inf, fill = "#bebebe", alpha = 0.3) +  # Frei
  annotate("rect", xmin = 2000, xmax = 2006, ymin = -Inf, ymax = Inf, fill = "#9898eb", alpha = 0.3) +  # Lagos
  annotate("rect", xmin = 2006, xmax = 2010, ymin = -Inf, ymax = Inf, fill = "#a0dbbe", alpha = 0.3) +  # Bachelet 1
  annotate("rect", xmin = 2010, xmax = 2014, ymin = -Inf, ymax = Inf, fill = "#bea0db", alpha = 0.3) +  # Piñera 1
  annotate("rect", xmin = 2014, xmax = 2018, ymin = -Inf, ymax = Inf, fill = "#a0dbbe", alpha = 0.3) +  # Bachelet 2
  annotate("rect", xmin = 2018, xmax = 2022, ymin = -Inf, ymax = Inf, fill = "#bea0db", alpha = 0.3) +  # Piñera 2
  # Línea y puntos
  geom_line(color = "#013e63", size = 1) +
  geom_point(color = "#013e63", size = 2) +
  # Escalas y etiquetas
  scale_x_continuous(breaks = seq(1990, 2022, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "N° de palabras pronunciadas por los gobiernos de la post-dictadura (1990 - 2022)",
    subtitle = "Distribución por año de discurso presidencial",
    x = "Año",
    y = "Tokens",
    caption = "Fuente: Discursos presidenciales (https://prensa.presidencia.cl/)",
    tag = "Figura 1"
  ) +
  # Estilo
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 12, colour = "#444444"),
    plot.subtitle = element_text(size = 10, colour = "#666666"),
    plot.caption = element_text(size = 8, colour = "#888888"),
    axis.title = element_text(size = 9, colour = "#444444"),
    axis.text = element_text(size = 8, colour = "#444444")
  )

# Guardar último gráfico generado
ggsave(
  filename = "practico/image/trends.png",  # nombre y extensión incluidos
  plot = last_plot(),
  width = 14,
  height = 8,
  dpi = 300
)


# Crea Sub Corpus
Aylwin_c <- corpus_subset(presi_c, Presidente == "Aylwin")
#summary (Aylwin_c)

Frei_c <- corpus_subset(presi_c, Presidente == "Frei")
#summary (Frei_c)

Lagos_c <- corpus_subset(presi_c, Presidente == "Lagos")
#summary (Lagos_c)

Bachelet_c <- corpus_subset(presi_c, Presidente == "Bachelet")
#summary (Bachelet_c)

Pinera_c <- corpus_subset(presi_c, Presidente == "Pinera")
summary (Pinera_c)


# P
tokens_presi <- tokens(presi_c)

# Luego aplica kwic sobre los tokens
desigual <- kwic(tokens_presi, pattern = "desigual*", window = 20)

# Ver resultado
print(desigual)
desigual <- as.data.frame(desigual)
#View(desigual)
class(desigual)
write_excel_csv2 (desigual, "clases/clase5/bbdd/discursos/desigual.csv")

# Corpus ya tokenizado
tokens_presi <- tokens(presi_c)

# Búsqueda de términos con comodines
desigual_kwic <- kwic(
  tokens_presi,  # este es 'x'
  pattern = c("desigual*", "ineq*", "brecha*", "estrati*"),
  window = 15
)

# Visualizar resultado
#View(desigual_kwic)



# Contar ocurrencias por año (extraído desde el nombre del archivo)
conteo_kwic <- as.data.frame(desigual_kwic) %>%
  mutate(anio = str_extract(docname, "^\\d{4}")) %>%
  count(anio, name = "ocurrencias")




# Gráfico de líneas
conteo_kwic %>% 
    ggplot(aes(x = as.numeric(anio), y = ocurrencias)) +
  geom_line(color = "#013e63", size = 1) +
  geom_point(color = "#013e63", size = 2) +
  geom_text(aes(label = ocurrencias), vjust = -0.5, size = 3) +
  scale_x_continuous(breaks = seq(min(conteo_kwic$anio), max(conteo_kwic$anio), by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Evolución de menciones a conceptos de desigualdad en Cuentas Públicas",
    subtitle = "Por año de discurso presidencial (1990–2021)",
    caption = "Fuente: Elaboración propia con base en corpus",
    x = "Año",
    y = "Número de menciones"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    axis.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )



textplot_xray(kwic(tokens_presi, "desigual*"))
textplot_xray(kwic(tokens_presi, "brecha*"))
#textplot_xray(kwic(tokens_presi, "educaci*"))
#textplot_xray(kwic(tokens_presi, "constituci*"))
#textplot_xray(kwic(tokens_presi, "salud*"))
#textplot_xray(kwic(tokens_presi, "migra*"))


textplot_xray(
  kwic(tokens_presi, "brecha*"),
  kwic(tokens_presi, "ineq*"),
  kwic(tokens_presi, "desigual*")
) +
  aes(color = keyword) +
  scale_color_manual(values = c("red", "blue", "black"))



textplot_xray(
  kwic(tokens_presi, "trabaj*"),
  kwic(tokens_presi, "empres*")
)

#textplot_xray(
#  kwic(tokens_presi, "todos"),
#  kwic(tokens_presi, "todas")
#)

textplot_xray(
  kwic(tokens_presi, "pobreza"),
  kwic(tokens_presi, "delincuencia")
)


#===============================================================================
# 7.  Análisis de sentimiento ------------------------------------------------------
#===============================================================================

library(tidytext)
library(stringr)

# Palabras asociadas a alegría (como patrones, no literales)
alegria <- c("feli", "entusias", "emocio", "alegre",
             "satis", "optimis", "maravill")

# Tokenización
tokens <- presi_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords::stopwords("es"))

# Detección de alegría usando str_detect con patrones
tokens_sentimiento <- tokens %>%
  mutate(sentimiento = case_when(
    str_detect(word, str_c(alegria, collapse = "|")) ~ "alegría",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(sentimiento))

tokens_sentimiento


# Contar cuántas veces aparece "alegría" por discurso
conteo_alegria <- tokens_sentimiento %>%
  filter(sentimiento == "alegría") %>%
  count(doc_id, name = "cantidad_alegria")

# Graficar
ggplot(conteo_alegria, aes(x = reorder(doc_id, cantidad_alegria), y = cantidad_alegria)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Cantidad de palabras de alegría por discurso",
       x = "Discurso (doc_id)",
       y = "Cantidad de palabras de alegría") +
  theme_minimal()

###

# Asociaciones temáticas con raíces de palabras
conceptos <- list(
  delincuencia = c("delincu", "criminal", "crimen", "robo", "insegur"),
  democracia   = c("democrac", "voto", "eleccion", "sufrag", "participac"),
  pueblos      = c("indigen", "mapuche", "originari", "etnia", "ancestr"),
  economia     = c("econom", "crecim", "desarroll", "emple", "productiv")
)

tokens_concepto <- tokens %>%
  mutate(concepto = case_when(
    str_detect(word, str_c(conceptos$delincuencia, collapse = "|")) ~ "delincuencia",
    str_detect(word, str_c(conceptos$democracia, collapse = "|"))   ~ "democracia",
    str_detect(word, str_c(conceptos$pueblos, collapse = "|"))      ~ "pueblos indígenas",
    str_detect(word, str_c(conceptos$economia, collapse = "|"))     ~ "economía",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(concepto))

conteo_conceptos <- tokens_concepto %>%
  count(doc_id, concepto)


ggplot(conteo_conceptos, aes(x = reorder(doc_id, n), y = n, fill = concepto)) +
  geom_col() +
  coord_flip() +
  labs(title = "Menciones temáticas por discurso",
       x = "Discurso (doc_id)", y = "Cantidad de menciones", fill = "Concepto") +
  theme_minimal()


#===============================================================================
# 8. Redes--------
#===============================================================================

library(igraph)
library(ggraph)

# Tokenizar y limpiar
tokens <- presi_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords("es"))

# Muestreo de 150 tokens
set.seed(123)
tokens_sample <- tokens #%>% sample_n(10050)

# Crear bigramas desde los tokens muestreados
bigramas <- tokens_sample %>%
  mutate(next_word = lead(word)) %>%
  filter(!is.na(next_word)) %>%
  select(palabra1 = word, palabra2 = next_word)

# Contar co-ocurrencias (pares)
coocurrencias <- bigramas %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 30)  # Umbral mínimo para la red

# Crear grafo
g <- graph_from_data_frame(coocurrencias)

# Dibujar red
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.6, color = "gray60") +
  geom_node_point(color = "steelblue", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
  theme_void() +
  labs(title = "Red de co-ocurrencia en muestra de XX tokens de discursos")
