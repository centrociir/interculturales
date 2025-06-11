# ======================================================
# CARGA DE PAQUETES
# ======================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr, dplyr, quanteda, quanteda.textstats, quanteda.textplots,
  tidytext, topicmodels, broom, ggplot2, stringr, tidyverse
)

# ======================================================
# ANÁLISIS DE CLUSTER CON QUANTEDA (5 NOTICIAS)
# ======================================================

# Leer archivo con 5 noticias
cinco_noticias <- read_csv("bbdd/cinco_noticias.csv")

# Convertir a objeto corpus
corpus_noticias <- corpus(cinco_noticias, text_field = "texto_noticia")
summary(corpus_noticias)

# Tokenización
tokens_noticias <- tokens(
  corpus_noticias,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE
)

# Crear matriz documento-palabra (DFM)
matriz_noticias <- dfm(tokens_noticias) |> 
  dfm_remove(stopwords("es"))

matriz_noticias

# Calcular distancias y aplicar clustering jerárquico
distancias_noticias <- as.dist(textstat_dist(matriz_noticias))
cluster <- hclust(distancias_noticias)

# Visualizar el dendrograma
plot(cluster)

# Calcular similitud entre documentos
textstat_simil(matriz_noticias, matriz_noticias["text1",], method = "cosine", margin = "documents")


#text1 tiene una similitud de 1.0000 consigo mismo (por definición).
# text3 es el documento más similar a text1 con un valor de 0.3440.
# text4 es el menos similar con 0.0469.
# En general, estos valores indican baja similitud léxica entre text1 y los demás textos
# ======================================================
# MODELADO DE TÓPICOS CON LDA (1000 NOTICIAS)
# ======================================================

# Leer archivo con 1000 noticias
mil_noticias <- read_csv("bbdd/mil-noticias.csv") 
#mil_noticias <- read_csv("bbdd/letras_jose.csv") |> rename(texto_noticia = Letra)

# Cargar stopwords personalizadas
lista_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt") |> 
  bind_rows(tibble(palabra = c("dijo", "dice", "año", "años", "país", "ser", "tiene")))

# Agregar identificador de documento
mil_noticias <- mil_noticias |> 
  mutate(documento = row_number(), .before = texto_noticia)

# Tokenización y limpieza
frecuencias_noticias <- mil_noticias |> 
  unnest_tokens(input = texto_noticia, output = palabra, strip_numeric = TRUE) |> 
  anti_join(lista_stopwords, by = "palabra") |> 
  count(documento, palabra, sort = TRUE) |> 
  arrange(documento)

# Convertir a DFM
dfm_noticias <- cast_dfm(frecuencias_noticias, documento, palabra, n)

# Modelado LDA (5 tópicos)
noticias_lda <- LDA(dfm_noticias, k = 5, control = list(seed = 1234))

# ======================================================
# ANÁLISIS DE PALABRAS (BETA)
# ======================================================

noticias_temas <- tidy(noticias_lda, matrix = "beta")

# Visualizar palabras más relevantes por tópico
noticias_temas %>% 
  slice_max(beta, n = 10, by = topic) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(y = term, x = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

# ======================================================
# ASIGNACIÓN DE TÓPICOS A DOCUMENTOS (GAMMA)
# ======================================================

temas_documentos <- tidy(noticias_lda, matrix = "gamma")

# Seleccionar tópico con mayor probabilidad por documento
noticias_con_topicos <- temas_documentos %>% 
  arrange(document, -gamma) |> 
  slice_max(gamma, by = document) |> 
  select(documento = document, topico_probable = topic) |> 
  mutate(documento = as.integer(documento)) |> 
  right_join(mil_noticias, by = "documento") |> 
  arrange(documento)

# Etiquetado descriptivo de tópicos (¡cuidado con el orden si se cambian parámetros del modelo!)
noticias_etiquetadas <- mutate(noticias_con_topicos, topico_probable = case_when(
  topico_probable == 1 ~ "cambio climático y medio ambiente",
  topico_probable == 2 ~ "educación",
  topico_probable == 3 ~ "tecnología",
  topico_probable == 4 ~ "covid/salud",
  topico_probable == 5 ~ "economía"
))

# ======================================================
# FIN DEL SCRIPT
# ======================================================
