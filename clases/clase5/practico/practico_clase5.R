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
url <- "https://github.com/centrociir/interculturales/raw/main/clases/clase5/practico/bbdd/cuentapublica_25.docx"

# Crear un archivo temporal con extensión .docx
temp_file <- tempfile(fileext = ".docx")

# Descargar el archivo desde la URL al archivo temporal (modo binario para archivos Word)
download.file(url, temp_file, mode = "wb")

# Leer el documento Word y extraer los párrafos
texto_df <- read_docx(temp_file) %>%          # Leer el archivo Word
  docx_summary() %>%                          # Extraer contenido estructurado (tipo, texto, estilo, etc.)
  filter(content_type == "paragraph") %>%     # Filtrar solo los elementos tipo párrafo
  transmute(parrafo = text) %>%               # Crear nueva columna 'parrafo' con el texto
  rowid_to_column("id")                       # Agregar columna 'id' con el número de párrafo

# Unir todos los párrafos en una sola cadena de texto
texto_completo <- paste(texto_df$parrafo, collapse = " ")

#===============================================================================
# 2. Análisis de contexto: palabra "estallido" ---------------------------------
#===============================================================================

# Extraer contextos de +/- 10 palabras alrededor de la palabra "estallido"
patron <- "(?:\\S+\\s+){0,10}estallido(?:\\s+\\S+){0,10}"  # Expresión regular para capturar hasta 10 palabras antes y después de "estallido"

# Aplicar la expresión regular al texto completo (ignorando mayúsculas/minúsculas)
contextos <- str_extract_all(texto_completo, regex(patron, ignore_case = TRUE))[[1]]

# Crear un tibble con cada contexto y contar cuántas palabras tiene cada uno
contexto_df <- tibble(
  contexto = contextos,                                 # Texto del fragmento extraído
  palabras = str_count(contextos, "\\S+")               # Contar número de palabras en cada contexto
)

# Imprimir el resultado
print(contexto_df)


#===============================================================================
# 3. Tokenización --------------------------------------------------------------
#===============================================================================

# Tokenizar por palabra (convierte cada palabra en una fila)
tokens <- texto_df %>%
  unnest_tokens(palabra, parrafo)  # Extrae palabras desde los párrafos

# Remover stopwords en español (palabras vacías como "el", "la", "de", etc.)
stopwords_es <- stopwords("es")
tokens_limpios <- tokens %>%
  filter(!palabra %in% stopwords_es)  # Filtra las palabras que no están en la lista de stopwords

# Calcular la frecuencia de palabras limpias
frecuencias_palabra <- tokens_limpios %>%
  count(palabra, sort = TRUE)  # Cuenta y ordena las palabras por frecuencia descendente

# Mostrar tabla de frecuencias
print(frecuencias_palabra)

# Seleccionar las 20 palabras más frecuentes
top_palabras <- frecuencias_palabra %>%
  slice_max(n, n = 20)  # Escoge las 20 con mayor frecuencia

# Graficar con ggplot2
ggplot(top_palabras, aes(x = reorder(palabra, n), y = n)) +
  geom_col(fill = "steelblue") +      
  coord_flip() +                      
  labs(
    title = "Top 20 palabras más frecuentes",  
    x = "Palabra",                             
    y = "Frecuencia"                           
  ) +
  theme_minimal()  # Tema visual limpio y claro



#install.packages("wordcloud")

library(wordcloud)

#. Ejecuta el wordcloud
wordcloud(
  words = frecuencias_palabra$palabra,
  freq = frecuencias_palabra$n,
  min.freq = 2,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

# . Cierra el dispositivo gráfico
#dev.off()


# Guardar último gráfico generado
ggsave(
  filename = "image/wordcloud.png",  # nombre y extensión incluidos
  plot = last_plot(),
  width = 14,
  height = 8,
  dpi = 300
)


#===============================================================================
# 4. Bigrama: creación, limpieza y conteo --------------------------------------
#===============================================================================

# Crear bigramas desde los párrafos (pares de palabras consecutivas)
bigrams <- texto_df %>%
  unnest_tokens(bigrama, parrafo, token = "ngrams", n = 2)  # Extrae n-gramas de 2 palabras (bigramas)

bigrams  # Muestra los bigramas generados

# Separar cada bigrama en dos columnas: palabra1 y palabra2
bigrams_separados <- bigrams %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ")  # Separa el bigrama en dos palabras

bigrams_separados  # Muestra los bigramas separados

# Filtrar bigramas que no contengan stopwords ni números
bigrams_limpios <- bigrams_separados %>%
  filter(
    !palabra1 %in% stopwords_es,                      # Excluye si la primera palabra es una stopword
    !palabra2 %in% stopwords_es,                      # Excluye si la segunda palabra es una stopword
    !str_detect(palabra1, "^[0-9]+$"),                # Excluye si palabra1 es un número
    !str_detect(palabra2, "^[0-9]+$")                 # Excluye si palabra2 es un número
  ) %>%
  unite("bigrama", palabra1, palabra2, sep = " ")     # Une las palabras filtradas nuevamente como bigrama

bigrams_limpios  # Muestra los bigramas limpios

# Contar bigramas más frecuentes
frecuencias_bigrama <- bigrams_limpios %>%
  count(bigrama, sort = TRUE)  # Cuenta y ordena los bigramas por frecuencia descendente

frecuencias_palabra  

#===============================================================================
# 5- Quanteda --------
#===============================================================================

# Cargar librerías necesarias
library(quanteda)
# install.packages("quanteda.textplots")
library(quanteda.textplots)
# install.packages("quanteda.textstats")  # si se requieren estadísticas

# Crear un corpus a partir del texto completo (asignando un nombre al documento)
corpus_cuenta <- corpus(texto_completo, docnames = "cuenta_25")

# Tokenizar el corpus
tokens_cuenta <- tokens(corpus_cuenta, 
                        remove_punct = TRUE,         # Eliminar puntuación
                        remove_numbers = TRUE) %>%   # Eliminar números
  tokens_tolower() %>%                              # Convertir a minúsculas
  tokens_remove(pattern = stopwords("spanish"))     # Eliminar stopwords en español

tokens_cuenta  # Mostrar tokens procesados

# Crear una matriz documento-término (DFM: document-feature matrix)
dfm_cuenta <- dfm(tokens_cuenta)

dfm_cuenta  # Mostrar DFM

# Ver las 20 palabras más frecuentes en el documento
topfeatures(dfm_cuenta, 20)

# Crear una nube de palabras con las 100 más frecuentes
textplot_wordcloud(
  dfm_cuenta,
  max_words = 100,
  color = RColorBrewer::brewer.pal(8, "Dark2")  # Paleta de colores
)

# Obtener las 10 palabras más frecuentes y construir un data.frame para graficar
top_terms <- topfeatures(dfm_cuenta, 10); top_terms
top_df <- data.frame(
  word = names(top_terms),               # Nombre de las palabras
  freq = as.numeric(top_terms)           # Frecuencia de aparición
); top_df

# Cargar librerías para treemap
# install.packages("treemapify")  # Ejecutar si no está instalada
library(treemapify)
library(ggplot2)

# Crear un treemap con las palabras más frecuentes
ggplot(top_df, aes(area = freq, fill = freq, label = word)) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "center", grow = TRUE, reflow = TRUE) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Treemap de las 50 palabras más frecuentes en cuenta pública") +
  theme_minimal()


#===============================================================================
# 6. Quanteda Avanzado ---------------------------------------------------------
#===============================================================================

# Cargar librería para importar texto como data frame
library(readtext)

# Importar archivos .txt desde la carpeta indicada y crear data frame “presi_df”
presi_df <- readtext("bbdd/discursos/*.txt",
                     encoding = "UTF-8")  # Asegura correcta codificación

# Explorar estructura del data frame importado
class(presi_df)     # Clase del objeto
names(presi_df)     # Nombres de las columnas
glimpse(presi_df)   # Vista general del contenido

# Crear corpus “presi_c” a partir del data frame “presi_df”
# Este corpus servirá como base para el análisis de texto
presi_c <- corpus(presi_df)

# Resumen básico del corpus (tokens, tipos, etc.)
summary(presi_c)

# Convertir resumen del corpus a tibble para manipulación posterior
resumen <- as_tibble(summary(presi_c))
resumen  # Muestra resumen del corpus en formato tibble

# Separar la columna 'Text' del resumen en Año, Presidente y Formato
metadata <- resumen %>%
  separate(col = Text,
           into = c("Año", "Presidente", "Formato"))  # Asume nombres separados por "_"

# Agregar metadata (docvars) al corpus con la información del resumen
docvars(presi_c, "Presidente") <- metadata$Presidente
docvars(presi_c, "Año")        <- metadata$Año
docvars(presi_c, "Formato")    <- metadata$Formato

# Ver resumen actualizado del corpus con metadata incluida
summary(presi_c)

# Graficar cantidad de tokens por texto
resumen <- arrange(resumen, Tokens)  # Ordenar por número de tokens
dotchart(resumen$Tokens, resumen$Text)  # Gráfico de puntos

# Convertir columnas "Año" y "Tokens" a variables numéricas
metadata <- metadata %>%
  mutate(
    Año = as.numeric(Año),
    Tokens = as.numeric(Tokens)
  )


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
  filename = "image/trends.png",  # nombre y extensión incluidos
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


tokens_presi <- tokens(presi_c); tokens_presi

# Luego aplica kwic sobre los tokens KWIC (Key Works in context)
desigual <- kwic(tokens_presi, pattern = "desigual*", window = 20)

# Ver resultado
print(desigual)
desigual <- as.data.frame(desigual)
#View(desigual)
class(desigual)
write_excel_csv2 (desigual, "bbdd/desigual.csv")

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


#===============================================================================
# Visualización tipo "rayos X" de menciones a términos clave en el corpus
#===============================================================================

# Visualizar menciones a "desigual*" (como desigualdad, desiguales, etc.)
textplot_xray(kwic(tokens_presi, "desigual*"))

# Visualizar menciones a "brecha*" (como brechas, brechando, etc.)
textplot_xray(kwic(tokens_presi, "brecha*"))

# También puedes explorar otras palabras clave descomentando las siguientes líneas:
# textplot_xray(kwic(tokens_presi, "educaci*"))      # educación, educativo...
# textplot_xray(kwic(tokens_presi, "constituci*"))   # constitución, constitucional...
# textplot_xray(kwic(tokens_presi, "salud*"))        # salud, salud pública...
# textplot_xray(kwic(tokens_presi, "migra*"))        # migración, migrante...

# Comparar varias palabras clave en un solo gráfico
textplot_xray(
  kwic(tokens_presi, "brecha*"),     # menciones a "brecha"
  kwic(tokens_presi, "ineq*"),       # menciones a "inequidad", "inequidades"...
  kwic(tokens_presi, "desigual*")    # menciones a "desigualdad", "desiguales"...
) +
  aes(color = keyword) +  # Asignar color por palabra clave
  scale_color_manual(values = c("red", "blue", "black"))  # Colores personalizados

# Otra comparación: "trabajo" y "empresa"
textplot_xray(
  kwic(tokens_presi, "trabaj*"),     # trabajo, trabajadores...
  kwic(tokens_presi, "empres*")      # empresa, empresarios...
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
# 7.1  Análisis de sentimiento ------------------------------------------------------
#===============================================================================

library(tidytext)
library(stringr)

# Definir raíces de palabras asociadas al sentimiento de alegría (no palabras literales)
alegria <- c("feli", "entusias", "emocio", "alegre",
             "satis", "optimis", "maravill")

# Tokenización del texto en palabras, removiendo stopwords
tokens <- presi_df %>%
  unnest_tokens(word, text) %>%                             # Separar texto en palabras
  filter(!word %in% stopwords::stopwords("es"))             # Eliminar stopwords en español

# Detectar si cada palabra contiene alguna raíz asociada a la alegría
tokens_sentimiento <- tokens %>%
  mutate(sentimiento = case_when(
    str_detect(word, str_c(alegria, collapse = "|")) ~ "alegría",  # Si la palabra contiene alguna raíz
    TRUE ~ NA_character_                                            # Si no, dejar como NA
  )) %>%
  filter(!is.na(sentimiento))  # Mantener solo las palabras que expresan "alegría"

tokens_sentimiento  # Mostrar palabras clasificadas como alegría

# Contar cuántas palabras de alegría aparecen por discurso (según `doc_id`)
conteo_alegria <- tokens_sentimiento %>%
  filter(sentimiento == "alegría") %>%
  count(doc_id, name = "cantidad_alegria")  # Conteo por documento

# Graficar cantidad de palabras asociadas a alegría por discurso
ggplot(conteo_alegria, aes(x = reorder(doc_id, cantidad_alegria), y = cantidad_alegria)) +
  geom_col(fill = "steelblue") +                          
  coord_flip() +                                           
  labs(title = "Cantidad de palabras de alegría por discurso",  # Título del gráfico
       x = "Discurso (doc_id)",                                # Eje X
       y = "Cantidad de palabras de alegría") +                # Eje Y
  theme_minimal()                                             # Tema visual limpio


#===============================================================================
# 7.2  Análisis de sentimiento --------------------------------------------------
#================================================================================

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
