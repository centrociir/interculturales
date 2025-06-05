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
patron <- "(?:\\S+\\s+){0,10}pobre(?:\\s+\\S+){0,10}"
contextos <- str_extract_all(texto_completo, regex(patron, ignore_case = TRUE))[[1]]

contextos

# Guardar en tibble y contar palabras por contexto
contexto_df <- tibble(
  contexto = contextos,
  palabras = str_count(contextos, "\\S+")
)

print(contexto_df)


#==

diccionario_pobreza <- c(
  # Términos directos
  "pobre", "pobreza", "carencia", "necesidad", "privación", "indigencia", "miseria",
  
  # Condiciones relacionadas
  "vulnerabilidad", "exclusión", "marginación", "desigualdad", "desamparo",
  "precariedad", "abandono", "informalidad", "desempleo", "inestabilidad",
  
  # Grupos afectados
  "niñez", "adultos mayores", "migrantes", "personas mayores", "personas con discapacidad",
  "comunidades indígenas", "campesinos", "trabajadores informales", "hogares monoparentales",
  "mujeres jefas de hogar", "personas sin hogar", "situación de calle",
  
  # Programas o políticas típicamente asociadas
  "transferencias", "bono", "subsidio", "asignación", "protección social", 
  "programa social", "ayuda estatal", "beneficio", "focalización", "apoyo económico",
  
  # Indicadores o contextos socioeconómicos
  "ingreso", "nivel socioeconómico", "vivienda precaria", "hacinamiento", "acceso limitado",
  "alimentación insuficiente", "servicios básicos", "educación incompleta", "brecha",
  
  # Marcos discursivos alternativos
  "equidad", "justicia social", "redistribución", "inclusión", "cohesión social",
  "superación", "movilidad social", "oportunidades", "desarrollo humano"
)


patron_diccionario <- paste(diccionario_pobreza, collapse = "|")
contextos_pobreza <- str_extract_all(texto_completo, regex(paste0("(?:\\S+\\s+){0,10}(", patron_diccionario, ")(?:\\s+\\S+){0,10}"), ignore_case = TRUE))[[1]]

# Guardar en tibble
contexto_pobreza_df <- tibble(
  contexto = contextos_pobreza,
  palabras = str_count(contextos_pobreza, "\\S+")
)




#===============================================================================
# 3. Conteo y visualización creativa de palabras relativas a pobreza -----------
#===============================================================================

# Tokenizar texto completo
tokens <- tibble(texto = texto_completo) %>%
  unnest_tokens(input = texto, output = palabra, token = "words", to_lower = TRUE) %>%
  filter(palabra %in% diccionario_pobreza)

# Contar frecuencias
frecuencias <- tokens %>%
  count(palabra, sort = TRUE)

# Visualización original: bubble plot
library(ggplot2)

ggplot(frecuencias, aes(x = reorder(palabra, n), y = n, size = n, fill = n)) +
  geom_point(shape = 21, color = "black", alpha = 0.8) +
  coord_flip() +
  scale_size_continuous(range = c(4, 15)) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  labs(
    title = "Gráfico 1 Frecuencia de palabras anidadas a pobreza",
    subtitle = "Visualización de tópicos a partir del discurso",
    x = NULL, y = "Frecuencia"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )


##==


#===============================================================================
# 4. Preparación para Topic Modeling -------------------------------------------
#===============================================================================

library(tidytext)
library(topicmodels)
library(tm)

# Agregar ID al contexto
contexto_pobreza_df <- contexto_pobreza_df %>%
  rowid_to_column("doc_id")

# Tokenizar cada contexto (una línea = un "documento" en LDA)
tokens <- contexto_pobreza_df %>%
  unnest_tokens(word, contexto) %>%
  anti_join(get_stopwords(language = "es"), by = "word") %>%  # Eliminar stopwords en español
  filter(str_detect(word, "^[a-záéíóúñ]+$"))  # Filtrar solo palabras válidas

# Contar palabras por documento
dtm_data <- tokens %>%
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, term = word, value = n)

#===============================================================================
# 5. Modelo LDA (Topic Modeling) -----------------------------------------------
#===============================================================================

# Ajustar un modelo con, por ejemplo, 3 tópicos
lda_model <- LDA(dtm_data, k = 6, control = list(seed = 123))

# Extraer palabras más representativas por tópico
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualizar
library(ggplot2)

top_terms %>% 
  filter(beta > 0.019) %>% 
  ggplot( aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Gráfico 2. Machine Learning no supervisado <Pobreza>",
    subtitle = "Cuenta Públic Boric - 2025",
    x = NULL, y = "Importancia (beta)"
  ) +
  theme_minimal(base_family = "Helvetica")

