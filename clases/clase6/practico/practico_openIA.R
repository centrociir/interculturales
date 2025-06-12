# ───────────────────────────────────────────────
# 🧩 LIBRERÍAS
# ───────────────────────────────────────────────
library(httr)
library(tidyverse)
library(jsonlite)
library(glue)
library(readxl)
library(stringr)
library(dplyr)


# ───────────────────────────────────────────────
# 🔑 API KEY (solo una vez por sesión)
# ───────────────────────────────────────────────
Sys.setenv(GEMINI_API_KEY = "API KEY")
openai_api_key <- Sys.getenv("API KEY")


# ───────────────────────────────────────────────
# 🤖 FUNCIÓN GEMINI
# ───────────────────────────────────────────────
gemini <- function(prompt,
                   temperature = 1,
                   max_output_tokens = 1024,
                   api_key = Sys.getenv("GEMINI_API_KEY"),
                   model = "gemini-2.0-flash") {
  
  if (nchar(api_key) < 1) {
    stop("❌ API key no definida.")
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(parts = list(list(text = prompt))),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if (response$status_code > 200) {
    stop(paste("Error -", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}

# ───────────────────────────────────────────────
# 🧠 PROMPT PARA DISCURSO DE ODIO
# ───────────────────────────────────────────────
prompt_odio <- function(texto) {
  glue(
    "Evalúa el siguiente texto según su nivel de discurso de odio.\n\n",
    "Instrucciones:\n",
    "- Responde exclusivamente con un número decimal entre 0 y 1.\n",
    "- 0 = no contiene nada de discurso de odio.\n",
    "- 1 = contiene discurso de odio extremo.\n",
    "- NO EXPLIQUES la razón.\n",
    "- NO entregues texto adicional.\n",
    "- SOLO escribe el número, por ejemplo: 0.74\n\n",
    "Texto a analizar:\n{texto}"
  )
}

# ───────────────────────────────────────────────
# 🗃️ CARGAR BASE
# ───────────────────────────────────────────────
discurso_odio <- read_excel("bbdd/discurso_odio.xlsx") |> 
  rename(texto = frase) |> 
  mutate(puntaje_odio = NA_real_)

# ───────────────────────────────────────────────
# 🔁 LOOP DE ANÁLISIS
# ───────────────────────────────────────────────
for (i in 1:nrow(discurso_odio)) {
  cat("📌 Analizando fila:", i, "\n")
  
  texto <- discurso_odio$texto[i]
  
  if (is.na(texto) || str_trim(texto) == "") {
    discurso_odio$puntaje_odio[i] <- NA
    next
  }
  
  prompt_text <- prompt_odio(texto)
  
  result <- tryCatch({
    gemini(prompt_text)
  }, error = function(e) {
    message("⚠️ Error en fila ", i, ": ", e$message)
    return(NA)
  })
  
  # Extrae solo el primer número decimal que aparezca
  numero <- as.numeric(str_extract(result, "\\d+(\\.\\d+)?"))
  
  discurso_odio$puntaje_odio[i] <- numero
  Sys.sleep(1.5)
}

# ───────────────────────────────────────────────
# 📊 RESULTADO FINAL
# ───────────────────────────────────────────────
cat("\n📋 Resultados del análisis de discurso de odio:\n")
print(discurso_odio |> select(texto, puntaje_odio))

# 🧾 OPCIONAL: Exportar a CSV
# write.csv(discurso_odio, "salida_discurso_odio.csv", row.names = FALSE)


