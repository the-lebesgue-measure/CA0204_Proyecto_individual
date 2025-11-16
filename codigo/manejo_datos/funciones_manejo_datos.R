# funciones_documetacion.R
# Hecho por Anthonny Flores C32975
# Proyecto Individual CA0204
# Para informacion sobre metodos no convencionales ver anexo 1

# Librerias
library(readr)
library(dplyr)

# Cuenta de equivalentes
equiv.count = list(
  a = 4,  # @,4,a,A
  b = 3,  # 8,b,B
  c = 4,  # <,(,C,c
  d = 2,  # D,d
  e = 4,  # E,€,3,e
  f = 2,  # F,f
  g = 4,  # 9,6,G,g
  h = 2,  # H,h
  i = 5,  # ¡,!,1,I,i
  j = 2,  # J,j
  k = 2,  # K,k
  l = 3,  # |,L,l
  m = 2,  # M,m
  n = 2,  # n,N
  o = 3,  # 0,O,o
  p = 2,  # P,p
  q = 2,  # Q,q
  r = 2,  # R,r
  s = 4,  # $,5,S,s
  t = 3,  # +,T,t
  u = 4,  # U,u,V,v
  v = 2,  # V,v
  w = 2,  # W,w
  x = 3,  # ×,X,x
  y = 2,  # Y,y
  z = 3,  # 2,Z,z
  `0` = 3, # O,0,o
  `1` = 4, # |,I,l,1
  `2` = 3, # Z,z,2
  `3` = 4, # E,€,3,e
  `4` = 1, # 4
  `5` = 3, # S,s,5
  `6` = 3, # G,g,6
  `7` = 1, # 7
  `8` = 1, # 8
  `9` = 1  # 9
)

# Lista de equivalentes
equiv.list <- list(
  a = c("@","4","a","A"),
  b = c("8","b","B"),
  c = c("<","(","C","c"),
  d = c("D","d"),
  e = c("E","€","3","e"),
  f = c("F","f"),
  g = c("9","6","G","g"),
  h = c("H","h"),
  i = c("¡","!","1","I","i"),
  j = c("J","j"),
  k = c("K","k"),
  l = c("|","L","l"),
  m = c("M","m"),
  n = c("n","N"),
  o = c("0","O","o"),
  p = c("P","p"),
  q = c("Q","q"),
  r = c("R","r"),
  s = c("$","5","S","s"),
  t = c("+","T","t"),
  u = c("U","u","V","v"),
  v = c("V","v"),
  w = c("W","w"),
  x = c("×","X","x"),
  y = c("Y","y"),
  z = c("2","Z","z"),
  `0` = c("O","0","o"),
  `1` = c("|","I","l","1"),
  `2` = c("Z","z","2"),
  `3` = c("E","€","3","e"),
  `4` = c("4"),
  `5` = c("S","s","5"),
  `6` = c("G","g","6"),
  `7` = c("7"),
  `8` = c("8"),
  `9` = c("9")
)


R_lookup = numeric(0)  # ¡AQUÍ ESTABA EL ERROR!
for (cls in names(equiv.list)) {
  for (sym in equiv.list[[cls]]) {
    R_lookup[sym] = as.numeric(equiv.count[[cls]])  # Forzar numérico
  }
}

# Funcion para medir entropia normalizada
entropy.normalized = function(password) {
  if (nchar(password) == 0) return(0)
  
  # H_total (Shannon en este caso)
  chars = unlist(strsplit(password, ""))
  freq = table(chars) / length(chars)
  H_total = -sum(freq * log2(freq + 1e-10))
  
  # R_total
  chars_used = unique(chars)
  R_total = sum(R_lookup[chars_used], na.rm = TRUE)
  
  if (is.na(R_total) || R_total <= 1) return(0)
  
  return(H_total / log2(R_total))
  
}

# Funcion para medir dencidad de entropia
entropy.density = function(password) {
  
  L = nchar(password)
  
  if (L == 0) return(0)
  
  chars = unlist(strsplit(password, ""))
  freq = table(chars) / length(chars)
  H_total = -sum(freq * log2(freq + 1e-10))
  
  return(H_total / L)
  
}

# Funcion para medir entropia efectiva
entropy.effective = function(password) {
  
  if (nchar(password) == 0) return(0)
  
  chars = unlist(strsplit(password, ""))
  unique_count = length(unique(chars))
  L = length(chars)
  
  if (unique_count <= 1) return(0)
  
  return(L * log2(unique_count))
  
}

# Funcion para medir la entropia shannon
entropy.shannon <- function(password) {
  
  if (nchar(password) == 0) return(0)
  
  chars = strsplit(password, "")[[1]]
  freq = table(chars) / length(chars)
  
  return(-sum(freq * log2(freq)))
  
}

# Funcion para medir la entropia minima
entropy.min <- function(password) {
  
  if (nchar(password) == 0) return(0)
  
  chars = strsplit(password, "")[[1]]
  freq = table(chars) / length(chars)
  
  return(-log2(max(freq)))  # -log2 del simbolo mas frecuente
  
}

# Funcion para medir la entropia cuadratica
entropy.quadratic <- function(password) {
  
  if (nchar(password) == 0) return(0)
  
  # Dividir en caracteres (UTF-8 seguro)
  chars = unlist(strsplit(password, ""))
  n = length(chars)
  
  if (n <= 1) return(0)
  
  # Numerico de UTF-8
  codepoints = sapply(chars, utf8ToInt)
  
  # Si es ASCII letras alfabetica
  is.letter.ascii = (codepoints >= 65 & codepoints <= 90) |   # A-Z
    (codepoints >= 97 & codepoints <= 122)   # a-z
  
  # Todo lo demas
  is.preferred = !is.letter.ascii
  
  # Asignar peso
  weight = ifelse(is.preferred, 2.0, 1.0)  # 2x para no-letras
  
  # Frecuencia ponderada
  weighted.count = tapply(weight, chars, sum)
  total.weight = sum(weighted.count)
  p.weighted = weighted.count / total.weight
  
  return(-log2(sum(p.weighted^2)))
  
}

# Main para aplicar funciones masivamente (profe, esto es pq me da pereza repetir los llamdos en el documento oficial xd)
main <- function() {
  
  repeat {
    cat("\nIngrese la ruta del archivo CSV de resultados previos (ejemplo: C:/ruta/resultados_acumulados.csv)\n")
    cat("O escriba 'exit' para salir:\n> ")
    
    file_path <- readline()
    
    if (tolower(file_path) == "exit") {
      cat("\nSaliendo...\n")
      return(invisible(NULL))
    }
    
    if (!file.exists(file_path)) {
      cat("\nError: no se encontró el archivo. Intente nuevamente.\n")
      next
    }
    
    cat("\nArchivo encontrado. Cargando datos...\n")
    results <- read.csv(
      file_path,
      fileEncoding = "latin1",
      comment.char = ""
    )
    # Caso R
    #results <- read.csv(
    #  file = file_path,
    #  fileEncoding = "UTF-8"
    #)
    break
  }
  
  key_columns <- names(results)
  key_columns <- key_columns[
      !grepl("^tiempo_", key_columns) &
      !grepl("^Tiempo_", key_columns) &
      !grepl("^entropy_", key_columns)
  ]
  
  cat("\nMétodos de claves detectados:\n")
  print(key_columns)
  
  # Aplicar métricas a cada clave
  for (col in key_columns) {
    base <- col
    
    results[[paste0("entropy_normalized_", base)]] <- sapply(results[[col]], entropy.normalized)
    results[[paste0("entropy_density_", base)]]    <- sapply(results[[col]], entropy.density)
    results[[paste0("entropy_effective_", base)]]  <- sapply(results[[col]], entropy.effective)
    results[[paste0("entropy_shannon_", base)]]    <- sapply(results[[col]], entropy.shannon)
    results[[paste0("entropy_min_", base)]]        <- sapply(results[[col]], entropy.min)
    results[[paste0("entropy_quadratic_", base)]]  <- sapply(results[[col]], entropy.quadratic)
  }
  
  # Generar nombre del archivo salida sin sobrescribir
  base_name <- sub("\\.csv$", "", basename(file_path))
  dir_name <- dirname(file_path)
  
  n <- 1
  repeat {
    out_path <- file.path(dir_name, paste0(base_name, "_", n, "_entropia.csv"))
    if (!file.exists(out_path)) break
    n <- n + 1
  }
  
  write.csv(
    results,
    file = out_path,
    row.names = FALSE,
    fileEncoding = "latin1",
    quote = TRUE
  )
  #write.csv(results, out_path, row.names = FALSE)
  
  cat("\nResultados guardados en:\n", out_path, "\n")
    
}

main()
