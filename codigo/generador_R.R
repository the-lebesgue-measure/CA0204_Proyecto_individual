# generador_R.R
# Hecho por Anthonny Flores C32975
# Claves algoritmicas y por red neuronal
# Proyecto Individual CA0204
# Para informacion sobre metodos no convencionales ver anexo 1

# Librerias
library(stringi)
library(dplyr)
library(tibble)
library(readr)
library(purrr)

# Equivalencias
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

# Palabras triviales
trivial.words = c(
  # Articulos
  "el","la","los","las","un","una","unos","unas",
  # Pronombres personales
  "yo","tu","usted","nosotros","nosotras","vosotros","vosotras",
  "ellos","ellas","me","te","se","nos","os",
  "lo","la","los","las","le","les",
  "mi","ti","si","conmigo","contigo","consigo",
  # Pronombres demostrativos
  "este","esta","estos","estas","ese","esa","esos","esas",
  "aquel","aquella","aquellos","aquellas",
  # Preposiciones
  "a","ante","bajo","cabe","con","contra","de","desde","en","entre",
  "hacia","hasta","para","por","segun","sin","so","sobre","tras",
  "durante","mediante","excepto","salvo","versus","via",
  # Contracciones
  "al","del",
  # Adverbios
  "muy","tambien","aqui","alli","ahi","ya","todavia","antes","despues",
  "ademas","aunque","asi","bien","mal","nunca","siempre","casi","solo",
  "pronto","tarde","temprano","lejos","cerca","hoy","manana","ayer",
  "aun","quizas","solamente","facilmente","dificilmente","rapidamente","lentamente",
  # English common words
  "a","an","the","i","you","he","she","it","we","they",
  "me","him","her","us","them",
  "my","your","his","her","its","our","their",
  "mine","yours","hers","ours","theirs",
  "myself","yourself","himself","herself","itself","ourselves","yourselves","themselves",
  # English prepositions / contractions / adverbs
  "about","above","across","after","against","along","among","around",
  "at","before","behind","below","beneath","beside","between","beyond",
  "by","despite","down","during","except","for","from","in","inside",
  "into","like","near","of","off","on","onto","out","outside","over",
  "past","since","through","throughout","till","to","toward","under",
  "underneath","until","up","upon","with","within","without",
  "aren't","can't","couldn't","didn't","doesn't","don't","hadn't",
  "hasn't","haven't","he's","i'm","isn't","it's","let's","she's",
  "shouldn't","that's","there's","they're","we're","weren't","what's",
  "won't","wouldn't","you'd","you'll","you're","you've",
  "again","almost","also","always","anyway","back","even","ever","here",
  "just","now","only","so","then","there","thus","well","yet","very"
)

# Funcion para limpiar la palabra
clean.words <- function(phrase) {
  
  clean.phrase = phrase %>% # Se pone en minuscula, se divide y se limpia " "
    stri_trans_tolower() %>%
    stri_trim_both() 
  
  clean.phrase = unlist(strsplit(clean.phrase, "\\s+")) # Se vectoriza
  clean.phrase = clean.phrase[!clean.phrase %in% trivial.words] # Se elimina las palabras triviales
  clean.phrase = clean.phrase[nchar(clean.phrase) > 2] # Solo palabras con mas de 2 caracteres
  
  return(paste(clean.phrase, collapse = ""))  # Se devuelve un solo string, no un vector, pongo return por formalidad
  
}

# Funcion para medir la entropia shannon
entropy.shannon <- function(password) {
  
  if (nchar(password) == 0) return(0)
  
  chars = strsplit(password, "")[[1]]
  freq = table(chars) / length(chars)
  
  return(-sum(freq * log2(freq)))
  
}

# Funcion de generacion algoritmica
aleatory.generation <- function(phrase) {
  
  phrase = sapply(strsplit(phrase, "")[[1]], function(ch) { # Se divide por caracter
    
    if (ch %in% names(equiv.list)) sample(equiv.list[[ch]], 1) else ch # Se toma uno aleatorio
    
  })
  
  return(paste0(phrase, collapse = "")) # Se devuelve pegado
  
}

# Funcion para construir tabla de frecuencias
build.equiv.freq <- function(phrase, equiv.list) {
  freq = list()
  
  for (k in names(equiv.list)) { # Por cada equivalencia base del diccionario
    
    freq[[k]] = setNames(rep(0L, length(equiv.list[[k]])), equiv.list[[k]]) # Se hace una subtabla
  
  }
  
  chars = stri_split_boundaries(phrase, type = "character")[[1]] # Se divide la frase completa
  
  for (ch in chars) { # Por cada caracter de la frase
    
    for (orig in names(equiv.list)) { # Por cada equivalencia
      
      if (ch %in% equiv.list[[orig]]) { # Si el caracter actual pertenece a la lista de equivalencias
        
        freq[[orig]][[ch]] = freq[[orig]][[ch]] + 1L # Se incrementa el contador
        break # Si ya se encontro la comparacion se para de comparar
      
      }
      
    }
    
  }
  
  return(freq)
  
}

# Funcion para elegir la equivalencia menos recurrente
choose.least.recurrent <- function(orig, equiv.list, freq.table = NULL) {
  
  choices = equiv.list[[orig]] # Se obtiene el conjunto de equivalencias posibles
  
  if (is.null(choices)) return(orig) # Si no hay equivalencias, se devuelve el caracter sin modificar
  
  if (is.null(freq.table) || is.null(freq.table[[orig]])) { # Si no hay tabla de equivalencias
    
    return(sample(choices, 1)) # Seleccion aleatoria uniforme entre las posibles
    
  }
  
  counts = freq.table[[orig]][choices] # Extrae los conteos de las equivalencias del caracter original
  min.count = min(counts, na.rm = TRUE) # Identifica la frecuencia minima observada
  candidates = names(counts)[counts == min.count] # Filtra las equivalencias con la frecuencia minima
  sample(candidates, 1) # Escoge una al azar entre las menos recurrentes
  
}

# Funcion para generacion con algoritmo discriminatorio/excluyente
discriminatory.iteration <- function(phrase) {
  
  freq.table = build.equiv.freq(phrase, equiv.list) # Se genera la tabla de frecuencias interna a partir de la frase
  chars = stri_split_boundaries(phrase, type = "character")[[1]] # Divide la frase en caracteres
  
  phrase = sapply(chars, function(ch) { # Se itera "sapply" sobre cada caracter
    
    if (ch %in% names(equiv.list)) { # Si el caracter tiene equivalencias definidas
      
      choose.least.recurrent(ch, equiv.list, freq.table) # Se selecciona la menos recurrente
      
    } else ch # Si no tiene equivalencias, se deja igual
    
  })
  
  return(paste0(phrase, collapse = ""))
}

# Funcion para elegir un caracter con la probabilidad inversa al uso previo
choose.weighted <- function(orig, freq.table) {
  
  choices = equiv.list[[orig]] # Se toma la lista de equivalentes
  counts = freq.table[[orig]][choices] # Se toma la frecuencia del caracter
  probs = 1 / (counts + 1) # Se toma la probabilidad
  sample(choices, 1, prob = probs) # Se toma el con menor probabilidad
  
}

# Funcion para generar clave usando la aparicio como inverso numerico
weighted.iteration <- function(phrase) {
  
  freq = build.equiv.freq(phrase, equiv.list) # Se hace la tabla de frecuencias
  chars = stri_split_boundaries(phrase, type="character")[[1]] # Se divide en caracteres la frase
  
  phrase = sapply(chars, function(ch) { # Al igual que el algoritmo excluyente se toma el menos frecuente
    
    if (ch %in% names(equiv.list)) {
      
      chosen = choose.weighted(ch, freq)
      freq[[ch]][chosen] <<- freq[[ch]][chosen] + 1L  
      chosen
      
    } else ch
    
  })
  
  return(paste0(phrase, collapse = ""))
  
}

# Funcion para generar el tensor con las posibles equivalencias
generate.tensor <- function(phrase, equiv.list) {
  
  chars = strsplit(phrase, "")[[1]] # Se divide la frase en caracteres individuales
  
  options = lapply(chars, function(ch) { # Se construyen las listas de opciones por caracter
    
    if (ch %in% names(equiv.list)) equiv.list[[ch]] else ch # Si tiene equivalencias se pasa la lista, si no se deja igual
    
  })
  
  grid = expand.grid(options, stringsAsFactors = FALSE) # Se genera el producto cartesiano de todas las combinaciones posibles
  
  return(apply(grid, 1, paste0, collapse = "")) # Se pega cada fila para formar una clave candidata completa
  
}

# Funcion para generar clave usando la eleccion tensorial
tensor.function <- function(phrase) {
  
  keys = generate.tensor(phrase, equiv.list) # Se generan todas las claves posibles con el tensor
  
  entropies = sapply(keys, entropy.shannon) # Se calcula la entropia de Shannon de cada clave generada
  
  if (all(is.na(entropies))) {
    return(phrase)
  }
  
  max.entropy = which.max(entropies) # Se identifica la clave con mayor entropia
  
  return(keys[max.entropy]) # Se devuelve la clave con entropia maxima
  
}

# Funcion para generar clave usando el metodo de busqueda local
greedy.function <- function(phrase) {
  
  chars = strsplit(phrase, "")[[1]] # Se divide en caracteres la frase
  local.phrase = character(length(chars)) # Se inicializa el vector de salida vacio
  
  for (i in seq_along(chars)) { # Se recorre cada posicion de la frase
    
    ch = chars[i]
    
    if (!ch %in% names(equiv.list)) { # Si no tiene equivalencias se deja el caracter tal cual
      
      local.phrase[i] = ch
      next
      
    }
    
    choices = equiv.list[[ch]] # Se obtienen las equivalencias del caracter actual
    
    ent.values = sapply(choices, function(cand) { # Se evalua cada posible reemplazo
      
      local.phrase.temp = local.phrase # Se copia el estado parcial
      local.phrase.temp[i] = cand # Se inserta la opcion candidata en la posicion actual
      
      entropy.shannon(paste0(local.phrase.temp[1:i], collapse = "")) # Se calcula la entropia incremental solo sobre lo construido hasta el momento
      
    })
    
    chosen = choices[which.max(ent.values)] # Se selecciona la opcion que maximiza la entropia local
    local.phrase[i] = chosen # Se fija en la posicion final
    
  }
  
  return(paste0(local.phrase, collapse = "")) # Se devuelve la clave final pegada en un solo string
  
}

# Funcion para generar clave usando metodo hibrido
hybrid.function <- function(phrase) {
  
  local.phrase = greedy.function(phrase) # Se genera solucion inicial con el algoritmo greedy
  chars = strsplit(phrase, "")[[1]]# Se divide la frase en caracteres individuales
  
  options = lapply(chars, function(ch) { # Se construyen las listas de opciones por caracter
    
    if (ch %in% names(equiv.list)) equiv.list[[ch]] else ch
  })
  
  
  variable.pos = which(sapply(options, length) > 2) # Se identifican las posiciones con mas de dos opciones
  
  if (length(variable.pos) == 0) return(local.phrase) # Si no hay posiciones variables se devuelve la clave greedy
  
  
  v = length(variable.pos) # Se calcula automaticamente el tamano del vecindario
  avg.options = mean(sapply(options[variable.pos], length))
  neighborhood.size = max(1, min(5, floor(log2(v * avg.options) + 1)))
  if (v <= 3) neighborhood.size = v
  
  
  local.chars = strsplit(local.phrase, "")[[1]] # Se obtienen los caracteres de la clave local
  
  gain = sapply(variable.pos, function(i) { # Se evalua la ganancia de entropia al cambiar cada posicion variable
    
    current = local.chars[i]
    alternatives = setdiff(options[[i]], current)
    
    if (length(alternatives) == 0) return(0)
    
    improvements = sapply(alternatives, function(cand) {
      
      temp = local.chars
      temp[i] = cand
      entropy.shannon(paste0(temp, collapse = "")) - entropy.shannon(local.phrase)
      
    })
    
    max(improvements, 0)
    
  })
  
  top.pos = variable.pos[order(-gain)[1:neighborhood.size]] # Se seleccionan las posiciones con mayor ganancia potencial
  
  # Se construye la grilla reducida solo en las posiciones seleccionadas
  grid = lapply(seq_along(local.chars), function(i) {
    
    if (i %in% top.pos) options[[i]] else local.chars[i]
    
  })
  
  candidates = do.call(expand.grid, c(grid, stringsAsFactors = FALSE)) # Se genera el producto cartesiano de las opciones reducidas
  candidates.str = apply(candidates, 1, paste0, collapse = "")
  entropies = sapply(candidates.str, entropy.shannon) # Se calcula la entropia de cada candidato
  max.entropy = which.max(entropies) # Se identifica la clave con mayor entropia
  
  return(candidates.str[max.entropy]) # Se devuelve la clave con entropia maxima
  
}

# Funcion para correr datos y guardar resultados
test.mode <- function(model = NULL) {
  
  cat("\n+--------------------------------+\n")
  cat("|        Bienvenido a test mode  |\n")
  cat("+--------------------------------+\n")
  
  repeat {
    
    cat("\nIngrese la ruta del CSV (ejemplo: C:/Users/antho/Documents/UCR/CA0204/Proyecto_Individual/datos/frases_test.csv)\n")
    cat("O escriba 'exit' para salir:\n> ")
    file_path <- readline()
    
    if (tolower(file_path) == "exit") return(invisible(NULL))
    
    if (!file.exists(file_path)) {
      
      cat("Error: no se encuentra el archivo, intente nuevamente.\n")
      next
      
    }
    
    # Lectura de frases
    phrases = read_csv(file_path, col_names = FALSE, show_col_types = FALSE)[[1]]
    
    # DataFrame resultado
    results = tibble(
      frase = phrases,
      clave_aleatory = NA_character_,
      tiempo_aleatory = NA_real_,
      clave_discriminatory = NA_character_,
      tiempo_discriminatory = NA_real_,
      clave_weighted = NA_character_,
      tiempo_weighted = NA_real_,
      clave_greedy = NA_character_,
      tiempo_greedy = NA_real_,
      #clave_tensor = NA_character_,
      #tiempo_tensor = NA_real_,
      clave_hybrid = NA_character_,
      tiempo_hybrid = NA_real_ 
    )
    
    for (i in seq_along(phrases)) {
      phrase_i = phrases[i]
      phrase_clean = clean.words(phrase_i)
      cat("Procesando frase...\n")
      
      # Algoritmo Aleatorio
      cat("aleatory.generation...\n")
      t1 = Sys.time()
      clave_aleatory = aleatory.generation(phrase_clean)
      t2 = Sys.time()
      
      # Algoritmo Discriminatorio
      cat("discriminatory.iteration...\n")
      t3 = Sys.time()
      clave_discriminatory = discriminatory.iteration(phrase_clean)
      t4 = Sys.time()
      
      # Algoritmo Ponderado
      cat("weighted.iteration...\n")
      t5 = Sys.time()
      clave_weighted = weighted.iteration(phrase_clean)
      t6 = Sys.time()
      
      # Algoritmo Greedy Entropy
      cat("greedy.function...\n")
      t7 = Sys.time()
      clave_greedy = greedy.function(phrase_clean)
      t8 = Sys.time()
      
      # Algoritmo Tensorial
      #cat("tensor.method...\n")
      #t9 = Sys.time()
      #clave_tensor = tensor.function(phrase_clean)
      #t10 = Sys.time()
      
      # Algoritmo Híbrido
      cat("hybrid.method...\n")
      t11 = Sys.time()
      clave_hybrid = hybrid.function(phrase_clean)
      t12 = Sys.time()
      
      # Guardar resultados
      results$clave_aleatory[i] = paste0('', clave_aleatory, '')
      results$clave_discriminatory[i] = paste0('', clave_discriminatory, '')
      results$clave_weighted[i] = paste0('', clave_weighted, '')
      results$clave_greedy[i] = paste0('', clave_greedy, '')
      # results$clave_tensor[i] = paste0('"', clave_tensor, '"')
      results$clave_hybrid[i] = paste0('', clave_hybrid, '')
      
      results$tiempo_aleatory[i] = as.numeric(difftime(t2, t1, units="secs"))
      results$tiempo_discriminatory[i] = as.numeric(difftime(t4, t3, units="secs"))
      results$tiempo_weighted[i] = as.numeric(difftime(t6, t5, units="secs"))
      results$tiempo_greedy[i] = as.numeric(difftime(t8, t7, units="secs"))
      #results$tiempo_tensor[i] = as.numeric(difftime(t10, t9, units="secs"))
      results$tiempo_hybrid[i] = as.numeric(difftime(t12, t11, units="secs"))  
    }
    
    # Guardar datos de forma
    base_file = tools::file_path_sans_ext(basename(file_path))
    base_name = paste0("../datos/", base_file, "_claves_R_")
    
    # Encontrar el primer n libre
    n = 1
    
    while (file.exists(paste0(base_name, n, ".csv"))) {
      
      n = n + 1
      
    }
    
    output_path = paste0(base_name, n, ".csv")
    write_csv(
      results,
      output_path,
      quote = "all",
      quote_escape = "double"
    )
    cat("\nResultados guardados en: ", normalizePath(output_path), "\n")
    
  }
  
}
