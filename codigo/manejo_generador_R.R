# manejo_generador_R.R
# Hecho por Anthonny Flores C32975
# Claves algoritmicas y por red neuronal
# Proyecto Individual CA0204
# Para informacion sobre metodos no convencionales ver anexo 1

# Sources
source("generador_R.R")

# main()
main <- function() {
  
  cat("\014") # Limpiar la consola en RStudio
  leave = F # Salida main()
  
  cat("Bienvenido al proyecto individual parte R para convertir frases a claves, para más documentación\n")
  
  # Consola para la manipulacion 
  while(!leave){
    
    # Captar opcion
    cat("\nIntroduzca una opción: \n\n\"0\" para generación algorítmica\n\n\"1\" para generación excluyente\n\n\"2\" para generación excluyente ponderada\n\n\"3\" para generación greedy\n\n\"4\" para generación tensorial\n\n\"5\" para generación híbrida\n\n\"6\" para correr datos\n\n\"7\" para salir")
    option = readline()
    
    repeat { # Verificar opcion
      
      if (!is.na(as.numeric(option))){
        
        if(as.integer(option) == option && 0 <= option && option <= 7 ) break
        
      }
      
      option = readline("\nPor favor, digite e imprima solo una de las opciones dadas: \n")
      
    }
    
    if (option >= "0" && option <= "5") {
      
      cat("\nIntroduzca la frase a convertir: ")
      phrase = readline()
      phrase = clean.words(phrase) # Limpiar frase
      
    }
    
    # Modos de prueba
    if (option == "0") {
      
      phrase = aleatory.generation(phrase) # Algoritmo aleatorio
      
    } else if (option == "1") {
      
      phrase = discriminatory.iteration(phrase) # Algoritmo excluyente
      
    } else if (option == "2") {
      
      phrase = weighted.iteration(phrase) # Algoritmo ponderado inverso
      
    } else if (option == "3") {
      
      phrase = greedy.function(phrase) # Algoritmo greedy
      
    } else if (option == "4") {
      
      phrase = tensor.function(phrase) # Algoritmo tensorial
      
    } else if (option == "5") {
      
      phrase = hybrid.function(phrase) # Algoritmo híbrido (greedy + tensor local)
      
    } else if (option == "6") {
      
      test.mode() # Modo test
      
    } else if (option == "7") {
      
      leave = TRUE # Salir
      
    }
    
    if (option >= "0" && option <= "5") {
      
      cat("\nLa frase convertida es: ")
      print(phrase)
      
    }
    
  }
  
  
  cat("\nHasta el ∞ ^ + ->\n")
  
}

main()
# xxx