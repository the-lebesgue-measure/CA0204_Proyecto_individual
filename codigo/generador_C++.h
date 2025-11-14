// generador_C++.h
// Hecho por Anthonny Flores C32975
// Claves algoritmicas cabecera de definicion de funciones
// Proyecto Individual CA0204
// Para informacion sobre metodos no convencionales ver anexo 1

#ifndef GENERADOR_C_H
#define GENERADOR_C_H

#include <string>

namespace genera {

float entropy_shannon(const std::string& password) noexcept;
void clean_words(std::string& phrase);
void greedy_fuction(std::string& phrase);
void nary_password(std::string& phrase);
void aleatory_iteration(std::string& phrase);
void discriminatory_iteration(std::string& phrase);
void aleatory_generation(std::string& phrase);
void hybrid_function(std::string& phrase);

}

#endif