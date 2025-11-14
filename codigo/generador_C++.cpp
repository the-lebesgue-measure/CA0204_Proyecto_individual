// generador_C++.cpp
// Hecho por Anthonny Flores C32975
// Claves algoritmicas
// Proyecto Individual CA0204
// Para informacion sobre metodos no convencionales ver anexo 1
// El codigo esta hiperdocumentado a peticion del profesor
// Se recomienda correr el programa sin ningun otro programa abierto debido al gran uso abierto de memoria

#include "generador_C++.h" // Libreria de las funciones algoritmicas

#include <algorithm>
#include <thread>
#include <mutex>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <random>
#include <cctype>
#include <cstdint>
#include <cmath>

using namespace std; // Evita tener que usar siempre std:: para funciones de la libreria estandar

namespace genera {

    random_device rd; mt19937 gen(rd()); // Generacion aleatoria metodo Mersenne Twister
    
    mutex mutual_exclusion; // Mutex global para proteger acceso al mejor puntaje de las claves

    // Letras equivalentes
    constexpr const char* a_equiv[] = {"@", "4", "a", "A", nullptr};
    constexpr const char* b_equiv[] = {"8", "b", "B", nullptr};
    constexpr const char* c_equiv[] = {"<", "(", "C", "c", nullptr};
    constexpr const char* d_equiv[] = {"D", "d", nullptr};
    constexpr const char* e_equiv[] = {"E", u8"€", "3", "e", nullptr}; // Utilizar u8 para indicar que no es ASCII
    constexpr const char* f_equiv[] = {"F", "f", nullptr};
    constexpr const char* g_equiv[] = {"9", "6", "G", "g", nullptr};
    constexpr const char* h_equiv[] = {"H", "h", nullptr};
    constexpr const char* i_equiv[] = {u8"¡", "!", "1", "I", "i", nullptr};
    constexpr const char* j_equiv[] = {"J", "j", nullptr};
    constexpr const char* k_equiv[] = {"K", "k", nullptr};
    constexpr const char* l_equiv[] = {"|", "L", "l", nullptr};
    constexpr const char* m_equiv[] = {"M", "m", nullptr};
    constexpr const char* n_equiv[] = {"n", "N", nullptr};
    constexpr const char* o_equiv[] = {"0", "O", "o", nullptr};
    constexpr const char* p_equiv[] = {"P", "p", nullptr};
    constexpr const char* q_equiv[] = {"Q", "q", nullptr};
    constexpr const char* r_equiv[] = {"R", "r", nullptr};
    constexpr const char* s_equiv[] = {"$", "5", "S", "s", nullptr};
    constexpr const char* t_equiv[] = {"+", "T", "t", nullptr};
    constexpr const char* u_equiv[] = {"U", "u", "V", "v",nullptr};
    constexpr const char* v_equiv[] = {"V", "v", nullptr};
    constexpr const char* w_equiv[] = {"W", "w", nullptr};
    constexpr const char* x_equiv[] = {u8"×", "X", "x", nullptr};
    constexpr const char* y_equiv[] = {"Y", "y", nullptr};
    constexpr const char* z_equiv[] = {"2", "Z", "z", nullptr};

    // Numeros equivalentes
    constexpr const char* n0[] = {"O", "0", "o", nullptr};
    constexpr const char* n1[] = {"|", "I", "l", "1", nullptr};
    constexpr const char* n2[] = {"Z", "z", "2", nullptr};
    constexpr const char* n3[] = {"E",  u8"€", "3", "e", nullptr};
    constexpr const char* n4[] = {"4", nullptr};
    constexpr const char* n5[] = {"S", "s", "5", nullptr};
    constexpr const char* n6[] = {"G", "g", "6", nullptr};
    constexpr const char* n7[] = {"7", nullptr};
    constexpr const char* n8[] = {"8", nullptr};
    constexpr const char* n9[] = {"9", nullptr};

    // Funcion para obtener caracteres equivalentes
    const char* const* get_equiv(char c) noexcept {

        static char char_buff[2] = {0, 0}; // Buffer para el caracter
        static const char* char_equiv[] = {char_buff, nullptr}; // Arreglo con el caracter original 

        // Convertir a minuscula en ASCII
        if (c >= 'A' && c <= 'Z') c += 32;

        switch (c) {
            case 'a': return a_equiv;
            case 'b': return b_equiv;
            case 'c': return c_equiv;
            case 'd': return d_equiv;
            case 'e': return e_equiv;
            case 'f': return f_equiv;
            case 'g': return g_equiv;
            case 'h': return h_equiv;
            case 'i': return i_equiv;
            case 'j': return j_equiv;
            case 'k': return k_equiv;
            case 'l': return l_equiv;
            case 'm': return m_equiv;
            case 'n': return n_equiv;
            case 'o': return o_equiv;
            case 'p': return p_equiv;
            case 'q': return q_equiv;
            case 'r': return r_equiv;
            case 's': return s_equiv;
            case 't': return t_equiv;
            case 'u': return u_equiv;
            case 'v': return v_equiv;
            case 'w': return w_equiv;
            case 'x': return x_equiv;
            case 'y': return y_equiv;
            case 'z': return z_equiv;
            case '0': return n0;
            case '1': return n1;
            case '2': return n2;
            case '3': return n3;
            case '4': return n4;
            case '5': return n5;
            case '6': return n6;
            case '7': return n7;
            case '8': return n8;
            case '9': return n9;
            default: 
                char_buff[0] = c; // Devolver el caracter si no tiene equivalente

                return char_equiv;

        }
    }

    // Palabras no significativas
    const unordered_set<string> trivial_words = { // Este diccionario es modificable a gusto 

        // Articulos singulares y plurales
        "el", "la", "los", "las", "un", "una", "unos", "unas",

        // Pronombres personales singulares y plurales
        "yo", "tu", "usted", "nosotros", "nosotras",
        "vosotros", "vosotras", "ellos", "ellas",
        "me", "te", "se", "nos", "os",
        "lo", "la", "los", "las", "le", "les",
        "mi", "ti", "si", "conmigo", "contigo", "consigo",

        // Pronombres demostrativos
        "este", "esta", "estos", "estas", "ese", "esa", "esos", "esas",
        "aquel", "aquella", "aquellos", "aquellas",

        // Preniciones
        "a", "ante", "bajo", "cabe", "con", "contra", "de", "desde", "en", "entre",
        "hacia", "hasta", "para", "por", "segun", "sin", "so", "sobre", "tras",
        "durante", "mediante", "excepto", "salvo", "versus", "via",

        // Contracciones
        "al", "del",

        // Adverbios 
        "muy", "tambien", "aqui", "alli", "ahi", "ya", "todavia", "antes", "despues",
        "ademas", "aunque", "asi", "bien", "mal", "nunca", "siempre", "casi", "solo",
        "pronto", "tarde", "temprano", "lejos", "cerca", "hoy", "mañana", "ayer",
        "aun", "quizas", "solamente", "facilmente", "difícilmente", "rapidamente", "lentamente"

        // Articles
        "a", "an", "the",

        // Pronouns
        "i", "you", "he", "she", "it", "we", "they",
        "me", "him", "her", "us", "them",
        "my", "your", "his", "her", "its", "our", "their",
        "mine", "yours", "hers", "ours", "theirs",
        "myself", "yourself", "himself", "herself", "itself", "ourselves", "yourselves", "themselves",

        // Prenitions
        "about", "above", "across", "after", "against", "along", "among", "around",
        "at", "before", "behind", "below", "beneath", "beside", "between", "beyond",
        "by", "despite", "down", "during", "except", "for", "from", "in", "inside",
        "into", "like", "near", "of", "off", "on", "onto", "out", "outside", "over",
        "past", "since", "through", "throughout", "till", "to", "toward", "under",
        "underneath", "until", "up", "upon", "with", "within", "without",

        // Common contractions
        "aren't", "can't", "couldn't", "didn't", "doesn't", "don't", "hadn't",
        "hasn't", "haven't", "he's", "i'm", "isn't", "it's", "let's", "she's",
        "shouldn't", "that's", "there's", "they're", "we're", "weren't", "what's",
        "won't", "wouldn't", "you'd", "you'll", "you're", "you've",

        // Adverbs
        "about", "above", "again", "almost", "also", "always", "anyway", "back",
        "even", "ever", "here", "just", "like", "now", "only", "over", "so", "then",
        "there", "thus", "well", "yet", "very"

    };

    // Funcion para devolver un numerico equivalente del UTF-8
    uint32_t decode_utf8(const string& c, size_t& i) noexcept { // size_t considera el tamanno del sistema 64 bits para sistemas normales y 32 bits para embebidos

        if (i >= c.size()) return 0; // Si indice excede tamanno, devolver 0

        unsigned char ch = static_cast<unsigned char>(c[i]); // Obtener byte actual

        if (ch < 0x80) { // Si es ASCII de 1 byte

            ++i; // Avanzar el indice

            return ch; // Devolver el codigo ASCII

        }
        else if ((ch & 0xE0) == 0xC0 && i + 1 < c.size()) { // Si es UTF-8 de 2 bytes

            uint32_t code = (ch & 0x1F) << 6; // Extraer bits primer byte
            code |= (static_cast<unsigned char>(c[i + 1]) & 0x3F); // Anadir bits segundo byte
            i += 2; // Avanzar indice

            return code; // Devolver punto de codigo

        }
        else if ((ch & 0xF0) == 0xE0 && i + 2 < c.size()) { // Si es UTF-8 de 3 bytes

            uint32_t code = (ch & 0x0F) << 12; // Extraer bits primer byte
            code |= (static_cast<unsigned char>(c[i + 1]) & 0x3F) << 6; // Anadir bits segundo byte
            code |= (static_cast<unsigned char>(c[i + 2]) & 0x3F); // Anadir bits tercer byte
            i += 3; // Avanzar indice

            return code; // Devolver punto de codigo

        }
        else if ((ch & 0xF8) == 0xF0 && i + 3 < c.size()) { // Si es UTF-8 de 4 bytes

            uint32_t code = (ch & 0x07) << 18; // Extraer bits primer byte
            code |= (static_cast<unsigned char>(c[i + 1]) & 0x3F) << 12; // Anadir bits segundo byte
            code |= (static_cast<unsigned char>(c[i + 2]) & 0x3F) << 6; // Anadir bits tercer byte
            code |= (static_cast<unsigned char>(c[i + 3]) & 0x3F); // Anadir bits cuarto byte
            i += 4; // Avanzar indice

            return code; // Devolver punto de codigo

        }
        ++i; // Avanzar indice

        return 0; 

    }

    // Funcion para evaluar entropia Shannon
    float entropy_shannon(const string& password) noexcept { 

        if (password.empty()) return 0.0f; // Devolver flotante 0 si cadena vacia (nada de entropia)

        unordered_map<uint32_t, uint16_t> freq; // Mapa para contar frecuencias
        size_t i = 0; // Indice para recorrer cadena

        while (i < password.size()) { // Iterar sobre codigo UTF-8

            uint32_t code = decode_utf8(password, i); // Obtener codigo UTF-8 numerico

            if (code != 0) { // Ignorar codigos invalidos

                ++freq[code]; // Incrementar frecuencia

                if (freq[code] == 0) return 0.0f; // Desbordamiento de 65,535

            }

        }

        float entropy = 0.0f; // Inicializar entropia
        const float len = static_cast<float>(password.size()); // Longitud como flotante

        for (const auto& [_, f] : freq) { // Iterar sobre frecuencias

            if (f > 0) { // Procesar caracteres presentes

                entropy -= (static_cast<float>(f) / len) * log2(static_cast<float>(f) / len); // Sumar termino de entropia Shannon
            
            }

        }

        return entropy; // Devolver entropia calculada
    }

    // Funcion para limpiar palabras significativas quitar espacios vacios
    void clean_words(string& phrase) {

        if (phrase.empty()) return;

        string result, word; // Control de datos
        size_t i = 0; // Contador para recorrer palabras

        while (i < phrase.size()) { // Mientas todavia se este en la frase

            word.clear(); // Se limpia la copia "Elimina lo escrito"

            while (i < phrase.size() && phrase[i] != ' ') { // Copiar palabra hasta encontrar un espacio

                char c = phrase[i]; // Copiar su caracter

                if (c >= 'A' && c <= 'Z') { // Si es caracter alfabetico

                    c += 32; // Convertir mayusculas a minusculas en ASCII y UTF-8

                }

                word += c; // Copiar la letra
                ++i; // Avanzar a la siguiente letra

            }
            
            if (word.length() >= 3 && trivial_words.find(word) == trivial_words.end()) {// Si la longitud >= 3 y no es palabra no significativa

                // if (!result.empty()) result += ' '; // Descomentar si se quiere las palabras separadas por espacio
                result += word; // Se copia la palabra 

            }

            // Saltar espacios, mejor control del while
            while (i < phrase.size() && phrase[i] == ' ') ++i;

        }

        phrase = result; // Se edita la frase original

    }

    // Funcion para busqueda aleatoria de escogencia iterada
    void aleatory_iteration(string& phrase) {

        string password = phrase; // Variable auxiliar
        int max_ite = 0; // Indice/contador auxiliar

        do { 

            
            for (size_t i = 0; i < password.size(); ++i) { // Mientras este en el tamanno de la palabra

                unsigned char c = static_cast<unsigned char>(password[i]); // Tomar valor numerico

                if (c > 123) continue; // Se salta caracteres no ASCII

                const char* const* equivs = get_equiv(password[i]); // Toma el equivalente
                size_t count = 0; // Contador auxiliar

                while (equivs[count] != nullptr) ++count; // Contar los caracteres equivalentes

                if (count > 0) { // Si hay al menos de uno

                    password.replace(i, 1, equivs[gen() % count]); 

                }

            }

            max_ite++;

        } while (entropy_shannon(password) < 3.0 && max_ite < pow(2, password.size())); // Mientras que no se encuentre una palabra que cumple la entropida o alla un desvordamiento 3^16
        
        phrase = password; // Se cambia la contraseña por referencia

    }

    // Funcion para busqueda excluyente aleatoria
    void discriminatory_iteration(string& phrase) {

        string password = phrase; 

        for (size_t i = 0; i < password.size(); ++i) { // Mientras no se hayan reemplazado todas las letras

            unsigned char c = static_cast<unsigned char>(password[i]); // Tomar valor numerico

            if (c > 123) continue; // Se salta caracteres no ASCII
            
            const char* const* equivs = get_equiv(password[i]); // Tomar los caracteres equivalentes
            size_t count = 0;

            while (equivs[count] != nullptr) ++count; // Contar los equivalentes

            if (count == 0) continue; // Si el caracter no tiene equivalente se pasa

            if (i == 0) { // Si es el primero se toma aleatoriamente
                // Reemplazar el primer caracter por un equivalente completo aleatorio
                password.replace(i, 1, equivs[gen() % count]);

            } else { // Sino, se busca y toma el caracter equivalente menos recurrente

                size_t min_count = i + 1; // El minimo a contar es el largo de lo editado
                const char* best = equivs[0]; // Ahora apunta al vecor completo

                for (size_t k = 0; k < count; ++k) { // Se pasa por cada caracter equivalente

                    const char* ch_equiv = equivs[k]; // Se toma el vector completo
                    size_t d = 0;

                    string s_local(ch_equiv); // Convertir a string para tomar todos los bytes

                    for (size_t j = 0; j < i; ++j) {
                        // Contar cuantas veces ya aparecio este equivalente
                        if (password.substr(j, s_local.size()) == s_local) ++d;
                    }

                    if (d < min_count) { // Si hay menos que el ya tomado
                        min_count = d; // Se cambia el valor del minimo
                        best = ch_equiv; // Se cambia al vector completo
                    }
                }

                // Reemplaza el caracter en la posicion i con el equivalente completo seleccionado
                password.replace(i, 1, best); 

            }
        }

        phrase = password; // Se cambia la contraseña por referencia

    }

    // Funcion para busqueda totalmente aleatoria
    void aleatory_generation(string& phrase) {

        string password = phrase; 

        for (size_t i = 0; i < password.size(); ++i) { // Se pasa por aca caracter

            unsigned char c = static_cast<unsigned char>(password[i]); // Tomar valor numerico

            if (c > 123) continue; // Se salta caracteres no ASCII

            const char* const* equivs = get_equiv(password[i]); // Se toma los equivalentes
            size_t count = 0;

            while (equivs[count] != nullptr) ++count; // Se cuenta cuantos equivalentes hay
            
            if (count > 0) { // Si hay por lo menos uno

                password.replace(i, 1, equivs[gen() % count]);

            }
        }

        phrase = password; // Se cambia la contraseña por referencia

    }

    // Funcion para busqueda n-aria de la clave generando hilos para todas las combinaciones
    void nary_password_search(string phrase, size_t n = 0, string* best = nullptr) {

        if(n == phrase.size()) { // Si se llego al total de letras "Iteraciones"

            float phrase_entropy = entropy_shannon(phrase); // Calcular entropia
            lock_guard<mutex> lock(mutual_exclusion); // Bloquear acceso a la mejor frase

            if(!best || phrase_entropy > entropy_shannon(*best)) *best = phrase; // Actualizar mejor frase si aplica

            return;

        }

        const char original = phrase[n]; // Se copia el caracter a evaluar
        const char* const* equivs = get_equiv(original); // Se toman los equivalentes

        vector<thread> threads; // Vector de hilos para paralelizar

        for (size_t i = 0; equivs[i] != nullptr; ++i) { // Por todos los equivalentes

            string local_phrase = phrase; // Copia local por hilo
            local_phrase.replace(n, 1, equivs[i]); // Toma el caracter correspodiente

                threads.emplace_back([local_phrase, n, best]() mutable {

                    nary_password_search(local_phrase, n + 1, best); // Busqueda en hilo

                });

        }

        // Esperar que todos los hilos terminen
        for (auto& t : threads) { if (t.joinable()) t.join(); }
    }
    
    // Funcion para la llamada recursiva del arbol
    void nary_password(string& phrase) {

        string best = phrase;
        nary_password_search(phrase, 0, &best);
        phrase = best;

    }

    // Funcion para busqueda local greedy (sin vectores, estructura igual a R)
    void greedy_fuction(string& phrase) {

        if (phrase.empty()) return;

        string password = phrase; // Resultado parcial
        const size_t N = password.size();

        for (size_t i = 0; i < N; ++i) {

            char c = password[i];
            const char* const* equivs = get_equiv(c);

            // Contar equivalentes
            size_t count = 0;
            while (equivs[count] != nullptr) count++;

            // Si solo hay una opción → no hay decisión
            if (count <= 1) {
                // password[i] ya es correcto
                continue;
            }

            // Probar cada alternativa y medir entropía
            float best_entropy = -1.0f;
            size_t best_index = 0;

            for (size_t k = 0; k < count; ++k) {

                const char* cand = equivs[k];

                // Construir prefijo parcial hasta i, pero con candidato reemplazado
                string prefix;
                prefix.assign(password.data(), i);  // copia directa con puntero
                prefix += cand; 

                // Calcular entropía de este prefijo
                float e = entropy_shannon(prefix);

                if (e > best_entropy) {
                    best_entropy = e;
                    best_index = k;
                }
            }

            // Fijar el mejor equivalente encontrado
            password.replace(i, 1, equivs[best_index]);
        }

        phrase = password; // Devolver por referencia
    }

    // Funcion para generar clave con busqueda hibrida, tipo greedy y busqueda binaria ponderada
    void hybrid_function_search(string& phrase, size_t n, string* best) {

        if (n == phrase.size()) { 
            // Se llego al final de la frase, se evalua la entropia completa
            float phrase_entropy = entropy_shannon(phrase);

            lock_guard<mutex> lock(mutual_exclusion); // Se asegura exclusion de escritura

            // Se actualiza el mejor resultado solo si la nueva frase es superior
            if (!best || phrase_entropy > entropy_shannon(*best)) 
                *best = phrase;

            return;

        }

        const char* const* equivs = get_equiv(phrase[n]); // Tomar equivalentes del caracter actual

        size_t count = 0;
        while (equivs[count]) ++count; // Contar equivalentes disponibles

        if (count <= 1) { 
            // Si el caracter no tiene alternativas uiles, continuar con el siguiente
            hybrid_function_search(phrase, n+1, best);

            return;

        }

        // Estructura de gruardado de evaluacion
        struct Candidate { size_t k; float entropy; };
        Candidate best1 = {0, -1e9f}, best2 = {0, -1e9f}; // Inicialización en valores minimos

        // Evaluacion local sin duplicar la frase completa
        for (size_t k = 0; k < count; ++k) {

            char original = phrase[n]; // Guardar estado actual
            phrase[n] = equivs[k][0];  // Aplicar variante directamente en la frase

            float e = entropy_shannon(phrase.substr(0, n+1)); // Medir entropía parcial

            // Selección de los dos mejores sin ordenar ni almacenar todos
            if (e > best1.entropy) {

                best2 = best1;
                best1 = {k, e};

            } 
            else if (e > best2.entropy) {

                best2 = {k, e};

            }

            phrase[n] = original; // Revertir para no contaminar las otras ramas
        }

        // Se ejecutan únicamente las dos mejores rutas encontradas
        vector<thread> threads;
        threads.reserve(2); // Se reservan 2 hilos maximo

        // Funcion para avanzar recursivamente sobre la frase modificando solo el punto actual
        auto launch = [&](size_t k){ // Lambda (estructura en c++) que permite hacer backtraking, una funcion no definida formalmente dentro del codigo

            char original = phrase[n];
            phrase[n] = equivs[k][0]; // Aplicar variante seleccionada
            hybrid_function_search(phrase, n+1, best); // Bajar al siguiente carácter
            phrase[n] = original; // Revertir al terminar
            
        };

        // Ejecutar primera mejor rama
        threads.emplace_back(launch, best1.k);

        // Ejecutar la segunda solo si aporta valor real (no es residual, osea, 0.00000001 de entropia)
        if (best2.entropy > -1e8f) { threads.emplace_back(launch, best2.k); }

        // Esperar a que ambas rutas terminen antes de retroceder
        for (auto& t : threads) { t.join(); }
    }

    // Funcion hibrida
    void hybrid_function(string& phrase) {

        string best = phrase; 
        hybrid_function_search(phrase,0,&best); // Llamar a la busqueda hibrida
        phrase = best; // Asignar mejor frase encontrada
        
    }

}