// manejo_generador.cpp
// Hecho por Anthonny Flores C32975
// Manejo de funciones
// Proyecto Individual CA0204
// Para informacion sobre metodos no convencionales ver anexo 1
// Copilar de la siguiente manera 
// cd "C:\Users\RUTA\codigo"
// g++ -std=c++17 -O2 manejo_generador.cpp -L. -lgenerador -o manejo_generador.exe

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <chrono>
#include <limits>
#include <cstring>
#include <windows.h>

#include "generador_C++.h"

using namespace std;

// Estructura Data_password
struct data_password {

    vector<string> phrase;
    //vector<string> phrase_nary_password;
    //vector<float> times_nary_password;
    vector<string> phrase_hybrid_function;
    vector<float> times_hybrid_function;
    vector<string> phrase_discriminatory_iteration;
    vector<float> times_discriminatory_iteration;
    vector<string> phrase_greedy_fuction;
    vector<float> times_greedy_fuction;
    vector<string> phrase_aleatory_iteration;
    vector<float> times_aleatory_iteration;
    vector<string> phrase_aleatory_generation;
    vector<float> times_aleatory_generation;

};

// Funcion para manejo de datos masivos
void test_mode() {

    // Separar sincronizacion entre streams narrow y wide // Solo usar a la hora de copilar en g++ en linux
    // ios::sync_with_stdio(false);
    // wcin.tie(nullptr);
    // wcout.tie(nullptr);

    wcout << L"\n+----------------------------+\n"
             "|   Bienvenido a test mode   |\n"
             "+----------------------------+\n" << endl;

    // Solicitar ruta del CSV
    wcout << L"\nIngrese la dirección del CSV (ejemplo: C:\\Users\\antho\\Documents\\UCR\\CA0204\\Proyecto_Individual\\datos\\frases_test.csv)\n"
             L"o escriba 'exit' para salir\n> " << flush;

    // Variables auxiliares
    wstring wfile; 
    string file;
    ifstream f;

    while (true) {

        getline(wcin, wfile); // Tomar entrada

        if (wfile == L"exit") return; // Salida de lector

        file.assign(wfile.begin(), wfile.end());// Convertir wstring a string UTF-8

        if (!file.empty() && file[0] == '~') { // Si el mensaje no es vacio y se esta en un entorno de UNIX o LINUX
            
            const char* home = getenv("HOME"); // Buscar la ruta de usuario

            if (home){ // Si no es nula
                
                file = string(home) + file.substr(1); // Se copia para la busqueda del archivo quitando "~"
            
            }
            else {

                wcout << L"Error: no se encuentra el archivo, intente nuevamente.\n> ";

                continue; // Repetir el loop y saltarse lo siguiente

            }
        }

        f.open(file); // Abrir la direccion 

        if (f.is_open()) break; // Si esta abierto salir del loop

        wcout << L"Error: " << std::strerror(errno) // Sino dar senal de error, no quitar el "std::" puede dar problemas con los llamados
              << L"\nVerifique la ruta y escribala de nuevo, o escriba exit para salir: > " << flush;
        f.close(); // Cerrar las direcciones abiertas

    }

    vector<string> phrases; // Vector de la frases a muestrear
    string line; 

    while (getline(f, line)) { // Mientras existan lineas por leer

        if (!line.empty()) { phrases.push_back(line); } // Si no son vacias, se copian en el vector phrases
    
    }

    f.close(); // Se cierra el archivo

    using password_func = void(*)(string&); // Puntero para el llamado de funciones del tipo void

    // Lista de punteros a funciones
    vector<password_func> methods = {

        // genera::nary_password,
        genera::hybrid_function,
        genera::discriminatory_iteration,
        genera::greedy_fuction,
        genera::aleatory_iteration,
        genera::aleatory_generation

    };

    // Inicializar contenedor de resultados
    data_password results;
    results.phrase = phrases;

        // Procesamiento de frases
    for (const auto& phrase_orig : phrases) { // Por cada frase en el vecto

        string phrase = phrase_orig; // Se copia la frase
        genera::clean_words(phrase); // Se limpia
        wcout << L"\nProcesando frase..." << endl; 

        for (size_t i = 0; i < methods.size(); ++i) { // Por cada uno de los metodos

            string phrase_copy = phrase; // Se genera una copia 
            auto start = chrono::high_resolution_clock::now(); // Se inica el contador

            // Se ejecuta el metodo
            methods[i](phrase_copy);

            auto end = chrono::high_resolution_clock::now(); // Se termina el contador
            double duration_s = chrono::duration<double>(end - start).count(); // Se toma la diferencia

            // Guardar los resultados para cada metodo (orden ya corregido)
            switch (i) {
                /*
                case 0:

                    wcout << L"\nnary_password..." << endl;
                    results.phrase_nary_password.push_back(phrase_copy);
                    results.times_nary_password.push_back(duration_ms);

                    break;
                   */ 
                case 0:

                    wcout << L"\nhybrid_function..." << endl; 
                    results.phrase_hybrid_function.push_back(phrase_copy);
                    results.times_hybrid_function.push_back(duration_s);

                    break;

                case 1:

                    wcout << L"\ndiscriminatory_iteration..." << endl; 
                    results.phrase_discriminatory_iteration.push_back(phrase_copy);
                    results.times_discriminatory_iteration.push_back(duration_s);

                    break;

                case 2:

                    wcout << L"\ngreedy_fuction..." << endl;
                    results.phrase_greedy_fuction.push_back(phrase_copy);
                    results.times_greedy_fuction.push_back(duration_s);

                    break;

                case 3:

                    wcout << L"\naleatory_iteration..." << endl;
                    results.phrase_aleatory_iteration.push_back(phrase_copy);
                    results.times_aleatory_iteration.push_back(duration_s);

                    break;

                case 4:

                    wcout << L"\naleatory_generation..." << endl; 
                    results.phrase_aleatory_generation.push_back(phrase_copy);
                    results.times_aleatory_generation.push_back(duration_s);

                    break;

            }
        }
    }

    // Exportar resultados a CSV
    string base_file = file.substr(0, file.find_last_of('.')) + "_claves_C++_";
    int i = 1;
    string output_file;

    while (true) { 

        output_file = base_file + to_string(i) + ".csv"; // Buscar el archivo de la forma nombre de archiv_claves_n.csv
        ifstream check(output_file);

        if (!check.is_open()) break; // Si no existe, usar este nombre

        ++i; // Si existe ir al siguiente n

    }

    ofstream out(output_file);

    if (!out.is_open()) {

        wcerr << L"Error al crear el archivo de salida: " << output_file.c_str() << endl;

        return;

    }

    // Encabezado CSV corregido segun el orden de metodos
    out << "Frase,"
        //<< "NaryPassword,Tiempo_Nary,"
        << "HybridFunction,Tiempo_Hybrid,"
        << "DiscriminatoryIteration,Tiempo_Discriminatory,"
        << "GreedyPassword,Tiempo_Greedy,"
        << "AleatoryIteration,Tiempo_AleatoryIter,"
        << "AleatoryGeneration,Tiempo_AleatoryGen\n";

    // Escribir resultados en el mismo orden
    for (size_t i = 0; i < results.phrase.size(); ++i) {

        out << "\"" << results.phrase[i] << "\",";

        //out << "\"" << results.phrase_nary_password[i] << "\"," << results.times_nary_password[i] << ",";
        out << "\"" << results.phrase_hybrid_function[i] << "\"," << results.times_hybrid_function[i] << ",";
        out << "\"" << results.phrase_discriminatory_iteration[i] << "\"," << results.times_discriminatory_iteration[i] << ",";
        out << "\"" << results.phrase_greedy_fuction[i] << "\"," << results.times_greedy_fuction[i] << ",";
        out << "\"" << results.phrase_aleatory_iteration[i] << "\"," << results.times_aleatory_iteration[i] << ",";
        out << "\"" << results.phrase_aleatory_generation[i] << "\"," << results.times_aleatory_generation[i];
        out << "\n";

    }


    out.close();
    wcout << L"\nResultados guardados en: " << output_file.c_str() << endl;

}

// Funcion de interface de manejo
void word_to_password() {

    bool condition = true; // Condicion auxiliar
    wcout << "Bienvenido al convertidor de frases en claves\n" << endl;

    while (condition) {

        wcout << "\nIntroduzca:\n" << endl;
        wcout << "\"0\": para utilizar la busqueda de arbol n-ario completo\n" << endl;
        wcout << "\"1\": para utilizar la funcion hibrida\n" << endl;
        wcout << "\"2\": para utilizar la generacion excluyente aleatoria\n" << endl;
        wcout << "\"3\": para utilizar la busqueda greedy\n" << endl;
        wcout << "\"4\": para utilizar la generacion aleatoria de escogencia iterada\n" << endl;
        wcout << "\"5\": para utilizar la generacion totalmente aleatoria\n" << endl;
	    wcout << "\"6\": para realizar pruebas\n" << endl;
        wcout << "\"7\": para salir\n" << endl;

        int method;

        while (!(cin >> method) || method < 0 || method > 7) { // Mientras la opcion no sea un entero

	        wcout << "Introduzca solo opciones validas por favor: " << endl;
            cin.clear(); // Resetea los flags de error
            cin.ignore(numeric_limits<streamsize>::max(), '\n');  // Ignora caracteres hasta un cambio de linea o enter
        }

        cin.ignore(numeric_limits<streamsize>::max(), '\n');

        string phrase;

        if (method < 6) { // Si el metodo no es con variedad de datos

            wcout << "Introduzca la frase a convertir: " << endl;
            getline(cin, phrase); // Se toma la frase unica a probar
            genera::clean_words(phrase); // Se limpia la frase antes de enviarse

        }

        switch (method) {

            case 0:

                genera::nary_password(phrase); // Llamadas por referencia, modifican los datos

                break;

            case 1:
            
                genera::hybrid_function(phrase); 

                break;

            case 2:

                genera::discriminatory_iteration(phrase);

                break;

            case 3:

                genera::greedy_fuction(phrase);

                break;

            case 4:

                genera::aleatory_iteration(phrase);

                break;

	        case 5:

                genera::aleatory_generation(phrase);

                break;

            case 6:

                test_mode();

                break;

            case 7:

                condition = false;

                break;

            default:

                wcout << "Error fatal de codigo\n";

                break;

        }

        if (method < 6) { cout << "La frase convertida es: " << phrase << endl;  }

    }

    wcout << "Hasta el ∞ ^ + ->\n" << endl;


}

// main()
int main() {

    SetConsoleOutputCP(CP_UTF8);
    SetConsoleCP(CP_UTF8);      
    word_to_password(); 

    return 0;

}