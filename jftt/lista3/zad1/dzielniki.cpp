#include <iostream>
#include <cmath>

void wypiszDzielniki(int liczba) {
    std::cout << "Dzielniki liczby " << liczba << ": ";
    
    for (int i = 1; i <= sqrt(liczba); ++i) {
        if (liczba % i == 0) {
            std::cout << i << " ";
            
            // Jeśli i nie jest pierwiastkiem kwadratowym, to dodaj także drugi dzielnik
            if (i != liczba / i) {
                std::cout << liczba / i << " ";
            }
        }
    }
    
    std::cout << std::endl;
}

int main() {
    int liczba = 1234576;
    
    wypiszDzielniki(liczba);
    
    return 0;
}