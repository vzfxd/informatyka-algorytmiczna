#include <cstdint>
#include <fstream>
#include <cmath>
#include <iostream>
#include <filesystem>
#include "entropy.h"

void tryEncodeByte(uint8_t* variable, uint8_t* bit_position, uint8_t setOrClear, std::ofstream& output){
    if(setOrClear) {
        *variable |= (1<<(*bit_position)); 
    }
    else {
        *variable &= ~(1<<(*bit_position));
    }
    if(*bit_position == 0){
        output << *variable;
        *bit_position = 7;
        *variable = 0;
    }else{
        *bit_position -= 1;
    }
}

static uint64_t precision = 32;
static uint64_t whole = pow(2,precision);
static uint64_t half = whole / 2;
static uint64_t quart = whole / 4;
    

int main(int argc, char* argv[]){
    uint64_t low = 0;
    uint64_t high = whole;

    uint8_t bit_position = 7;
    uint8_t encodedByte = 0;

    std::ifstream source;
    source.open(argv[1]);

    std::ofstream output;
    output.open(argv[2]);

    int licznik = 0;
    int totalFreq = 256;
    int charFreq[256];
    for(int i=0; i<256; i++){
        charFreq[i] = 1;
    }

    uint8_t c = 0;

    while(source >> std::noskipws >> c){
        int f = 0;
        for(int i=0; i<c; i++){
            f += charFreq[i];
        }

        uint64_t dist = high - low;
        high = low + llround(dist * ((double)(f + charFreq[c])/totalFreq));
        low = low + llround(dist * ((double)f/totalFreq));

        while(true){
            if(high < half){
                tryEncodeByte(&encodedByte,&bit_position,0,output);
                while(licznik > 0){
                    tryEncodeByte(&encodedByte,&bit_position,1,output);
                    licznik --;
                }
            }else if(low > half){
                low -= half;
                high -= half;

                tryEncodeByte(&encodedByte,&bit_position,1,output);
                while(licznik > 0){
                    tryEncodeByte(&encodedByte,&bit_position,0,output);
                    licznik --;
                }
            }else if(low > quart && high < 3*quart){
                low -= quart;
                high -= quart;
                licznik += 1;
            }else{
                break;
            }
            high *= 2;
            low *= 2;
        }
        totalFreq += 1;
        charFreq[c]++;
    }

    licznik += 1;
    if (low <= quart) {
        tryEncodeByte(&encodedByte,&bit_position,0,output);
        while(licznik > 0){
            tryEncodeByte(&encodedByte,&bit_position,1,output);
            licznik --;
        }
    }
    else {
        tryEncodeByte(&encodedByte,&bit_position,1,output);
        while(licznik > 0){
            tryEncodeByte(&encodedByte,&bit_position,0,output);
            licznik --;
        }
    }
    source.close();
    output.close();

    double cr = double(std::filesystem::file_size(argv[1])) / std::filesystem::file_size(argv[2]);
    
    totalFreq = totalFreq - 256;
    for(int i=0; i<256; i++){
        charFreq[i] -= 1;
    }

    std::cout << "Entropy:" << getEntropy(charFreq,totalFreq) << std::endl;
    std::cout << "Compression:" << cr << std::endl;
    std::cout << "AVG code length: " << 8.0/cr << std::endl;
}