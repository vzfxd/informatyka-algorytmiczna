#include <cstdint>
#include <fstream>
#include <cmath>
#include <iostream>
#include <filesystem>
#include "entropy.h"

static uint64_t precision = 32;
static uint64_t whole = pow(2,precision);
static uint64_t half = whole / 2;
static uint64_t quart = whole / 4;
static bool eof = false;

int readBit(uint8_t* variable,uint8_t* bit_position, std::ifstream& source){
    int bit = (*variable >> *bit_position) & 1;
    if(*bit_position == 0){
        if(!(source >> std::noskipws >> *variable)) eof = true;
        *bit_position = 7;
    }else{
        *bit_position -= 1;
    }
    return bit;
}

int main(int argc, char* argv[]) {
    std::ifstream source;
    source.open(argv[1]);

    std::ofstream output;
    output.open(argv[2]);
    
    uint64_t low = 0;
    uint64_t high = whole;
    uint64_t z = 0;
    
    uint8_t c = 0;
    uint8_t bitIdx = 7;
    uint8_t currByte = 0;

    source >> std::noskipws >> currByte;
    
    uint64_t i = 1;
    int totalFreq = 256;
    int charFreq[256];
    for (int i = 0; i < 256; i++) {
        charFreq[i] = 1;
    }
    
    while(i<= precision and !eof){
        if(readBit(&currByte,&bitIdx,source)){
            z += pow(2,precision-i);  
        }
        i++;
    }
    
    while(!eof){
        for(int symbol=0; symbol<256; symbol++){
            uint64_t dist = high - low;
            int f = 0;
            for(int i=0; i<symbol; i++){
                f += charFreq[i];
            }
            uint64_t b = low + llround(dist * ((double)(f+charFreq[symbol])/totalFreq));
            uint64_t a = low + llround(dist * ((double)f/totalFreq));
            
            if(a <= z && z < b){
                c = symbol;
                totalFreq++;
                charFreq[symbol]++;
                output << c;
                low = a;
                high = b;
                break;
            }
        }

        while(true){
            if(high < half){
                
            }else if(low > half){
                low -= half;
                high -= half;
                z -= half;
            }else if(low > quart && high < 3 * quart){
                low -= quart;
                high -= quart;
                z -= quart;
            }
            else{
                break;
            }

            low *= 2;
            high *= 2;
            z *= 2;

            if(!eof && readBit(&currByte,&bitIdx,source)){
                z += 1;
            }
            
        }   
    }
    source.close();
    output.close();
    return 0;
}