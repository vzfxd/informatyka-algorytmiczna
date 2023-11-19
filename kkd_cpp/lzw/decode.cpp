
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <string>
#include <filesystem>
#include <bitset>
#include "entropy.h"
#include "universal_code.h"

int main(int argc, char* argv[]){
    std::ifstream source;
    source.open(argv[1]);
    std::vector<int> input;

    std::ofstream output;
    output.open(argv[2]);

    uint8_t curr_char;
    std::string buffer;

    int dict_size = 256;
    std::unordered_map<int,std::string> dict;
    for(int i=0; i<dict_size; i++){
        dict[i] = char(i);
    }

    std::vector<int>(*decode)(std::string) = decode_omega;

    while(source >> std::noskipws >> curr_char){
        buffer.append(std::bitset<8>(curr_char).to_string());
    }

    input = decode(buffer);
    std::string n;
    std::string o(1,input[0]);
    output << o;
    input.erase(input.begin());

    for(int code: input){
        if(dict.find(code) != dict.end()){
            n = dict[code];
        }else{
            n = o + o[0];
        }
        dict[dict_size++] = o + n[0];
        o = n;

        output << n;
    }

    source.close();
    output.close();
}