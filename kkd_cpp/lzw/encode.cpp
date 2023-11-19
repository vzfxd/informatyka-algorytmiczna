
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <string>
#include <filesystem>
#include "entropy.h"
#include "universal_code.h"

void save_buffer_to_file(std::string buffer, std::ofstream& output, int output_char_freq[]){
    int size = buffer.size();

    if (size % 8 != 0){
        int rest = 8 - (size % 8);
        buffer.append(rest, '0');
        size += rest;
    }

    for (int i = 0; i < size; i += 8){
        uint8_t val = bin_to_dec(buffer.substr(i, 8));
        output << val;
        output_char_freq[val]++;
    }
}

int main(int argc, char* argv[]){
    int input_char_freq[256];
    int output_char_freq[256];
    for(int i=0; i<256;i++){
        input_char_freq[i] = 0;
        output_char_freq[i] = 0;
    }
   
    int file_len = 0;
    std::ifstream source;
    source.open(argv[1]);

    std::ofstream output;
    output.open(argv[2]);

    std::string buffer;

    uint8_t curr_char = 0;
    std::string found_chars = "";

    int dict_size = 256;
    std::unordered_map<std::string,int> dict;
    for(int i=0; i<dict_size; i++){
        std::string s(1,char(i));
        dict[s] = i;
    }

    std::string(*encode)(int) = encode_omega;

    while(source >> std::noskipws >> curr_char){
        std::string char_string(1,curr_char);
        std::string chars_to_add = found_chars + char_string;

        if(dict.find(chars_to_add) != dict.end()){
            found_chars = chars_to_add;
        }else{
            buffer.append(encode(dict[found_chars]));
            dict[chars_to_add] = dict_size;
            dict_size++;
            found_chars = char_string;
        }

        file_len++;
        input_char_freq[curr_char]++;
    }

    if(found_chars.length()>0){
        buffer.append(encode(dict[found_chars]));
    }
    
    save_buffer_to_file(buffer,output,output_char_freq);

    source.close();
    output.close();

    double cr = double(std::filesystem::file_size(argv[1])) / std::filesystem::file_size(argv[2]);
    int code_len = buffer.size() / 8;

    std::cout << "Source file length:" << file_len << std::endl;
    std::cout << "Code length: " << code_len << std::endl;
    std::cout << "Compression rate: " << cr << std::endl;
    std::cout << "Source file entropy: " << getEntropy(input_char_freq,file_len) << std::endl;
    std::cout << "Code Entropy: " << getEntropy(output_char_freq,code_len) << std::endl;
}