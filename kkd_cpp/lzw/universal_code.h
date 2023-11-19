#ifndef UNIVERSAL_CODE_H  
#define UNIVERSAL_CODE_H

#include <string>
#include <vector>

std::string dec_to_bin(int n){
    std::string bit_string = "";
    while(n>0){
        bit_string.insert(0,std::to_string(n%2));
        n = n / 2;
    }
    return bit_string;
}

int bin_to_dec(std::string number){
    int result = 0;
    int size = number.size();

    for (int i = 0 ;i < size; i++) {
        result *= 2;
        result += int(number[i]) - 48;
    }

    return result;
}

std::string encode_omega(int n){
    std::string code = "0";
    std::string x;

    while (n > 1) {
        x = dec_to_bin(n);
        code.insert(0, x);
        n = int(x.size()) - 1;
    }

    return code;
}

std::vector<int> decode_omega(std::string code){
    std::vector<int> result;
    int size = code.size();
    int n;
    int i = 0;

    while (i < size) {
        n = 1;

        while (i < size && code[i] != '0') {
            i += n + 1;
            n = bin_to_dec(code.substr(i - n - 1, n + 1));
        }

        if (i <= size) {
            result.push_back(n);
            i++;
        }
    }

    return result;
}

std::string encode_fib(int n){

}

std::vector<int> decode_fib(std::string code){

}

std::string encode_gamma(int n) {
        std::string binary_part = dec_to_bin(n);
        int z = binary_part.length()-1;
        std::string zeros_part(z,'0');
        return zeros_part + binary_part;
}

std::vector<int> decode_gamma(std::string code){
        std::vector<int> result;
        int size = code.size();
        int zeroCount;
        int i = 0;

        while (i < size){
            zeroCount = 0;

            while (code[i] == '0'){
                zeroCount++;
                i++; 
            }

            int val_len = zeroCount + 1;
            int val = bin_to_dec(code.substr(i, val_len));
            result.push_back(val);
            i += val_len;
        }

        return result;
}

std::string encode_delta(int n){
    std::string x = dec_to_bin(n);
    int len = x.length();
    std::string len_gamma = encode_gamma(len);
    x.erase(0,1);
    return len_gamma + x;
}

std::vector<int> decode_delta(std::string code){
    std::vector<int> result;
    int size = code.size();
    int zeroCount;
    int i = 0;

    while (i < size) {
        zeroCount = 0;

        while (code[i] == '0') {
            zeroCount++;
            i++;
        }

        int len_len = zeroCount + 1;
        int len = bin_to_dec(code.substr(i, len_len)) - 1;
        i += len_len;
        if (i + len < size){
            result.push_back(bin_to_dec("1" + code.substr(i, len)));
        }
        i += len;
    }

    return result;
}

#endif