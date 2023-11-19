#ifndef ENTROPY_H  
#define ENTROPY_H

#include <tgmath.h>

double getEntropy(int charFreq[], int totalFreq){
    double ent = 0;
    for(int c = 0; c<256; c++){
        int freq = charFreq[c];
        if(charFreq[c] == 0) continue;
        double p = (double) freq/totalFreq;
        double info = -log2(p);
        ent += p * info;
    }
    return ent;
}

#endif