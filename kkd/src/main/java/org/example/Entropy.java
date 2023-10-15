package org.example;

public class Entropy {
    public double getEntropy(int[] charFreq, int noSymbols){
        double ent = 0;
        for (int freq : charFreq) {
            if(freq == 0) continue;
            double probability = (double) freq / noSymbols;
            double info = -(Math.log(probability) / Math.log(2));
            ent += probability * info;
        }
        return ent;
    }

    public double getCondEntropy(int[][] condCharFreq, int[] charFreq, int noSymbols){
        double ent = 0;
        for(int i=0; i<charFreq.length; i++){
            int freq = charFreq[i];
            if(freq == 0) continue;
            double p1 = (double) freq / noSymbols;
            double sum = 0;
            for(int j=0; j<condCharFreq.length; j++){
                int condFreq = condCharFreq[i][j];
                if(condFreq == 0) continue;
                double p2 = (double) condFreq / freq;
                double info = -(Math.log(p2)/Math.log(2));
                sum += p2 * info;
            }
            ent += p1 * sum;
        }
        return ent;
    }

    public double getEntropyDiff(double ent, double condEnt){
        return Math.abs(ent - condEnt);
    }
}
