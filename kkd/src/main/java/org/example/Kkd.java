package org.example;

import java.io.IOException;

public class Kkd {
    public static void main(String[] args) throws IOException {
        String filename = args[0];
        FileHandler fileHandler = new FileHandler(filename);
        int noSymbols = fileHandler.getNoSymbols();
        int[] charFreq = fileHandler.getCharFreq();
        int[][] condCharFreq = fileHandler.getCondCharFreq();
        Entropy entropy = new Entropy();
        double ent = entropy.getEntropy(charFreq,noSymbols);
        double condEnt = entropy.getCondEntropy(condCharFreq,charFreq,noSymbols);
        System.out.println("\n\nTest File = " + filename);
        System.out.println("Entropy: " + ent);
        System.out.println("Cond entropy : " + condEnt);
        System.out.println("Entropy diff: " + entropy.getEntropyDiff(ent,condEnt));
    }
}
