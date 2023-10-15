package org.example;

import java.io.*;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

public class FileHandler{
    private int[] charFreq;
    private int[][] condCharFreq;
    private int noSymbols;
    public FileHandler(String filename) throws IOException{
        Path path = Paths.get(Objects.requireNonNull(getClass().getClassLoader().getResource(filename)).getPath());
        byte[] data = Files.readAllBytes(path);
        getData(data);
    }

    private void getData(byte[] data) throws IOException {
        int lim = 256;
        int[][] condCharFreq = new int[lim][lim];
        int[] charFreq = new int[lim];
        int currChar;
        int prevChar = 0;
        int noSymbols = 0;
        for(byte b: data){
            currChar = Byte.toUnsignedInt(b);
            charFreq[currChar]++;
            condCharFreq[prevChar][currChar]++;
            prevChar = currChar;
            noSymbols++;
        }
        this.condCharFreq = condCharFreq;
        this.charFreq = charFreq;
        this.noSymbols = noSymbols;
    }

    public int getNoSymbols(){
        return this.noSymbols;
    }

    public int[] getCharFreq(){
        return this.charFreq;
    }

    public int[][] getCondCharFreq(){
        return this.condCharFreq;
    }
}
