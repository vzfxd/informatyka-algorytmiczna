import org.example.Entropy;
import org.example.FileHandler;
import org.junit.Test;

import java.io.IOException;

public class EntropyTest{
    public void testEntropy(String filename) throws IOException{
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

    @Test
    public void testMain() throws IOException{
        testEntropy("pan-tadeusz-czyli-ostatni-zajazd-na-litwie.txt");
        testEntropy("test1.bin");
        testEntropy("test2.bin");
        testEntropy("test3.bin");
    }
}
