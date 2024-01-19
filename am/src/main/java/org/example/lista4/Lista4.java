package org.example.lista4;

import org.example.FileHandler;
import org.example.Graph;

import java.io.IOException;

public class Lista4 {
    public static void run() throws IOException, InterruptedException {
        String filename = "bcl380.tsp";
        FileHandler fh = new FileHandler("data/" + filename);
        Graph g = fh.makeGraph();
        int[][] distMatrix = g.calcDistMatrix();
        int V = g.getNodes().size();


        int w = PopulationSimulation.run(distMatrix,V);
        System.out.println(w);
    }
}
