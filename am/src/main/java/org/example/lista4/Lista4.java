package org.example.lista4;

import org.example.FileHandler;
import org.example.Graph;

import java.io.IOException;

public class Lista4 {
    public static void run() throws IOException, InterruptedException {
        String filename = "xql662.tsp";
        FileHandler fh = new FileHandler("data/" + filename);
        Graph g = fh.makeGraph();
        int[][] distMatrix = g.calcDistMatrix();
        int V = g.getNodes().size();
        int ov = 0;
        int min = Integer.MAX_VALUE;
        int r = 20;
        for(int i=0; i<r; i++){
            int w = PopulationSimulation.run(distMatrix,V);
            if(w<min){
                min = w;
            }
            ov += w;
            System.out.println("iteracja " + i);
            System.out.println(w);
        }

        System.out.println(ov/r);
        System.out.println(min);
    }
}
