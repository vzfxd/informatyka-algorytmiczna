package org.example;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

public class Lista3 {
    public static void tabu(String filename) throws IOException {
        FileHandler fh = new FileHandler("data/" + filename);
        Graph g = fh.makeGraph();
        int[][] distMatrix = g.calcDistMatrix();

        int V = g.getNodes().size();

        MST mst = new MST(V);
        int[] parent = mst.primMST(distMatrix);

        int total_w = 0;
        int min_w = Integer.MAX_VALUE;
        int max_iter = V*30;
        int iters = 10;

        System.out.print(filename + " ");
        for(int i=0; i<iters; i++){
            System.out.print(i + " ");
            int randomV = ThreadLocalRandom.current().nextInt(0, V);
            Conv conv = new Conv(parent,randomV);
            List<Integer> perm = conv.getConv();
            perm.remove(V);
            int w = LocalSearch.tabuSearch(perm,distMatrix,max_iter);

            total_w += w;
            if(w<min_w){
                min_w = w;
            }
        }

        String out_string = filename + " & ";
        out_string += total_w/iters + " & ";
        out_string += min_w + " & ";

        System.out.println("\n\n" + out_string + "\n\n");
    }

    public static void annealing(String filename) throws IOException {
        FileHandler fh = new FileHandler("data/" + filename);
        Graph g = fh.makeGraph();
        int[][] distMatrix = g.calcDistMatrix();

        int V = g.getNodes().size();

        int total_w = 0;
        int min_w = Integer.MAX_VALUE;

        double T = V;
        double deltaT = 0.95;
        int max_iter = (int)(V*0.3);
        int iters = 10;

        System.out.print(filename + " ");
        for(int i=0; i<iters; i++){
            System.out.print(i + " ");
            List<Integer> perm = Conv.getRandomConv(V);
            perm.remove(V);
            int w = LocalSearch.simulatedAnnealing(perm,distMatrix,T,deltaT,max_iter);

            total_w += w;
            if(w<min_w){
                min_w = w;
            }
        }

        String out_string = filename + " & ";
        out_string += total_w/iters + " & ";
        out_string += min_w + " & ";

        System.out.println("\n\n" + out_string + "\n\n");
    }
}
