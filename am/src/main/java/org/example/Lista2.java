package org.example;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class Lista2 {
    public static void zad1() throws IOException {
        Path path = Paths.get("data").toAbsolutePath();
        File[] files = new File(path.toUri()).listFiles();
        assert files != null;
        for(File f: files) {
            String filename = f.getName();
            FileHandler fh = new FileHandler("data/" + filename);
            Graph g = fh.makeGraph();
            int[][] distMatrix = g.calcDistMatrix();

            int V = g.getNodes().size();
            int sqrt_v = (int)Math.sqrt(V);

            MST mst = new MST(V);
            int[] parent = mst.primMST(distMatrix);

            int steps = 0;
            int total_w = 0;
            int min_w = Integer.MAX_VALUE;

//            for(int i=0; i<sqrt_v; i++){
//                int randomV = ThreadLocalRandom.current().nextInt(0, V);
//                Conv conv = new Conv(parent,randomV);
//                List<Integer> perm = conv.getConv();
//                perm.remove(V);
//                int[] result = LocalSearch.localSearch(perm,distMatrix);
//                int s = result[0];
//                int w = result[1];
//                steps += s;
//                total_w += w;
//                if(w<min_w){
//                    min_w = w;
//                }
//            }

            String out_string = filename + " & ";
            out_string += mst.calcWeight(parent,distMatrix) + " & ";
            out_string += "0 & ";
            out_string += total_w/sqrt_v + " & ";
            out_string += steps/sqrt_v + " & ";
            out_string += min_w + " & ";

            System.out.println(out_string+"\n\n\n");
        }
    }

    public static void zad2(boolean fast) throws IOException {
        Path path = Paths.get("data").toAbsolutePath();
        File[] files = new File(path.toUri()).listFiles();
        assert files != null;
        for(File f: files) {
            String filename = f.getName();
            FileHandler fh = new FileHandler("data/" + filename);
            Graph g = fh.makeGraph();
            int[][] distMatrix = g.calcDistMatrix();

            int V = g.getNodes().size();
            int iters;
            if(V>1000){
                iters = 100;
            }else{
                iters = V;
            }

            int steps = 0;
            int total_w = 0;
            int min_w = Integer.MAX_VALUE;

//            for(int i=0; i<iters; i++){
//                List<Integer> perm =  Conv.getRandomConv(V);
//                int[] result;
//                perm.remove(V);
//                if(fast) {
//                    result = LocalSearch.localSearchFast(perm,distMatrix,V);
//                }else {
//                    result = LocalSearch.localSearch(perm,distMatrix);
//                }
//
//                int s = result[0];
//                int w = result[1];
//                steps += s;
//                total_w += w;
//                if(w<min_w){
//                    min_w = w;
//                }
//            }

            String out_string = "";
            out_string += total_w/iters + " & ";
            out_string += steps/iters + " & ";
            out_string += min_w + " & ";

            System.out.println(filename);
            System.out.println(out_string+"\n\n\n");
        }
    }

    public static void zad3() throws IOException {
            Lista2.zad2(true);
    }
}
