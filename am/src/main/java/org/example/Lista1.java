package org.example;

import com.github.sh0nk.matplotlib4j.PythonExecutionException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;


public class Lista1 {
    public void run() throws IOException, PythonExecutionException {
        Path path = Paths.get("data").toAbsolutePath();
        File[] files = new File(path.toUri()).listFiles();
        assert files != null;
        for(File f: files) {
            String filename = f.getName();
            FileHandler fh = new FileHandler("data/" + filename);
            Graph g = fh.makeGraph();
            int[][] distMatrix = g.calcDistMatrix();

            MST mst = new MST(g.getNodes().size());
            int[] parent = mst.primMST(distMatrix);
            int mstWeight = mst.calcWeight(parent, distMatrix);

            Conv conv = new Conv(parent);
            int convWeight = conv.calcWeight(distMatrix, conv.getConv());

            int minTotal = Integer.MAX_VALUE;
            int a = Integer.MAX_VALUE;
            int aTotal = Integer.MAX_VALUE;
            int b = Integer.MAX_VALUE;
            int bTotal = Integer.MAX_VALUE;

            for(int i=0; i<1000; i++){
                int randWeight = conv.calcWeight(distMatrix,conv.getRandomConv());
                if(i%10 == 0){
                    aTotal += a;
                    a = Integer.MAX_VALUE;
                }
                if(i%50 == 0){
                    bTotal += b;
                    b = Integer.MAX_VALUE;
                }
                if(randWeight<minTotal) minTotal = randWeight;
                if(randWeight<a) a = randWeight;
                if(randWeight<b) b = randWeight;
            }


//            conv.drawConv(g.getNodes(),conv.getConv(),"conv_"+filename);
//            mst.drawMST(g.getNodes(),parent,"mst_"+filename);
            System.out.println("tsp:"+filename);
            System.out.println("mst weight:"+mstWeight);
            System.out.println("conv weight:"+convWeight);
            System.out.println("(a):"+aTotal/100);
            System.out.println("(b):"+bTotal/20);
            System.out.println("(c):"+minTotal);
            System.out.print("\n\n");
        }
    }
}
