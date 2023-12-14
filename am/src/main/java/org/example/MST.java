package org.example;

import com.github.sh0nk.matplotlib4j.Plot;
import com.github.sh0nk.matplotlib4j.PythonExecutionException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class MST {
    private final int V;

    public MST(int V){
        this.V = V;
    }

    public void drawMST(List<Node> nodes, int[] parent, String title) throws PythonExecutionException, IOException {
        Plot plt = Plot.create();
        List<Double> x = new ArrayList<>();
        List<Double> y = new ArrayList<>();

        for (int i = 0; i < parent.length; i++) {
            if (parent[i] != -1) {
                x.clear();
                y.clear();

                x.add((double) nodes.get(parent[i]).getX());
                y.add((double) nodes.get(parent[i]).getY());
                x.add((double) nodes.get(i).getX());
                y.add((double) nodes.get(i).getY());

                plt.plot().add(x, y, "r-");
                plt.plot().add(x, y,"go");
            }
        }

        plt.title(title);
        plt.savefig("sprawozdanie1/"+title+".png");
        plt.executeSilently();
    }

    private int minKey(int[] key, Boolean[] mstSet) {
        int min = Integer.MAX_VALUE;
        int minIndex = -1;

        for (int v = 0; v < V; v++){
            if (!mstSet[v] && key[v] < min) {
                min = key[v];
                minIndex = v;
            }
        }
        return minIndex;
    }

    public int[] primMST(int[][] distMatrix){
        int[] parent = new int[V];
        int[] key = new int[V];

        Boolean[] mstSet = new Boolean[V];

        for (int i = 0; i < V; i++){
            key[i] = Integer.MAX_VALUE;
            mstSet[i] = false;
        }

        key[0] = 0;
        parent[0] = -1;

        for (int i = 0; i < V - 1; i++){
            int u = minKey(key, mstSet);
            mstSet[u] = true;
            for (int v = 0; v < V; v++){
                if (!mstSet[v] && distMatrix[u][v] < key[v]){
                    parent[v] = u;
                    key[v] = distMatrix[u][v];
                }
            }
        }
        return parent;
    }

    public int calcWeight(int[] parent, int[][] distMatrix){
        int weight = 0;
        for (int i = 1; i < V; i++){
            weight += distMatrix[i][parent[i]];
        }
        return weight;
    }
}
