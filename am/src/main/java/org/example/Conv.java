package org.example;


import com.github.sh0nk.matplotlib4j.Plot;
import com.github.sh0nk.matplotlib4j.PythonExecutionException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

public class Conv {
    private final List<Integer> conv;
    public Conv(int[] parent){
        this.conv = calcConv(parent);
    }

    public List<Integer> getConv() {
        return conv;
    }

    public List<Integer> getRandomConv(){
        List<Integer> randConv = new ArrayList<>();
        for(int i=0; i<this.conv.size()-1;i++){
            randConv.add(i);
        }
        Collections.shuffle(randConv);
        randConv.add(randConv.get(0));
        return randConv;
    }

    public int calcWeight(int[][] distMatrix, List<Integer> conv){
        int weight = 0;
        for(int i=0; i<conv.size()-1; i++){
            weight += distMatrix[conv.get(i)][conv.get(i + 1)];
        }
        return weight;
    }

    public void drawConv(List<Node> nodes,List<Integer> conv, String title) throws IOException, PythonExecutionException {
        Plot plt = Plot.create();

        List<Double> x = new ArrayList<>();
        List<Double> y = new ArrayList<>();

        List<Double> startX = new ArrayList<>();
        List<Double> startY = new ArrayList<>();

        for(int n: conv){
            x.add((double)nodes.get(n).getX());
            y.add((double)nodes.get(n).getY());
        }

        startX.add(x.get(0));
        startY.add(y.get(0));

        plt.plot().add(x,y,"ro");
        plt.plot().add(startX,startY,"go");
        plt.plot().add(x,y).linestyle("-");

        plt.title(title);
        plt.savefig("sprawozdanie1/"+title+".png");
        plt.executeSilently();
    }

    private List<Integer> calcConv(int[] parent){
        int start = 0;
        int n = parent.length;
        boolean[] visited = new boolean[n];
        Stack<Integer> stack = new Stack<>();
        List<Integer> conv = new ArrayList<>();

        stack.push(start);
        visited[start] = true;

        while(!stack.isEmpty()){
            int v = stack.pop();
            conv.add(v);

            for (int neighbor = 0; neighbor < n; neighbor++){
                if (!visited[neighbor] && parent[neighbor] == v) {
                    visited[neighbor] = true;
                    stack.push(neighbor);
                }
            }
        }
        conv.add(start);
        return conv;
    }
}
