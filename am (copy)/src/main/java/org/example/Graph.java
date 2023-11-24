package org.example;

import java.util.ArrayList;
import java.util.List;

public class Graph {
    private final List<Node> nodes;
    public Graph(){
        this.nodes = new ArrayList<>();
    }

    public List<Node> getNodes() {
        return nodes;
    }


    public static List<List<Integer>> getAdjList(int[] parent){
        List<List<Integer>> adjList = new ArrayList<>();

        for(int i=0; i<parent.length; i++){
            adjList.add(new ArrayList<>());
        }

        for(int i=0; i<parent.length; i++){
            int v = parent[i];
            if(v == -1) continue;
            adjList.get(i).add(v);
            adjList.get(v).add(i);
        }

        return adjList;
    }
    public void addNode(int x, int y){
        Node node = new Node(x,y);
        nodes.add(node);
    }

    public int[][] calcDistMatrix(){
        int V = nodes.size();
        int[][] distMatrix = new int[V][V];
        for(int i=0; i<V; i++){
            for(int j=0; j<V; j++){
                Node n1 = this.nodes.get(i);
                Node n2 = this.nodes.get(j);
                distMatrix[i][j]  = n1.calcDist(n2);
            }
        }
        return distMatrix;
    }
}
