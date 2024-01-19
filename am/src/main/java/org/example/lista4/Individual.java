package org.example.lista4;

import org.example.LocalSearch;

import java.util.List;

public class Individual implements Comparable<Individual>{
    private List<Integer> conv;
    private int weight;
    public Individual(List<Integer> conv, int weight){
        this.conv = conv;
        this.weight = weight;
    }

    public void mutation(int[][] distMatrix){
        int size = conv.size();
        int prev_weight = this.weight;
        this.conv.remove(size-1);
        this.weight = LocalSearch.localSearchFast(this.conv,distMatrix,size-1,prev_weight)[1];
    }

    public List<Integer> getConv(){
        return this.conv;
    }
    public int getWeight() { return this.weight; }

    @Override
    public int compareTo(Individual o) {
        return this.weight -o.getWeight();
    }
}
