package org.example.lista4;

import org.example.LocalSearch;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class Individual implements Comparable<Individual>{
    private List<Integer> conv;
    private int weight;
    public Individual(List<Integer> conv, int weight){
        this.conv = conv;
        this.weight = weight;
    }

    public void mutation(int[][] distMatrix){
        int size = conv.size();
        this.conv.remove(size-1);

        int i = ThreadLocalRandom.current().nextInt(0, size-2);
        int j = ThreadLocalRandom.current().nextInt(i+1, size-1);

        this.weight = LocalSearch.weightAfterInvert(this.conv,i,j,distMatrix,this.weight);
        LocalSearch.invert(this.conv,i,j);

        this.conv.add(conv.get(0));
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
