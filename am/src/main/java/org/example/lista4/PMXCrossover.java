package org.example.lista4;

import java.util.*;

public class PMXCrossover {
    public static List<List<Integer>> crossover(List<Integer> parent1, List<Integer> parent2) {
        int size = parent1.size();
        List<Integer> child1 = new ArrayList<>(parent1);
        List<Integer> child2 = new ArrayList<>(parent2);
        child1.remove(child1.size()-1);
        child2.remove(child2.size()-1);
        HashMap<Integer,Integer> map_child1 = new HashMap<>();
        HashMap<Integer,Integer> map_child2 = new HashMap<>();

        Random rand = new Random();
        int startPoint = rand.nextInt(0,size - 2);
        int endPoint = rand.nextInt(startPoint+1,size-1);

        for (int i = startPoint; i <= endPoint; i++) {
            int val1 = child1.get(i);
            int val2 = child2.get(i);
            child1.set(i, val2);
            child2.set(i, val1);
            map_child1.put(val2,val1);
            map_child2.put(val1,val2);
        }

        for(int i=0; i<startPoint; i++){
            int key1 = child1.get(i);
            int key2 = child2.get(i);
            while(map_child1.containsKey(key1)){
                key1 = map_child1.get(key1);
                child1.set(i,key1);
            }
            while(map_child2.containsKey(key2)){
                key2 = map_child2.get(key2);
                child2.set(i,key2);
            }
        }

        for(int i=endPoint+1; i<child1.size(); i++){
            int key1 = child1.get(i);
            int key2 = child2.get(i);
            while(map_child1.containsKey(key1)){
                key1 = map_child1.get(key1);
                child1.set(i,key1);
            }
            while(map_child2.containsKey(key2)){
                key2 = map_child2.get(key2);
                child2.set(i,key2);
            }
        }

        List<List<Integer>> children = new ArrayList<>();
        child1.add(child1.get(0));
        child2.add(child2.get(0));
        children.add(child1);
        children.add(child2);
        return children;
    }
}