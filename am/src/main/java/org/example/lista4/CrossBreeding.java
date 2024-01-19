package org.example.lista4;

import org.example.Conv;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.SplittableRandom;

public class CrossBreeding {
    public static List<Individual> singlePoint(Individual p1, Individual p2, int V, int[][] distMatrix){
        SplittableRandom random = new SplittableRandom();
        int x1 = random.nextInt(0, V);
        List<Integer> p1c = p1.getConv();
        List<Integer> p2c = p2.getConv();

        List<Integer> c1 = new ArrayList<>(p1c.subList(0, x1));
        List<Integer> c2 = new ArrayList<>(p2c.subList(0, x1));
        HashSet<Integer> c1_contains = new HashSet<>(c1);
        HashSet<Integer> c2_contains = new HashSet<>(c2);

        for(int x: p2c){
            if(!c1_contains.contains(x)){
                c1.add(x);
                c1_contains.add(x);
            }
        }
        c1.add(c1.get(0));

        for(int x: p1c){
            if(!c2_contains.contains(x)){
                c2.add(x);
                c2_contains.add(x);
            }
        }
        c2.add(c2.get(0));

        Individual child1 = new Individual(c1, Conv.calcWeight(distMatrix,c1));
        Individual child2 = new Individual(c2, Conv.calcWeight(distMatrix,c2));

        List<Individual> children = new ArrayList<>();
        children.add(child1);
        children.add(child2);
        return children;
    }
}
