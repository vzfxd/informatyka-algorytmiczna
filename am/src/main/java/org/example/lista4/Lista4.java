package org.example.lista4;

import org.example.Conv;
import org.example.FileHandler;
import org.example.Graph;

import java.io.IOException;
import java.util.*;

public class Lista4 {
    public static void run() throws IOException {
        String filename = "bcl380.tsp";
        FileHandler fh = new FileHandler("data/" + filename);
        Graph g = fh.makeGraph();
        int[][] distMatrix = g.calcDistMatrix();
        int V = g.getNodes().size();

        int r = 20;
        int overall = 0;
        int min = Integer.MAX_VALUE;

        for(int i=0; i<r; i++){
            int w = simulatePopulation(distMatrix,V);
            if(w<min){
                min = w;
            }
            overall += w;
            System.out.println(i);
        }

       System.out.println(min);
       System.out.println(overall/r);
    }

    private static List<Individual> tournament(List<Individual> population, int crossovers){
        List<Individual> parents = new ArrayList<>();
        while(parents.size() != 2){
            int min = Integer.MAX_VALUE;
            Individual parent = null;
            List<Individual> selected = new ArrayList<>(population);
            Collections.shuffle(selected);
            selected = new ArrayList<>(selected.subList(0,crossovers));
            for(Individual x: selected){
                if(x.getWeight() < min){
                    min = x.getWeight();
                    parent = x;
                }
            }
            if(!parents.contains(parent)){
                parents.add(parent);
            }
        }

        return parents;
    }

    private static int simulatePopulation(int[][] distMatrix, int V){
        int population_size = 100;
        double mutation_chance = 0.1;
        double crossover_chance = 0.9;
        int crossovers = population_size/5;

        List<Individual> population = new ArrayList<>();
        for(int i=0; i<population_size; i++){
            List<Integer> conv = Conv.getRandomConv(V);
            int w = Conv.calcWeight(distMatrix,conv);
            population.add(new Individual(conv,w));
        }
        Collections.sort(population);
        int best_w = population.get(0).getWeight();

        int without_imp = 0;
        while(without_imp<V){
            Random random = new Random();
            List<Integer> c = new ArrayList<>();
            for(int i=0; i<5; i++){
                for(int j=i+1; j<5; j++){
                    c.add(i);
                    c.add(j);
                }
            }
            //Krzyzowanie
            List<Individual> children_all = new ArrayList<>();
            for(int i=0; i<c.size()-1; i++){
                if(random.nextDouble() < crossover_chance){
                    List<Integer> p1 = population.get(c.get(i)).getConv();
                    List<Integer> p2 = population.get(c.get(i+1)).getConv();
                    List<List<Integer>> children = PMXCrossover.crossover(p1,p2);
                    for(List<Integer> child: children){
                        children_all.add(new Individual(child,Conv.calcWeight(distMatrix,child)));
                    }
                }
            }
            population.addAll(children_all);

            //Mutacja
            for(Individual x: population){
                if(random.nextDouble() < mutation_chance){
                    x.mutation(distMatrix);
                }
            }

            //Selekcja
            Collections.sort(population);
            population = new ArrayList<>(population.subList(0,population_size));
            int curr_best = population.get(0).getWeight();
            if(curr_best >= best_w){
                without_imp += 1;
            }else{
                best_w = curr_best;
                without_imp = 0;
            }
        }
        return best_w;
    }
}
