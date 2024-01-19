package org.example.lista4;

import org.example.Conv;

import java.util.*;

public class PopulationSimulation{
    private static List<Individual> tournament(List<Individual> population, int crossovers) {
        List<Individual> parents = new ArrayList<>();

        List<Individual> selected = new ArrayList<>(population);
        Collections.shuffle(selected);
        selected = selected.subList(0, crossovers);

        while (parents.size() != 2) {
            Individual parent = selected.stream()
                    .min(Comparator.comparingInt(Individual::getWeight))
                    .orElse(null);
            parents.add(parent);
            selected.remove(parent);
        }

        return parents;
    }
    public static int run(int[][] distMatrix, int V){
        double mutation_chance = 0.1;
        double crossover_chance = 0.9;
        int crossovers = V/5;

        List<Individual> population = new ArrayList<>();
        for(int i=0; i<V; i++){
            List<Integer> conv = Conv.getRandomConv(V);
            int w = Conv.calcWeight(distMatrix,conv);
            population.add(new Individual(conv,w));
        }
        Collections.sort(population);
        int best_w = population.get(0).getWeight();

        Random random = new Random();
        int without_imp = 0;
        while(without_imp<V){
            //Krzyzowanie
            List<Individual> children_all = new ArrayList<>();
            for(int i=0; i<crossovers; i++){
                if(random.nextDouble() < crossover_chance){
                    List<Individual> parents = tournament(population,crossovers);
                    List<Integer> p1 = parents.get(0).getConv();
                    List<Integer> p2 = parents.get(1).getConv();
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
            population = new ArrayList<>(population.subList(0,V));
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
