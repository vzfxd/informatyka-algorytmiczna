package org.example;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class LocalSearch {
    private static void invert(List<Integer> perm, int i, int j){
        while(i<j){
            int temp = perm.get(i);
            perm.set(i,perm.get(j));
            perm.set(j,temp);
            i++;
            j--;
        }
    }

    private static int weightAfterInvert(List<Integer> perm, int i, int j, int[][] distMatrix, int currWeight){
        int n = perm.size();

        if(i == 0 && j==n-1){
            return currWeight;
        }

        int i_prev = i - 1;
        int j_next = j + 1;
        int i_v = perm.get(i);
        int j_v = perm.get(j);
        int first_v = perm.get(0);
        int last_v = perm.get(n-1);
        int old_w = 0;
        int new_w = 0;
        int old_cycle = distMatrix[first_v][last_v];

        if(i != 0){
            int i_prev_v = perm.get(i_prev);
            new_w += distMatrix[i_prev_v][j_v];
            old_w += distMatrix[i_prev_v][i_v];
        }else{
            first_v = j_v;
        }

        if(j != n-1){
            int j_next_v = perm.get(j_next);
            new_w += distMatrix[j_next_v][i_v];
            old_w += distMatrix[j_next_v][j_v];
        }else{
            last_v = i_v;
        }

        int new_cycle = distMatrix[first_v][last_v];
        return currWeight - old_w + new_w + new_cycle - old_cycle;
    }

    private static int calcWeight(List<Integer> perm, int[][] distMatrix){
        int weight = 0;
        for(int i=0; i<perm.size()-1; i++){
            weight += distMatrix[perm.get(i)][perm.get(i + 1)];
        }
        return weight;
    }

    public static int[] localSearch(List<Integer> perm, int[][] distMatrix){
        int i_best = 0;
        int j_best = 0;
        int steps = 0;
        int n = perm.size();
        int w = calcWeight(perm,distMatrix) + distMatrix[perm.get(n-1)][perm.get(0)];
        while(true){
            steps++;
            int best_neighor_w = w;

            for(int i = 0; i<n; i++){
                for(int j = i+1; j<n; j++){
                    int curr_neighbor_w = weightAfterInvert(perm,i,j,distMatrix,w);
                    if(curr_neighbor_w < best_neighor_w){
                        best_neighor_w = curr_neighbor_w;
                        i_best = i;
                        j_best = j;
                    }
                }
            }

            if(best_neighor_w < w){
                invert(perm,i_best,j_best);
                w = best_neighor_w;
            }else{
                perm.add(perm.get(0));
                break;
            }
        }

        return new int[]{steps,w};
    }
    public static int[] localSearchFast(List<Integer> perm, int[][] distMatrix, int iters){
        int i_best = 0;
        int j_best = 0;
        int steps = 0;
        int n = perm.size();
        int w = calcWeight(perm,distMatrix) + distMatrix[perm.get(n-1)][perm.get(0)];
        while(true){
            steps++;
            int best_neighor_w = w;

            for (int k = 0; k < iters; k++) {
                int i = ThreadLocalRandom.current().nextInt(0, n-2);
                int j = ThreadLocalRandom.current().nextInt(i+1, n-1);;

                int curr_neighbor_w = weightAfterInvert(perm,i,j,distMatrix,w);
                if(curr_neighbor_w < best_neighor_w){
                    best_neighor_w = curr_neighbor_w;
                    i_best = i;
                    j_best = j;
                }
            }

            if(best_neighor_w < w){
                invert(perm,i_best,j_best);
                w = best_neighor_w;
            }else{
                perm.add(perm.get(0));
                break;
            }
        }

        return new int[]{steps,w};
    }
}
