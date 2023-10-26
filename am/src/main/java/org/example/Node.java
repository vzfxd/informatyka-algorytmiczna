package org.example;

public class Node {
    private final int x;
    private final int y;

    public Node(int x, int y){
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public int calcDist(Node n){
        double x = Math.pow((this.x - n.getX()),2);
        double y = Math.pow((this.y - n.getY()),2);
        double dist = Math.sqrt(x+y);
        return (int) Math.floor(dist);
    }
}
