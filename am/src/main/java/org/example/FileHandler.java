package org.example;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class FileHandler {
    List<String> data;
    public FileHandler(String filename) throws IOException {
        Path path = Paths.get(filename).toAbsolutePath();
        this.data = Files.readAllLines(path);
        this.data = data.subList(8,data.size()-1);
    }

    public Graph makeGraph(){
        Graph g = new Graph();
        for(String s: this.data){
            int x = Integer.parseInt(s.split(" ")[1]);
            int y = Integer.parseInt(s.split(" ")[2]);
            g.addNode(x,y);
        }
        return g;
    }
}
