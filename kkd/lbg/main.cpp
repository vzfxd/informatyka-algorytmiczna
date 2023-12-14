#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <random>
#include <algorithm>
#include <cstring>

using namespace std;

struct Pixel{
    uint8_t red;
    uint8_t green;
    uint8_t blue;
};

struct Node {
  Pixel pixel;
  Node* left;
  Node* right;
};

uint16_t pixel_dist(Pixel p1, Pixel p2){
    return abs(p1.red - p2.red) + abs(p1.green - p2.green) + abs(p1.blue - p2.blue);
}

void calc_centroid(vector<Pixel> pixels, Node* node){
    size_t r = 0;
    size_t g = 0;
    size_t b = 0;
    size_t s = pixels.size();

    if(s==0) return; 

    for(Pixel pixel: pixels){
        r += pixel.red;
        g += pixel.green;
        b += pixel.blue;
    }

    node->pixel = Pixel{(uint8_t)(r/s), (uint8_t)(g/s), (uint8_t)(b/s)};
}

void add_node(vector<Pixel> pixels, Node* node, uint8_t depth, uint8_t height){
    if(depth == height) return;

    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<int> distribution(-5, 5);
    int r,g,b;
    
    do{
        r = distribution(gen);
        g = distribution(gen);
        b = distribution(gen);
    }
    while(r==0 && g==0 && b==0);

    r = max(min(r + node->pixel.red, 255), 0);
    g = max(min(g + node->pixel.green, 255), 0);
    b = max(min(b + node->pixel.blue, 255), 0);

    node->left = new Node{node->pixel,  nullptr, nullptr};
    node->right = new Node{Pixel{(uint8_t)r,(uint8_t)g,(uint8_t)b}, nullptr, nullptr};

    vector<Pixel> l_v;
    vector<Pixel> r_v;
    for(Pixel pixel: pixels){
        if(pixel_dist(pixel,node->left->pixel) < pixel_dist(pixel,node->right->pixel)){
            l_v.push_back(pixel);
        }else{
            r_v.push_back(pixel);
        }
    }
    calc_centroid(l_v, node->left);
    calc_centroid(r_v, node->right);

    add_node(l_v, node->left, depth+1, height);
    add_node(r_v, node->right, depth+1, height);
}

Node* LBG(vector<Pixel> pixels, uint8_t colors){
    Node* root = new Node{Pixel{0,0,0}, nullptr, nullptr};
    calc_centroid(pixels,root);
    add_node(pixels,root,0,colors);
    return root;
}

Pixel find_closest_color(Pixel pixel, Node* root){
    while(root->left != nullptr && root->right != nullptr){
        if(pixel_dist(pixel,root->left->pixel) < pixel_dist(pixel,root->right->pixel)){
            root = root->left;
        }
        else{
            root = root->right;
        }
    }
    return root->pixel;
}

vector<Pixel> read_tga(string file_path, int* w, int* h, uint8_t* _header) {
    ifstream file(file_path);

    uint8_t header[18];
    file.read(reinterpret_cast<char*>(header), 18);
    memcpy(_header,header,18);

    uint16_t width = *reinterpret_cast<uint16_t*>(header + 12);
    uint16_t height = *reinterpret_cast<uint16_t*>(header + 14);
    *w = width;
    *h = height;

    size_t size = width * height * 3;
    vector<Pixel> img_data;

    uint8_t raw_data[size];
    file.read(reinterpret_cast<char*>(raw_data), size);

    for (size_t i = 0; i < size; i += 3) {
        Pixel pixel = {raw_data[i + 2], raw_data[i + 1], raw_data[i]};
        img_data.push_back(pixel);
    }

    file.close();

    return img_data;
}

void free_tree(Node* root){
    if(root == nullptr) return;
    free_tree(root->left);
    free_tree(root->right);
    delete root;
}

int main(int argc, char* argv[]) {
    string filename = argv[1];
    ofstream output(argv[2]);
    uint8_t colors = atoi(argv[3]);
    int width, height;
    uint8_t header[18];
    vector<Pixel> img_data = read_tga(filename, &width, &height, header);

    Node* root = LBG(img_data,colors);

    output.write((char*)header,18);
    for(Pixel pixel: img_data){
        Pixel closest = find_closest_color(pixel,root);
        output << closest.blue;
        output << closest.green;
        output << closest.red;
    }

    output.close();
    free_tree(root);
    return 0;
}