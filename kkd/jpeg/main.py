import numpy as np
import struct
import sys
from predictors import predictors
from entropy import entropy
from pixel import Pixel

def img_color_occurence(img):
    r = [0] * 256
    g = [0] * 256
    b = [0] * 256
    rgb_dict = dict()
    for row in img:
        for pixel in row:
            X = Pixel(pixel[0],pixel[1],pixel[2])
            r[X.r] += 1
            g[X.g] += 1
            b[X.b] += 1
            rgb = (X.r,X.g,X.b)
            if rgb not in rgb_dict:
                rgb_dict[rgb] = 1
            else:
                rgb_dict[rgb] += 1

    return r,g,b,rgb_dict

def read_tga(file_path):
    with open(file_path, 'rb') as f:
        header = f.read(18)
        width, height = struct.unpack("<HH", header[12:16])
        
        image_size = width * height
        image_data = np.frombuffer(f.read(image_size * 3), dtype=np.uint8).reshape((height, width, 3))[::-1, :, ::-1]  

    return image_data, image_size


img,size = read_tga(sys.argv[1])
print(img[0][0])
print(img[0][1])
print(img[0][2])
# r,g,b,rgb = predictors(img)

# r_best = sys.maxsize
# g_best = sys.maxsize
# b_best = sys.maxsize
# rgb_best = sys.maxsize

# print("\n")
# for i in range(8):
#     r_ent = entropy(r[i],size)
#     g_ent = entropy(g[i],size)
#     b_ent = entropy(b[i],size)
#     rgb_ent = entropy(rgb[i],size)

#     print(f"predictor: {i+1}, r:{r_ent}, g:{g_ent}, b:{b_ent}")

#     if(r_ent < r_best): r_best = r_ent
#     if(g_ent < g_best): g_best = g_ent
#     if(b_ent < b_best): b_best = b_ent
#     if(rgb_ent < rgb_best): rgb_best = rgb_ent

# r,g,b,rgb_dict = img_color_occurence(img)

# print(f"\nOriginal img red entropy {entropy(r,size)}")
# print(f"Original img green entropy {entropy(g,size)}")
# print(f"Original img blue entropy {entropy(b,size)}")
# print(f"Original img entropy {entropy(rgb_dict,size)}")

# print(f"\nBest red code diff entropy {r_best}")
# print(f"Best green code diff entropy {g_best}")
# print(f"Best blue code diff entropy {b_best}")
# print(f"Best img code diff entropy {rgb_best}")