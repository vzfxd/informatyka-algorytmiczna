import sys
import huffman

def read_tga(filename):
    with open(filename, "br") as f:
        header = list(map(int, f.read(18)))
        width = header[13]*256+header[12]
        height = header[15]*256+header[14]
        image_data = [[Pixel(*(list(map(int, f.read(3))))) for _ in range(width)] for _ in range(height)]
        return image_data, header

class Pixel:
    def __init__(self, r, g, b):
        self.r = r
        self.g = g
        self.b = b
    
    def get_color_bitmap_1d(bitmap, color):
        return {
            'r' : [pixl.r for row in bitmap for pixl in row],
            'g' : [pixl.g for row in bitmap for pixl in row],
            'b' : [pixl.b for row in bitmap for pixl in row]
        }[color]

def filter(bitmap):
    color_channels = ['r', 'g', 'b']
    result_lower = tuple([([((channel[idx] + channel[idx-1]) // 2) for idx in range(1, len(channel),2)]) for channel in [Pixel.get_color_bitmap_1d(bitmap, color) for color in color_channels]])
    result_upper = tuple([([((channel[idx] - channel[idx-1]) // 2) for idx in range(1, len(channel),2)]) for channel in [Pixel.get_color_bitmap_1d(bitmap, color) for color in color_channels]])
    return result_lower, result_upper

def reconstruct_from_diff(y,z):
    c = y[0]
    x = [c]
    for i in range(1,len(y)):
        x_n = max(min(c+y[i],255),0)
        x.append(x_n)
        c += y[i]
    return x

def diff_list(colors,quant=None):
    c = colors[0]
    if(quant != None):
        c = quant[c]
    x = [c]
    for c2 in colors[1:]:
        d = c2 - c
        if(quant != None):
            d = quant[d]
        x.append(d)
        c += d
    return x   

def encode(low,high,bits):
    low_r, low_g, low_b = low
    high_r, high_g, high_b = high

    r_diff, g_diff, b_diff = diff_list(low_r), diff_list(low_g), diff_list(low_b)
    low_quantizer = tuple(nonuniform_quantizer(diff, bits, -255, 255) for diff in [r_diff, g_diff, b_diff])
    r_diff, g_diff, b_diff = (diff_list(low_channel, quantizer) for low_channel, quantizer in zip([low_r, low_g, low_b], low_quantizer))
    
    high_quantizer = tuple(nonuniform_quantizer(high_channel, bits, -255, 255) for high_channel in [high_r, high_g, high_b])
    z_r, z_g, z_b = ([quantizer[v] for v in high_channel] for quantizer, high_channel in zip(high_quantizer, [high_r, high_g, high_b]))

    data = z_r + z_g + z_b + r_diff + g_diff + b_diff
    
    number_probabilities = huffman.calculate_probabilities(data)
    huffman_tree = huffman.build_huffman_tree(number_probabilities)
    huffman_mapping = huffman.generate_huffman_codes(huffman_tree)

    buff = huffman.huffman_encode(data, huffman_mapping)

    return buff,huffman_mapping

def decode(file_in, file_out):
    with open(file_in,"br") as f:
        header = list(map(int, f.read(18)))
        padding = int.from_bytes(f.read(1),byteorder='big')
        huff_mapping = eval(f.readline().decode('utf-8'))
        huffman_tree = huffman.build_tree_from_mapping(huff_mapping)
        width = header[13]*256+header[12]
        height = header[15]*256+header[14]
        chunk = width*height//2
        buff = ''.join([bin(c)[2:].zfill(8) for c in f.read()])
        buff = buff[:len(buff)-padding]

    data = huffman.huffman_decode(buff, huffman_tree)
    r_diff,g_diff,b_diff,z_r,z_g,z_b = [],[],[],[],[],[] 
    
    for i,el in enumerate([z_r,z_g,z_b,r_diff,g_diff,b_diff]):
        el.extend(data[i*chunk:(i+1)*chunk])

    r, g, b = (reconstruct_from_diff(diff, z) for diff, z in zip([r_diff, g_diff, b_diff], [z_r, z_g, z_b]))

    r_w = [item for y, z in zip(r, z_r) for item in [max(min(y - z, 255), 0), max(min(y + z, 255), 0)]]
    g_w = [item for y, z in zip(g, z_g) for item in [max(min(y - z, 255), 0), max(min(y + z, 255), 0)]]
    b_w = [item for y, z in zip(b, z_b) for item in [max(min(y - z, 255), 0), max(min(y + z, 255), 0)]]
    
    bitmap = [channel for sublist in zip(r_w, g_w, b_w) for channel in sublist]

    with open(file_out, "bw") as f:
        f.write(bytes(header) + bytes(bitmap))

def nonuniform_quantizer(values, bits, min_p, max):
    n = 2**bits
    freq = {i:0 for i in range(min_p, max+1)}
    
    for v in values:
        freq[v] += 1

    intervals = {(i, i) : freq[i] for i in range(min_p,max) if freq[i] != 0}

    while len(intervals) > n:
        min_interval = min(intervals, key=intervals.get)
        dict_list = list(intervals)
        k = dict_list.index(min_interval)

        if k == 0:
            to_join = dict_list[1]
        elif k == len(dict_list) - 1:
            to_join = dict_list[-2]
        else:
            if intervals[dict_list[k-1]] < intervals[dict_list[k+1]]:
                to_join = dict_list[k-1]
            else:
                to_join = dict_list[k+1]

        if to_join[0] > min_interval[0]:
            new_interval = (min_interval[0], to_join[1])
        else:
            new_interval = (to_join[0], min_interval[1])

        new_interval_value = intervals[min_interval] + intervals[to_join]
        intervals[new_interval] = new_interval_value
        del intervals[min_interval]
        del intervals[to_join]
        intervals = dict(sorted(intervals.items()))

    centroids = []
    for i in intervals:
        cluster = []
        for j in range(i[0],i[1]+1):
            cluster += [j for _ in range(freq[j])]
        centroids.append(cluster[len(cluster)//2])
    
    approx = {i:0 for i in range(min_p,max+1)}
    c_idx = 0
    for i in range(min_p, max+1):
        if(c_idx+1<len(centroids) and abs(centroids[c_idx+1]-i) <= abs(centroids[c_idx]-i)):
            c_idx+=1
        approx[i] = centroids[c_idx]
    
    return approx

def save_to_file(buff, header, file_out,huffman_mapping):
    padding = 8 - len(buff)%8
    buff += padding*'0'
    
    
    bytes_list = bytes([padding]) + bytes(str(huffman_mapping)+"\n", 'utf-8') + bytes([int(buff[i:i+8],2) for i in range(0, len(buff), 8)])

    with open(file_out, "bw") as f:
        f.write(bytes(header) + bytes_list)

def main():
    if(len(sys.argv) < 4):
        print("Usage python3 main.py -e/-d  source output k")
        return 
    
    if sys.argv[1] == '-e':
        bitmap, header = read_tga(sys.argv[2])   
        low,high = filter(bitmap)
        buff,huffman_mapping = encode(low,high,int(sys.argv[4]))
        save_to_file(buff, header, sys.argv[3],huffman_mapping)

    if sys.argv[1] == '-d':
        decode(sys.argv[2],sys.argv[3])

if __name__ == "__main__":
    main()