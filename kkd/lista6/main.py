import sys

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
    result_lower = tuple([(channel[0], *[((channel[idx] + channel[idx-1]) // 2) for idx in range(1, len(channel))]) for channel in [Pixel.get_color_bitmap_1d(bitmap, color) for color in color_channels]])
    result_upper = tuple([(channel[0], *[((channel[idx] - channel[idx-1]) // 2) for idx in range(1, len(channel))]) for channel in [Pixel.get_color_bitmap_1d(bitmap, color) for color in color_channels]])
    return result_lower, result_upper

def reconstruct_from_diff(y,z):
    x = [y[0]]
    for i in range(1,len(y)):
        x_n = max(min(y[i]+y[i-1]-z[i],255),0)
        x.append(x_n)
    return x

def diff_list(colors,quant=None):
    c = colors[0]
    x = [c]
    for c2 in colors[1:]:
        d = c2 - c
        if(quant != None):
            d = quant[d]
        x.append(d)
        c = d
    return x   

def encode(low,high,bits,header):
    low_r, low_g, low_b = low
    high_r, high_g, high_b = high

    r_diff, g_diff, b_diff = diff_list(low_r), diff_list(low_g), diff_list(low_b)
    low_quantizer = tuple(nonuniform_quantizer(diff, bits, -255, 255) for diff in [r_diff, g_diff, b_diff])
    r_diff, g_diff, b_diff = (diff_list(low_channel, quantizer) for low_channel, quantizer in zip([low_r, low_g, low_b], low_quantizer))

    high_quantizer = tuple(nonuniform_quantizer(high_channel, bits, -255, 255) for high_channel in [high_r, high_g, high_b])

    z_r, z_g, z_b = ([quantizer[v] for v in high_channel] for quantizer, high_channel in zip(high_quantizer, [high_r, high_g, high_b]))

    r, g, b = (reconstruct_from_diff(diff, z) for diff, z in zip([r_diff, g_diff, b_diff], [z_r, z_g, z_b]))

    bitmap = [channel for sublist in zip(r, g, b) for channel in sublist]

    with open("test.tga", "bw") as f:
        f.write(bytes(header) + bytes(bitmap))

    return 

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

def save_to_file(buff, header, file_out):
    padding = 8 - len(buff)%8
    buff += padding*'0'
    bytes_list = bytes([padding]) + bytes([int(buff[i:i+8],2) for i in range(0, len(buff), 8)])

    with open(file_out, "bw") as f:
        f.write(bytes(header) + bytes_list)

def main():
    if(len(sys.argv) < 4):
        print("Usage python3 main.py -e/-d  source output k")
        return 
    
    if sys.argv[1] == '-e':
        bitmap, header = read_tga(sys.argv[2])   
        low,high = filter(bitmap)
        buff = encode(low,high,int(sys.argv[4]),header)
        save_to_file(buff, header, sys.argv[3])

if __name__ == "__main__":
    main()