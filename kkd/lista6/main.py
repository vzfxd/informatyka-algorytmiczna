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

    def get_color_bitmap_2d(bitmap,color):
        return {
            'r' : [[pixl.r for pixl in row] for row in bitmap],
            'g' : [[pixl.g for pixl in row] for row in bitmap],
            'b' : [[pixl.b for pixl in row] for row in bitmap]
        }[color]
    
    def get_color_bitmap_1d(bitmap, color):
        return {
            'r' : [pixl.r for row in bitmap for pixl in row],
            'g' : [pixl.g for row in bitmap for pixl in row],
            'b' : [pixl.b for row in bitmap for pixl in row]
        }[color]

def apply_filter(m, i, j, matrix):
    cords = [(0,0),(-1,0),(1,0),(0,-1),(0,1),(-1,-1),(-1,1),(1,-1),(1,1)]
    color = 0
    w = 0
    for cord in cords:
        y = cord[0] + i
        x = cord[1] + j
        if(y<0 or y>=len(m) or x<0 or x>=len(m[0])):
            continue
        w += matrix[cord[0]+1][cord[1]+1]
        color += m[y][x] * matrix[cord[0]+1][cord[1]+1]
    
    return max(min(color//w,255),0)

def filter(map, matrix):
    r, g, b = Pixel.get_color_bitmap_2d(map,'r'),Pixel.get_color_bitmap_2d(map,'g'),Pixel.get_color_bitmap_2d(map,'b')
    
    filtered = [[None for _ in row] for row in map]

    for i, row in enumerate(map):
        for j, _ in enumerate(row):
            filtered[i][j] = Pixel(
                apply_filter(r, i, j, matrix),
                apply_filter(g, i, j, matrix),
                apply_filter(b, i, j, matrix)
            )

    return filtered

def get_colors_from_diff_list(diff_list):
    c = max(min(diff_list[0],255),0)
    colors = [c]
    for diff in diff_list[1:]:
        c = max(min(c+diff,255),0)
        colors.append(c)
    return colors

def diff_list(colors):
    c = colors[0]
    diff = [c]
    for c2 in colors[1:]:
        diff.append(c2-c)
        c = c2
    return diff    

def quantizer_encoding(bitmap, bits,diff=False):
    r, g, b = Pixel.get_color_bitmap_1d(bitmap,'r'),Pixel.get_color_bitmap_1d(bitmap,'g'),Pixel.get_color_bitmap_1d(bitmap,'b')
    min,max=0,255

    if(diff):
        min,max=-255,255
        r,g,b = diff_list(r),diff_list(g),diff_list(b)

    r_c, rc_idx = nonuniform_quantizer(r, bits, min, max)
    g_c, gc_idx = nonuniform_quantizer(g, bits, min, max)
    b_c, bc_idx = nonuniform_quantizer(b, bits, min, max)
    
    r_a = [rc_idx[c-min] for c in r]
    g_a = [gc_idx[c-min] for c in g]
    b_a = [bc_idx[c-min] for c in b]
    c_idx = r_a + g_a + b_a
    
    buff = bin(bits)[2:].zfill(8)
    if(diff):
        buff += ''.join(['1' + bin(abs(c))[2:].zfill(8) if c<0 else '0' + bin(c)[2:].zfill(8) for c in r_c+g_c+b_c])
    else:
        buff += ''.join([bin(c)[2:].zfill(8) for c in r_c+g_c+b_c])

    return buff + ''.join([bin(c)[2:].zfill(bits) for c in c_idx])

def quantizer_decoding(file,diff=False):
    buff, header, bits = read_encoded(file)
    
    centroids = None
    if(diff):
        centroid_bits = 2**bits*3*9
        centroids = [int(el[1:], 2) if el[0] == '0' else -int(el[1:], 2) for el in [buff[i:i+9] for i in range(0, centroid_bits, 9)]]
    else:
        centroid_bits = 2**bits*3*8
        centroids = [int(buff[i:i+8], 2) for i in range(0, centroid_bits, 8)]

    r_c = [c for c in centroids[:len(centroids)//3]]
    g_c = [c for c in centroids[len(centroids)//3 : 2*len(centroids)//3]]
    b_c = [c for c in centroids[2*len(centroids)//3:]]

    buff = buff[centroid_bits:]
    c_idx = [int(buff[i:i+bits], 2) for i in range(0, len(buff), bits)]
    
    r = [r_c[idx] for idx in c_idx[:len(c_idx)//3]]
    g = [g_c[idx] for idx in c_idx[len(c_idx)//3 : 2*len(c_idx)//3]]
    b = [b_c[idx] for idx in c_idx[2*len(c_idx)//3:]]

    bitmap = None
    if(diff):
        bitmap = [channel for sublist in 
            zip(get_colors_from_diff_list(r), get_colors_from_diff_list(g), get_colors_from_diff_list(b))
            for channel in sublist]
    else:
        bitmap = [channel for sublist in zip(r,g,b) for channel in sublist]
    
    return bitmap, header

def nonuniform_quantizer(values, bits, min_p, max):
    n = 2**bits
    freq = [0 for _ in range(min_p, max+1)]
    
    for v in values:
        freq[v-min_p] += 1

    intervals = {(i, i+1) : freq[i-min_p]+freq[i-min_p+1] for i in range(min_p,max,2)}
    for key, value in list(intervals.items()):
        if value == 0:
            del intervals[key]

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

    while len(intervals) < n:
        intervals = dict(sorted(intervals.items(), key=lambda item: item[1], reverse=True))
        dict_list = list(intervals)
        k = 0
        max_interval = dict_list[k]
        while(max_interval[0] == max_interval[1] and k<len(dict_list)):
            max_interval = dict_list[k]
            k += 1
        if(k==len(dict_list)):
            intervals = dict(sorted(intervals.items()))
            break

        c = (max_interval[0] + max_interval[1])//2
        left_interval = (max_interval[0],c)
        right_interval = (c+1,max_interval[1])
        left_val = 0
        right_val = 0
        for i in range(max_interval[0],c+1):
            left_val += freq[i-min_p]
        for i in range(c+1,max_interval[1]+1):
            right_val += freq[i-min_p]

        if(left_val != 0):
            intervals[left_interval] = left_val
        if(right_val != 0):
            intervals[right_interval] = right_val
        
        del intervals[max_interval]
        intervals = dict(sorted(intervals.items()))

    centroids = []
    for i in intervals:
        cluster = []
        for j in range(i[0],i[1]+1):
            cluster += [j for _ in range(freq[j-min_p])]
        centroids.append(cluster[len(cluster)//2])
    
    for i in range(n-len(centroids)):
        centroids.append(255)

    labels= [None for _ in range(min_p,max+1)]
    c_idx = 0
    for i in range(min_p, max+1):
        if(c_idx+1<n and abs(centroids[c_idx+1]-i) <= abs(centroids[c_idx]-i)):
            c_idx+=1
        labels[i-min_p] = c_idx
    
    return centroids, labels

def read_encoded(file_in):
    with open(file_in, "br") as f:
        header = list(map(int, f.read(18)))
        bits = int.from_bytes(f.read(1),byteorder='big')
        buff = ''.join([bin(byte)[2:].zfill(8) for byte in f.read()])
    return buff, header, bits  

def save_to_file(buff, header, file_out):
    bytes_list = bytes([int(buff[i:i+8],2) for i in range(0, len(buff), 8)])

    with open(file_out, "bw") as f:
        f.write(bytes(header) + bytes_list)

def main():
    if(len(sys.argv) < 5):
        print("Usage python3 main.py -e -h/-l source output k(optional)")
        return 
    
    if sys.argv[1] == '-e':
        bitmap, header = read_tga(sys.argv[3])
        
        buff = None
        if(sys.argv[2] == '-h'):
            buff = quantizer_encoding(filter(bitmap,[[-1,-1,-1],[-1,9,-1],[-1,-1,-1]]),int(sys.argv[5]))
        elif(sys.argv[2] == '-l'):
            buff = quantizer_encoding(filter(bitmap,[[1,1,1],[1,1,1],[1,1,1]]),int(sys.argv[5]),True)
        
        save_to_file(buff, header, sys.argv[4])

    elif sys.argv[1] == '-d':

        if sys.argv[2] == '-l':
            bitmap, header = quantizer_decoding(sys.argv[3],diff=True)
            with open(sys.argv[4], "wb") as f:
                f.write(bytes(header) + bytes(bitmap))

        elif sys.argv[2] == '-h':
            bitmap, header = quantizer_decoding(sys.argv[3])
            with open(sys.argv[4], "wb") as f:
                f.write(bytes(header) + bytes(bitmap))

if __name__ == "__main__":
    main()