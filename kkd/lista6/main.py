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
    n = 0
    
    for cord in cords:
        y = cord[0] + i
        x = cord[1] + j
        if(y<0 or y>=len(m) or x<0 or x>=len(m[0])):
            continue
        n += 1
        color += ( m[y][x] * matrix[cord[0]+1][cord[1]+1] )
    
    if(matrix[1][1] == 1):
        color //= n

    return max(min(color,255),0)

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
    c = diff_list[0]
    colors = [c]
    for diff in diff_list[1:]:
        c += diff
        colors.append(c)
    return colors

def diff_list(colors):
    c = colors[0]
    diff = [c]
    for c2 in colors[1:]:
        diff.append(c2-c)
        c = c2
    return diff

def differential_encoding(bitmap):
    r, g, b = Pixel.get_color_bitmap_1d(bitmap,'r'),Pixel.get_color_bitmap_1d(bitmap,'g'),Pixel.get_color_bitmap_1d(bitmap,'b')

    buff = ['1' + bin(abs(diff))[2:].zfill(8) if diff < 0 
            else '0' + bin(abs(diff))[2:].zfill(8)
            for color in [diff_list(r), diff_list(g), diff_list(b)] for diff in color]
            
    return ''.join(buff)

def differential_decoding(file):
    buff, header = read_encoded(file)

    diffs = [int(diff[1:], 2) if diff[0] == '0' 
            else -int(diff[1:], 2) 
            for diff in [buff[i:i+9] for i in range(0, len(buff), 9)]]
    
    diff_r = [d for d in diffs[0 : len(diffs)//3]]
    diff_g = [d for d in diffs[len(diffs)//3 : 2*len(diffs)//3]]
    diff_b = [d for d in diffs[2*len(diffs)//3 :]]
    
    bitmap = [channel for sublist in 
            zip(get_colors_from_diff_list(diff_r), get_colors_from_diff_list(diff_g), get_colors_from_diff_list(diff_b))
            for channel in sublist]

    return bitmap, header

def quantizer_encoding(bitmap, bits):
    r, g, b = Pixel.get_color_bitmap_1d(bitmap,'r'),Pixel.get_color_bitmap_1d(bitmap,'g'),Pixel.get_color_bitmap_1d(bitmap,'b')
    
    r_c, rc_idx = nonuniform_quantizer(r, bits)
    g_c, gc_idx = nonuniform_quantizer(g, bits)
    b_c, bc_idx = nonuniform_quantizer(b, bits)
    
    r_a = [rc_idx[c] for c in r]
    g_a = [gc_idx[c] for c in g]
    b_a = [bc_idx[c] for c in b]
    c_idx = r_a + g_a + b_a
    
    buff = bin(bits)[2:].zfill(8)
    buff += ''.join([bin(c)[2:].zfill(8) for c in r_c+g_c+b_c])
    return buff + ''.join([bin(c)[2:].zfill(bits) for c in c_idx])

def quantizer_decoding(file):
    buff, header, bits = read_encoded_quant(file)

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

    bitmap = [channel for sublist in zip(r,g,b) for channel in sublist]
 
    return bitmap, header

def nonuniform_quantizer(colors, bits):
    n = 2**bits
    freq = {i : 0 for i in range(0, 256)}
    
    for c in colors:
        freq[c] += 1

    intervals = {(i, i+1) : freq[i]+freq[i+1] for i in freq if i%2 == 0}

    while len(intervals) > n:
        min_interval = sorted(intervals, key=intervals.get)[0]
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

    centroids = [(el[0]+el[1])//2 for el in intervals]
    pixel_c_idx= [None for _ in range(256)]
    c_idx = 0
    for i in range(0, 256):
        if(c_idx+1<n and abs(centroids[c_idx+1] - i) <= abs(centroids[c_idx] - i)):
            c_idx+=1
        pixel_c_idx[i] = c_idx
    
    return centroids, pixel_c_idx

def read_encoded(file_in):
    with open(file_in, "br") as f:
        header = list(map(int, f.read(18)))
        buff = ''.join([bin(byte)[2:].zfill(8) for byte in f.read()])
    return buff, header

def read_encoded_quant(file_in):
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
            buff = quantizer_encoding(filter(bitmap,[[-1,-1,-1],[-1,9,-1],[-1,-1,-1]]), int(sys.argv[5]))
        elif(sys.argv[2] == '-l'):
            buff = differential_encoding(filter(bitmap,[[1,1,1],[1,1,1],[1,1,1]]))
        
        save_to_file(buff, header, sys.argv[4])

    elif sys.argv[1] == '-d':

        if sys.argv[2] == '-l':
            bitmap, header = differential_decoding(sys.argv[3])
            with open(sys.argv[4], "wb") as f:
                f.write(bytes(header) + bytes(bitmap))

        elif sys.argv[2] == '-h':
            bitmap, header = quantizer_decoding(sys.argv[3])
            with open(sys.argv[4], "wb") as f:
                f.write(bytes(header) + bytes(bitmap))

if __name__ == "__main__":
    main()