from pixel import Pixel

def predicator_formula(N,W,NW,id):
    color = 0
    match id:
        case 1:
            color = W
        case 2:
            color = N
        case 3:
            color = NW
        case 4:
            color = N+W-NW
        case 5:
            color = N+(W-NW)//2
        case 6:
            color = W+(N-NW)//2
        case 7:
            color = (N+W)//2
        case 8:
            if(NW >= max(W,N)):
                color = max(W,N)
            elif(NW <= min(W,N)):
                color = min(W,N)
            else:
                color = W + N - NW
    return color

def predictors(img):
    r = [[0] * 256 for _ in range(8)]
    g = [[0] * 256 for _ in range(8)]
    b = [[0] * 256 for _ in range(8)]
    rgb = [{} for _ in range(8)]

    for r_id,row in enumerate(img):
        for c,pixel in enumerate(row):
            X = Pixel(pixel[0],pixel[1],pixel[2])

            W_RGB = [0,0,0] if c == 0 else img[r_id][c-1]
            N_RGB = [0,0,0] if r_id == 0 else img[r_id-1][c]
            NW_RGB = [0,0,0] if c == 0 or r_id == 0 else img[r_id-1][c-1]

            W = Pixel(W_RGB[0],W_RGB[1],W_RGB[2])
            N = Pixel(N_RGB[0],N_RGB[1],N_RGB[2])
            NW = Pixel(NW_RGB[0],NW_RGB[1],NW_RGB[2])

            for id in range(8):
                diff_r = (X.r - predicator_formula(N.r,W.r,NW.r,id+1))%256
                diff_g = (X.g - predicator_formula(N.g,W.g,NW.g,id+1))%256
                diff_b = (X.b - predicator_formula(N.b,W.b,NW.b,id+1))%256
                diff_rgb_dict = rgb[id]
                diff_rgb = (diff_r,diff_g,diff_b)

                r[id][diff_r] += 1
                g[id][diff_g] += 1
                b[id][diff_b] += 1

                if diff_rgb not in diff_rgb_dict:
                    diff_rgb_dict[diff_rgb] = 1
                else:
                    diff_rgb_dict[diff_rgb] += 1
            
    return r,g,b,rgb

        