import sys
from main import read_tga
import numpy as np

def calculate_mse_snr(image1, image2, color="all"):
    mse = 0
    snr = 0
    if color == "red":
        for idx,p in enumerate(image1):
            p2 = image2[idx]
            mse  += abs(p.r - p2.r)
            snr += p2.r**2
        return mse/len(image1),snr
    
    if color == "green":
        for idx,p in enumerate(image1):
            p2 = image2[idx]
            mse  += abs(p.g - p2.g)
            snr += p2.g**2
        return mse/len(image1),snr
    
    if color == "blue":
        for idx,p in enumerate(image1):
            p2 = image2[idx]
            mse  += abs(p.b - p2.b)
            snr += p2.b**2
        return mse/len(image1),snr
    
    for idx,p in enumerate(image1):
        p2 = image2[idx]
        mse += abs(p.r - p2.r) + abs(p.g - p2.g) + abs(p.b - p2.b)
        snr += p2.r**2 + p2.g**2 + p2.b**2
    return mse/len(image1),snr

def calculate_psnr(snr,mse,len):
    return 10 * np.log10((snr/len)/mse)

original,_ = read_tga(sys.argv[1])
decoded,_ = read_tga(sys.argv[2])

original_flat = [item for row in original for item in row]
decoded_flat = [item for row in decoded for item in row]
l = len(decoded_flat)

mse_total,snr_total = calculate_mse_snr(original_flat, decoded_flat)
mse_red,snr_red = calculate_mse_snr(original_flat, decoded_flat, "red")
mse_green,snr_green = calculate_mse_snr(original_flat, decoded_flat, "green")
mse_blue,snr_blue = calculate_mse_snr(original_flat, decoded_flat, "blue")

psnr_total = calculate_psnr(snr_total,mse_total,l)
psnr_red = calculate_psnr(snr_red,mse_red,l)
psnr_green = calculate_psnr(snr_green,mse_green,l)
psnr_blue = calculate_psnr(snr_blue,mse_blue,l)

print(f"Błąd średniokwadratowy całego obrazu: {mse_total}")
print(f"Błąd średniokwadratowy składowej czerwonej: {mse_red}")
print(f"Błąd średniokwadratowy składowej zielonej: {mse_green}")
print(f"Błąd średniokwadratowy składowej niebieskiej: {mse_blue}")
print(f"PSNR całego obrazu: {psnr_total} dB")
print(f"PSNR składowej czerwonej: {psnr_red} dB")
print(f"PSNR składowej zielonej: {psnr_green} dB")
print(f"PSNR składowej niebieskiej: {psnr_blue} dB")