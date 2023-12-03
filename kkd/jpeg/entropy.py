import math

def entropy(map,map_size):
    container = map.values() if isinstance(map,dict) else map
    ent = 0.0
    for freq in container:
        if(freq == 0): continue
        p = freq/map_size
        info = -math.log(p,2)
        ent += p * info
    return ent