import numpy as np
import geopandas as gpd
from tqdm import tqdm
from shapely.geometry import Point

def get_basin(geodf,basins_col,long_lats,ifnot_found = "UNDEF",is_upper=False):
    basins_lst = []

    for i in tqdm(range(long_lats.shape[0])):
        try:
            if is_upper:
                basins_lst.append(geodf[geodf.contains(Point(long_lats[i]))][basins_col].values[0].upper())
            else:
                basins_lst.append(geodf[geodf.contains(Point(long_lats[i]))][basins_col].values[0])

        except:
            basins_lst.append(ifnot_found)


    return basins_lst
