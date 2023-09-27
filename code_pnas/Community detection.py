import numpy as np
import pandas as pd
from tqdm import tqdm_notebook as tqdm
import os
import networkx as nx
import igraph 
from igraph import *
import csv
from collections import Counter


def get_infomap_res(normal_data):
    #  Remove loops
    normal_data = normal_data[normal_data['o_grid']!=normal_data['d_grid']]
    normal_data = normal_data[['o_grid','d_grid','ufre']]
    normal_data = normal_data.groupby(['o_grid','d_grid'])['ufre'].sum().reset_index()

    #  Remove edges with weights less than 1 and calculate average flow
    normal_data['ufre'] = normal_data['ufre']/30
    normal_data = normal_data[normal_data['ufre']>=1]

    #  Construct mobility network and use infomap for community partitioning
    G = nx.from_pandas_edgelist(normal_data, 'o_grid','d_grid','ufre',create_using=nx.MultiDiGraph)
    ig = igraph.Graph.from_networkx(G)
    community_list = ig.community_infomap(edge_weights=ig.get_edge_dataframe().ufre) 
    res = pd.DataFrame(zip(list(G),community_list.membership),columns=['node','cluster'])
    return res


def write_res(data, group):
    root = './res/CitywideLockdown_'+group+'.csv'
    f = open(root, 'a+', newline='', encoding='utf-8-sig')
    csv_writer = csv.writer(f)
    csv_writer.writerow(data)
    f.close()


Apr = pd.read_csv(r'E:\复旦上海疫情相关\上海疫情\2022上海移动数据\上海数据_daily\shanghai_od_202204_2_day_ss.csv',
                  usecols=['date_dt', 'o_grid', 'd_grid', 'sex', 'ufre'])
num_samples = 100   # Set the number of iterations for bootstrap sampling 
data = Apr[Apr['sex']==1].reset_index(drop=True)  # Extract mobility data with gender as male
group = 'male'  
data_size = len(data)
for i in tqdm(list(range(num_samples))):
    res = get_infomap_res(data)
    df = pd.DataFrame(Counter(res['cluster']).most_common(), columns=['cluster', 'size'])
    TotalOfArea = (df.head(10)['size'].sum()/7355)*100
    CommSize = len(df)
    CommSize2 = len(df[df['size']>=10])
    write_res([TotalOfArea, CommSize, CommSize2], group)