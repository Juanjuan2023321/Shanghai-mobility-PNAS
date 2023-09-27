import pandas as pd
import numpy as np
import os
import networkx as nx

#导入数据，按照日期切分原始数据集
path = "F:\\shanghai od matrix\\daily\\"
csv_list = os.listdir(path)
#构建网络
normal_data = []
#合并二月份数据
for csv in csv_list[14:28]:
    data = pd.read_csv(path + csv)
    data = data[data['o_grid'] != data['d_grid']]#删除重复边
    data = data[['o_grid', 'd_grid', 'ufre']]
    normal_data.append(data)

normal_data = pd.concat(normal_data)
#删除自环
normal_data = normal_data[normal_data['o_grid'] != normal_data['d_grid']]
normal_data = normal_data.groupby(['o_grid', 'd_grid'])['ufre'].sum().reset_index()
#按照天数进行平均，删除权重小于1的边
normal_data['ufre'] = normal_data['ufre'] / 14
normal_data = normal_data[normal_data['ufre'] >= 1]
G = nx.from_pandas_edgelist(normal_data,
                            'o_grid', 'd_grid', 'ufre', create_using=nx.MultiDiGraph)
#保存节点信息
edge_list = nx.to_pandas_edgelist(G)
#读取二月份的节点信息
node_data = pd.read_csv("7.26 result/network phase/node_feb.csv")
#将同一社区的节点进行聚合，并计算地理质心
cluster_geo = node_data.groupby('cluster')[["long","lat"]].mean().reset_index()
cluster_data = pd.DataFrame()
cluster_data["cluster"] = node_data.cluster.value_counts().index
cluster_data["area"] = node_data.cluster.value_counts().values
cluster_data = cluster_geo.merge(cluster_data)
cluster_data = cluster_data[cluster_data.area>=10]
lagre_cluster = cluster_data.cluster.values
cluster_data = cluster_data.rename(columns={"cluster":"id"})
cluster_data.to_csv("修图230721/node_feb.csv", index = False)

node2cluster = dict(zip(node_data.node,node_data.cluster))
edge_list.source = edge_list.source.apply(lambda x:node2cluster[x])
edge_list.target = edge_list.target.apply(lambda x:node2cluster[x])
#计算社区之间的权重
edge_list = edge_list[(edge_list.source.isin(lagre_cluster)) & (edge_list.target.isin(lagre_cluster))]
edge_list = edge_list.groupby(['source','target'])['ufre'].sum().reset_index()
edge_list = edge_list[edge_list.source!=edge_list.target]
edge_list = edge_list.rename(columns={"ufre":"weight"})

edge_list.to_csv("修图230721/feb_edge_list.csv", index = False)


