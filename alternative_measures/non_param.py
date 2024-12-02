'''
This file uses PCA and k-means to cluster CEOs
'''

import pandas as pd
import numpy as np

from sklearn.cluster import k_means


# USE BASELINE SAMPLE WITH ONE CEO DROPPED

ceo_data = pd.read_csv("/Users/hugovandenbelt/Desktop/Data Science Assignment/2017765data/behaviors/python_index-construction/baseline_results/survey_response_data.csv",
                       low_memory=False)
ceo_data.set_index(['id'], inplace=True)
ceo_data = ceo_data[(ceo_data.level1 == 'interacting')]
ceo_data = ceo_data[ceo_data.type != 'personal_family']
ceo_data = ceo_data[ceo_data.index != 1115]
# missing F_participants and function info


# CONTRUCT DATASET WITH SIX FEATURES, EXPRESSED AS TIME SHARE DURING WEEK

df1 = pd.crosstab(ceo_data.index, ceo_data.F_duration)
df2 = pd.crosstab(ceo_data[ceo_data.F_planned != 'missing'].
                  index, ceo_data[ceo_data.F_planned != 'missing'].F_planned)
df3 = pd.crosstab(ceo_data[ceo_data.F_participants != 'missing'].index,
                  ceo_data[ceo_data.F_participants != 'missing']
                  .F_participants)

ceo_data['ins_alone'] = 0
ceo_data.loc[(ceo_data.ins == 1.0) & (ceo_data.out == 0.0), 'ins_alone'] = 1

ceo_data['out_alone'] = 0
ceo_data.loc[(ceo_data.ins == 0.0) & (ceo_data.out == 1.0), 'out_alone'] = 1

ceo_data['ins_out'] = 0
ceo_data.loc[(ceo_data.ins == 1.0) & (ceo_data.out == 1.0), 'ins_out'] = 1

ceo_data['coordinate'] = 0
ceo_data.loc[ceo_data.n_functions > 1, 'coordinate'] = 1

ceo_data['activity_dummy'] = 1

df4 = pd.crosstab(ceo_data.index, ceo_data.ins_alone)
df4a = pd.crosstab(ceo_data.index, ceo_data.ins)

df5 = pd.crosstab(ceo_data.index, ceo_data.ins_out)
df5a = pd.crosstab(ceo_data.index, ceo_data.out)

df6 = pd.crosstab(ceo_data.index, ceo_data.coordinate)

df_production = pd.crosstab(ceo_data.index, ceo_data.production)

df_groupcom = pd.crosstab(ceo_data.index, ceo_data.groupcom)
df_bunits = pd.crosstab(ceo_data.index, ceo_data.bunits)
df_coo = pd.crosstab(ceo_data.index, ceo_data.coo)
df_cao = pd.crosstab(ceo_data.index, ceo_data.cao)

agg_data = pd.DataFrame(index=df1.index)

agg_data['long'] = df1['1hrplus']
agg_data['planned'] = df2['planned']
agg_data['large'] = df3['two_plus_ppl']
agg_data['out'] = df5a[1]
agg_data['coordinate1'] = df_groupcom[1] + df_bunits[1] + df_coo[1] + df_cao[1]
agg_data['coordinate2'] = df6[1]

activities = ceo_data['activity_dummy'].groupby(ceo_data.index).agg(np.sum)
agg_data_share = agg_data.div(activities, axis=0)


# PRINCIPAL COMPONENTS DECOMPOSITION

cor = agg_data_share.corr()
eig_vals, eig_vecs = np.linalg.eig(cor)
print(eig_vals)
pca = np.dot(agg_data_share.values, eig_vecs[:, np.argsort(eig_vals)[-2:]])
# use first two components


# K-MEANS

x = k_means(agg_data_share.values, 2)
print('k-means centroids = \n', x[0])


# OUTPUT RESULTS TO FILE

ceo_type = pd.DataFrame(index=agg_data.index)
ceo_type.index.name = "id"

ceo_type['pca1'] = pca[:, 1]
ceo_type['pca2'] = pca[:, 0]
ceo_type['k_means'] = x[1]

ceo_type.to_csv('clusters_test.csv')
