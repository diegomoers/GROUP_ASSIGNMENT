'''
This file uses PCA and k-means to cluster CEOs
'''

import pandas as pd
import numpy as np

from sklearn.cluster import k_means

# USE BASELINE SAMPLE WITH ONE CEO DROPPED
# Change directory so it works on your laptop
print("Loading CEO data...")
ceo_data = pd.read_csv("/Users/hugovandenbelt/Desktop/Data Science Assignment/2017765data/behaviors/python_index-construction/baseline_results/survey_response_data.csv",
                       low_memory=False)
print(f"Initial data shape: {ceo_data.shape}")

# Setting index
ceo_data.set_index(['id'], inplace=True)
print("Data index set to 'id'.")

# Filtering data
print("Filtering data...")
ceo_data = ceo_data[(ceo_data.level1 == 'interacting')]
print(f"After filtering level1 == 'interacting': {ceo_data.shape}")
ceo_data = ceo_data[ceo_data.type != 'personal_family']
print(f"After excluding type 'personal_family': {ceo_data.shape}")
ceo_data = ceo_data[ceo_data.index != 1115]
print(f"After dropping CEO with id 1115: {ceo_data.shape}")

# CONSTRUCT DATASET WITH SIX FEATURES, EXPRESSED AS TIME SHARE DURING WEEK
print("Constructing feature datasets...")

df1 = pd.crosstab(ceo_data.index, ceo_data.F_duration)
print(f"df1 shape (F_duration): {df1.shape}")

df2 = pd.crosstab(ceo_data[ceo_data.F_planned != 'missing'].index, 
                 ceo_data[ceo_data.F_planned != 'missing'].F_planned)
print(f"df2 shape (F_planned): {df2.shape}")

df3 = pd.crosstab(ceo_data[ceo_data.F_participants != 'missing'].index,
                 ceo_data[ceo_data.F_participants != 'missing'].F_participants)
print(f"df3 shape (F_participants): {df3.shape}")

# Creating new features
print("Creating new binary features...")
ceo_data['ins_alone'] = 0
ceo_data.loc[(ceo_data.ins == 1.0) & (ceo_data.out == 0.0), 'ins_alone'] = 1

ceo_data['out_alone'] = 0
ceo_data.loc[(ceo_data.ins == 0.0) & (ceo_data.out == 1.0), 'out_alone'] = 1

ceo_data['ins_out'] = 0
ceo_data.loc[(ceo_data.ins == 1.0) & (ceo_data.out == 1.0), 'ins_out'] = 1

ceo_data['coordinate'] = 0
ceo_data.loc[ceo_data.n_functions > 1, 'coordinate'] = 1

ceo_data['activity_dummy'] = 1
print("Binary features created.")

# More crosstabs
print("Creating additional crosstabs...")
df4 = pd.crosstab(ceo_data.index, ceo_data.ins_alone)
print(f"df4 shape (ins_alone): {df4.shape}")

df4a = pd.crosstab(ceo_data.index, ceo_data.ins)
print(f"df4a shape (ins): {df4a.shape}")

df5 = pd.crosstab(ceo_data.index, ceo_data.ins_out)
print(f"df5 shape (ins_out): {df5.shape}")

df5a = pd.crosstab(ceo_data.index, ceo_data.out)
print(f"df5a shape (out): {df5a.shape}")

df6 = pd.crosstab(ceo_data.index, ceo_data.coordinate)
print(f"df6 shape (coordinate): {df6.shape}")

df_production = pd.crosstab(ceo_data.index, ceo_data.production)
print(f"df_production shape (production): {df_production.shape}")

df_groupcom = pd.crosstab(ceo_data.index, ceo_data.groupcom)
print(f"df_groupcom shape (groupcom): {df_groupcom.shape}")

df_bunits = pd.crosstab(ceo_data.index, ceo_data.bunits)
print(f"df_bunits shape (bunits): {df_bunits.shape}")

df_coo = pd.crosstab(ceo_data.index, ceo_data.coo)
print(f"df_coo shape (coo): {df_coo.shape}")

df_cao = pd.crosstab(ceo_data.index, ceo_data.cao)
print(f"df_cao shape (cao): {df_cao.shape}")

# Aggregating data
print("Aggregating data into agg_data DataFrame...")
agg_data = pd.DataFrame(index=df1.index)

agg_data['long'] = df1.get('1hrplus', 0)
agg_data['planned'] = df2.get('planned', 0)
agg_data['large'] = df3.get('two_plus_ppl', 0)
agg_data['out'] = df5a.get(1, 0)
agg_data['coordinate1'] = df_groupcom.get(1, 0) + df_bunits.get(1, 0) + df_coo.get(1, 0) + df_cao.get(1, 0)
agg_data['coordinate2'] = df6.get(1, 0)
print("agg_data constructed:")
print(agg_data.head(10))

# Calculating activity share
print("Calculating activity shares...")
activities = ceo_data['activity_dummy'].groupby(ceo_data.index).agg(np.sum)
agg_data_share = agg_data.div(activities, axis=0)
print("Activity shares calculated:")
print(agg_data_share.head())

# PRINCIPAL COMPONENTS DECOMPOSITION
print("\n--- Principal Component Analysis (PCA) ---")
print("Calculating correlation matrix...")
cor = agg_data_share.corr()
print("Correlation matrix:")
print(cor)

print("\nCalculating eigenvalues and eigenvectors...")
eig_vals, eig_vecs = np.linalg.eig(cor)
print(f"Eigenvalues:\n{eig_vals}")
print(f"Eigenvectors:\n{eig_vecs}")

# Sorting eigenvalues and selecting the top 2
sorted_indices = np.argsort(eig_vals)[::-1]
top_indices = sorted_indices[:2]
print(f"\nTop 2 eigenvalues indices: {top_indices}")
print(f"Top 2 eigenvalues: {eig_vals[top_indices]}")
print(f"Corresponding eigenvectors:\n{eig_vecs[:, top_indices]}")

# Projecting data onto top 2 principal components
print("Projecting data onto top 2 principal components...")
pca = np.dot(agg_data_share.values, eig_vecs[:, top_indices])
print(f"PCA-transformed data shape: {pca.shape}")
print("First 5 PCA-transformed data points:")
print(pca[:5])

# K-MEANS CLUSTERING
print("\n--- K-Means Clustering ---")
num_clusters = 2
print(f"Performing K-Means clustering with {num_clusters} clusters...")
centroids, labels, inertia = k_means(agg_data_share.values, num_clusters, random_state=42)
print(f"K-Means centroids:\n{centroids}")
print(f"K-Means labels (first 10):\n{labels[:10]}")
print(f"K-Means inertia: {inertia}")

# OUTPUT RESULTS TO FILE
print("\n--- Outputting Results ---")
ceo_type = pd.DataFrame(index=agg_data.index)
ceo_type.index.name = "id"

ceo_type['pca1'] = pca[:, 0]  # Usually the first component is pca1
ceo_type['pca2'] = pca[:, 1]
ceo_type['k_means'] = labels

print("Sample of clustered data:")
print(ceo_type.head())

ceo_type.to_csv('clusters_test.csv')
print("Clustered data saved to 'clusters_test.csv'.")
