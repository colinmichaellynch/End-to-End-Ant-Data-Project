from IPython import get_ipython;   
get_ipython().magic('reset -sf')

import pandas as pd
import numpy as np
from sklearn.cluster import KMeans
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import silhouette_score
from sklearn.cluster import AgglomerativeClustering
from sklearn.cluster import DBSCAN
from sklearn.mixture import GaussianMixture
import warnings
import matplotlib.pyplot as plt
import seaborn as sns 
from scipy.stats import chi2_contingency
from sklearn.decomposition import PCA
from sklearn.linear_model import LinearRegression

warnings.filterwarnings("ignore")
df = pd.read_csv(r'C:\Users\user\Documents\Metabolic Scaling\Clustering\indBehavioralData.csv')

###define functions

#define scorer
def scorer(estimator, X, y=None):
    y_pred = estimator.fit_predict(X)
    return silhouette_score(X, y_pred)

#choose scoring method for grid search
#method = 'rand_score'
method = scorer

###data preprocessing 

#Clean colony size vector
colonySize = df['colonySize']
colonySize = colonySize[0:15]
colonySize = np.concatenate([colonySize]*10)

#remove unnecessary variables 
df = df.drop(['Unnamed: 0', 'Colony', 'AntID', 'Date', 'Analyst', 'session', 'totalMass', 'colonySize'], axis=1)

#remove variables with too many nans
removalThreshold = .5
df = df.loc[:,df.columns[df.isnull().mean() < removalThreshold]]

#normalize variables
#df = (df-df.min())/(df.max()-df.min()) 

# remove correlated features 
correlationThreshold = .95 #.95 if need many
corr_matrix = df.corr().abs()
upper = corr_matrix.where(np.triu(np.ones(corr_matrix.shape), k=1).astype(np.bool))
to_drop = [column for column in upper.columns if any(upper[column] > correlationThreshold)]
df.drop(to_drop, axis=1, inplace=True)

#replace all nans with 0's
df = df.fillna(0)

###k means
param_grid = {
    'n_clusters': [2, 3, 4, 5, 6, 7],
    'init': ['k-means++', 'random'],
    'n_init': [10, 20, 30],
    'max_iter': [100, 200, 300]
}

model = KMeans()

gsKmeans = GridSearchCV(estimator=model, param_grid=param_grid, cv=5, n_jobs=-1, verbose=2, scoring=method)
gsKmeans.fit(df)
best_params_ = gsKmeans.best_params_

kMeansModel = KMeans(n_clusters=best_params_['n_clusters'], init = best_params_['init'], n_init = best_params_['n_init'], max_iter = best_params_['max_iter'])

kMeansModel.fit(df)
labels = kMeansModel.labels_
scoreKmeans = silhouette_score(df, labels)

###hierarchical cluster
param_grid = {
    'n_clusters': [2, 3, 4, 5, 6, 7],
    'linkage': ['ward', 'complete', 'average']
}

model = AgglomerativeClustering()

gsAgglomerativeClustering = GridSearchCV(estimator=model, param_grid=param_grid, cv=5, n_jobs=-1, verbose=2, scoring=method)
gsAgglomerativeClustering.fit(df)
best_params_ = gsAgglomerativeClustering.best_params_

AgglomerativeClusteringModel = AgglomerativeClustering(n_clusters=best_params_['n_clusters'], linkage = best_params_['linkage'])

AgglomerativeClusteringModel.fit(df)
labels = AgglomerativeClusteringModel.labels_
scoreAgglomerativeClustering= silhouette_score(df, labels)

###dbscan
param_grid = {
    'eps': [0.1, 0.5, 1.0],
    'min_samples': [5, 10, 15, 20, 25]
}

model = DBSCAN()

gsDBSCAN = GridSearchCV(estimator=model, param_grid=param_grid, cv=5, n_jobs=-1, verbose=2, scoring=method)
gsDBSCAN.fit(df)
best_params_ = gsDBSCAN.best_params_

DBSCANModel = DBSCAN(eps=best_params_['eps'], min_samples = best_params_['min_samples'])

DBSCANModel.fit(df)
labels = DBSCANModel.labels_
scoreDBSCANModel = silhouette_score(df, labels)

### gaussian mixture distribution based algorithm
param_grid = {
    'n_components': [2, 3, 4, 5, 6, 7],
    'covariance_type': ['full', 'tied', 'diag', 'spherical']
}

model =  GaussianMixture()

gsGM = GridSearchCV(estimator=model, param_grid=param_grid, cv=5, n_jobs=-1, verbose=2, scoring=method)
gsGM.fit(df)
best_params_ = gsGM.best_params_
GaussianMixtureModel = GaussianMixture(n_components=best_params_['n_components'], covariance_type = best_params_['covariance_type'])

GaussianMixtureModel.fit(df)
labels = GaussianMixtureModel.predict(df)
scoreGaussianMixtureModel = silhouette_score(df, labels)

print(scoreKmeans, scoreAgglomerativeClustering, scoreDBSCANModel, scoreGaussianMixtureModel)

#do pca to visualize clusters 
pca = PCA(n_components=2)

# Fit the model to the data and transform the data
X_pca = pca.fit_transform(df)
loadings = pca.components_

X_pca = pd.DataFrame(X_pca)
loadings = pd.DataFrame(loadings)

df['Cluster'] =  kMeansModel.labels_
X_pca['Cluster'] = df['Cluster']

palette = sns.color_palette('viridis', 3)

ax = sns.scatterplot(x=X_pca.iloc[:, 0], y=X_pca.iloc[:,1], c=kMeansModel.labels_, hue = kMeansModel.labels_, palette=palette)
plt.xlabel('PC 1')
plt.ylabel('PC 2')
ax.legend().set_title('Cluster')
ax.figure.set_size_inches(7,7)

pc1_sorted_vec = sorted(loadings.loc[0,:].astype(float), reverse=True)
loadingsVec = loadings.loc[0,:]
pc1_indices = loadingsVec.nlargest(5).index.tolist()
pc1_values = pc1_sorted_vec[:5]

pc2_sorted_vec = sorted(loadings.loc[1,:].astype(float), reverse=True)
loadingsVec = loadings.loc[1,:]
pc2_indices = loadingsVec.nlargest(5).index.tolist()
pc2_values = pc2_sorted_vec[:5]

allIndeces = pd.concat([pd.Series(pc1_indices), pd.Series(pc2_indices), pd.Series(len(df.columns)-1)])
uniqueIndeces = allIndeces.unique()

mostImportantFeatures = df.columns[uniqueIndeces]

dfBoxplot = df[mostImportantFeatures]
#boxplot = dfBoxplot.boxplot(by='Cluster', sharey=False)

fig, axs = plt.subplots(1,7, figsize=(35, 5), sharey=False)
dfBoxplot.boxplot(['movementAreaForaging', 'exploringLengthForaging','movementAreaNest', 
       'exploringLengthNest', 'numberOfTaskSwitches', 'velocity', 'numberOfAntennations'], 'Cluster', axs)

###for now kmeans has the highest score, so see if cluster frequency changes over colony sizes 

df['colonySize'] = colonySize
df['Cluster'] = abs(kMeansModel.labels_)
df.to_csv('clusteringData.csv', index=False)

boolVec = colonySize > 40
boolVec = ['Large' if x else 'Small' for x in boolVec]
df['colonySizeCat'] = boolVec

contingency_table = pd.crosstab(df['colonySizeCat'], df['Cluster'])
chi2, p, dof, expected = chi2_contingency(contingency_table)

print("Chi-squared test statistic: {}".format(chi2))
print("p-value: {}".format(p))

fig = plt.figure(num=None, figsize=(8, 6), dpi=80, facecolor='w', edgecolor='k')

plt.clf()
ax = fig.add_subplot(111)
ax.set_aspect(1)
res = sns.heatmap(contingency_table, annot=True, cmap="YlGnBu", vmin=0.0, vmax=50.0)
plt.savefig("plot_contingency_table_seaborn_matplotlib_01.png", bbox_inches='tight', dpi=100)
plt.ylabel('Colony Size')
plt.show()

n, bins, patches = plt.hist(x=colonySize, bins='auto', color='#0504aa',alpha=1, rwidth=0.85)
plt.xlabel('Colony Size')
plt.ylabel('Frequency')

#do multiple types of clustering (k means (centroid based), heirarchial, dbscan (density based), distribution-based (gaussian)), can you compare which is best? max silouhette coef, sse/elbow, min gap statistic. AIC? 

#see how clusters change across colony size, and what makes up each cluster. 
#Do this for 4 most important features. Do PCA and make graph to see if it clusters well. Do radar graph of all features
