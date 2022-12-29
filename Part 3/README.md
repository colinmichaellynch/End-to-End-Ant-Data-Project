# Part 3

## Summary

Now that we have collected and processed the data, we can start to figure out how the behavioral profiles of ants in larger colonies differ from those of smaller colonies. This can be done in two broad ways. 1) We can use traditional linear models to test explicit hypotheses ([Prospectus](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Supporting%20Documentation/Colin%20Prospectus.docx) page 14) and 2) we can use unsupervised machine learning to categorize individuals and we can see how the distribution of individual archtypes changes over colony sizes. This latter method will be particularly useful as we have more features in this dataset than hypotheses and we risk problems associated with multiple comparisons if we aren't careful. 

### Hypothesis Testing 

While there are numerous ways that ants can be changing their behavior as the colony increases in size, here I will just focus on the division of labor hypothesis. The assertion here is that as the colony size increases, workers become less specialized and therefore spend more energy performing tasks that they are less proficient at. The DOL index is a measure of how specialized ants are in a colony. If its 1, then everyone is a specialist. If its 0, then everyone does everything. We can see in panel A that this index does decrease with colony size (each point is a colony). In panel C we also see that higher levels of division of labor are also associated with lower metabolic rates, and since colony size and metabolic rate are inversely proportional ([Prospectus](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Supporting%20Documentation/Colin%20Prospectus.docx) page 15), this means that the lower levels of division of labor in larger colonies could contribute to decreasing levels of metabolic rate. Panel B provides the glue between division of labor and metabolic rate. Here, we have individuals listed rather than colonies, and we can see that the more specialized ants get, the less often they switch tasks (blue points), and the more likely they are to complete tasks (yellow points). This means that ants who are specialized are less likely to burn energy than those who are less specialized. This could therefore be the behavioral underpinning of the connecton between division of labor and metabolic rate. 

![](/Images/divisionOfLabor.png)

### Behavioral Archetypes through Clustering

Division of labor is unlikely to be the only behavioral pattern underlying metabolic scaling. I therefore use a combination of principal component analysis and unsupervised machine learning to categorize ants and see if ant type changes across colony sizes. To do this, I go through this process:

* I remove columns of data which have too many NANs, and fill the rest of the NANs with 0's. 

* I remove variables that are 95% correlated with one another. 

* I perform 5-fold cross validation over a grid search of hyperparameters (where I maximize silhouette score) for 4 different clustering models: k-means clustering algorithm, hierarchical clustering algorithm, DBSCAN, and a Guassian Mixture model. The following table shows the final silhouette scores for each model: 

| K-Means  | Hierarchical | DBSCAN  | Guassian Mixture |
| ------------- | ------------- | ------------- | ------------- |
| 0.6456  | 0.6289  | -0.0687  | 0.6441 |

* K-Means has the closest score to 1, so I use this as my clustering algorithm. This algorithm has 3 clusters, shown here across the two principal components of the dataset: 

![](/Images/PCA.png)

* To determine feature importance, I measure the loadings of each behavioral feature on PC 1 and 2 and subset out the 5 features with the highest loading. The two components share 3 important features, so there are 7 features which primarily drive these clusters. They are:
  - Area covered in foraging arena
  - Area covered in the nest
  - Path length while foraging
  - Path length inside the nest
  - Number of task switches
  - Velocity
  - Number of antennations (communication events)

* Ants within each cluster (or 'archetype') behave differently with respect to each of these behavioral attributes:

![](/Images/importantFeatures.png)

* Ants in cluster 1 tend to minimize all of these features and are generally inactive. Ants in cluster 0 tend to move a lot in the foraging arena. Ants in cluster 2 move a lot in the nest, they move quickly, and they switch between tasks often. The 3 archetypes can be summarized then inactive ants, foragers, and nest workers. 

* The distribution of colony sizes is bimodal, so we categorize colonies as either being large or small to simplify analyses. 

![](/Images/histogram.png)

* Finally, we look at the distribution of ant archetypes across these two colony sizes (chi-squared  test, chi-squared = 24.49, p-value < 0.001). 

![](/Images/heatmap.png)

* There are more inactive ants in small colonies, and virtually no nest workers. This means that the more active ants are present in the large colonies, which helps explain why the larger colonies have a higher metabolic rate. Further analysis into these archetypes could yield other insights as well. 
