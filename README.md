# End-to-End-Ant-Data-Project
Contains Project for Collecting, Cleaning, and Analyzing Ant Behavior Data

## Table of Contents

* Supporting Documentation
  - [Biological Background](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Supporting%20Documentation/Colin%20Prospectus.docx) (Ph.D. Propspectus) 
  - [Optimizing Methods for Sampling Behavior](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Supporting%20Documentation/How%20to%20effectively%20sample%20to%20estimate%20distributions%20of%20behavioral%20states%20and%20transitions%20in%20social%20insects.docx) (Unpublished Paper)
* Part 1: Data Collection Optimization for Categorical Time Series 
  - [Estimating Sample Size](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Part%201/EstimateSampleSize.R) 
  - [Finding the Number of Sampling Segments](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Part%201/FinalGraphsandAnalyses.R)
  - [Validation with Simulation](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Part%201/ValidationSimulations.R) 
* Part 2: Designing User-Interface for Collecting Data
  - [Splice Video into Optimal Number of Segments](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Part%202/video_segmenter_marking.m)
  - [MATLAB Application for Collecting Spatial/Behavioral Data](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Part%202/SpatialPositionMarking.mlapp)
  - [Derive Higher-Order Features from Raw Data](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Part%202/Compile%20Data/DerivingSpatialBehavioralData.R)
* Part 3: Hypothesis-Based and Unsupervised Machine Learning Analysis
  - [Linear Models for Hypothesis Testing](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Part%202/Compile%20Data/DerivingSpatialBehavioralData.R)
  - [Clustering Algorithms Define Ant Archetypes](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Part%203/antClustering.py) 

## Background

Capturing qualitative features of animal behavior often requires manual tracking. Continuous sampling can capture many salient features of behavior, but can be time consuming if many individuals need to be recorded over long time periods. Instantaneous sampling can be less costly and be performed over longer time periods, but it can also miss many relevant features of behavior. We therefore explore the gradient in between these two extreme sampling techniques by continuously sampling multiple intervals of behavior which are randomly scattered throughout a time series. We first develop a protocol for estimating the number of instances necessary to reconstruct an animal’s behavior. We then divide this sample size into equally-sized segments which are randomly distributed across the time series. For each sample, we calculate two types of error. Independent error measures how poorly a sample estimates a feature of animal behavior that should be independent of the time at which the behavior occurs: the proportion of time spent in various states. Dependent error measures how poorly a sample estimates a feature of animal behavior that is dependent on the time at which the behavior occurs: average bout length for each state. We can then find the number of segments which minimizes both types of error simultaneously. Finally, we measure which behavioral attributes affect the optimal number of segments. Subsampling strategies were tested with recordings of harvester ant behavior and were validated with simulations. We find that an intermediate number of intervals optimally traded-off both types of error, and that this finding was robust to changes in underlying ant behavior, implying that this method could be used for other model organisms as well. See pages 1-10 of [prospectus](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Supporting%20Documentation/Colin%20Prospectus.docx) for futher discussion on the biological relevance of this question. See READMEs of each part for a further discussion of results.   

## Results Summary

* Developed back-of-the-envelope equation for estimating the sample size necessary to reconstruct a categorical time series (sample size = 1 / proportion of the rarest state in the time series)

* Introduced novel method of sampling time series data, which can reduce sampling bias by 57% compared to current methods.

* Designed GUI which eases the process of data collection. The time needed to analyze data from a single colony was reduced from 7 hours to 4 hours. 

* Created data pipeline which converts 2 raw measurements to 60+ behavioral features. 

* Used a combination of traditional hypothesis testing and machine learning to show that ants in large colonies tend to be less specialized (and are therefore less efficient) and are more active within the nest. This increase in activity likely increases the metabolic rate of large colonies relative to small colonies. 

## Acknowledgements

This work could not have been done without the support of some fantastic scientists. I would like to thank Dr. Ioulia Bespalova for providing data I used for optimization and her comments on manuscripts, Dr. Xiaohui Guo for providing behavior videos and metabolic measurments, Dr. Jennifer Fewell and Dr. Ted Pavlic for guidance, [Cole Busby](https://github.com/ColeBusbyMedTech) for helping to develop the MATLAB application, and my undergraduate research assistants Gailan Khanania, Nathaniel Maslanka, Coleen Furey, Mariah Merriam, Tejaswini Nandakumar, and Emma Siebrandt for collecting data and managing ant colonies. Finally, I would like to thank Michaela Starkey for her advice, assistance with math and programming, and much-needed emotional support! 
