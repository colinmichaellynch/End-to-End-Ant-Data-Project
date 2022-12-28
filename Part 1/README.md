# Part 1

## Summary

Collecting behavioral data on ants can be a tricky affair, as many subtle features of their behavior cannot be tracked remotely. Instead, videos of ant colonies need to be annoted manually, which is a time-consuming process. Generally, animal behavior (as well as other types of categorical time-series) is sampled in one of two ways. It can be continuously sampled, where the animal is continuously watched for a predetermined amount of time. Alternativily, or it can be instananeously sampled, where behavior is recorded for a moment over regular intervals of time. Here, I developed a hybrid scheme called piecewise scan sampling where we record multiple continuous segments of time that are randomly scattered throughout the entire possible time series (page 4). The number of segments or intervals is I, and my goal here is twofold. I want to determine how long we need to watch each ant for, and I want to know what the optimal value of I is. Optimization is performed on a premade dataset where 9 ants were observed for 3 hours and their behaviors were recorded on a second-by-second basis. Data and R scripts are provided in this folder. Page numbers refer to this [manuscript](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/Supporting_Documentation/How_to_effectively_sample_to_estimate_distributions_of_behavioral_states_and_transitions_in_social_insects.docx).

### Estimating Sample Size 

* I show that one can estimate the sample size of a time series by finding the sample size where sample variance converges on population variance (pages 7 - 9).

* This sample size depends on how rare the rarest state is in the population. 

* My ants need to be observed for 11 minutes each. 

* Results are validated on simulated ants (pages 11 - 12). 

### Finding Optimal Segment Number

* A behavioral feature could be independent of the time it is sampled or dependent of the time it is sampled. Sampling strategies should minimize error for both types of features (pages 9 - 11). 

* Having 8 segments (I = 8) minimizes the normalized sum of both these types of error for both real ants (panel A) and simulated ants (Panel B). 

![](/Images/optimizationCurves.png)

* Normally this data would be sampled continuously (I = 1), so I = 8 represents a 57% decrease in the sum of the biases. 

* While it will likely be the case that having intermediate values of I will be optimal for most systems, this particular result occurs here because of how underlying data is distributed (Appendices 4 and 6). 
