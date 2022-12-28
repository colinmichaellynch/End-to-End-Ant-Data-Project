# Part 1

## Summary

Collecting behavioral data on ants can be a tricky affair, as many subtle features of their behavior cannot be tracked remotely. Instead, videos of ant colonies need to be annoted manually, which is a time-consuming process. Generally, animal behavior (as well as other types of categorical time-series) is sampled in one of two ways. It can be continuously sampled, where the animal is continuously watched for a predetermined amount of time. Alternativily, or it can be instananeously sampled, where behavior is recorded for a moment over regular intervals of time. Here, I developed a hybrid scheme called piecewise scan sampling where we record multiple continuous segments of time that are randomly scattered throughout the entire possible time series (page 4). The number of segments or intervals is I, and my goal here is twofold. I want to determine how long we need to watch each ant for, and I want to know what the optimal value of I is. These goals are then validated on simulated data. 

## Estimating Sample Size 

