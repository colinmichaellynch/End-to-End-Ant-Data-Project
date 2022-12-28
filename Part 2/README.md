# Part 2

## Summary

In order to use the sampling technique developed in Part 1, I took behavioral recordings of ants in their nests while also recording metabolism (Prospectus pages 13 - 15). I then spliced the video into 8 segments, and then recombined them to make a new video. Undergraduate research assistants then install a custom-made app onto MATLAB which they then use to record the spatial location of randomly-selected ants (which have a 3-color code painted on their backs), and then later they record the behavior of that ant. Note that the attached scripts will not work without videos, which are too large to upload to Github. 

### Video Splicing and Data Collection Process 

* Raw video is first fed into segmenter

* Segmenter randomly selects starting points for continuous segments. The number and length of segments is determined by the user. 

* Segmenter splices these segments out of the original video, and then recombines them to form two new videos. One is a low resolution (1 frame per second) video which is used for spatial analysis. The high resolution video (24 frames per second) is used for manually determining behavior of focal ant. 

* Segmenter also outputs csv file which will be used as the input of spatial analysis. 

* RA opens spatial analysis and clicks location of focal ant with the following GUI: 

![](/Images/userInterface.png)

* After the spatial analysis software outputs a new CSV, the RA opens it and watches the high resolution video. They then record behavior into Excel. 

### Deriving Interesting Features of Behavior

* First I compile dataframes from different RAs, and then clean it so that the text from each RA matches standard format. 

* I then derive different features of individual and colony-level behaviors, although future analyses in Github will only focus on the former. 

* For instance, I can determine path lengths by tracking the trajectories of different ants (color) in the foraging arena:

![](/Images/antPaths.png)

* I can also measure where in the nest an ant performs different tasks (color): 

![](/Images/taskLocation.png)

* Ultimatly I derive 60+ features from 2 raw data points: task and location of task performance. 
