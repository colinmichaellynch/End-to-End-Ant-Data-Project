# Part 2

## Summary

In order to use the sampling technique developed in Part 1, I took behavioral recordings of ants in their nests while also recording metabolism ([Prospectus](https://github.com/colinmichaellynch/End-to-End-Ant-Data-Project/blob/main/Supporting%20Documentation/Colin%20Prospectus.docx) pages 13 - 15). I then spliced the video into 8 segments, and then recombined them to make a new video. Undergraduate research assistants then install a custom-made app onto MATLAB which they then use to record the spatial location of randomly-selected ants (which have a 3-color code painted on their backs), and then later they record the behavior of that ant. Note that the attached scripts will not work without videos, which are too large to upload to Github. 

### Video Splicing and Data Collection Process 

* Raw video is first fed into segmenter

* Segmenter randomly selects starting points for continuous segments. The number and length of segments is determined by the user. 

* Segmenter splices these segments out of the original video, and then recombines them to form two new videos. One is a low resolution (1 frame per second) video which is used for spatial analysis. The high resolution video (24 frames per second) is used for manually determining behavior of focal ant. 

* Segmenter also outputs csv file which will be used as the input of spatial analysis. 

* RA opens spatial analysis and clicks location of focal ant with the following GUI: 

<p align="center">
  <img src=/Images/userInterface.png>
</p>

* After the spatial analysis software outputs a new CSV, the RA opens it and watches the high resolution video. They then record behavior into Excel. 

* The time it took for undergraduates to complete video analyses for each colony was 7 hours on average before we introduced the GUI. This time was reduced to 4 hours after they started using the GUI. RAs also reported less strain as a result of the analysis, indicating that their results could be more accurate. 

### Deriving Interesting Features of Behavior

* First I compile dataframes from different RAs, and then clean it so that the text from each RA matches standard format. 

* I then derive different features of individual and colony-level behaviors, although future analyses in Github will only focus on the former. 

* For instance, I can determine path lengths by tracking the trajectories of different ants (color) in the foraging arena:

<p align="center">
  <img src=/Images/antPaths.png>
</p>

* I can also measure where in the nest an ant performs different tasks (color): 

<p align="center">
  <img src=/Images/taskLocation.png>
</p>

* Ultimatly I derive 60+ features from 2 raw data points: task and location of task performance. 
