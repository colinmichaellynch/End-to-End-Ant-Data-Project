# Part 2

## Summary

In order to use the sampling technique developed in Part 1, I took behavioral recordings of ants in their nests while also recording metabolism (Prospectus pages 13 - 15). I then spliced the video into 8 segments, and then recombined them to make a new video. Undergraduate research assistants then install a custom-made app onto MATLAB which they then use to record the spatial location of randomly-selected ants (which have a 3-color code painted on their backs), and then later they record the behavior of that ant. 

### Video Splicing and Data Collection Process 

* Raw video is first fed into segmenter

* Segmenter randomly selects starting points for continuous segments. The number and length of segments is determined by the user. 

* Segmenter splices these segments out of the original video, and then recombines them to form two new videos. One is a low resolution (1 frame per second) video which is used for spatial analysis. The high resolution video (24 frames per second) is used for manually determining behavior of focal ant. 

* Segmenter also outputs csv file which will be used as the input of spatial analysis. 

* RA opens spatial analysis and clicks location of focal ant with the following GUI: 

![](/Images/userInterface.png)

* After the spatial analysis software outputs a new CSV, the RA opens it and watches the high resolution video. They then record behavior into Excel. 
