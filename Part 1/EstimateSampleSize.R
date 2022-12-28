rm(list = ls())

library(ggplot2)

maxSampleSize = 3500
repeats = 100
data = read.csv("~/behavioral_sampling/true_final_folder/from Michaela PC/all_ant_behaviors.csv")
data = data[,3:11]
CThreshold = .99

for(i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    if(data[i,j] == "Grooming"){
      data[i, j]= "Self Maintenance"
    } else if(data[i,j] == "Minimal-Movement"){
      data[i, j]= "Exploring"
    } else if(data[i,j] == "NM-chewcot"){
      data[i, j]= "Nest Maintenance"
    } else if(data[i,j] == "Walking"){
      data[i, j]= "Exploring"
    }
  }
}

MinSampleSize = c()
MinSampleSizeSimple = c()

for(i in 1:ncol(data)){
  
  certainty = c()
  NumericData = match(data[,i], unique(data[,i]))
  TaskNumber = length(unique(data[,i]))
  TheoreticalVariance = 2*(TaskNumber-1)
  TrueCounts = table(NumericData)
  TrueProbs = as.numeric(TrueCounts)/sum(TrueCounts)
  StatesPresent = 1:TaskNumber
  MinSampleSizeSimple[i] = 1/min(TrueProbs)
  
  certainty = c()
  
  for(j in 1:maxSampleSize){
    
    ChiSquared = c()
    
    for(k in 1:repeats){

      ObservedCounts = c()
      RandSample = sample(NumericData, j, replace = TRUE)
      ObservedCounts = table(RandSample)
      SampleStates = as.numeric(names(ObservedCounts))
      if(length(SampleStates) != length(StatesPresent)){
        for(t in 1:length(StatesPresent)){
          SampleStates = as.numeric(names(ObservedCounts)) ### add to sims script 
          SampleStates = SampleStates[is.na(SampleStates)==FALSE]
          BooleanLog = c()
          if((StatesPresent[t] %in% SampleStates)==FALSE){
            BooleanLog = as.numeric(StatesPresent[t]) < SampleStates
            if(all(BooleanLog == TRUE)){
              ObservedCounts = c(0, ObservedCounts)
            } else if(all(BooleanLog == FALSE)) {
              ObservedCounts = c(ObservedCounts, 0)
            } else {
              index = min(which(BooleanLog))-1
              ObservedCounts = c(ObservedCounts[1:index], 0, ObservedCounts[(index+1):length(ObservedCounts)])
            }
          }
        }
      }

      ObservedCounts = as.numeric(ObservedCounts)
      ExpectedCounts = j*TrueProbs
      ChiSquared[k] = sum(((ObservedCounts-ExpectedCounts)^2)/ExpectedCounts)

    }
    
    certainty[j] = TheoreticalVariance/var(ChiSquared)
    print(i/9)
    print(j/maxSampleSize)
      
  }
  
  fit = loess(certainty ~ c(1:maxSampleSize))
  prediction = predict(fit, c(1:maxSampleSize))
  MinSampleSize[i] = min(which(prediction > CThreshold))
  tempData = data.frame(SampleSize = 1:maxSampleSize, Certainty = certainty, MinSampleSize = MinSampleSize[i], cross = CThreshold)
  
  if(i == 4){
    ggplot(tempData, aes(SampleSize, Certainty)) +
      geom_line() +
      geom_smooth(method = 'loess', formula = 'y ~ x') + 
      geom_vline(data = tempData, aes(xintercept = MinSampleSize), linetype = "dashed", size = 1.1, col = "darkgoldenrod") + 
      geom_hline(data = tempData, aes(yintercept = cross), linetype = "dashed", size = 1.1, col = "darkgoldenrod") +
      theme_bw() + xlab("Sample Size") + theme(text = element_text(size=12.5)) 
  }

}
    
mean(MinSampleSize)
mean(MinSampleSizeSimple)

summary(lm(MinSampleSizeSimple~MinSampleSize))
summary(lm(MinSampleSize~MinSampleSizeSimple))
tempData2 = data.frame(MinSampleSize = MinSampleSize, MinSampleSizeSimple = MinSampleSizeSimple)

ggplot(tempData2, aes(x = MinSampleSize, y = MinSampleSizeSimple)) + geom_point() + geom_smooth(method = 'lm', formula = 'y ~ x') + theme_bw() + xlab("Chi Squared Sample Size") + ylab("Binomial Sample Size") + theme(text = element_text(size=12.5)) + coord_fixed()
