rm(list = ls())

library(tidyverse)
library(tools)
library(dplyr)
library(sp)
library(devtools)
library(trajr)
library(ggpubr)
library(ptinpoly)
library(rsdepth)
library(rgeos)
library(SimilarityMeasures)
library(DataCombine)
library(reshape2)
library(multcompView)

setwd("C:/Users/user/Documents/Github/Compile Data")

a = character(0)

### Import and clean data 

for (data in list.files()){
  
  # Create the first data if no data exist yet
  if (!exists("dataset")){
    dataset <- read.csv(data, header=TRUE)
  }
  
  # if data already exist, then append it together
  if (exists("dataset")){
    if(file_ext(data) == "csv"){ #make sure other csv files other than raw data are in a different analysis folder
      tempory <-read.csv(data, header=TRUE)
      if(names(tempory)[1] == "?..Analyst"){
        names(tempory)[1] = "Analyst"
      }
      dataset <-unique(rbind(dataset, tempory))
      rm(tempory)
    }
  }
}

rm(data)

unique(dataset$Task)

dataset = na.omit(dataset)

dataset$Task[dataset$Task=="Nest-Maintenance"] = "Nest Maintenance"
dataset$Task[dataset$Task=="Self-Maintenance"] = "Self Maintenance"
dataset$Task[dataset$Task=="Broodcare"] = "Brood Care"
dataset$Task[dataset$Task=="Brood Care "] = "Brood Care"
dataset$Task[dataset$Task=="brood care"] = "Brood Care"
dataset$Task[dataset$Task=="brood Care"] = "Brood Care"
dataset$Task[dataset$Task=="Brood care"] = "Brood Care"
dataset$Task[dataset$Task=="Exploring "] = "Exploring"
dataset$Task[dataset$Task=="exploring "] = "Exploring"
dataset$Task[dataset$Task=="exploring"] = "Exploring"
dataset$Task[dataset$Task=="Resting "] = "Resting"
dataset$Task[dataset$Task=="resting "] = "Resting"
dataset$Task[dataset$Task=="resting"] = "Resting"
dataset$Task[dataset$Task==""] = "Resting"
dataset$Task[dataset$Task=="allogrooming"] = "Allogrooming"
dataset$Task[dataset$Task=="Trash Maintanence "] = "Trash Maintenance"
dataset$Task[dataset$Task=="Trash Maintenance "] = "Trash Maintenance"
dataset$Task[dataset$Task=="trash maintanence"] = "Trash Maintenance"
dataset$Task[dataset$Task=="self maintanence"] = "Self Maintenance"
dataset$Task[dataset$Task=="self Maintanence"] = "Self Maintenance"
dataset$Task[dataset$Task=="Self Maintanence "] = "Self Maintenance"
dataset$Task[dataset$Task=="self Maintenance"] = "Self Maintenance"
dataset$Task[dataset$Task=="Self Mainenance "] = "Self Maintenance"
dataset$Task[dataset$Task=="Self Maintenance "] = "Self Maintenance"
dataset$Task[dataset$Task=="Self Maintenace"] = "Self Maintenance"
dataset$Task[dataset$Task=="Antennating Nestmate "] = "Antennating Nestmate"
dataset$Task[dataset$Task=="antennating Nestmate"] = "Antennating Nestmate"
dataset$Task[dataset$Task=="trash maintenance"] = "Trash Maintenance"
dataset$Task[dataset$Task=="Trash Maintanence"] = "Trash Maintenance"
dataset$Task[dataset$Task=="Trash Mainentance"] = "Trash Maintenance"
dataset$Task[dataset$Task=="food Processing"] = "Food Processing"
dataset$Task[dataset$Task=="Food Processing "] = "Food Processing"
dataset$Task[dataset$Task=="Food processing"] = "Food Processing"
dataset$Task[dataset$Task=="Nest Maintenance "] = "Nest Maintenance"
dataset$Task[dataset$Task=="Nest Mainentance"] = "Nest Maintenance"
dataset$Task[dataset$Task=="Allogrooming "] = "Allogrooming"
dataset$Task[dataset$Task==" Allogrooming"] = "Allogrooming"

listOfTasks = c("Resting", "Exploring", "Food Processing", "Trash Maintenance", "Self Maintenance", "Nest Maintenance", "Brood Care", "Antennating Nestmate", "Allogrooming")

dataset$Colony = gsub("'", '', dataset$Colony)
dataset$Date = gsub("'", '', dataset$Date)
dataset$Time = gsub("'", '', dataset$Time)
dataset$AntID = gsub("'", '', dataset$AntID)

indNumber = 10 
colID = unique(dataset$Colony)
colNumber = as.numeric(length(colID))
kMax = 9 
segmentNumber = 8 
lazyThreshold = .5 #resting proportion for when considered 'lazy' 
individualSeconds = 700
numberTasksOverlap = 8

dataTemp = subset(dataset, Colony=="")

#fix this for different sessions 
colonySize = c()
totalMass = c()
dataset$Session = rep(1, nrow(dataset))

for(i in 1:nrow(dataset)){
  if(dataset$Colony[i] == "17-1"){
    colonySize[i] = 12
    totalMass[i] = 0.1012
  } else if(dataset$Colony[i] == "17-5"){
    colonySize[i] = 20
    totalMass[i] = 0.3124
  } else if(dataset$Colony[i] == "17-6"){
    colonySize[i] = 17
    totalMass[i] = 0.1432
  }  else if(dataset$Colony[i] == "17-23"){
    colonySize[i] = 25
    totalMass[i] = 0.1207
  } else if(dataset$Colony[i] == "17-19"){
    colonySize[i] = 56
    totalMass[i] = 0.2733
  } else if(dataset$Colony[i] == "17-20"){
      colonySize[i] = 22 
      totalMass[i] = 0.2189
  } else if(dataset$Colony[i] == "17-10"){
    colonySize[i] = 56 
    totalMass[i] = 0.2808
  } else if(dataset$Colony[i] == "17-15"){
    colonySize[i] = 47 
    totalMass[i] = 0.2413
  } else if(dataset$Colony[i] == "17-2"){
    colonySize[i] = 41 
    totalMass[i] = 0.2587
  } else if(dataset$Colony[i] == "17-9"){
    colonySize[i] = 71
    totalMass[i] = 0.3165
  } else if(dataset$Colony[i] == "17-12"){
    colonySize[i] = 55 
    totalMass[i] = 0.199
  } else if(dataset$Colony[i] == "17-26"){
    colonySize[i] = NA 
    totalMass[i] = 0.244
  } else if(dataset$Colony[i] == "17-27"){
    colonySize[i] = 99 
    totalMass[i] = 0.5101
  } else if(dataset$Colony[i] == "17-25"){
    colonySize[i] = 44 
    totalMass[i] = 0.2541
  } else if(dataset$Colony[i] == "17-11"){
    colonySize[i] = 48
    totalMass[i] = 0.2707
  } else if(dataset$Colony[i] == "17-8"){
    colonySize[i] = 53
    totalMass[i] = 0.3221
  } else if(dataset$Colony[i] == "17-29"){
    colonySize[i] = 62 
    totalMass[i] = 0.5442
  } else if(dataset$Colony[i] == "17-7"){
    colonySize[i] = 15
    totalMass[i] = 0.1599
  } else if(dataset$Colony[i] == "17-22"){
    colonySize[i] = 66
    totalMass[i] = 0.3205
  } else if(dataset$Colony[i] == "17-13"){
    colonySize[i] = 24 
    totalMass[i] = 0.1294
  } else if(dataset$Colony[i] == "17-16"){
    colonySize[i] = 7
  } else if(dataset$Colony[i] == "17-21"){
    colonySize[i] = 44
    totalMass[i] = 0.1118
  } else if(dataset$Colony[i] == "17-17"){
    colonySize[i] = 8 
    totalMass[i] = 0.056
  } else if(dataset$Colony[i] == "17-14"){
    colonySize[i] = 9
    totalMass[i] = 0.0715
  } else if(dataset$Colony[i] == "17-28"){
    colonySize[i] = 29
    totalMass[i] = 0.1528
  }
}

dataset$colonySize = colonySize
dataset$totalMass = totalMass
#dataset$session = session 

taskList = unique(dataset$Task)
colonyList = unique(dataset$Colony)
rm(dataTemp)

### Delete out of bounds points for nest

dataset = dataset[dataset$XCoord > 0,]
inTube = c()

for(i in 1:length(unique(dataset$Colony))){
  
  colonyID = colonyList[i] 
  dataColonyNest = subset(dataset, Colony == colonyID)
  #plot(dataColonyNest$YCoord~dataColonyNest$XCoord)
  #par(new = TRUE)
  
  if(colonyID == "17-23"){
    xCoord = c(666, 700, 1000, 1400, 1400, 1100, 1000, 700)
    yCoord = c(100,0, 0, 300, 340, 130, 140, 135)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon 
      } else {
        inTube = c(inTube, 0) 
      }
    }
    
  } else if(colonyID == "17-6"){
    
    xCoord = c(666, 700, 1000, 1100, 1000, 700)
    yCoord = c(100,0, 0, 130, 140, 135)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-5"){
    
    xCoord = c(-120, 0, -120, 0)
    yCoord = c(800, 850, 850, 800)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-10"){
    
    xCoord = c(685, 715, 907, 1036, 1090, 980, 755)
    yCoord = c(572, 533, 655, 533, 531, 686, 671)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-12"){
    
    xCoord = c(576, 490, 760, 998, 861, 578)
    yCoord = c(142, 125, 3, 228, 215, 144)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-22"){
    
    xCoord = c(742, 894, 1094, 1097, 1063, 770)
    yCoord = c(160, 57, 121, 209, 210, 181)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-8"){
    
    xCoord = c(541, 601, 1057, 1029, 934, 609)
    yCoord = c(36, -15, -5, 196, 201, 68)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-11"){
    
    xCoord = c(541, 601, 1057, 1029, 934, 609)
    yCoord = c(36, -15, -5, 196, 201, 68)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-13"){
    
    xCoord = c(541, 601, 1057, 1029, 934, 609)
    yCoord = c(36, -15, -5, 196, 201, 68)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-17"){
    
    xCoord = c(682, 750, 878, 1000, 1120, 1091, 1056, 703)
    yCoord = c(96, -20, -20, -20, 142, 264, 264, 120)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-29"){
    
    xCoord = c(684, 868, 1030, 1031, 1004, 712)
    yCoord = c(80, -3, 85, 161, 161, 101)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-6"){
    
    xCoord = c(661, 768, 1064, 1072, 1022, 697)
    yCoord = c(113, -26, 18, 134, 146, 138)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-15"){
    
    xCoord = c(720, 860, 1200, 1046, 1008, 739)
    yCoord = c(140, 1, 226, 264, 264, 164)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-16"){
    
    xCoord = c(689, 993, 1025, 835, 662)
    yCoord = c(600, 521, 539, 1000, 626)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  } else if(colonyID == "17-25"){
    
    xCoord = c(666, 950, 1099, 1060, 697)
    yCoord = c(69, -500, 210, 220, 87)
    #polygon(xCoord, yCoord)
    poly = cbind(xCoord, yCoord)
    
    for(j in 1:nrow(dataColonyNest)){
      point = rbind(c(dataColonyNest$XCoord[j], dataColonyNest$YCoord[j]))
      if(pip2d(poly, point) == 1){
        inTube = c(inTube, 1) #inside of polygon, so in the tube 
      } else {
        inTube = c(inTube, 0) 
      }
      
    }
  }
  
  else {
    for(j in 1:nrow(dataColonyNest)){
      inTube = c(inTube, 0) 
    }
  }
  
}

dataset$inTube = inTube

colonyID = colonyList[15] 
dataColonyNest = subset(dataset, Colony == colonyID)
ggplot(dataColonyNest, aes(x = XCoord, y = YCoord, col = as.factor(inTube))) + geom_point() 


### --- Behavioral data --- ### 


# Individual measurements (not including prop for every task or average bout length for all tasks)

#add trajectory length of exploring for each individual 

#constants for estimated joules / mass used by individuals during different tasks 
averageKG = .0000032
pixelsToMConverter = 8700.2067
MCOT = (137.9+147+129)/3 #Messor pergandei: 137.9, pogonomyrmex rugosus: 147, pogo maricopa: 129
Y = (0+2.03+1.06)/3 #Messor pergandei: 0, pogonomyrmex rugosus: 2.03, pogo maricopa: 1.06
SMR = (4.2+0.008537902)/2 #Messor pergandei: 4.2, pogonomyrmex rugosus: NA, pogo maricopa: 1143 * averageKG^.933 = 0.008537902

#individual measurements
date = analyst = antID = numUniqueTasks = degreeOfSpecialization = propResting = propBrood = propExploring = propAllogrooming = propAntennatingNestmate = propTrashMaintenance = propNestMaintenance = propSelfMaintenance = propFoodProcessing = numberOfTaskSwitches = numberOfAntennations = interboutInterval = colonySize = totalMass = boutLengthBC = boutLengthR = boutLengthFP = boutLengthTM = boutLengthAN = boutLengthNM = boutLengthSM = boutLengthE = boutLengthA = velocity = colony = movementAreaForaging = movementAreaNest = sinuosityTrailNest = sinuosityTrailForaging = sessionInd = exploringLengthNest = exploringLengthForaging = probStop = probReturn = bcDistance = eDistance = fpDistance = rDistance = tmDistance = anDistance = nmDistance = smDistance = aDistance = bcSpeed = eSpeed = fpSpeed = rSpeed = tmSpeed = anSpeed = nmSpeed = smSpeed = aSpeed = bcTime = eTime = fpTime = rTime = tmTime = anTime = nmTime = smTime = aTime = bcjoulesPerMG = ejoulesPerMG = fpjoulesPerMG = rjoulesPerMG = tmjoulesPerMG = anjoulesPerMG = nmjoulesPerMG = smjoulesPerMG = ajoulesPerMG = matrix(, nrow = colNumber, ncol = indNumber)

#colony measurements 
dolTaskIntoInd = dolIndIntoTask = symmetricDol = meanDegreeOfSpecialization = colonyCol = dateCol = analystCol = colonySizeCol = totalMassCol =  broodCareTime = restingTime = exploringTime = foodProcessingTime = trashMaintenanceTime = selfMaintenanceTime = nestMaintenanceTime = antennatingNestmateTime = allogroomingTime = broodAreaNest = broodAreaForaging = lazyAntNumber = trailOverlapNest = trailOverlapForaging = overlappingForagingAntNumber = overlappingNestAntNumber = numberOfCoincidenceTasks = distanceOfCoincidenceTasks = distanceOfCoincidenceTasksMAD = sessionCol = taskOverlap = broodCentralDistance = nestArea = c()

for(i in 1:colNumber){
  
  dataTemp = subset(dataset, Colony == colID[i])
  indID = unique(dataTemp$AntID)
  
  taskMatrix = matrix(, nrow = kMax, ncol = indNumber)
  
  for(j in 1:indNumber){
    
    #individual measurements
    dataTemp2 = subset(dataTemp, AntID == indID[j])
    
    colony[i,j] = dataTemp2$Colony[1]
    date[i,j] = dataTemp2$Date[1]
    analyst[i,j] = dataTemp2$Analyst[1]
    antID[i,j] = dataTemp2$AntID[1]
    colonySize[i,j] = dataTemp2$colonySize[1]
    sessionInd[i,j] = dataTemp2$Session[1]
    totalMass[i,j] = dataTemp2$totalMass[1]
    
    #Time on task
    
    bcTime[i,j] = sum(dataTemp2$Task=="Brood Care")
    eTime[i,j] = sum(dataTemp2$Task=="Exploring")
    fpTime[i,j] = sum(dataTemp2$Task=="Food Processing")
    rTime[i,j] = sum(dataTemp2$Task=="Resting")
    tmTime[i,j] = sum(dataTemp2$Task=="Trash Maintenance")
    anTime[i,j] = sum(dataTemp2$Task=="Antennating Nestmate")
    nmTime[i,j] = sum(dataTemp2$Task=="Nest Maintenance")
    smTime[i,j] = sum(dataTemp2$Task=="Self Maintenance")
    aTime[i,j] = sum(dataTemp2$Task=="Allogrooming")
      
    #task distance/speed
    if(nrow(dataTemp2)>0){
      tasksVec = c()
      distancesVec = c()
      lengthsVec = c()
      for(s in 1:8){
        
        dataTempS = subset(dataTemp2, SegmentNumber==s)
        
        runLength = rle(dataTempS$Task)
        distance = c()
        counter = 0
        for(k in 1:length(runLength$lengths)){
          dataTempSL = dataTempS[(counter+1):(counter+runLength$lengths[k]),]
          coords <- data.frame(x = dataTempSL$XCoord, 
                               y = dataTempSL$YCoord, 
                               times = 1:nrow(dataTempSL))
          trj = TrajFromCoords(coords)
          distance[k] = sum(TrajStepLengths(trj))
          counter = counter+runLength$lengths[k]
        }
        tasksVec = c(tasksVec, runLength$values)
        distancesVec = c(distancesVec, distance)
        lengthsVec = c(lengthsVec, runLength$lengths)
        
      }
      
      dataTempBehavior = data.frame(task = tasksVec, distance = distancesVec, length = lengthsVec)
      
      dataTempBC = subset(dataTempBehavior, task == "Brood Care")
      if(nrow(dataTempBC)==0){
        bcDistance[i,j] = NA
        bcSpeed[i,j] = NA
        bcjoulesPerMG[i,j] = 0
      } else {
        bcDistance[i,j] = sum(dataTempBC$distance/pixelsToMConverter)
        bcSpeed[i,j] = bcDistance[i,j]/(sum(dataTempBC$length))
        #bcjoulesPerMG[i,j] = sum(dataTempBC$length)*averageKG*(SMR + Y +MCOT*bcSpeed[i,j])
        bcjoulesPerMG[i,j] = (SMR + Y +MCOT*bcSpeed[i,j])
      }
      dataTempE = subset(dataTempBehavior, task == "Exploring")
      if(nrow(dataTempE)==0){
        eDistance[i,j] = NA
        eSpeed[i,j] = NA
        ejoulesPerMG[i,j] = 0
      } else {
        eDistance[i,j] = sum(dataTempE$distance/pixelsToMConverter)
        eSpeed[i,j] = eDistance[i,j]/(sum(dataTempE$length))
        #ejoulesPerMG[i,j] = sum(dataTempE$length)*averageKG*(SMR + Y +MCOT*eSpeed[i,j])
        ejoulesPerMG[i,j] = (SMR + Y +MCOT*eSpeed[i,j])
      }
      dataTempFP = subset(dataTempBehavior, task == "Food Processing")
      if(nrow(dataTempE)==0){
        fpDistance[i,j] = NA
        fpSpeed[i,j] = NA
        fpjoulesPerMG[i,j] = 0
      } else {
        fpDistance[i,j] = sum(dataTempFP$distance/pixelsToMConverter)
        fpSpeed[i,j] = fpDistance[i,j]/(sum(dataTempFP$length))
        #fpjoulesPerMG[i,j] = sum(dataTempFP$length)*averageKG*(SMR + Y +MCOT*fpSpeed[i,j])
        fpjoulesPerMG[i,j] = (SMR + Y +MCOT*fpSpeed[i,j])
      }
      dataTempR = subset(dataTempBehavior, task == "Resting")
      if(nrow(dataTempR)==0){
        rDistance[i,j] = NA
        rSpeed[i,j] = NA
        rjoulesPerMG[i,j] = 0
      } else {
        rDistance[i,j] = sum(dataTempR$distance/pixelsToMConverter)
        rSpeed[i,j] = rDistance[i,j]/(sum(dataTempR$length))
        #rjoulesPerMG[i,j] = sum(dataTempR$length)*averageKG*(SMR + Y +MCOT*rSpeed[i,j])
        rjoulesPerMG[i,j] = (SMR + Y +MCOT*rSpeed[i,j])
      }
      dataTempTM = subset(dataTempBehavior, task == "Trash Maintenance")
      if(nrow(dataTempTM)==0){
        tmDistance[i,j] = NA
        tmSpeed[i,j] = NA
        tmjoulesPerMG[i,j] = 0
      } else {
        tmDistance[i,j] = sum(dataTempTM$distance/pixelsToMConverter)
        tmSpeed[i,j] = tmDistance[i,j]/(sum(dataTempTM$length))
        #tmjoulesPerMG[i,j] = sum(dataTempTM$length)*averageKG*(SMR + Y +MCOT*tmSpeed[i,j])
        tmjoulesPerMG[i,j] = (SMR + Y +MCOT*tmSpeed[i,j])
      }
      dataTempNM = subset(dataTempBehavior, task == "Nest Maintenance")
      if(nrow(dataTempNM)==0){
        nmDistance[i,j] = NA
        nmSpeed[i,j] = NA
        nmjoulesPerMG[i,j] = 0
      } else {
        nmDistance[i,j] = sum(dataTempNM$distance/pixelsToMConverter)
        nmSpeed[i,j] = nmDistance[i,j]/(sum(dataTempNM$length))
        #nmjoulesPerMG[i,j] = sum(dataTempNM$length)*averageKG*(SMR + Y +MCOT*nmSpeed[i,j])
        nmjoulesPerMG[i,j] = (SMR + Y +MCOT*nmSpeed[i,j])
      }
      dataTempAN = subset(dataTempBehavior, task == "Antennating Nestmate")
      if(nrow(dataTempAN)==0){
        anDistance[i,j] = NA
        anSpeed[i,j] = NA
        anjoulesPerMG[i,j] = 0
      } else {
        anDistance[i,j] = sum(dataTempAN$distance/pixelsToMConverter)
        anSpeed[i,j] = anDistance[i,j]/(sum(dataTempAN$length))
        #anjoulesPerMG[i,j] = sum(dataTempAN$length)*averageKG*(SMR + Y +MCOT*anSpeed[i,j])
        anjoulesPerMG[i,j] = (SMR + Y +MCOT*anSpeed[i,j])
      }
      dataTempSM = subset(dataTempBehavior, task == "Self Maintenance")
      if(nrow(dataTempSM)==0){
        smDistance[i,j] = NA
        smSpeed[i,j] = NA
        smjoulesPerMG[i,j] = 0
      } else {
        smDistance[i,j] = sum(dataTempSM$distance/pixelsToMConverter)
        smSpeed[i,j] = smDistance[i,j]/(sum(dataTempSM$length))
        #smjoulesPerMG[i,j] = sum(dataTempSM$length)*averageKG*(SMR + Y +MCOT*smSpeed[i,j])
        smjoulesPerMG[i,j] = (SMR + Y +MCOT*smSpeed[i,j])
      }
      dataTempA = subset(dataTempBehavior, task == "Allogrooming")
      if(nrow(dataTempA)==0){
        aDistance[i,j] = NA
        aSpeed[i,j] = NA
        ajoulesPerMG[i,j] = 0
      } else {
        aDistance[i,j] = sum(dataTempA$distance/pixelsToMConverter)
        aSpeed[i,j] = aDistance[i,j]/(sum(dataTempA$length))
        #ajoulesPerMG[i,j] = sum(dataTempA$length)*averageKG*(SMR + Y +MCOT*aSpeed[i,j])
        ajoulesPerMG[i,j] = (SMR + Y +MCOT*aSpeed[i,j])
      }
      
    }
    
    #movement range 
    dataTempForaging = subset(dataTemp2, Chamber == "Foraging Arena")
    if(nrow(dataTempForaging) == 0){
      movementAreaForaging[i,j]  <- NA
    } else {
      box.coords <- matrix(c(dataTempForaging$XCoord, dataTempForaging$YCoord), ncol = 2, nrow = nrow(dataTempForaging))
      box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
      box.hpts <- c(box.hpts, box.hpts[1])
      box.chull.coords <- box.coords[box.hpts,]
      chull.poly <- Polygon(box.chull.coords, hole=F)
      movementAreaForaging[i,j]  <- chull.poly@area
    }
    
    dataTempNest = subset(dataTemp2, Chamber == "Nest")
    if(nrow(dataTempNest) == 0){
      movementAreaNest[i,j]  <- NA
    } else {
      box.coords <- matrix(c(dataTempNest$XCoord, dataTempNest$YCoord), ncol = 2, nrow = nrow(dataTempNest))
      box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
      box.hpts <- c(box.hpts, box.hpts[1])
      box.chull.coords <- box.coords[box.hpts,]
      chull.poly <- Polygon(box.chull.coords, hole=F)
      movementAreaNest[i,j]  <- chull.poly@area
    }
  
    behaviorData = rle(dataTemp2$Task)
    
    numUniqueTasks[i,j] = length(unique(behaviorData[[2]]))
    numberOfTaskSwitches[i,j] = length(behaviorData$lengths)
    
    propResting[i,j] = sum(behaviorData$lengths[behaviorData$values == "Resting"])/sum(behaviorData$lengths)
    propBrood[i,j] = sum(behaviorData$lengths[behaviorData$values == "Brood Care"])/sum(behaviorData$lengths)
    propExploring[i,j] = sum(behaviorData$lengths[behaviorData$values == "Exploring"])/sum(behaviorData$lengths)
    propAllogrooming[i,j] = sum(behaviorData$lengths[behaviorData$values == "Allogrooming"])/sum(behaviorData$lengths)
    propAntennatingNestmate[i,j] = sum(behaviorData$lengths[behaviorData$values == "Antennating Nestmate"])/sum(behaviorData$lengths)
    propTrashMaintenance[i,j] = sum(behaviorData$lengths[behaviorData$values == "Trash Maintenance"])/sum(behaviorData$lengths)
    propNestMaintenance[i,j] = sum(behaviorData$lengths[behaviorData$values == "Nest Maintenance"])/sum(behaviorData$lengths)
    propSelfMaintenance[i,j] = sum(behaviorData$lengths[behaviorData$values == "Self Maintenance"])/sum(behaviorData$lengths)
    propFoodProcessing[i,j] = sum(behaviorData$lengths[behaviorData$values == "Food Processing"])/sum(behaviorData$lengths)
    
    numberOfAntennations[i,j] = length(behaviorData$lengths[behaviorData$values == "Antennating Nestmate"])
    
    #Bout lengths
    boutLengthBC[i,j] = mean(behaviorData$lengths[behaviorData$values == "Brood Care"])
    if(is.na(boutLengthBC[i,j])){
      boutLengthBC[i,j] = 0
    }
    
    boutLengthR[i,j] = mean(behaviorData$lengths[behaviorData$values == "Resting"])
    if(is.na(boutLengthR[i,j])){
      boutLengthR[i,j] = 0
    }
    
    boutLengthFP[i,j] = mean(behaviorData$lengths[behaviorData$values == "Food Processing"])
    if(is.na(boutLengthFP[i,j])){
      boutLengthFP[i,j] = 0
    }
    
    boutLengthTM[i,j] = mean(behaviorData$lengths[behaviorData$values == "Trash Maintenance"])
    if(is.na(boutLengthTM[i,j])){
      boutLengthTM[i,j] = 0
    }

    boutLengthAN[i,j] = mean(behaviorData$lengths[behaviorData$values == "Antennating Nestmate"])
    if(is.na(boutLengthAN[i,j])){
      boutLengthAN[i,j] = 0
    }
    
    boutLengthNM[i,j] = mean(behaviorData$lengths[behaviorData$values == "Nest Maintenance"])
    if(is.na(boutLengthNM[i,j])){
      boutLengthNM[i,j] = 0
    }
    
    boutLengthSM[i,j] = mean(behaviorData$lengths[behaviorData$values == "Self Maintenance"])
    if(is.na(boutLengthSM[i,j])){
      boutLengthSM[i,j] = 0
    }
    
    boutLengthE[i,j] = mean(behaviorData$lengths[behaviorData$values == "Exploring"])
    if(is.na(boutLengthE[i,j])){
      boutLengthE[i,j] = 0
    }
    
    boutLengthA[i,j] = mean(behaviorData$lengths[behaviorData$values == "Allogrooming"])
    if(is.na(boutLengthA[i,j])){
      boutLengthA[i,j] = 0
    }
    
    #prob of resting after task and probability of returning to a task after you've switched out of it
    behaviorDataFrame = data.frame(lengths = behaviorData$lengths, values = behaviorData$values)
    statesPresent = unique(behaviorDataFrame$values)
    statesPresent = statesPresent[statesPresent!="Resting"]
    numState = length(statesPresent)
    probStopVec = c()
    probReturnVec = c()
    
    if(identical(a, statesPresent) || !("Resting" %in% unique(behaviorDataFrame$values))){
      
      probStop[i,j] = NA
      probReturn[i,j] = NA
      
    } else {
      
        for(k in 1:numState){
          countStop = 0
          countSwitch = 0
          countState = sum(behaviorDataFrame$values==statesPresent[k])
          for(z in 1:(nrow(behaviorDataFrame)-1)){
            if(behaviorDataFrame$values[z]==statesPresent[k] && behaviorDataFrame$values[z+1]=="Resting"){
              countStop = countStop+1
            }
          }
          
          probStopVec[k] = countStop/countState
          
          if(numState>1){
              for(z in 1:(nrow(behaviorDataFrame)-2)){
                if(behaviorDataFrame$values[z]==statesPresent[k] && behaviorDataFrame$values[z+2]==statesPresent[k]){
                  countSwitch = countSwitch+1
                }
              }
              
              probReturnVec[k] = countSwitch/countState
              
          } else {
            probReturnVec[k] = NA
          }
        } 
        
        probStopVec[is.na(probStopVec)] = 0
        probReturnVec[is.na(probReturnVec)] = 0
        probStop[i,j] = mean(probStopVec)
        probReturn[i,j] = mean(probReturnVec)
      
    }

    #degree of specialization 
    indTaskList = unique(behaviorData$values)
    propTasks = c()
    K = numUniqueTasks[i,j]
    
    for(k in 1:K){
      propTasks[k] = sum(behaviorData$lengths[behaviorData$values == indTaskList[k]])/sum(behaviorData$lengths)
    }
    
    if(K < 9){
      propTasks = c(propTasks, rep(0, 9-K))
    }
    
    degreeOfSpecialization[i,j] = sum(abs(propTasks-1/kMax))/(2-(2/kMax))
    
    rm(propTasks)
    sInterboutIntervals = c()
    sVelocity = c()

    #interbout interval and velocity 
    for(s in 1:segmentNumber){
      
      dataTemp3 = subset(dataTemp2, SegmentNumber == s)
      behaviorData2 = rle(dataTemp3$Task)
      dataVis = data.frame(Task = behaviorData2$values, Length = behaviorData2$lengths)
      allInterboutIntervals = c()
      
      for(k in 1:K){
        if(length(behaviorData2$lengths[behaviorData2$values == indTaskList[k]])>1){
          counter = 0
          boutLength = c()
          startIndex = which.min(match(dataVis$Task, indTaskList[k]))
          for(l in startIndex:nrow(dataVis)){
            if(dataVis$Task[l] != indTaskList[k]){
              counter = counter + dataVis$Length[l]
            } else {
              boutLength = c(boutLength, counter)
              counter = 0
            }
          }
          boutLength = boutLength[boutLength!=0]
          allInterboutIntervals[k] = mean(boutLength)
        } else {
          allInterboutIntervals[k] = NA
        }
      }
      
      sInterboutIntervals[s] = mean(allInterboutIntervals, na.rm=TRUE)
      if(is.na(sInterboutIntervals[s])){
        sInterboutIntervals[s] = 0
      }
      
      rm(allInterboutIntervals)
      
      velocitySingle = sqrt((dataTemp3$XCoord - lag(dataTemp3$XCoord, default = 0))^2 + (dataTemp3$YCoord - lag(dataTemp3$YCoord, default = 0))^2)
      velocitySingle[1] = NA
      sVelocity[s] = mean(velocitySingle, na.rm=TRUE)
      
    }
    
    interboutInterval[i,j] = mean(sInterboutIntervals, na.rm=TRUE)
    velocity[i,j] = mean(sVelocity, na.rm=TRUE)
      
    #tortuosity or sinuosity
    if(nrow(dataTempForaging) == 0){
      sinuosityTrailForaging[i,j] = NA
    } else { 
      sins = c()
      for(s in 1:segmentNumber){
        dataTempForagingSeg = subset(dataTempForaging, SegmentNumber == s)
        if(nrow(dataTempForagingSeg) == 0){
          sins[s] = NA
        } else {
          coords <- data.frame(x = dataTempForagingSeg$XCoord, 
                               y = dataTempForagingSeg$YCoord, 
                               times = 1:nrow(dataTempForagingSeg))
          trj <- TrajFromCoords(coords)
          if(sum(trj$displacement) == 0){
            sins[s] = NA
          } else {
            #plot(trj)
            resampled <- TrajRediscretize(trj, 1)
            sins[s] <- TrajSinuosity2(resampled)
          }
        }

      }
      
      sinuosityTrailForaging[i,j] = mean(sins, na.rm=TRUE)

    }
    
    if(nrow(dataTempNest) == 0){
      sinuosityTrailNest[i,j] = NA
    } else { 
      sins = c()
      for(s in 1:segmentNumber){
        dataTempNestSeg = subset(dataTempNest, SegmentNumber == s)
        if(nrow(dataTempNestSeg) ==0){
          sins[s] = NA
        } else {
          coords <- data.frame(x = dataTempNestSeg$XCoord, 
                               y = dataTempNestSeg$YCoord, 
                               times = 1:nrow(dataTempNestSeg))
          trj <- TrajFromCoords(coords)
          if(sum(trj$displacement) == 0){
            sins[s] = NA
          } else {
            #plot(trj)
            resampled <- TrajRediscretize(trj, 1)
            sins[s] <- TrajSinuosity2(resampled)
          }
        }

      }
      
      sinuosityTrailNest[i,j] = mean(sins, na.rm=TRUE)

    }
    
    #exploring trail length in nest
    
    dataTempExploringNest = subset(dataTempNest, Task == "Exploring")
    uniqueSegments = unique(dataTempExploringNest$SegmentNumber)
    segmentNumberExploring = length(uniqueSegments)
    trajectoryLengthVector = c()
    
    if(nrow(dataTempExploringNest) == 0){
      exploringLengthNest[i,j] = NA
      trajectoryLengthVector = NA
    } else {
      for(s in 1:segmentNumberExploring){
        dataTempExploringNestSegment = subset(dataTempExploringNest, SegmentNumber == uniqueSegments[s])
        coords <- data.frame(x = dataTempExploringNestSegment$XCoord, 
                             y = dataTempExploringNestSegment$YCoord, 
                             times = 1:nrow(dataTempExploringNestSegment))
        trjNest <- TrajFromCoords(coords)
        trajectoryLengthVector[s] = TrajLength(trjNest)
      }
    }
    exploringLengthNest[i,j] = sum(trajectoryLengthVector)
    
    #now foraging 
    
    dataTempExploringForaging = subset(dataTempForaging, Task == "Exploring")
    uniqueSegments = unique(dataTempExploringForaging$SegmentNumber)
    segmentNumberExploring = length(uniqueSegments)
    trajectoryLengthVector = c()
    
    if(nrow(dataTempExploringForaging) == 0){
      exploringLengthForaging[i,j] = NA
      trajectoryLengthVector = NA
    } else {
      trajectoryLengthVector = c()
      for(s in 1:segmentNumberExploring){
        dataTempExploringNestSegment = subset(dataTempExploringForaging, SegmentNumber == uniqueSegments[s])
        coords <- data.frame(x = dataTempExploringNestSegment$XCoord, 
                             y = dataTempExploringNestSegment$YCoord, 
                             times = 1:nrow(dataTempExploringNestSegment))
        trjForge <- TrajFromCoords(coords)
        trajectoryLengthVector[s] = TrajLength(trjForge)
      }
    }
    exploringLengthForaging[i,j] = sum(trajectoryLengthVector)
    
    #division of labor 
    
    for(k in 1:kMax){
      task = taskList[k]
      taskMatrix[k, j] = sum(behaviorData$lengths[behaviorData$values == task])
    }
    
    rm(dataTemp3, dataVis, behaviorData2, sInterboutIntervals)
    
  }
  
  taskMatrixProb = taskMatrix / sum(taskMatrix)
  
  #pInd = rep(1/nrow(taskMatrix), ncol(taskMatrix))
  pInd = rep(1/ncol(taskMatrix), ncol(taskMatrix))
  pTask = c()
  
  for(n in 1:nrow(taskMatrix)){
    pTask[n] = sum(taskMatrix[n, ]) / sum(taskMatrix)
  }
  
  hInd = (-1)*sum(pInd*log(pInd))
  internal = pTask*log(pTask)
  internal[is.na(internal)] = 0
  hTask = (-1)*sum(internal)
  
  IMat = taskMatrixProb
  
  for(m in 1:nrow(taskMatrix)){
    for(n in 1:ncol(taskMatrix)){
      IMat[m, n] = taskMatrixProb[m, n] * log(taskMatrixProb[m, n] / (pInd[n]*pTask[m]))
    }
  }
  
  IMat[is.na(IMat)] = 0
  I = sum(IMat)
  
  dolTaskIntoInd[i] = I/hInd #could be backwards?
  dolIndIntoTask[i] = I/hTask
  symmetricDol[i] = I/sqrt(hInd*hTask)
  meanDegreeOfSpecialization[i] = mean(degreeOfSpecialization[i,])
  
  colonyCol[i] = dataTemp$Colony[1]
  dateCol[i] = dataTemp$Date[1]
  analystCol[i] = dataTemp$Analyst[1]
  colonySizeCol[i] = dataTemp$colonySize[1]
  totalMassCol[i] = dataTemp$totalMass[1]
  sessionCol[i] = dataTemp$Session[1]
  
  #task allocation 
  
  broodCareTime[i] = length(dataTemp$Task[dataTemp$Task == "Brood Care"])
  restingTime[i] = length(dataTemp$Task[dataTemp$Task == "Resting"])
  exploringTime[i] = length(dataTemp$Task[dataTemp$Task == "Exploring"])
  foodProcessingTime[i] = length(dataTemp$Task[dataTemp$Task == "Food Processing"])
  trashMaintenanceTime[i] = length(dataTemp$Task[dataTemp$Task == "Trash Maintenance"])
  selfMaintenanceTime[i] = length(dataTemp$Task[dataTemp$Task == "Self Maintenance"])
  nestMaintenanceTime[i] = length(dataTemp$Task[dataTemp$Task == "Nest Maintenance"])
  antennatingNestmateTime[i] = length(dataTemp$Task[dataTemp$Task == "Antennating Nestmate"])
  allogroomingTime[i] = length(dataTemp$Task[dataTemp$Task == "Allogrooming"])
  
  #brood area and brood centrality 
  dataTempBroodNest = subset(dataTemp, Task == "Brood Care" & Chamber == "Nest" & inTube == 0)
  dataTempNestNoTube = subset(dataTemp, Chamber == "Nest" & inTube == 0)
  if(nrow(dataTempBroodNest) == 0){
    broodAreaNest[i]  <- NA
    broodCentralDistance[i]  = NA
    
    box.coords <- matrix(c(dataTempNestNoTube$XCoord, dataTempNestNoTube$YCoord), ncol = 2, nrow = nrow(dataTempNestNoTube))
    box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
    box.hpts <- c(box.hpts, box.hpts[1])
    box.chull.coords <- box.coords[box.hpts,]
    chull.poly <- Polygon(box.chull.coords, hole=F)
    nestArea[i]  <- chull.poly@area
    
  } else {
    box.coords <- matrix(c(dataTempBroodNest$XCoord, dataTempBroodNest$YCoord), ncol = 2, nrow = nrow(dataTempBroodNest))
    box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
    box.hpts <- c(box.hpts, box.hpts[1])
    box.chull.coords <- box.coords[box.hpts,]
    chull.poly <- Polygon(box.chull.coords, hole=F)
    broodAreaNest[i]  <- chull.poly@area
    
    #brood centrality 
    broodCentroid = centroid(box.chull.coords)
    
    box.coords <- matrix(c(dataTempNestNoTube$XCoord, dataTempNestNoTube$YCoord), ncol = 2, nrow = nrow(dataTempNestNoTube))
    box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
    box.hpts <- c(box.hpts, box.hpts[1])
    box.chull.coords <- box.coords[box.hpts,]
    chull.poly <- Polygon(box.chull.coords, hole=F)
    nestArea[i]  <- chull.poly@area
      
    nestCentroid = centroid(box.chull.coords)
    
    broodCentralDistance[i] = sqrt((broodCentroid[1]-nestCentroid[1])^2+(broodCentroid[2]-nestCentroid[2])^2)
  }
  
  dataTempBroodForaging = subset(dataTemp, Task == "Brood Care" & Chamber == "Foraging Arena"& inTube == 0)
  if(nrow(dataTempBroodForaging) == 0){
    broodAreaForaging[i]  <- NA
  } else {
    box.coords <- matrix(c(dataTempBroodForaging$XCoord, dataTempBroodForaging$YCoord), ncol = 2, nrow = nrow(dataTempBroodForaging))
    box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
    box.hpts <- c(box.hpts, box.hpts[1])
    box.chull.coords <- box.coords[box.hpts,]
    chull.poly <- Polygon(box.chull.coords, hole=F)
    broodAreaForaging[i]  <- chull.poly@area
  }
  
  #number of lazy ants
  lazyAnts = propResting[i,]
  lazyAnts = lazyAnts[lazyAnts>lazyThreshold]
  lazyAntNumber[i] = length(lazyAnts)
  
  #distinctness of walking trails, measured as overlap of trails when exploring
  #for pull out timesteps that are common between two individuals, divide rows into segments, find trajectory for each individual, and then find frecjet distance. Get average distance. Do this for all possible pairs of individuals and then average again
  dataTempExploringForaging = subset(dataTemp, Task == "Exploring" & Chamber == "Foraging Arena")
  antIDForaging = unique(dataTempExploringForaging$AntID)
  individualNumber = length(antIDForaging)
  overlappingForagingAntNumber[i] = individualNumber
  
  if(individualNumber<2){
    trailOverlapForaging[i] = NA
  } else {
    
    trailOverlapForagingInd = c()
    
    for(n in 1:(individualNumber-1)){
      
      trailOverlapForagingGroup = c()
      
      for(m in 1:(individualNumber-n)){
        dataTempIndividual = subset(dataTempExploringForaging, AntID == antIDForaging[n])
        dataTempIndividual2 = subset(dataTempExploringForaging, AntID == antIDForaging[n+m])
        
        commonTimes = dataTempIndividual$Time[(dataTempIndividual$Time %in% dataTempIndividual2$Time)]
        dataTempFinal = dataTempExploringForaging[(dataTempExploringForaging$Time %in% commonTimes),]
        segmentsPresent = unique(dataTempFinal$SegmentNumber)
        trailOverlapForagingSegment = c()
        overlapCount = c()
        
        for(s in 1:length(segmentsPresent)){
          dataTempFinalSegment = subset(dataTempFinal, SegmentNumber == segmentsPresent[s])
          dataTempIndividualSegment = subset(dataTempFinalSegment, AntID == antIDForaging[n])
          dataTempIndividualSegment2 = subset(dataTempFinalSegment, AntID == antIDForaging[n+m])
          coords1 <- matrix(c(dataTempIndividualSegment$XCoord, dataTempIndividualSegment$YCoord), nrow(dataTempIndividualSegment))
          coords2 <- matrix(c(dataTempIndividualSegment2$XCoord, dataTempIndividualSegment2$YCoord), nrow(dataTempIndividualSegment2))
          
          trailOverlapForagingSegment[s] = as.numeric(Frechet(coords1, coords2))
          overlapCount[s] = nrow(coords1)
          
        }
        
        weights = overlapCount/sum(overlapCount)
        trailOverlapForagingGroup[m] = weighted.mean(trailOverlapForagingSegment, weights, na.omit=TRUE)
        
      }
      
      trailOverlapForagingInd[n] = mean(trailOverlapForagingGroup, na.omit=TRUE)

    }
    
    trailOverlapForagingInd = as.numeric(na.omit(trailOverlapForagingInd))
    trailOverlapForaging[i] = mean(trailOverlapForagingInd, na.omit=TRUE)
    
  }
  
  #now trail overlap of nest
  
  dataTempExploringNest = subset(dataTemp, Task == "Exploring" & Chamber == "Nest")
  antIDNest = unique(dataTempExploringNest$AntID)
  individualNumber = length(antIDNest)
  overlappingNestAntNumber[i] = individualNumber
  
  if(individualNumber<2){
    trailOverlapForaging[i] = NA
  } else {
    
    trailOverlapNestInd = c()
    
    for(n in 1:(individualNumber-1)){
      
      trailOverlapNestGroup = c()
      
      for(m in 1:(individualNumber-n)){
        dataTempIndividual = subset(dataTempExploringNest, AntID == antIDNest[n])
        dataTempIndividual2 = subset(dataTempExploringNest, AntID == antIDNest[n+m])
        
        commonTimes = dataTempIndividual$Time[(dataTempIndividual$Time %in% dataTempIndividual2$Time)]
        dataTempFinal = dataTempExploringNest[(dataTempExploringNest$Time %in% commonTimes),]
        segmentsPresent = unique(dataTempFinal$SegmentNumber)
        trailOverlapNestSegment = c()
        overlapCount = c()
        
        for(s in 1:length(segmentsPresent)){
          dataTempFinalSegment = subset(dataTempFinal, SegmentNumber == segmentsPresent[s])
          dataTempIndividualSegment = subset(dataTempFinalSegment, AntID == antIDNest[n])
          dataTempIndividualSegment2 = subset(dataTempFinalSegment, AntID == antIDNest[n+m])
          coords1 <- matrix(c(dataTempIndividualSegment$XCoord, dataTempIndividualSegment$YCoord), nrow(dataTempIndividualSegment))
          coords2 <- matrix(c(dataTempIndividualSegment2$XCoord, dataTempIndividualSegment2$YCoord), nrow(dataTempIndividualSegment2))
          
          trailOverlapNestSegment[s] = as.numeric(Frechet(coords1, coords2))
          overlapCount[s] = nrow(coords1)
          
        }
        
        weights = overlapCount/sum(overlapCount)
        trailOverlapNestGroup[m] = weighted.mean(trailOverlapNestSegment, weights, na.omit=TRUE)
          
        trailOverlapNestGroup[m] = mean(trailOverlapNestSegment, na.omit=TRUE)
        
      }
      
      trailOverlapNestInd[n] = mean(trailOverlapNestGroup, na.omit=TRUE)
      
    }
    
    trailOverlapNestInd = as.numeric(na.omit(trailOverlapNestInd))
    trailOverlapNest[i] = mean(trailOverlapNestInd, na.omit=TRUE)
    
  }
  


  #https://link.springer.com/article/10.1007/s00265-019-2761-1, file:///C:/Users/user/Downloads/geoprocessing_2016_4_10_30078.pdf, https://cran.r-project.org/web/packages/trajectories/trajectories.pdf, https://cran.rstudio.com/web/packages/trajr/vignettes/trajr-vignette.html, 
  
  #maybe do average similarity between all pairwise comparisons? https://cran.r-project.org/web/packages/SimilarityMeasures/index.html, from paper above use fretchet distance? 
  
  #Number and distance of coincidence tasks, and indication of cooperation 
  #number means number of rows in dataframe where ants are doing same task

  individuals = unique(dataTemp$AntID)
  taskIndMatrix = matrix(, nrow = 727 , ncol = length(individuals))
  xIndMatrix = matrix(, nrow = 727 , ncol = length(individuals))
  yIndMatrix = matrix(, nrow = 727 , ncol = length(individuals))
  for(n in 1:length(individuals)){
    dataTempInd = subset(dataTemp, AntID == individuals[n])
    taskIndMatrix[1:nrow(dataTempInd), n] = dataTempInd$Task
    xIndMatrix[1:nrow(dataTempInd), n] = dataTempInd$XCoord
    yIndMatrix[1:nrow(dataTempInd), n] = dataTempInd$YCoord
  }
  taskIndMatrix = na.omit(taskIndMatrix)

  distance = c()
  counter = 0
  for(n in 1:(length(individuals)-1)){
    for(m in 1:(length(individuals)-n)){
      for(k in 1:nrow(taskIndMatrix)){
        if(taskIndMatrix[k, n] == taskIndMatrix[k, n+m]){
          if(taskIndMatrix[k, n] != "Resting" & taskIndMatrix[k, n] !="Self Maintenance"){
            counter = counter + 1
            euclidDistance = sqrt((xIndMatrix[k, n] - xIndMatrix[k, n+m])^2+(yIndMatrix[k, n] - yIndMatrix[k, n+m])^2)
            distance = c(distance, euclidDistance)
          }
        }
      }
    }
  }
  
  numberOfCoincidenceTasks[i] = counter
  distanceOfCoincidenceTasks[i] = median(distance)
  distanceOfCoincidenceTasksMAD[i] = mad(distance)
  
  print(i/colNumber)
  
  rm(dataTemp, dataTemp2, behaviorData, indTaskList, indID, K)  
  
}

#task overlap, make sure inTube == 0

uniqueTaskMatrix = matrix(, nrow = 9, ncol = colNumber)
for(i in 1:colNumber){
  dataTempTemp = subset(dataset, Colony == colID[i] & Chamber == "Nest")
  uniqueTaskMatrix[1:length(unique(dataTempTemp$Task)), i] = unique(dataTempTemp$Task)
}
uniqueTaskMatrix = as.factor(na.omit(c(uniqueTaskMatrix)))
counts = table(uniqueTaskMatrix)
commonTasks = counts[counts>(colNumber-1)]
commonTasks = names(commonTasks) 

for(i in 1:colNumber){
  dataTemp = subset(dataset, Colony == colID[i] & Chamber == "Nest" & inTube == 0)
  areaSum = c()
  for(n in 1:(length(commonTasks)-1)){
    areaVec = c()
    for(m in 1:(length(commonTasks)-n)){
      dataTempTask = subset(dataTemp, Task == commonTasks[n])
      box.coords <- matrix(c(dataTempTask$XCoord, dataTempTask$YCoord), ncol = 2, nrow = nrow(dataTempTask))
      box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
      box.hpts <- c(box.hpts, box.hpts[1])
      box.chull.coords <- box.coords[box.hpts,]
      chull.poly1 <- Polygon(box.chull.coords, hole=F)
      ps1 = Polygons(list(chull.poly1),1)
      sps1 = SpatialPolygons(list(ps1))
      
      dataTempTask = subset(dataTemp, Task == commonTasks[m])
      box.coords <- matrix(c(dataTempTask$XCoord, dataTempTask$YCoord), ncol = 2, nrow = nrow(dataTempTask))
      box.hpts <- chull(x = box.coords[,1], y = box.coords[,2])
      box.hpts <- c(box.hpts, box.hpts[1])
      box.chull.coords <- box.coords[box.hpts,]
      chull.poly2 <- Polygon(box.chull.coords, hole=F)
      ps2 = Polygons(list(chull.poly2),1)
      sps2 = SpatialPolygons(list(ps2))
      
      intersection = gIntersection(sps1, sps2)
      if(is.null(intersection)){
        areaVec[m] = 0
      } else {
        areaVec[m] = gArea(intersection, byid=FALSE)
      }

    }
    areaSum[n] = sum(areaVec)
  }
  taskOverlap[i] = sum(areaSum)
}

### ---- Plots ---- ###

### Plot distributions of tasks within nest and foraging arena: find a way of mapping colors automatically 
for(j in 1:4){
  
  colonyID = colonyList[j]
  
  dataColonyNest = subset(dataset, Colony == colonyID & Chamber == "Nest" & inTube == 0)
  dataColonyForaging = subset(dataset, Colony == colonyID & Chamber == "Foraging Arena" & inTube == 0)
  
  groupColors = data.frame(Task = taskList[order(taskList)], col = c("#CC79A7", "#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#999999"))
  
  taskListCol = unique(dataColonyNest$Task)
  taskListCol = taskListCol[order(taskListCol)]
  colVec = c()
  
  for(i in 1:length(taskListCol)){
    if(taskListCol[i] == "Allogrooming"){
      colVec[i] = "#CC79A7"
    } else if(taskListCol[i] == "Antennating Nestmate"){
      colVec[i] = "#E69F00"
    } else if(taskListCol[i] == "Brood Care"){
      colVec[i] = "#000000"
    } else if(taskListCol[i] == "Exploring"){
      colVec[i] = "#56B4E9"
    } else if(taskListCol[i] == "Food Processing"){
      colVec[i] = "#009E73"
    } else if(taskListCol[i] == "Nest Maintenance"){
      colVec[i] = "#F0E442"
    } else if(taskListCol[i] == "Resting"){
      colVec[i] = "#0072B2"
    } else if(taskListCol[i] == "Self Maintenance"){
      colVec[i] = "#D55E00"
    } else {
      colVec[i] = "#999999"
    }
  }
  
  if(j == 1){
    p1 = ggplot(dataColonyNest, aes(x = XCoord, y = YCoord, col = Task)) + geom_point() + theme_bw() + ggtitle("Nest") +
      coord_fixed() + labs(x="x (pixels)", y="y (pixels)") + scale_color_manual(values=colVec) + theme(text = element_text(size=15))
    for(i in 1:length(taskList)){
      dataTemp = subset(dataColonyNest, Task == taskList[i])
      hull = dataTemp %>%
        slice(chull(XCoord, YCoord))
      
      p1 = p1 + geom_polygon(data = hull, alpha = 0.1, size = 1.5)
    }
    
  } else if(j == 2){
    p2 = ggplot(dataColonyNest, aes(x = XCoord, y = YCoord, col = Task)) + geom_point() + theme_bw() + ggtitle("Nest") +
      coord_fixed() + labs(x="x (pixels)", y="y (pixels)") + scale_color_manual(values=colVec) + theme(text = element_text(size=15))
    for(i in 1:length(taskList)){
      dataTemp = subset(dataColonyNest, Task == taskList[i])
      hull = dataTemp %>%
        slice(chull(XCoord, YCoord))
      
      p2 = p2 + geom_polygon(data = hull, alpha = 0.1, size = 1.5)
    }
  } else if(j == 3){
    p3 = ggplot(dataColonyNest, aes(x = XCoord, y = YCoord, col = Task)) + geom_point() + theme_bw() + ggtitle("Nest") +
      coord_fixed() + labs(x="x (pixels)", y="y (pixels)") + scale_color_manual(values=colVec) + theme(text = element_text(size=15))
    for(i in 1:length(taskList)){
      dataTemp = subset(dataColonyNest, Task == taskList[i])
      hull = dataTemp %>%
        slice(chull(XCoord, YCoord))
      
      p3 = p3 + geom_polygon(data = hull, alpha = 0.1, size = 1.5)
    }
  } else {
    p4 = ggplot(dataColonyNest, aes(x = XCoord, y = YCoord, col = Task)) + geom_point() + theme_bw() + ggtitle("Nest") +
      coord_fixed() + labs(x="x (pixels)", y="y (pixels)") + scale_color_manual(values=colVec) + theme(text = element_text(size=15))
    for(i in 1:length(taskList)){
      dataTemp = subset(dataColonyNest, Task == taskList[i])
      hull = dataTemp %>%
        slice(chull(XCoord, YCoord))
      
      p4 = p4 + geom_polygon(data = hull, alpha = 0.1, size = 1.5)
    }
  }
  
}

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

taskListCol = unique(dataColonyForaging$Task)
taskListCol = taskListCol[order(taskListCol)]
colVec = c()

for(i in 1:length(taskListCol)){
  if(taskListCol[i] == "Allogrooming"){
    colVec[i] = "#CC79A7"
  } else if(taskListCol[i] == "Antennating Nestmate"){
    colVec[i] = "#E69F00"
  } else if(taskListCol[i] == "Brood Care"){
    colVec[i] = "#000000"
  } else if(taskListCol[i] == "Exploring"){
    colVec[i] = "#56B4E9"
  } else if(taskListCol[i] == "Food Processing"){
    colVec[i] = "#009E73"
  } else if(taskListCol[i] == "Nest Maintenance"){
    colVec[i] = "#F0E442"
  } else if(taskListCol[i] == "Resting"){
    colVec[i] = "#0072B2"
  } else if(taskListCol[i] == "Self Maintenance"){
    colVec[i] = "#D55E00"
  } else {
    colVec[i] = "#999999"
  }
}

p2 = ggplot(dataColonyForaging, aes(x = XCoord, y = YCoord, col = Task)) + geom_point() + theme_bw() + ggtitle("Foraging Arena") + coord_fixed() + labs(x="x (pixels)", y="y (pixels)") + scale_color_manual(values=colVec) + theme(text = element_text(size=15))
for(i in 1:length(taskList)){
  dataTemp = subset(dataColonyForaging, Task == taskList[i])
  hull = dataTemp %>%
    slice(chull(XCoord, YCoord))
  
  p2 = p2 + geom_polygon(data = hull, alpha = 0.1, size = 1.5)
}

ggarrange(p1, p2, nrow = 1, ncol = 2)

### Plot trajectories of exploring through nest and foraging arena 

colorIndividuals = c("#CC79A7", "#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#999999", "#5D3A9B")

for(j in 1:colNumber){

  colonyID = colonyList[j]
  
  dataColonyNest = subset(dataset, Colony == colonyID & Chamber == "Nest" & inTube == 0 & Task == "Exploring")
  dataColonyNestResting = subset(dataset, Colony == colonyID & Chamber == "Nest" & inTube == 0 & Task == "Resting")
  
  antIDTags = unique(dataColonyNest$AntID)
  individualNumber = length(antIDTags)
  segmentsPresent = unique(dataColonyNest$SegmentNumber)
  segmentNumber = length(segmentsPresent)
  
  for(i in 1:individualNumber){
      
      dataTemp = subset(dataColonyNest, AntID == antIDTags[i])
      segmentsPresent = unique(dataTemp$SegmentNumber)
      segmentNumber = length(segmentsPresent)
      
      for(s in 1:segmentNumber){
        dataTempSegment = subset(dataTemp, SegmentNumber == segmentsPresent[s])
        
      if(i == 1 & s == 1){
        p = ggplot(dataTempSegment, aes(x = XCoord, y = YCoord)) + geom_path(arrow = arrow(type = 'closed', angle = 30, length = unit(0.05, "inches")), color = colorIndividuals[i]) + theme_bw() + coord_fixed() + labs(x="x (pixels)", y="y (pixels)") + theme(text = element_text(size=15))
      } else {
        p = p + geom_path(data = dataTempSegment, aes(x = XCoord, y = YCoord), arrow = arrow(type = 'closed', angle = 30, length = unit(0.05, "inches")), color = colorIndividuals[i]) 
      }
      }
      
      #do explorers avoid resters? 
      p = p + geom_point(data = dataColonyNestResting)
      
  }
  
  if(j == 1){
    p1 = p
  } else if(j == 2){
    p2 = p 
  } else if(j == 3){
    p3 = p 
  } else {
    p4 = p
  }
  
}

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

p1Temp = p1 + ggtitle("A)") 
p2Temp = p2 + ggtitle("B)") 

for(j in 1:colNumber){
  
  colonyID = colonyList[j]
  
  dataColonyForaging = subset(dataset, Colony == colonyID & Chamber == "Foraging Arena" & inTube == 0 & Task == "Exploring")
  dataColonyForagingResting = subset(dataset, Colony == colonyID & Chamber == "Foraging Arena" & inTube == 0 & Task == "Resting")
  
  antIDTags = unique(dataColonyForaging$AntID)
  individualNumber = length(antIDTags)
  segmentsPresent = unique(dataColonyForaging$SegmentNumber)
  segmentNumber = length(segmentsPresent)
  
  for(i in 1:individualNumber){
    
    dataTemp = subset(dataColonyForaging, AntID == antIDTags[i])
    segmentsPresent = unique(dataTemp$SegmentNumber)
    segmentNumber = length(segmentsPresent)
    
    for(s in 1:segmentNumber){
      dataTempSegment = subset(dataTemp, SegmentNumber == segmentsPresent[s])
      
      if(i == 1 & s == 1){
        p = ggplot(dataTempSegment, aes(x = XCoord, y = YCoord)) + geom_path(arrow = arrow(type = 'closed', angle = 30, length = unit(0.05, "inches")), color = colorIndividuals[i]) + theme_bw() + coord_fixed() + labs(x="x (pixels)", y="y (pixels)") + theme(text = element_text(size=15))
      } else {
        p = p + geom_path(data = dataTempSegment, aes(x = XCoord, y = YCoord), arrow = arrow(type = 'closed', angle = 30, length = unit(0.05, "inches")), color = colorIndividuals[i]) 
      }
    }
    
    #do explorers avoid resters? 
    #p = p + geom_point(data = dataColonyForagingResting)
    
  }
  
  if(j == 1){
    p1 = p
  } else if(j == 2){
    p2 = p 
  } else if(j == 3){
    p3 = p 
  } else {
    p4 = p
  }
  
}

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
#lots of resting in colony 17-5?

indBehavioralData = data.frame(Colony = c(colony), AntID = c(antID), Date = c(date), Analyst = c(analyst), numUniqueTasks = c(numUniqueTasks), degreeOfSpecialization = c(degreeOfSpecialization), propResting = c(propResting), propBrood = c(propBrood), propExploring = c(propExploring), propAllogrooming = c(propAllogrooming), propAntennatingNestmate = c(propAntennatingNestmate), propTrashMaintenance = c(propTrashMaintenance), propNestMaintenance = c(propNestMaintenance), propSelfMaintenance = c(propSelfMaintenance), propFoodProcessing = c(propFoodProcessing),numberOfTaskSwitches = c(numberOfTaskSwitches), numberOfAntennations = c(numberOfAntennations), interboutInterval = c(interboutInterval), boutLengthBC = c(boutLengthBC), boutLengthR = c(boutLengthR), boutLengthFP = c(boutLengthFP), boutLengthTM = c(boutLengthTM), boutLengthAN = c(boutLengthAN), boutLengthNM = c(boutLengthNM), boutLengthSM = c(boutLengthSM), boutLengthE = c(boutLengthE), boutLengthA = c(boutLengthA), velocity = c(velocity), movementAreaForaging = c(movementAreaForaging), movementAreaNest = c(movementAreaNest), colonySize = c(colonySize), totalMass = c(totalMass), sinuosityTrailNest = c(sinuosityTrailNest), sinuosityTrailForaging = c(sinuosityTrailForaging), session = c(sessionInd), exploringLengthForaging = c(exploringLengthForaging), exploringLengthNest = c(exploringLengthNest), probStop = c(probStop), probReturn = c(probReturn), bcDistance=c(bcDistance), eDistance=c(eDistance), fpDistance=c(fpDistance), rDistance = c(rDistance), tmDistance=c(tmDistance), anDistance=c(anDistance), nmDistance=c(nmDistance), smDistance=c(smDistance), aDistance=c(aDistance), bcSpeed=c(bcSpeed), eSpeed=c(eSpeed), fpSpeed=c(fpSpeed), rSpeed=c(rSpeed), tmSpeed=c(tmSpeed), anSpeed=c(anSpeed), nmSpeed=c(nmSpeed), smSpeed=c(smSpeed), aSpeed=c(aSpeed), bcTime = c(bcTime), eTime = c(eTime), fpTime = c(fpTime), rTime = c(rTime), tmTime = c(tmTime), anTime = c(anTime), nmTime = c(nmTime), smTime = c(smTime), aTime = c(aTime), bcjoulesPerMG = c(bcjoulesPerMG), ejoulesPerMG = c(ejoulesPerMG), fpjoulesPerMG = c(fpjoulesPerMG), rjoulesPerMG = c(rjoulesPerMG), tmjoulesPerMG = c(tmjoulesPerMG), anjoulesPerMG = c(anjoulesPerMG), nmjoulesPerMG = c(nmjoulesPerMG), smjoulesPerMG = c(smjoulesPerMG), ajoulesPerMG=c(ajoulesPerMG))

write.csv(indBehavioralData, "indBehavioralData.csv")
