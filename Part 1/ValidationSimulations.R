rm(list = ls())

setwd("C:/Users/user/Documents/behavioral_sampling/true_final_folder/from Michaela PC/Github")
data = read.csv("all_ant_behaviors.csv")

rareThreshold = .95
data = data[,3:11]
sims=5 #small sample size is used to save processing power
midSampleSize = 650
repeats = 250
SampleSizeVector = c((midSampleSize-(repeats/2)):(midSampleSize+(repeats/2)))
repeats = repeats+1
states = 9 
timesteps = 11041
sVec = c(1, 2, 4, 8, 16, 32, 64, 128)

trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}


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

ants = c(data$bbb_behavior, NA, data$gpw_behavior, NA, data$gwb_behavior, data$wwr_behavior, NA, data$ybr_behavior, NA, data$ygr_behavior, NA, data$ywr_behavior, NA, data$yyr_behavior, NA, data$yyw_behavior)

OverallProportions = as.vector(table(ants))/sum(as.vector(table(ants)))

events = rle(ants)
dataEvents = data.frame(BoutLength = events[[1]], State = events[[2]])

BroodCare = subset(dataEvents, State == "Brood Care")
BroodCare = BroodCare$BoutLength
BroodCareRare = quantile(BroodCare, c(rareThreshold)) 
Exploring = subset(dataEvents, State == "Exploring")
Exploring = Exploring$BoutLength
ExploringRare = quantile(Exploring, c(rareThreshold)) 
AntennatingNestmate = subset(dataEvents, State == "Antennating Nestmate")
AntennatingNestmate = AntennatingNestmate$BoutLength
AntennatingNestmateRare = quantile(AntennatingNestmate, c(rareThreshold)) 
SelfMaintenance = subset(dataEvents, State == "Self Maintenance")
SelfMaintenance = SelfMaintenance$BoutLength
SelfMaintenanceRare = quantile(SelfMaintenance, c(rareThreshold)) 
FoodProcessing = subset(dataEvents, State == "Food Processing")
FoodProcessing = FoodProcessing$BoutLength
FoodProcessingRare = quantile(FoodProcessing, c(rareThreshold)) 
TrashMaintenance = subset(dataEvents, State == "Trash Maintenance")
TrashMaintenance = TrashMaintenance$BoutLength
TrashMaintenanceRare = quantile(TrashMaintenance, c(rareThreshold)) 
Resting = subset(dataEvents, State == "Resting")
Resting = Resting$BoutLength
RestingRare = quantile(Resting, c(rareThreshold)) 
Allogrooming = subset(dataEvents, State == "Allogrooming")
Allogrooming = Allogrooming$BoutLength
AllogroomingRare = quantile(Allogrooming, c(rareThreshold)) 
NestMaintenance = subset(dataEvents, State == "Nest Maintenance")
NestMaintenance = NestMaintenance$BoutLength
NestMaintenanceRare = quantile(NestMaintenance, c(rareThreshold)) 

OverallRare = quantile(events[[1]], c(rareThreshold))
percentRareinFull = sum(events[[1]][events[[1]]>OverallRare])/(sum(events[[1]]))

BehaviorList = unique(dataEvents$State)
BehaviorList = BehaviorList[!is.na(BehaviorList)]
BehaviorListNumeric = 1:states

### Simulated ants ###

SMat = array(numeric(), c(sims, length(sVec), repeats))
FinalPropErrorMat = array(numeric(), c(sims, length(sVec), repeats))
TransitionErrorVecMat = array(numeric(), c(sims, length(sVec), repeats))
propRareSampleMat = array(numeric(), c(sims, length(sVec), repeats))
TotalTransMat = array(numeric(), c(sims, length(sVec), repeats))
ProbInputMat = array(numeric(), c(sims, length(sVec), repeats))
InitialStateMat = array(numeric(), c(sims, length(sVec), repeats))
minSampSizeMat = array(numeric(), c(sims, length(sVec), repeats))
DOSMat = array(numeric(), c(sims, length(sVec), repeats))
SampSizeMat = array(numeric(), c(sims, length(sVec), repeats))
SimsMat = array(numeric(), c(sims, length(sVec), repeats))
DurationMat= array(numeric(), c(sims, length(sVec), repeats))
DurationErrorMat= array(numeric(), c(sims, length(sVec), repeats))
TruePropRare = array(numeric(), c(sims, length(sVec), repeats))
TaskNumber = array(numeric(), c(sims, length(sVec), repeats))

for(i in 1:sims){
  
  stateVec = c()
  rareBoutVec = c()
  TempBehaviorList = sample(BehaviorListNumeric, states, replace = FALSE)
  initialState = c(TempBehaviorList[1])
  initialState1 = initialState
  r1 = runif(1)
  ProbInput = r1
  probs = dgeom(1:states, prob = r1)/sum(dgeom(1:states, prob = r1))
  probsCum = cumsum(probs)
  counter = 1
  
  while(length(stateVec) <= timesteps){
    
    success = 0
    counter = counter + 1
    while(success == 0){
      r2 = runif(1)
      bolleanLogic = r2 < probsCum
      state = min(which(bolleanLogic == TRUE))
      initialState[counter] = TempBehaviorList[state]
      if(initialState[counter] != initialState[counter-1]){
        success = 1
      }
    }
    
    if(initialState[counter] == 1){
      boutLength = sample(BroodCare, 1)
      stateVec = c(rep(1, boutLength), stateVec)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    } else if(initialState[counter] == 2){
      boutLength = sample(Exploring, 1)
      stateVec = c(rep(2, boutLength), stateVec)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    } else if(initialState[counter] == 3){
      boutLength = sample(AntennatingNestmate, 1)
      stateVec = c(rep(3, boutLength), stateVec)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    } else if(initialState[counter] == 4){
      boutLength = sample(SelfMaintenance, 1)
      stateVec = c(rep(4, boutLength), stateVec)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    } else if(initialState[counter] == 5){
      boutLength = sample(FoodProcessing, 1)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    } else if(initialState[counter] == 6){
      boutLength = sample(TrashMaintenance, 1)
      stateVec = c(rep(6, boutLength), stateVec)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    } else if(initialState[counter] == 7){
      boutLength = sample(Resting, 1)
      stateVec = c(rep(7, boutLength), stateVec)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    } else if(initialState[counter] == 8){
      boutLength = sample(Allogrooming, 1)
      stateVec = c(rep(8, boutLength), stateVec)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    } else {
      boutLength = sample(NestMaintenance, 1)
      stateVec = c(rep(9, boutLength), stateVec)
      if(boutLength > OverallRare){
        rareBoutVec = c(rep(1, boutLength), rareBoutVec)
      } else{
        rareBoutVec = c(rep(0, boutLength), rareBoutVec)
      }
    }
  }
  
  finalStateVec = stateVec[1:timesteps]
  finalStateVec
  counts = table(finalStateVec)
  StatesPresent = names(counts)
  proportionOfStates = as.vector(counts)/timesteps
  proportionOfStatesVec = proportionOfStates
  
  statesNeeded = states-length(proportionOfStates)
  if(statesNeeded > 0){
    proportionOfStates = c(proportionOfStates, rep(0, statesNeeded))
  }
  
  TruetransCounts = trans.matrix(t(finalStateVec), prob = F)
  TruetransCounts = t(TruetransCounts)
  diag(TruetransCounts) = 0
  TrueTransitionRate = colSums(TruetransCounts)/length(finalStateVec)
  
  durations = rle(finalStateVec)
  dataDurations = data.frame(bouts = durations[[1]], state = durations[[2]])
  durationMeans = aggregate(bouts~state, dataDurations, mean)
  durationMeans = durationMeans$bouts
  
  percentRareinFullSim = sum(durations[[1]][durations[[1]]>OverallRare])/(sum(durations[[1]]))
  
  for(j in 1:length(sVec)){
    
    S = sVec[j]
    
    for(n in 1:repeats){
      
      minSampSizeMat[i, j, n] = 1/min(proportionOfStatesVec)
      DOSMat[i, j, n] = sum(abs(proportionOfStates - 1/states))/(2-2/states)
      TotalTransMat[i, j, n] = sum(colSums(TruetransCounts))
      SampSizeMat[i, j, n] = SampleSizeVector[n]
      SMat[i, j, n] = S
      ProbInputMat[i, j, n] = ProbInput
      InitialStateMat[i, j, n] = initialState1
      SimsMat[i, j, n] = i
      DurationMat[i, j, n] = mean(durationMeans)
      TruePropRare[i, j, n] = percentRareinFullSim
      TaskNumber[i, j, n] = length(StatesPresent)

      segment_size = round(SampleSizeVector[n]/S)
      if(segment_size == 0){
        segment_size = 1
      }
      sample = matrix(, nrow = segment_size, ncol = S)
      sampleRare =  matrix(, nrow = segment_size, ncol = S)
      
      if(length(finalStateVec)-(S*segment_size)+1 > 1){
        first_point = round(runif(1, min = 1, max = length(finalStateVec)-(S*segment_size)+1))
      } else {
        first_point = 1 
      }
      
      for(k in 1:S){
        k_i = k-1
        starting_point = first_point + round((k_i*length(finalStateVec[first_point:length(finalStateVec)]))/S)
        sample[, k] = c(finalStateVec[starting_point:(starting_point+segment_size-1)])
        sampleRare[, k] = c(rareBoutVec[starting_point:(starting_point+segment_size-1)])
      }
      
      transCounts = trans.matrix(t(sample), prob = F)
      transCounts = t(transCounts)
      diag(transCounts) = 0
      SampleTransitionRate= colSums(transCounts)/length(c(sample))
      
      if(length(SampleTransitionRate) == 0){
        SampleTransitionRate = rep(0, length(StatesPresent))
        names(SampleTransitionRate) = StatesPresent
      }
      
      SampleStates = names(SampleTransitionRate)
      counts = table(c(sample))
      proportionOfStatesSample = as.vector(counts)/length(c(sample))
      
      sample = rbind(sample, NA)  
      durations2 = rle(c(sample))
      dataDurations = data.frame(bouts = durations2[[1]], state = durations2[[2]])
      durationMeansSample = aggregate(bouts~state, dataDurations, mean)
      durationMeansSample = durationMeansSample$bouts
      
      if(length(SampleStates) != length(StatesPresent)){
        for(k in 1:length(StatesPresent)){
          if((StatesPresent[k] %in% SampleStates)==FALSE){
            BooleanLog = as.numeric(StatesPresent[k]) < as.numeric(SampleStates)
            if(all(BooleanLog == TRUE)){
              SampleTransitionRate = c(0, SampleTransitionRate)
              proportionOfStatesSample = c(0, proportionOfStatesSample)
              durationMeansSample = c(0, durationMeansSample)
            } else if(all(BooleanLog == FALSE)) {
              SampleTransitionRate = c(SampleTransitionRate, 0)
              proportionOfStatesSample = c(proportionOfStatesSample, 0)
              durationMeansSample = c(durationMeansSample, 0)
            } else {
              index = min(which(BooleanLog))-1
              SampleTransitionRate = c(SampleTransitionRate[1:index], 0, SampleTransitionRate[(index+1):length(SampleTransitionRate)])
              proportionOfStatesSample = c(proportionOfStatesSample[1:index], 0, proportionOfStatesSample[(index+1):length(proportionOfStatesSample)])
              durationMeansSample = c(durationMeansSample[1:index], 0, durationMeansSample[(index+1):length(durationMeansSample)])
            }
          }
        }
      }

      propRareSampleMat[i, j, n] = sum(c(sampleRare))/length(c(sample))
      TransitionErrorVecMat[i, j, n] = sum((TrueTransitionRate-SampleTransitionRate)^2)
      FinalPropErrorMat[i, j, n] = sum((proportionOfStatesVec-proportionOfStatesSample)^2)
      DurationErrorMat[i, j, n] = sum((durationMeans-durationMeansSample)^2)

    }
  }
  
  print(i/sims)
  
}

dataSims = data.frame(S = c(SMat), PropError = c(FinalPropErrorMat), TransRateError = c(TransitionErrorVecMat), propRareSample = c(propRareSampleMat), DOS = c(DOSMat), TotalTransitions = c(TotalTransMat), InputGeometricProb = c(ProbInputMat), InitialState = c(InitialStateMat), minSampleSize = c(minSampSizeMat), SampleSize = c(SampSizeMat), Simulation = c(SimsMat), DurationError = c(DurationErrorMat), TrueDuration = c(DurationMat), TruePropRare = c(TruePropRare), TaskNumber = c(TaskNumber))

### Real ants ###

SMat = array(numeric(), c(9, length(sVec), repeats))
FinalPropErrorMat = array(numeric(), c(9, length(sVec), repeats))
TransitionErrorVecMat = array(numeric(), c(9, length(sVec), repeats))
propRareSampleMat = array(numeric(), c(9, length(sVec), repeats))
TotalTransMat = array(numeric(), c(9, length(sVec), repeats))
minSampSizeMat = array(numeric(), c(9, length(sVec), repeats))
DOSMat = array(numeric(), c(9, length(sVec), repeats))
SampSizeMat = array(numeric(), c(9, length(sVec), repeats))
DurationMat= array(numeric(), c(9, length(sVec), repeats))
DurationErrorMat= array(numeric(), c(9, length(sVec), repeats))
TruePropRare = array(numeric(), c(9, length(sVec), repeats))
AntsMat = array(numeric(), c(9, length(sVec), repeats))
TaskNumber = array(numeric(), c(9, length(sVec), repeats))

for(i in 1:9){
  
  tempData = data[, i]
  tempData2 = c()
  for(j in 1:length(tempData)){
    if(tempData[j] == "Brood Care"){
      tempData2[j] = 1
    } else if(tempData[j] == "Exploring"){
      tempData2[j] = 2
    } else if(tempData[j] == "Antennating Nestmate"){
      tempData2[j] = 3
    } else if(tempData[j] == "Self Maintenance"){
      tempData2[j] = 4
    } else if(tempData[j] == "Food Processing"){
      tempData2[j] = 5
    } else if(tempData[j] == "Trash Maintenance"){
      tempData2[j] = 6
    } else if(tempData[j] == "Resting"){
      tempData2[j] = 7
    } else if(tempData[j] == "Allogrooming"){
      tempData2[j] = 8
    } else {
      tempData2[j] = 9
    }
  }
  counts = table(tempData2)
  proportionOfStates = as.vector(counts)/timesteps
  proportionOfStatesVec = proportionOfStates
  statesNeeded = states-length(proportionOfStates)
  if(statesNeeded > 0){
    proportionOfStates = c(proportionOfStates, rep(0, statesNeeded))
  }
  
  TruetransCounts = trans.matrix(t(tempData2), prob = F)
  TruetransCounts = t(TruetransCounts)
  diag(TruetransCounts) = 0
  TrueTransitionRate = colSums(TruetransCounts)/length(tempData2)
  
  counts = table(c(tempData2))
  StatesPresent = names(counts)

  TransitionErrorVec = c()
  proportionOfStatesErrorVec = c()
  propRareSampleVec = c()
  
  durations = rle(tempData2)
  dataDurations = data.frame(bouts = durations[[1]], state = durations[[2]])
  durationMeans = aggregate(bouts~state, dataDurations, mean)
  durationMeans = durationMeans$bouts
  
  percentRareinFullSim = sum(durations[[1]][durations[[1]]>OverallRare])/(sum(durations[[1]]))
  
  for(j in 1:length(sVec)){
    
    S = sVec[j]
    
    for(n in 1:repeats){
      
      minSampSizeMat[i, j, n] = 1/min(proportionOfStatesVec)
      DOSMat[i, j, n] = sum(abs(proportionOfStates - 1/states))/(2-2/states)
      TotalTransMat[i, j, n] = sum(colSums(TruetransCounts))
      SampSizeMat[i, j, n] = SampleSizeVector[n]
      SMat[i, j, n] = S
      DurationMat[i, j, n] = mean(durationMeans)
      TruePropRare[i, j, n] = percentRareinFullSim
      AntsMat[i, j, n] = i
      TaskNumber[i, j, n] = length(proportionOfStatesVec)

      segment_size = round(SampleSizeVector[n]/S)
      if(segment_size == 0){
        segment_size = 1
      }
      sample = matrix(, nrow = segment_size, ncol = S)

      if(length(tempData2)-(S*segment_size)+1 > 1){
        first_point = round(runif(1, min = 1, max = length(tempData2)-(S*segment_size)+1))
      } else {
        first_point = 1 
      }
      
      for(k in 1:S){
        k_i = k-1
        starting_point = first_point + round((k_i*length(tempData2[first_point:length(tempData2)]))/S)
        sample[, k] = c(tempData2[starting_point:(starting_point+segment_size-1)])      
      }
      
      eventsSample = rle(as.character(sample))
      eventList = unique(eventsSample[[2]])
      rareBouts = c()
      
      for(k in 1:length(eventList)){
        tempData3 = eventsSample[[1]]
        bool = eventsSample[[2]] == eventList[k]
        tempData4 = tempData3[bool]
        
        if(eventList[k] == 1){
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        } else if(eventList[k] == 2){
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        } else if(eventList[k] == 3){
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        } else if(eventList[k] == 4){
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        } else if(eventList[k] == 5){
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        } else if(eventList[k] == 6){
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        } else if(eventList[k] == 7){
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        } else if(eventList[k] == 8){
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        } else {
          rareBouts[k] = sum(tempData4[tempData4>OverallRare])
        }
        
      }
      
      transCounts = trans.matrix(t(sample), prob = F)
      transCounts = t(transCounts)
      diag(transCounts) = 0
      SampleTransitionRate = colSums(transCounts)/length(c(sample))
      if(length(SampleTransitionRate) == 0){
        SampleTransitionRate = rep(0, length(StatesPresent))
        names(SampleTransitionRate) = StatesPresent
      }
      SampleStates = names(SampleTransitionRate)
      counts = table(c(sample))
      proportionOfStatesSample = as.vector(counts)/length(c(sample))
      
      sample = rbind(sample, NA)  
      durations2 = rle(c(sample))
      dataDurations = data.frame(bouts = durations2[[1]], state = durations2[[2]])
      durationMeansSample = aggregate(bouts~state, dataDurations, mean)
      durationMeansSample = durationMeansSample$bouts
      
      if(length(SampleStates) != length(StatesPresent)){
        for(k in 1:length(StatesPresent)){
          if((StatesPresent[k] %in% SampleStates)==FALSE){
            BooleanLog = as.numeric(StatesPresent[k]) < as.numeric(SampleStates)
            if(all(BooleanLog == TRUE)){
              SampleTransitionRate = c(0, SampleTransitionRate)
              proportionOfStatesSample = c(0, proportionOfStatesSample)
              durationMeansSample = c(0, durationMeansSample)
            } else if(all(BooleanLog == FALSE)) {
              SampleTransitionRate = c(SampleTransitionRate, 0)
              proportionOfStatesSample = c(proportionOfStatesSample, 0)
              durationMeansSample = c(durationMeansSample, 0)
            } else {
              index = min(which(BooleanLog))-1
              SampleTransitionRate = c(SampleTransitionRate[1:index], 0, SampleTransitionRate[(index+1):length(SampleTransitionRate)])
              proportionOfStatesSample = c(proportionOfStatesSample[1:index], 0, proportionOfStatesSample[(index+1):length(proportionOfStatesSample)])
              durationMeansSample = c(durationMeansSample[1:index], 0, durationMeansSample[(index+1):length(durationMeansSample)])
            }
          }
        }
      }
      
      propRareSampleMat[i, j, n] = sum(c(rareBouts))/length(c(sample))
      TransitionErrorVecMat[i, j, n] = sum((TrueTransitionRate-SampleTransitionRate)^2)
      FinalPropErrorMat[i, j, n] = sum((proportionOfStatesVec-proportionOfStatesSample)^2)
      DurationErrorMat[i, j, n] = sum((durationMeans-durationMeansSample)^2)
      
    }
  }
  
  print(i/9)
  
}

dataReal = data.frame(S = c(SMat), PropError = c(FinalPropErrorMat), TransRateError = c(TransitionErrorVecMat), propRareSample = c(propRareSampleMat), DOS = c(DOSMat), TotalTransitions = c(TotalTransMat), minSampleSize = c(minSampSizeMat), SampleSize = c(SampSizeMat), AverageDurationError = c(DurationErrorMat), TruePropRare = c(TruePropRare), AverageDuration = c(DurationMat), Ant = c(AntsMat), TaskNumber = c(TaskNumber))

getwd()

write.csv(dataSims,"FullSimulatedAntsData95RareConstantSS.csv", row.names = FALSE)
write.csv(dataReal, "FullRealAntsData95RareConstantSS.csv", row.names = FALSE)
