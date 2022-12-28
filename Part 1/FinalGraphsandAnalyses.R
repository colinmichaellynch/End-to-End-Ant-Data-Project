rm(list = ls())

library(ggplot2)
library(dplyr)
library(Hmisc) 
library(corrplot)
library(ggpubr)
library(car)
library(viridis)
library(MASS)

### Correlations among variables for causal map ###

setwd("~/behavioral_sampling/true_final_folder/from Michaela PC/Github")
dataReal= read.csv("FullRealAntsData95RareConstantSS.csv")
dataSims = read.csv("FullSimulatedAntsData95RareConstantSS.csv")

DOSRange = range(dataReal$DOS)
ADRange = range(dataReal$AverageDuration)
PRRange = range(dataReal$TruePropRare)
StatesRange = range(dataReal$TaskNumber)
SList = c(1, 2, 4, 8, 16, 32, 64, 128)
SPenalty = 0
SPenaltyVec = SList*SPenalty

dataSimsAltered = data.frame(DegreeofSpecialization = dataSims$DOS, AverageDuration = dataSims$TrueDuration, TrueRareProp = dataSims$TruePropRare, TaskNumber = dataSims$TaskNumber, S = dataSims$S, SampleSize = dataSims$SampleSize, PropStatesError = dataSims$PropError, DurationError = dataSims$DurationError)

dataSimsAltered = subset(dataSimsAltered, DegreeofSpecialization > DOSRange[1] & DegreeofSpecialization < DOSRange[2] & AverageDuration > ADRange[1] & AverageDuration < ADRange[2] & TaskNumber > StatesRange[1] & TaskNumber < StatesRange[2])

drops = c("TrueRareProp")
dataSimsAltered = dataSimsAltered[ , !(names(dataSimsAltered) %in% drops)]

df = scale(dataSimsAltered)# normalize the data frame. This will also convert the df to a matrix.  

corr = rcorr(df, type = "spearman")
corr_r = as.matrix(corr[[1]])
pval = as.matrix(corr[[3]])

corr_r[corr_r^2<.05] = 0

corrplot(corr_r,p.mat = pval,sig.level=0.05/21,insig = "blank",method="ellipse",type="lower",diag=FALSE,tl.col="black",tl.cex=1,tl.offset=0.8,tl.srt=45, title = "", mar=c(0,0,1,0))

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

dataSimsAlteredSmall = sample_n(dataSimsAltered, 5000)
dataSimsAlteredSmall$density <- get_density(dataSimsAlteredSmall$AverageDuration, dataSimsAlteredSmall$DurationError, n = 100)
ggplot(dataSimsAlteredSmall, aes(x = AverageDuration, y = DurationError, color = density)) + geom_point()

dataSimsAlteredReal = data.frame(DegreeofSpecialization = dataReal$DOS, AverageDuration = dataReal$AverageDuration, TaskNumber = dataReal$TaskNumber, S = dataReal$S, SampleSize = dataReal$SampleSize, PropStatesError = dataReal$PropError, DurationError = dataReal$AverageDurationError)

df = scale(dataSimsAlteredReal)
corr = rcorr(df, type = "spearman")
corr_r = as.matrix(corr[[1]])
pval = as.matrix(corr[[3]])

corr_r[corr_r^2<.05] = 0

corrplot(corr_r,p.mat = pval,sig.level=0.05/21,insig = "blank",method="ellipse",type="lower",diag=FALSE,tl.col="black",tl.cex=1,tl.offset=0.8,tl.srt=45, title = "", mar=c(0,0,1,0))

dataRealUnique= dataReal[1:9,]
dataRealUniqueAltered = data.frame(DegreeofSpecialization = dataRealUnique$DOS, AverageDuration = dataRealUnique$AverageDuration, TaskNumber = dataRealUnique$TaskNumber, PropStatesError = dataRealUnique$PropError, DurationError = dataRealUnique$AverageDurationError)

df = scale(dataRealUniqueAltered)
corr = rcorr(df, type = "spearman")
corr_r = as.matrix(corr[[1]])
pval = as.matrix(corr[[3]])

corr_r[corr_r^2<.05] = 0

corrplot(corr_r,p.mat = pval,sig.level=0.05,insig = "blank",method="ellipse",type="lower",diag=FALSE,tl.col="black",tl.cex=1,tl.offset=0.8,tl.srt=45, title = "", mar=c(0,0,1,0))

### Optimal S ###

totalSims = max(dataSims$Simulation)
SVector = c(1, 2, 4, 8, 16, 32, 64, 128)
SVec = c()
errorSum = c()
DOSVec = c()
DurationVec = c()
propRareVec = c()
TaskNumberVec = c()

for(i in 1:totalSims){
  
  #dataTemp = subset(dataSims, Simulation == i & DOS > DOSRange[1] & DOS < DOSRange[2] & TrueDuration > ADRange[1] & TrueDuration < ADRange[2] & TaskNumber >= StatesRange[1] & TaskNumber <= StatesRange[2])
  dataTemp = subset(dataSims, Simulation == i)
  
  if(nrow(dataTemp) == 0){
    SVec[i] = NA
    errorSum[i] = NA
    DOSVec[i] = NA
    DurationVec[i] = NA
    propRareVec[i] = NA
  } else{
    
    coeff <- mean(dataTemp$DurationError)/mean(dataTemp$PropError)
    dataTemp$normDurationError = dataTemp$DurationError/coeff
    
    aggregatePropError = aggregate(PropError~S, data = dataTemp, mean)
    aggregateDurationError = aggregate(normDurationError~S, data = dataTemp, mean)
    aggregateErrorSum = aggregatePropError$PropError + aggregateDurationError$normDurationError + SPenaltyVec
    errorSum[i] = min(aggregateErrorSum)
    SIndex = which.min(aggregateErrorSum)
    SVec[i]= SVector[SIndex]
    
    aggregateDOS = aggregate(DOS~S, data = dataTemp, mean)
    aggregateDuration = aggregate(TrueDuration~S, data = dataTemp, mean)
    aggregatePropRare = aggregate(TruePropRare~S, data = dataTemp, mean)
    aggregateTaskNumber = aggregate(TaskNumber~S, data = dataTemp, mean)
    DOSVec[i] = aggregateDOS$DOS[SIndex]
    DurationVec[i] = aggregateDuration$TrueDuration[SIndex]
    propRareVec[i] = aggregatePropRare$TruePropRare[SIndex]
    TaskNumberVec[i] = aggregateTaskNumber$TaskNumber[SIndex]
    
  }
}

errorRangeRatio = 128/(range(na.omit(errorSum))[2] - range(na.omit(errorSum))[1])
finalPenalty = 1/errorRangeRatio

dataOptimal = data.frame(DOS = DOSVec, AverageTaskDuration = DurationVec, ProportionRare = propRareVec, TaskNumber = TaskNumberVec, S = SVec)
dataOptimal =na.omit(dataOptimal)

DOSTest = (lm(DOS ~ S, dataOptimal))
BLTest = (lm(AverageTaskDuration ~ S, dataOptimal))
PRTest = (lm(ProportionRare ~ S, dataOptimal))
TNTest = (lm(TaskNumber ~ S, dataOptimal))
summary(aov(TaskNumber ~ S, dataOptimal))

pValdos = anova(DOSTest)$'Pr(>F)'[1]
pValbl = anova(BLTest)$'Pr(>F)'[1]
pValpr = anova(PRTest)$'Pr(>F)'[1]
pValtn = anova(TNTest)$'Pr(>F)'[1]

pvec = c(pValdos, pValbl, pValpr, pValtn)
p.adjust(pvec, method = "bonferroni", n = length(pvec))

plot(S ~ DOS, data = dataOptimal)

test = lm(log(S)~DOS*AverageTaskDuration*TaskNumber, data = dataOptimal)
test2 = lm(log(S)~DOS+AverageTaskDuration*TaskNumber, data = dataOptimal)
test3 = lm(log(S)~DOS*AverageTaskDuration+TaskNumber,data = dataOptimal)
test4 = lm(log(S)~TaskNumber*AverageTaskDuration+DOS, data = dataOptimal)
test5 = lm(log(S)~TaskNumber+AverageTaskDuration+DOS, data = dataOptimal)
BIC(test, test2, test3, test4, test5)
summary(test5)

test = glm(log(S)~DOS*AverageTaskDuration*TaskNumber, data = dataOptimal, family = gaussian(link = "identity"))
test2 = glm(log(S)~DOS+AverageTaskDuration*TaskNumber, data = dataOptimal, family = gaussian(link = "identity"))
test3 = glm(log(S)~DOS*AverageTaskDuration+TaskNumber,data = dataOptimal, family = gaussian(link = "identity"))
test4 = glm(log(S)~TaskNumber*AverageTaskDuration+DOS, data = dataOptimal, family = gaussian(link = "identity"))
test5 = glm(log(S)~TaskNumber+AverageTaskDuration+DOS, data = dataOptimal, family = gaussian(link = "identity"))
BIC(test, test2, test3, test4, test5)

plot(log(S)~DOS, data = dataOptimal)
plot(log(S)~AverageTaskDuration, data = dataOptimal)
plot(log(S)~TaskNumber, data = dataOptimal)

summary(test5)
car::vif(test5)

P1 = ggplot(dataOptimal, aes(x = DOS, y = S)) + geom_point() + theme_bw() + theme(text = element_text(size=12.5)) + xlab("Degree of Specialization") + ylab("# Intervals (I)") + ggtitle("A") + geom_smooth(method='lm', formula= y~x) + ylim(0, 35)
P2 = ggplot(dataOptimal, aes(x = AverageTaskDuration, y = S)) + geom_point() + theme_bw() + theme(text = element_text(size=12.5)) + xlab("Average Task Duration (seconds)") + ylab("# Intervals (I)") + ggtitle("B")  + ylim(0, 35)
P3 = ggplot(dataOptimal, aes(x = ProportionRare, y = S)) + geom_point() + theme_bw() + theme(text = element_text(size=12.5)) + xlab("Proportion of Rare Events in Time Series") + ylab("# Intervals (I)") + ggtitle("C") + geom_smooth(method='lm', formula= y~x)+ ylim(0, 35)
P4 = ggplot(dataOptimal, aes(x = TaskNumber, y = S)) + geom_point() + theme_bw() + theme(text = element_text(size=12.5)) + xlab("Number of Tasks Performed") + ylab("# Intervals (I)")+ geom_jitter(width = 0.25) + ggtitle("C")  + ylim(0, 35)
ggarrange(P1, P2, P4, nrow = 3, ncol = 1)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(SVec[!is.na(SVec)])

### Validate Model ###

dataSimsSubset = sample_n(dataSims, 10000)
fit = loess(minSampleSize ~ DOS, dataSimsSubset)

RealDOS = unique(dataReal$DOS)
RealSS = unique(dataReal$minSampleSize)
UniqueDataReal = data.frame(DOS = RealDOS, MinSampleSize = RealSS)

predictedMinss = predict(fit, RealDOS)
summary(lm(predictedMinss~RealSS))

ggplot(dataSimsSubset, aes(x = DOS, y = minSampleSize)) + theme_bw() + geom_point(size = 2)+ stat_smooth(method = "loess", formula = y ~ x, size = 1) + labs(x = "Degree of Specialization", y = "Min Sample Size") + geom_point(data = UniqueDataReal, aes(x = DOS, y = MinSampleSize), size = 2, col = "#d8b365") + ylim(0, 11041)+ theme(text = element_text(size=12.5)) 

ggplot(dataSimsSubset, aes(x = InputGeometricProb, y = DOS)) + geom_point() + theme_bw() +xlab("DOS Input") + ylab("DOS Output")
summary(lm(DOS~InputGeometricProb, dataSimsSubset))

dataSimsUnique = dataSims[1:250, ]
dataSimsUnique = subset(dataSimsUnique, DOS > DOSRange[1] & DOS < DOSRange[2] & TrueDuration > ADRange[1] & TrueDuration < ADRange[2] & TruePropRare > PRRange[1] & TruePropRare < PRRange[2] & TaskNumber > StatesRange[1] & TaskNumber < StatesRange[2])

dataDOS = data.frame(DOS = dataSimsUnique$DOS, Label = "DOS")  
dataAverageDuration = data.frame(AverageDuration = dataSimsUnique$TrueDuration, Label = "AverageDuration")                            
dataProportionRareEvent = data.frame(ProportionRareEvent = dataSimsUnique$TruePropRare, Label = "ProportionRareEvent") 
dataTaskNumber = data.frame(TaskNumber = dataSimsUnique$TaskNumber, Label = "Task Number")

p1 = ggplot(dataDOS, aes(x = Label, y = DOS)) + geom_point() + theme_bw() + geom_jitter(width = 0.25) + geom_errorbar(aes(ymin=DOSRange[1], ymax=DOSRange[2]), width=.15, col = "deepskyblue3", size = 1.5) + xlab("") + ylab("Value") + theme(text = element_text(size=12.5)) + scale_x_discrete(labels=c("Degree of Specialization")) + ggtitle("A")
                                                                                                       
p2 = ggplot(dataAverageDuration, aes(x = Label, y = AverageDuration)) + geom_point() + theme_bw() + geom_jitter(width = 0.25) + geom_errorbar(aes(ymin=ADRange[1], ymax=ADRange[2]), width=.15, col = "deepskyblue3", size = 1.5) + xlab("") + ylab("") + ylim(c(0, 200)) + theme(text = element_text(size=12.5)) + scale_x_discrete(labels=c("Mean Bout Length (sec)"))+ ggtitle("B")

#p3 = ggplot(dataProportionRareEvent, aes(x = Label, y = ProportionRareEvent)) + geom_point() + theme_bw() + geom_jitter(width = 0.25) + geom_errorbar(aes(ymin=PRRange[1], ymax=PRRange[2]), width=.15, col = "deepskyblue3", size = 1.5) + xlab("") + ylab("") + theme(text = element_text(size=12.5)) + scale_x_discrete(labels=c("Prop. Rare Events"))+ ggtitle("C")

p4 = ggplot(dataTaskNumber, aes(x = Label, y = TaskNumber)) + geom_point() + theme_bw() + geom_jitter(width = 0.25) + geom_errorbar(aes(ymin=StatesRange[1], ymax=StatesRange[2]), width=.15, col = "deepskyblue3", size = 1.5) + xlab("") + ylab("") + theme(text = element_text(size=12.5)) + scale_x_discrete(labels=c("Task Number")) + ggtitle("C") + scale_y_discrete(limits=c(1:9))

ggarrange(p1, p2, p4, nrow = 1, ncol = 3)

### Visualize Tradeoff Over S ### 

SMat = array(numeric(), c(totalSims, length(SList)))
PropStatesError = array(numeric(), c(totalSims, length(SList)))
DurationError = array(numeric(), c(totalSims, length(SList)))

for(i in 1:totalSims){
  for(j in 1:length(SList)){
    
    SVal = SList[j]
    SMat[i,j] = SVal
    
    dataTemp = subset(dataSims, Simulation == i & DOS > DOSRange[1] & DOS < DOSRange[2] & TrueDuration > ADRange[1] & TrueDuration < ADRange[2] & TaskNumber > StatesRange[1] & TaskNumber < StatesRange[2] & S == SVal)
    
    PropStatesError[i,j] = mean(dataTemp$PropError)
    DurationError[i,j] = mean(dataTemp$DurationError)
    
  }
}

visualDataProp = data.frame(S = c(SMat), PropError = c(PropStatesError))
visualDataProp1 = aggregate(PropError~S, data = visualDataProp, mean)
visualDataProp2 = aggregate(PropError~S, data = visualDataProp, FUN= function(x) sqrt(var(x)/length(x)))
visualDataProp1$StandardErrorProp = visualDataProp2$PropError

visualDataDur = data.frame(S = c(SMat), DurationError = c(DurationError))
visualDataDur1 = aggregate(DurationError~S, data = visualDataDur, mean)
visualDataDur2 = aggregate(DurationError~S, data = visualDataDur, FUN= function(x) sqrt(var(x)/length(x)))
visualDataDur1$StandardErrorDur = visualDataDur2$DurationError

drops = c("S")
visualDataDur1 = visualDataDur1[ , !(names(visualDataDur1) %in% drops)]
visualDataFull = cbind(visualDataProp1, visualDataDur1)

coeff <- mean(visualDataFull$DurationError)/mean(visualDataFull$PropError)
visualDataFull$Sum = visualDataFull$PropError+(visualDataFull$DurationError/coeff) + SPenaltyVec

S2 = ggplot(visualDataFull, aes(x=as.factor(S), y=PropError)) +
geom_point(size = 2, col = "5ab4ac") + geom_errorbar(aes(ymin=PropError-StandardErrorProp, ymax=PropError+StandardErrorProp), width=.2, col = "5ab4ac") + geom_path(aes(y = PropError), group = 1, size = 1, col = "5ab4ac") +
geom_point(aes(x=as.factor(S), y=DurationError/coeff), size = 2, col = "#d8b365") + geom_errorbar(aes(ymin=(DurationError-StandardErrorDur)/coeff, ymax=(DurationError+StandardErrorDur)/coeff), width=.2, col = "#d8b365") + geom_path(aes(y=DurationError/coeff), group = 1, size = 1, col = "#d8b365") +
scale_y_continuous(name = "Proporion of States Error", sec.axis = sec_axis(~.*coeff, name="Mean Bout Length Error")) + xlab("Number of Intervals (I)")+ theme_bw() + theme(text = element_text(size=12.5)) + ggtitle("B") + geom_point(aes(x=as.factor(S), y=Sum), size = 2, col = "#999999") + geom_path(aes(y=Sum), group = 1, size = 1, col = "#999999")

### Real data optimal S ###

SVector = c(1, 2, 4, 8, 16, 32, 64, 128)
SVec = c()
errorSum = c()
DOSVec = c()
DurationVec = c()
propRareVec = c()
TaskNumberVec = c()
SampleSizeVec = c()

for(i in 1:9){
  
  dataTemp = subset(dataReal, Ant == i)
  
  if(nrow(dataTemp) == 0){
    SVec[i] = NA
    errorSum[i] = NA
    DOSVec[i] = NA
    DurationVec[i] = NA
    propRareVec[i] = NA
  } else{
    
    coeff <- mean(dataTemp$AverageDurationError)/mean(dataTemp$PropError)
    dataTemp$normDurationError = dataTemp$AverageDurationError/coeff 
    
    aggregatePropError = aggregate(PropError~S, data = dataTemp, mean)
    aggregateDurationError = aggregate(normDurationError~S, data = dataTemp, mean)
    aggregateErrorSum = aggregatePropError$PropError + aggregateDurationError$normDurationError + SPenaltyVec
    errorSum[i] = min(aggregateErrorSum)
    SIndex = which.min(aggregateErrorSum)
    SVec[i]= SVector[SIndex]
    
    aggregateDOS = aggregate(DOS~S, data = dataTemp, mean)
    aggregateDuration = aggregate(AverageDuration~S, data = dataTemp, mean)
    aggregatePropRare = aggregate(TruePropRare~S, data = dataTemp, mean)
    aggregateTaskNumber = aggregate(TaskNumber~S, data = dataTemp, mean)
    aggregateSampleSize = aggregate(SampleSize~S, data = dataTemp, mean)
    DOSVec[i] = aggregateDOS$DOS[SIndex]
    DurationVec[i] = aggregateDuration$AverageDuration[SIndex]
    propRareVec[i] = aggregatePropRare$TruePropRare[SIndex]
    TaskNumberVec[i] = aggregateTaskNumber$TaskNumber[SIndex]
    SampleSizeVec[i] = aggregateSampleSize$SampleSize[SIndex]
    
  }
}

dataOptimalReal = data.frame(DOS = DOSVec, AverageTaskDuration = DurationVec, ProportionRare = propRareVec, TaskNumber = TaskNumberVec, S = SVec, SampleSize = SampleSizeVec)
dataOptimalReal = dataOptimalReal[!is.na(dataOptimalReal),]

DOSTest = (lm(DOS ~ S, dataOptimalReal))
BLTest = (lm(AverageTaskDuration ~ S, dataOptimalReal))
PRTest = (lm(ProportionRare ~ S, dataOptimalReal))
TNTest = (lm(TaskNumber ~ S, dataOptimalReal))
summary(aov(TaskNumber ~ S, dataOptimalReal))

pValdos = anova(DOSTest)$'Pr(>F)'[1]
pValbl = anova(BLTest)$'Pr(>F)'[1]
pValpr = anova(PRTest)$'Pr(>F)'[1]
pValtn = anova(TNTest)$'Pr(>F)'[1]

pvec = c(pValdos, pValbl, pValpr, pValtn)
p.adjust(pvec, method = "bonferroni", n = length(pvec))

summary(PRTest)

test = lm(log(S)~DOS*AverageTaskDuration*TaskNumber, data = dataOptimalReal)
test2 = lm(log(S)~DOS+AverageTaskDuration*TaskNumber, data = dataOptimalReal)
test3 = lm(log(S)~DOS*AverageTaskDuration+TaskNumber,data = dataOptimalReal)
test4 = lm(log(S)~TaskNumber*AverageTaskDuration+DOS, data = dataOptimalReal)
test5 = lm((S)~TaskNumber+AverageTaskDuration+DOS, data = dataOptimalReal)
BIC(test, test2, test3, test4, test5)
summary(test5)
car::vif(test5)

P1 = P1 + geom_point(data = dataOptimalReal, aes(x = DOS, y = S), col = "#d8b365") + theme_bw() + theme(text = element_text(size=12.5)) + xlab("Degree of Specialization") + ylab("# Intervals (I)")
P2 = P2 + geom_point(data = dataOptimalReal, aes(x = AverageTaskDuration, y = S), col = "#d8b365") + theme_bw() + theme(text = element_text(size=12.5)) + xlab("Mean Bout Length") + ylab("# Intervals (I)")
P3 = P3 + geom_point(data = dataOptimalReal, aes(x = ProportionRare, y = S), col = "#d8b365") + theme_bw() + theme(text = element_text(size=12.5)) + xlab("Prop. Rare Events") + ylab("# Intervals (I)")
P4 = P4 + geom_point(data = dataOptimalReal, aes(x = TaskNumber, y = S), col = "#d8b365") + theme_bw() + theme(text = element_text(size=12.5)) + xlab("Number of Tasks Performed") + ylab("# Intervals (I)")
ggarrange(P1, P2, P4, nrow = 3, ncol = 1)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(SVec[!is.na(SVec)])

### Real Data Visualize Tradeoff Over S ### 

SMat = array(numeric(), c(9, length(SList)))
PropStatesError = array(numeric(), c(9, length(SList)))
DurationError = array(numeric(), c(9, length(SList)))

for(i in 1:9){
  for(j in 1:length(SList)){
    
    SVal = SList[j]
    SMat[i,j] = SVal
    
    dataTemp = subset(dataReal, Ant == i & S == SVal)
    
    PropStatesError[i,j] = mean(dataTemp$PropError)
    DurationError[i,j] = mean(dataTemp$AverageDurationError)
    
  }
}


visualDataProp = data.frame(S = c(SMat), PropError = c(PropStatesError))
visualDataProp1 = aggregate(PropError~S, data = visualDataProp, mean)
visualDataProp2 = aggregate(PropError~S, data = visualDataProp, FUN= function(x) sqrt(var(x)/length(x)))
visualDataProp1$StandardErrorProp = visualDataProp2$PropError

visualDataDur = data.frame(S = c(SMat), DurationError = c(DurationError))
visualDataDur1 = aggregate(DurationError~S, data = visualDataDur, mean)
visualDataDur2 = aggregate(DurationError~S, data = visualDataDur, FUN= function(x) sqrt(var(x)/length(x)))
visualDataDur1$StandardErrorDur = visualDataDur2$DurationError

drops = c("S")
visualDataDur1 = visualDataDur1[ , !(names(visualDataDur1) %in% drops)]
visualDataFull = cbind(visualDataProp1, visualDataDur1)

coeff <- mean(visualDataFull$DurationError)/mean(visualDataFull$PropError)
visualDataFull$Sum = visualDataFull$PropError+(visualDataFull$DurationError/coeff) + SPenaltyVec

S1 = ggplot(visualDataFull, aes(x=as.factor(S), y=PropError)) +
  geom_point(size = 2, col = "5ab4ac") + geom_errorbar(aes(ymin=PropError-StandardErrorProp, ymax=PropError+StandardErrorProp), width=.2, col = "5ab4ac") + geom_path(aes(y = PropError), group = 1, size = 1, col = "5ab4ac") +
  geom_point(aes(x=as.factor(S), y=DurationError/coeff), size = 2, col = "#d8b365") + geom_errorbar(aes(ymin=(DurationError-StandardErrorDur)/coeff, ymax=(DurationError+StandardErrorDur)/coeff), width=.2, col = "#d8b365") + geom_path(aes(y=DurationError/coeff), group = 1, size = 1, col = "#d8b365") +  scale_y_continuous(name = "Proporion of States Error", sec.axis = sec_axis(~.*coeff, name="Mean Bout Length Error")) + xlab("Number of Intervals (I)")+ theme_bw() + theme(text = element_text(size=12.5)) + ggtitle("A") + geom_point(aes(x=as.factor(S), y=Sum), size = 2, col = "#999999") + geom_path(aes(y=Sum), group = 1, size = 1, col = "#999999")

ggarrange(S1, S2, nrow = 1, ncol = 2)

ggplot(visualDataFull, aes(x=as.factor(S), y=PropError)) +
  geom_point(size = 3.5, col = "#C00000") + geom_errorbar(aes(ymin=PropError-StandardErrorProp, ymax=PropError+StandardErrorProp), width=.3, col = "#C00000") + geom_path(aes(y = PropError), group = 1, size = 1.5, col = "#C00000") +
  geom_point(aes(x=as.factor(S), y=DurationError/coeff), size = 3.5, col = "#62737A") + geom_errorbar(aes(ymin=(DurationError-StandardErrorDur)/coeff, ymax=(DurationError+StandardErrorDur)/coeff), width=.3, col = "#62737A") + geom_path(aes(y=DurationError/coeff), group = 1, size = 1.5, col = "#62737A") +  scale_y_continuous(name = "Proporion of States Error", sec.axis = sec_axis(~.*coeff, name="Mean Bout Length Error")) + xlab("Number of Intervals (I)")+ theme_bw() + theme(text = element_text(size=12.5)) + geom_point(aes(x=as.factor(S), y=Sum), size = 3.5, col = "#000000") + geom_path(aes(y=Sum), group = 1, size = 1.5, col = "#000000")
