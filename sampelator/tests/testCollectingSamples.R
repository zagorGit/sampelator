# Project: sampelator_git
# 
# Author: mvarewyck
###############################################################################

library(sampelator)


## Create example data set
makeRandomString <- function(n = 1, length = 12){
  
  randomString <- c(1:n)
  
  for (i in 1:n) {
    
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
            length, replace = TRUE),
        collapse="")
    
  }
  
  return(randomString)
  
}


set.seed(1)
n <- 1000
sampleData <- data.frame(
    centers = makeRandomString(n),
    weightClass = sample( paste0("Class", 1:5), n, replace = TRUE, 
        prob = c(0.1, 0.2, 0.2, 0.45, 0.05)),
    region = sample( paste0("Region", 1:10), n, replace = TRUE, 
        prob = c(0.1, 0.2, 0.01, 0.02, 0.03, 0.04, 0.05, 0.3, 0.1, 0.15)),
    stratumName = sample( c("alpha", "papa", "tango"), n, replace = TRUE,
        prob = c(0.2, 0.5, 0.3)),
    timePeriods = sample(1:12, n, replace = TRUE)
)
head(sampleData)

if(FALSE){
  
  sampleData$timePeriods <- NULL
  
  write.table(sampleData,
      file = "/home/mvarewyck/Documents/Sampelator/EFSA_background/exampleData.csv", 
      sep = ",", row.names = FALSE)
  
}

dataDuplicated <- sampleData[rep(row.names(sampleData), sampleData$timePeriods), ]
head(dataDuplicated)

newProbabilities <- function(weightClass){
  
  if ( weightClass == "Class1" ) c(0.4, 0.2, 0.2, 0.1, 0.1)
  else if( weightClass == "Class2") c(0.2, 0.4, 0.2, 0.1, 0.1)
  else if( weightClass == "Class3") c(0.1, 0.2, 0.4, 0.2, 0.1)
  else if( weightClass == "Class4") c(0.1, 0.1, 0.2, 0.4, 0.2)
  else c(0.1, 0.1, 0.2, 0.2, 0.4)
  
}

dataDuplicated$weightClass <- apply(dataDuplicated, 1, function(x)
      sample( paste0("Class", 1:5), 1, prob = newProbabilities(x["weightClass"])))
dataDuplicated$timePeriods <- NULL

head(dataDuplicated)


if(FALSE){
  
  write.table(dataDuplicated,
      file = "/home/mvarewyck/Documents/Sampelator/EFSA_background/exampleData2.txt", 
      sep = "\t", row.names = FALSE)
  
}

collapsedData <- collapseData(inputData = dataDuplicated, collapseVariable = "weightClass")
head(collapsedData)

selectedSample <- selectSample(data = sampleData, seed = 1, size = 10)
selectedSample
#          centers weightClass   region stratumName
# 62  bREYu5Ncu6h0      Class2  Region2       tango
# 201 rBoGVK58fyyA      Class3 Region10       tango
# 266 Buq6Qtq7z7zq      Class4  Region1        papa
# 372 8glTbFFbupCS      Class4 Region10        papa
# 572 3gUsPFxYNIPn      Class4  Region8        papa
# 625 9TDsohMaLtpz      Class3  Region1       tango
# 657 UjpsIuheUmm6      Class2  Region2        papa
# 894 BQxKuemTqoki      Class4 Region10        papa
# 906 DsVw1eflS9tc      Class5 Region10        papa
# 940 hHUKcbFwMLmZ      Class2  Region2       tango


simulatedSamples <- simulateSampling(data = sampleData, seed = 1, size = 10,
    nSimulations = 10)
head(simulatedSamples)


summarizeSampling(selectedSample = selectedSample, 
    simulatedSamples = simulatedSamples, varName = "weightClass")



if(FALSE){
  
  collectedData <- read.csv("/home/mvarewyck/Documents/Sampelator/EFSA_background/results_exampleData.csv")
  summary(collectedData)
  
}



#### Oyster sample size calculation

# Code Jose
z <- 1.96
d <- 0.05
Wh <- c(9, 5, 2015, 2, 3, 12, 110, 33, 1, 21, 70, 3, 41)/2325
Sh <- 0.25
Ntot <- 2325*260

n <- (z^2/d^2)*sum(Wh*Sh)
nadj <- (Ntot*n)/(Ntot+n-1)
c(n,nadj)
# [1] 384.1600 357.1377
country <- c("Croatia", "Denmark", "France", "Germany", "Greece", "Ireland",
    "Italy", "The Netherlands", "Norway", "Portugal", "Spain", "Sweden",
    "United Kingdom")
out <- data.frame(country, roundUp(nadj*Wh, delta = 0.1))
names(out) <- c("Country", "NumberSampleActive")

sum(roundUp(nadj*Wh, delta = 0.1))
# [1] 364


B <- 6
D <- 2.5
missprop <- 0.2
numberSampleActive <- roundUp((1+missprop)*(ceiling(nadj)*D*Wh)/B, delta = 0.1)
numberSampleActive
#  [1]  2  3 74  1  2 22  7  5  2  5 14  3 31
c(sum(numberSampleActive), sum(numberSampleActive)*B)
# [1]  171 1026


## My code
productionAreas <- c(5, 6, 189, 1, 3, 56, 18, 11, 3, 11, 34, 6, 79)/ 422

if(FALSE){
  
  write.table(data.frame(stratumName = country, stratumProportions = productionAreas,
          stratumVariances = 0.25),
      file = "/home/mvarewyck/Documents/Sampelator/EFSA_background/productionAreas.csv", 
      sep = ",", row.names = FALSE)
  
}

getTwoStage(purpose = "estimation", desiredDifference = 0.05, 
    sampleSize = NA, typeIerror = 0.05, clusterSize = 6, designEffect = 2.5,
    stratumVariances = 0.25, inflationFactor = 1.1, 
    stratumProportions = productionAreas, 
    adjustFinitePopulation = TRUE, populationSize = 5064)
# $sampleAllocation
#    stratumProportions stratumVariances availableClusters numberClusters
# 1         0.011848341             0.25                 9              2
# 2         0.014218009             0.25                12              3
# 3         0.447867299             0.25               378             74
# 4         0.002369668             0.25                 2              1
# 5         0.007109005             0.25                 6              2
# 6         0.132701422             0.25               112             22
# 7         0.042654028             0.25                36              7
# 8         0.026066351             0.25                22              5
# 9         0.007109005             0.25                 6              2
# 10        0.026066351             0.25                22              5
# 11        0.080568720             0.25                68             14
# 12        0.014218009             0.25                12              3
# 13        0.187203791             0.25               158             31
#    stratumSize
# 1           12
# 2           18
# 3          444
# 4            6
# 5           12
# 6          132
# 7           42
# 8           30
# 9           12
# 10          30
# 11          84
# 12          18
# 13         186
# 
# $designStatistics
#   sampleSize desiredDifference power
# 1       1026              0.05    NA


dispatchCenters <- c(9, 5, 2015, 2, 3, 12, 110, 33, 1, 21, 70, 3, 41)/2325

if(FALSE){
  
  write.table(data.frame(stratumName = country, stratumProportions = dispatchCenters,
          stratumVariances = 0.25),
      file = "/home/mvarewyck/Documents/Sampelator/EFSA_background/dispatchCenters.csv", 
      sep = ",", row.names = FALSE)
  
}
getTwoStage(purpose = "estimation", desiredDifference = 0.05, sampleSize= NA,
    typeIerror = 0.05, clusterSize = 6, designEffect = 2.5,
    stratumVariances = 0.25, inflationFactor = 1.2, 
    stratumProportions = dispatchCenters,
    adjustFinitePopulation = FALSE, populationSize = NA)
# $sampleAllocation
#    stratumProportions stratumVariances availableClusters numberClusters
# 1        0.0038709677             0.25                NA              1
# 2        0.0021505376             0.25                NA              1
# 3        0.8666666667             0.25                NA            167
# 4        0.0008602151             0.25                NA              1
# 5        0.0012903226             0.25                NA              1
# 6        0.0051612903             0.25                NA              1
# 7        0.0473118280             0.25                NA              9
# 8        0.0141935484             0.25                NA              3
# 9        0.0004301075             0.25                NA              0
# 10       0.0090322581             0.25                NA              2
# 11       0.0301075269             0.25                NA              6
# 12       0.0012903226             0.25                NA              1
# 13       0.0176344086             0.25                NA              4
#    stratumSize
# 1            6
# 2            6
# 3         1002
# 4            6
# 5            6
# 6            6
# 7           54
# 8           18
# 9            0
# 10          12
# 11          36
# 12           6
# 13          24
# 
# $designStatistics
#   sampleSize desiredDifference power
# 1       1182              0.05    NA



inputData <- read.table("~/git/sampelator/sampelator/inst/extdata/exampleData4.txt", header = TRUE)
collapsedData <- collapseData(inputData = inputData, collapseVariable = "Month")
head(collapsedData)

inputData <- read.csv("~/git/sampelator/sampelator/inst/extdata/exampleDataSample.csv", header = TRUE)
