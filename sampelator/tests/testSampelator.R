# Project: sampelator_git
# 
# Author: mvarewyck
###############################################################################

library(sampelator)

## Note: Check these results with those in the Milanzi et al. paper 

## getSimpleRandom() ##

getSimpleRandom(purpose = "estimation", desiredDifference = 0.05, 
    typeIerror = 0.05, sampleVariance = 0.16)
#   sampleSize desiredDifference power
# 1        246              0.05    NA

getSimpleRandom(purpose = "testing", desiredDifference = 0.05, power = 0.8, 
    typeIerror = 0.05, sampleVariance = 0.16)
#   sampleSize desiredDifference power
# 1        503              0.05   0.8

getSimpleRandom(purpose = "testing", desiredDifference = 0.05, power = 0.8, 
    typeIerror = 0.05, sampleVariance = 0.16, populationSize = 1000)
#   sampleSize desiredDifference power
# 1        335              0.05   0.8

getSimpleRandom(purpose = "testing", desiredDifference = 0.05, power = 0.8, 
    typeIerror = 0.05, sampleVariance = 0.16, inflationFactor = 1.1)
#   sampleSize desiredDifference power
# 1        553              0.05   0.8


power.t.test(n = NULL, delta = 0.05, power = 0.8, sd = sqrt(0.16), 
    type = "one.sample")
# 
#      One-sample t test power calculation 
# 
#               n = 504.2524
#           delta = 0.05
#              sd = 0.4
#       sig.level = 0.05
#           power = 0.8
#     alternative = two.sided
# Slightly different result because we use infinite sample size approximations


## getClustered() & getDesignEffect() ##

getClustered(purpose = "estimation", desiredDifference = 0.05,
    typeIerror = 0.05, clusterVariance = 0.1, clusterSize = 10)
#   numberClusters sampleSize desiredDifference power
# 1            154       1537              0.05    NA

getClustered(purpose = "testing", desiredDifference = 0.05, power = 0.8,  
    typeIerror = 0.05, clusterVariance = 0.1, clusterSize = 10)
#   numberClusters sampleSize desiredDifference power
# 1            314       3140              0.05   0.8

getClustered(purpose = "testing", sampleSize = 3140, power = 0.8,
    typeIerror = 0.05, clusterVariance = 0.1, clusterSize = 10)
#   numberClusters sampleSize desiredDifference power
# 1            314       3140              0.05   0.8




designEffect <- unlist(wrapFunctionMilanzi("getDesignEffect",
    randomInput = list(correlation = seq(0, 0.1, 0.02)),
    fixedInput = list(clusterSize = 10)))
getClustered(purpose = "estimation", desiredDifference = 0.05,
    typeIerror = 0.05, sampleVariance = 0.16, clusterSize = 10,
    designEffect = designEffect)
#   Number of clusters Total sample size Desired difference Power
# 1                 25               246               0.05    NA
# 2                 30               291               0.05    NA
# 3                 34               335               0.05    NA
# 4                 38               379               0.05    NA
# 5                 43               423               0.05    NA
# 6                 47               468               0.05    NA
# Slightly different results 
# because we do not round intermediately or because we round up values?

getClustered(purpose = "testing", desiredDifference = 0.05, power = 0.8, 
    typeIerror = 0.05, sampleVariance = 0.16, clusterSize = 10,
    designEffect = designEffect)
#   numberClusters sampleSize desiredDifference power
# 1             51        503              0.05   0.8
# 2             60        593              0.05   0.8
# 3             69        684              0.05   0.8
# 4             78        774              0.05   0.8
# 5             87        865              0.05   0.8
# 6             96        955              0.05   0.8
# Slightly different results

getClustered(purpose = "testing", sampleSize = 960, desiredDifference = 0.05, 
    typeIerror = 0.05, sampleVariance = 0.16, clusterSize = 10,
    designEffect = 1.9)
#   numberClusters sampleSize desiredDifference power
# 1             96        960              0.05   0.8



## getStratified() ##

getStratified(purpose = "estimation", desiredDifference = 0.05, 
    typeIerror = 0.05, allocation = "proportional", stratumVariances = 0.25,
    stratumProportions = c(0.02, 0.03, 0.04, 0.08, 0.1, 0.11, 0.12, 0.13, 0.17, 0.2))
# $sampleAllocation
#    stratumProportions stratumVariances availableSamples stratumSize
# 1                0.02             0.25               NA           8
# 2                0.03             0.25               NA          12
# 3                0.04             0.25               NA          16
# 4                0.08             0.25               NA          31
# 5                0.10             0.25               NA          39
# 6                0.11             0.25               NA          43
# 7                0.12             0.25               NA          47
# 8                0.13             0.25               NA          50
# 9                0.17             0.25               NA          66
# 10               0.20             0.25               NA          77
# 
# $designStatistics
#   sampleSize desiredDifference power
# 1        389              0.05    NA



resultStratified <- getStratified(purpose = "estimation", desiredDifference = 0.05, 
    typeIerror = 0.05, allocation = "proportional", stratumVariances = 0.125,
    stratumProportions = c(0.02, 0.03, 0.04, 0.08, 0.1, 0.11, 0.12, 0.13, 0.17, 0.2))
resultStratified
# $sampleAllocation
#    stratumProportions stratumVariances availableSamples stratumSize
# 1                0.02            0.125               NA           4
# 2                0.03            0.125               NA           6
# 3                0.04            0.125               NA           8
# 4                0.08            0.125               NA          16
# 5                0.10            0.125               NA          20
# 6                0.11            0.125               NA          22
# 7                0.12            0.125               NA          24
# 8                0.13            0.125               NA          25
# 9                0.17            0.125               NA          33
# 10               0.20            0.125               NA          39
# 
# $designStatistics
#   sampleSize desiredDifference power
# 1        197              0.05    NA

sum((resultStratified$sampleAllocation)$stratumSize)
# [1] 197


getStratified(purpose = "estimation", desiredDifference = 0.05, 
    typeIerror = 0.05, allocation = "neyman", 
    stratumVariances = seq(0.025, 0.25, 0.025),
    stratumProportions = 0.1)
# $sampleAllocation
#    stratumProportions stratumVariances availableSamples stratumSize
# 1                 0.1            0.025               NA           9
# 2                 0.1            0.050               NA          13
# 3                 0.1            0.075               NA          15
# 4                 0.1            0.100               NA          18
# 5                 0.1            0.125               NA          20
# 6                 0.1            0.150               NA          22
# 7                 0.1            0.175               NA          23
# 8                 0.1            0.200               NA          25
# 9                 0.1            0.225               NA          26
# 10                0.1            0.250               NA          28
# 
# $designStatistics
#   sampleSize desiredDifference power
# 1        199              0.05    NA



## getTwoStage() ##
getTwoStage(purpose = "estimation", desiredDifference = 0.05, 
    typeIerror = 0.05, clusterSize = 10, clusterVariances = 0.1, 
    stratumProportions = c(0.02, 0.03, 0.04, 0.08, 0.1, 0.11, 0.12, 0.13, 0.17, 0.2))
# $sampleAllocation
#    stratumProportions clusterVariances availableClusters numberClusters
# 1                0.02              0.1                NA              4
# 2                0.03              0.1                NA              5
# 3                0.04              0.1                NA              7
# 4                0.08              0.1                NA             13
# 5                0.10              0.1                NA             16
# 6                0.11              0.1                NA             17
# 7                0.12              0.1                NA             19
# 8                0.13              0.1                NA             20
# 9                0.17              0.1                NA             27
# 10               0.20              0.1                NA             31
#    stratumSize
# 1           40
# 2           50
# 3           70
# 4          130
# 5          160
# 6          170
# 7          190
# 8          200
# 9          270
# 10         310
# 
# $designStatistics
#   sampleSize desiredDifference power
# 1       1590              0.05    NA



#Multistage sampling with design effect 0, is stratified sampling
getTwoStage(purpose = "estimation", desiredDifference = 0.05,
    typeIerror = 0.05, stratumVariances = 0.125, designEffect = 1, 
    stratumProportions = c(0.02, 0.03, 0.04, 0.08, 0.1, 0.11, 0.12, 0.13, 0.17, 0.2),
    exactProduct = FALSE)
# $sampleAllocation
#    stratumProportions stratumVariances availableClusters numberClusters
# 1                0.02            0.125                NA             NA
# 2                0.03            0.125                NA             NA
# 3                0.04            0.125                NA             NA
# 4                0.08            0.125                NA             NA
# 5                0.10            0.125                NA             NA
# 6                0.11            0.125                NA             NA
# 7                0.12            0.125                NA             NA
# 8                0.13            0.125                NA             NA
# 9                0.17            0.125                NA             NA
# 10               0.20            0.125                NA             NA
#    stratumSize
# 1            4
# 2            6
# 3            8
# 4           16
# 5           20
# 6           22
# 7           24
# 8           26
# 9           33
# 10          39
# 
# $designStatistics
#   sampleSize desiredDifference power
# 1        193              0.05    NA
resultStratified


getTwoStage(purpose = "testing", desiredDifference = 0.05, 
    typeIerror = 0.05, power = 0.8, clusterSize = 10, clusterVariances = 0.1, 
    stratumProportions = c(0.02, 0.03, 0.04, 0.08, 0.1, 0.11, 0.12, 0.13, 0.17, 0.2),
    exactProduct = FALSE)
# $sampleAllocation
#    stratumProportions clusterVariances availableClusters numberClusters
# 1                0.02              0.1                NA              7
# 2                0.03              0.1                NA             10
# 3                0.04              0.1                NA             13
# 4                0.08              0.1                NA             26
# 5                0.10              0.1                NA             32
# 6                0.11              0.1                NA             35
# 7                0.12              0.1                NA             38
# 8                0.13              0.1                NA             41
# 9                0.17              0.1                NA             54
# 10               0.20              0.1                NA             63
#    stratumSize
# 1           63
# 2           95
# 3          126
# 4          252
# 5          314
# 6          346
# 7          377
# 8          409
# 9          534
# 10         628
# 
# $designStatistics
#   sampleSize desiredDifference power
# 1       3140              0.05   0.8





# Check results with EFSA document: Sample size considerations for hierarchical models

## getThreeStage()
getThreeStage(purpose = "testing", sampleSize = NA, 
    marginalPrevalence0 = 0.01, marginalPrevalenceA = 0.015,
    power = 0.8, typeIerror = 0.1, 
    sizeLevel3 = NA, sizeLevel2 = 50, sizeLevel1 = 100,
    varianceLevel3 = 1, varianceLevel2 = 2.5, varianceLevel1 = NA)
#   sampleSize desiredDifference power sizeLevel3 sizeLevel2 sizeLevel1
# 1     100607              0.59   0.8         21         50        100
# Figure 13, right panel: OK


getThreeStage(purpose = "testing", sampleSize = NA, 
    marginalPrevalence0 = 0.01, marginalPrevalenceA = 0.015,
    power = 0.8, typeIerror = 0.1, 
    sizeLevel3 = 20, sizeLevel2 = NA, sizeLevel1 = 100,
    varianceLevel3 = 1, varianceLevel2 = 4.2, varianceLevel1 = NA)
#   sampleSize desiredDifference power sizeLevel3 sizeLevel2 sizeLevel1
# 1      61569             0.659   0.8         20         31        100


getThreeStage(purpose = "testing", sampleSize = NA, 
    marginalPrevalence0 = 0.01, marginalPrevalenceA = 0.015,
    power = 0.8, typeIerror = 0.1, 
    sizeLevel3 = 20, sizeLevel2 = 40, sizeLevel1 = NA,
    varianceLevel3 = 1, varianceLevel2 = 4.2, varianceLevel1 = NA)
#   sampleSize desiredDifference power sizeLevel3 sizeLevel2 sizeLevel1
# 1      55159             0.659   0.8         20         40         69


getThreeStage(purpose = "testing", sampleSize = 10*10*100, desiredDifference = 0.5, 
    power = NA, typeIerror = 0.1, 
    sizeLevel3 = 10, sizeLevel2 = 10, sizeLevel1 = 100,
    varianceLevel3 = 1, varianceLevel2 = 4.2, varianceLevel1 = 1)
#   sampleSize desiredDifference power sizeLevel3 sizeLevel2 sizeLevel1
# 1      10000               0.5 0.375         10         10        100

getThreeStage(purpose = "testing", sampleSize = 20*50*100, desiredDifference = 0.5, 
    power = NA, typeIerror = 0.1, 
    sizeLevel3 = 20, sizeLevel2 = 50, sizeLevel1 = 100,
    varianceLevel3 = 1, varianceLevel2 = 2.5, varianceLevel1 = 1)
#   sampleSize desiredDifference power sizeLevel3 sizeLevel2 sizeLevel1
# 1      1e+05               0.5 0.704         20         50        100

wrapFunctionMilanzi("getThreeStage", 
    randomInput = list(desiredDifference = c(0.4, 0.5, 0.6)),
    fixedInput = list(purpose = "testing", sampleSize = 20*50*100,  
        power = NA, typeIerror = 0.1, 
        sizeLevel3 = 20, sizeLevel2 = 50, sizeLevel1 = 100,
        varianceLevel3 = 1, varianceLevel2 = 2.5, varianceLevel1 = 1)
)

## getChangeOverTime() and getVarianceCovariance() ##

getVarianceCovariance(varianceCorrelation = 
        matrix(c(0.25, 0.95, 0.95, 0.25), nrow = 2))
#        [,1]   [,2]
# [1,] 0.2500 0.2375
# [2,] 0.2375 0.2500


getVarianceCovariance(varianceSingle = 0.25, correlationSingle = 0.95, 
    dimension = 2)
#        [,1]   [,2]
# [1,] 0.2500 0.2375
# [2,] 0.2375 0.2500



getChangeOverTime(purpose = "testing", desiredDifference = 0.05, 
    typeIerror = 0.05, power = 0.8,
    varianceCovariance = getVarianceCovariance(varianceSingle = 0.25, 
        correlationSingle = 0.95, dimension = 2))
#   sampleSize desiredDifference power
# 1         79              0.05   0.8


getChangeOverTime(purpose = "testing", desiredDifference = 0.05, 
    typeIerror = 0.05, power = 0.8,
    varianceCovariance = getVarianceCovariance(varianceSingle = 0.25, 
        correlationSingle = 0.5, dimension = 2))
#   sampleSize desiredDifference power
# 1        785              0.05   0.8


getChangeOverTime(purpose = "testing", desiredDifference = 0.05, 
    typeIerror = 0.05, power = 0.8,
    varianceCovariance = getVarianceCovariance(varianceSingle = 0.25, 
        correlationSingle = 0.01, dimension = 2))
#   sampleSize desiredDifference power
# 1       1555              0.05   0.8



getChangeOverTime(purpose = "testing", desiredDifference = 0.05, 
    typeIerror = 0.05, power = 0.8,
    varianceCovariance = getVarianceCovariance(varianceSingle = 0.25, 
        correlationSingle = 0.95, dimension = 2))


## Pesticide Monitoring Data: Example in Milanzi et al. paper, p. 33 onwards ##
getSimpleRandom(purpose = "estimation", desiredDifference = 0.01, 
    typeIerror = 0.05, sampleVariance = 0.05)
#   sampleSize desiredDifference power
# 1       1921              0.01    NA



## wrapFunctionMilanzi() ##

# Table 6, p37
wrapFunctionMilanzi(wrappedFunction = "getSimpleRandom", 
    randomInput = list(sampleSize = c(2057, 999, 961, 1568)),
    fixedInput = list(purpose = "estimation", typeIerror = 0.05, 
        sampleVariance = 0.05))

# Figure 4, p38
helpRandomInput <- list(power = c(0.7, 0.8, 0.9), 
    desiredDifference = c(0.01, 0.05, 0.075), typeIerror = c(0.05, 0.1))
randomInput <- expand.grid(helpRandomInput)
randomInput$sampleVariance <- randomInput$desiredDifference * 
    (1 - randomInput$desiredDifference)
    
out <- wrapFunctionMilanzi(wrappedFunction = "getSimpleRandom", 
    randomInput = as.list(randomInput),
    fixedInput = list(purpose = "testing"))
as.data.frame(t(sapply(out, rbind)))

