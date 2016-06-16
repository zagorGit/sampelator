
library(sampelator)

## Formulas implemented for Simple random sampling with Single/Multiple areas ## 
## or Riskbased sampling ##

## Check results with spreadsheets RiBESS_NoRF.xlsx; RiBESS_RiF-1.xlsx:
## differences are due to rounding errors

# p. 12 Figure 3
getSampleSizeGse(sse = 0.95, totalN = 7000, dp = 0.01, tse = 0.85, 
    method = "binom")
#   sampleSize      gse
# 1        351 0.950026

getSampleSizeGse(sse = 0.95, totalN = 7000, dp = 0.01, tse = 0.85, 
    method = "hyper")
#   sampleSize       gse
# 1        344 0.9501946

# p. 13 different areas
if(FALSE){
  
  getSampleSizeGse(sse = 0.95, totalN = c(250, 50, 300, 3400, 3000),
      dp = 0.01, tse = 0.85, method = "binom")
#   populationSize sampleSize       gse
# 1            250        250 0.8816469
# 2             50         50 0.3474167
# 3            300        300 0.9227647
# 4           3400        351 0.9500260
# 5           3000        351 0.9500260
}


getSampleSizeGse(sse = 0.95, totalN = c(250, 50, 300, 3400, 3000),
    dp = 0.01, tse = 0.85, method = "hyper")
#   populationSize sampleSize  gse
# 1            250        205 0.95
# 2             50         60 0.95
# 3            300        223 0.95
# 4           3400        336 0.95
# 5           3000        335 0.95




## Check results with Ribess ShinyApp

## optimizeSampleSizeGse() ##

resultBin <- optimizeSampleSizeGse(targetAse = 0.95, totalN = NA, 
    dp = 0.01, tse = 0.85, method = "binom", isRiskbased = TRUE,
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), secondRF = c(0.4,0.4,0.2))))
resultBin
#                    sampleSize       gse
# secondRFa_firstRFa         14 0.2968032
# secondRFb_firstRFa         20 0.2839042
# secondRFc_firstRFa         40 0.2829053
# secondRFa_firstRFb         20 0.2839042
# secondRFb_firstRFb         30 0.2832372
# secondRFc_firstRFb         60 0.2825745
# secondRFa_firstRFc         40 0.2829053
# secondRFb_firstRFc         60 0.2825745
# secondRFc_firstRFc        119 0.2802584
# Replace ceiling(sampleSize) by round(numberDiseased)
# to obtain same numbers as in shinyApp


resultHyper <- optimizeSampleSizeGse(targetAse = 0.95, totalN = 700, 
    dp = 0.01, tse = 0.85, method = "hyper", isRiskbased = TRUE,
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), secondRF = c(0.4,0.4,0.2))))
resultHyper
#                    sampleSize       gse
# secondRFa_firstRFa         11 0.2812627
# secondRFb_firstRFa         16 0.3008212
# secondRFc_firstRFa         16 0.3212852
# secondRFa_firstRFb         18 0.2938084
# secondRFb_firstRFb         25 0.2921513
# secondRFc_firstRFb         28 0.2812337
# secondRFa_firstRFc         37 0.2842906
# secondRFb_firstRFc         53 0.2828515
# secondRFc_firstRFc         75 0.2827349
# OK with shinyApp

aseHyper <- round(attr(resultHyper, "ase"), 2)
aseHyper
# [1] 0.95


## getSampleSizeGse() ##

gseHyper <- getSampleSizeGse(sse = NA, n = resultHyper$sampleSize, totalN = 700, 
    dp = 0.01, tse = 0.85, method = "binom", isRiskbased = TRUE,
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), 
            secondRF = c(0.4,0.4,0.2))))
gseHyper$gse
# [1] 0.2416912 0.2344441 0.1245515 0.2595870 0.2423309 0.1435622 0.2647953
# [8] 0.2542335 0.1871971
# OK with shinyApp

round(attr(gseHyper, "ase"), 2)
# [1] 0.89

wrapFunction(wrappedFunction = "getSampleSizeGse",
    nMC = 100, sse = NA, n = resultHyper$sampleSize, totalN = 700, 
    dp = 0.01, tse = 0.85, method = "binom", isRiskbased = TRUE,
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), 
            secondRF = c(0.4,0.4,0.2))))

wrapFunction(wrappedFunction = "getSampleSizeGse",
    sse = NA, n = 1000, totalN = 7000, 
    dp = 0.01, tse = 0.85, method = "binom")


## getConvenience() ##

getConvenience(targetAse = 0.95, 
    inputConvenience = c(10, 7, 4, 1, 1, 1, 4, 3, 2), maxn = NA,
    totalN = 7000, dp = 0.01, tse = 0.85, method = "hyper", isRiskbased = TRUE,
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), 
            secondRF = c(0.4,0.4,0.2))))
#                    sampleSize        gse
# secondRFa_firstRFa         60 0.81075360
# secondRFb_firstRFa         42 0.52724214
# secondRFc_firstRFa         24 0.19337045
# secondRFa_firstRFb          6 0.09566186
# secondRFb_firstRFb          6 0.06466255
# secondRFc_firstRFb          6 0.03290165
# secondRFa_firstRFc         24 0.18169948
# secondRFb_firstRFc         18 0.09515523
# secondRFc_firstRFc         12 0.03277058
# OK with shinyApp

getConvenience(targetAse = 0.95, 
    inputConvenience = c(10, 7, 4, 1, 1, 1, 4, 3, 2), maxn = 7,
    totalN = 7000, dp = 0.01, tse = 0.85, method = "hyper", isRiskbased = TRUE,
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7),
            secondRF = c(0.4,0.4,0.2))))
#                    sampleSize        gse
# secondRFa_firstRFa         63 0.82753100
# secondRFb_firstRFa         42 0.52724214
# secondRFc_firstRFa         21 0.16981211
# secondRFa_firstRFb          7 0.11077152
# secondRFb_firstRFb          7 0.07508109
# secondRFc_firstRFb          7 0.03833707
# secondRFa_firstRFc         21 0.16083206
# secondRFb_firstRFc         14 0.07476125
# secondRFc_firstRFc          7 0.01920713
# OK with shinyApp

getConvenience(targetAse = 0.95, 
    inputConvenience = c(10, 7, 4, 1, 1, 1, 4, 3, 2),
    totalN = 7000, dp = 0.01, tse = 0.85, method = "hyper", isRiskbased = TRUE,
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7),
            secondRF = c(0.4,0.4,0.2))))
#                    sampleSize        gse
# secondRFa_firstRFa         60 0.81075360
# secondRFb_firstRFa         42 0.52724214
# secondRFc_firstRFa         24 0.19337045
# secondRFa_firstRFb          6 0.09566186
# secondRFb_firstRFb          6 0.06466255
# secondRFc_firstRFb          6 0.03290165
# secondRFa_firstRFc         24 0.18169948
# secondRFb_firstRFc         18 0.09515523
# secondRFc_firstRFc         12 0.03277058
# Slightly different results because of 
# different implementation than in shinyApp


# Provide warning if populationSize is too small
if(FALSE){
  
  getConvenience(targetAse = 0.95, 
      inputConvenience = c(10, 7, 4, 1, 1, 1, 4, 3, 2), maxn = 5,
      totalN = 700, dp = 0.01, tse = 0.85, method = "hyper", isRiskbased = TRUE,
      relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
      proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), 
              secondRF = c(0.4,0.4,0.2))))
  
}






## calculatePfree() ##

calculatePfree(targetPFree = 0.95, pFreePrior = 0.5, 
    pIntro = c(0.2, 0.2, 0.2), sse = aseHyper, timePoints = 3)
#             pFreePrior    pFree  sse pIntro pFreeAdjusted
# timePeriod1  0.5000000 0.952381 0.95    0.2     0.7619048
# timePeriod2  0.7619048 0.952381 0.84    0.2     0.7619048
# timePeriod3  0.7619048 0.952381 0.84    0.2     0.7619048
# OK with shinyApp



## helpExpandNames() ##

inputList <- list(firstRF = c(a = 1, b = 2, c = 3), 
    secondRF = c(g = 3, k = 2, l = 1))
helpExpandNames(inputList = inputList)
expandRisks(inputList)
# secondRFg_firstRFa secondRFk_firstRFa secondRFl_firstRFa secondRFg_firstRFb 
#                  3                  2                  1                  6 
# secondRFk_firstRFb secondRFl_firstRFb secondRFg_firstRFc secondRFk_firstRFc 
#                  4                  2                  9                  6 
# secondRFl_firstRFc 
#                  3 



## createRandom(), wrapperFunction() and summaryWrapper() ##

tse <- createRandom(distributionName = "beta",
    distributionParameters = c(shape1 = 100, shape2 = 15))

#optimizeSampleSizeGse
outputSampleSize <- wrapFunction(wrappedFunction = "optimizeSampleSizeGse", 
    tse = tse, targetAse = 0.95, totalN = NA, dp = 0.01, method = "binom", 
    isRiskbased = TRUE, 
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), 
            secondRF = c(0.4,0.4,0.2))))

test <- summaryWrapper(outputWrapper = outputSampleSize, percentile = 0.5, 
    parameterName = "sampleSize")
test
#                    sampleSize       gse
# secondRFa_firstRFa         13 0.2809233
# secondRFb_firstRFa         20 0.2859549
# secondRFc_firstRFa         40 0.2849417
# secondRFa_firstRFb         20 0.2859549
# secondRFb_firstRFb         30 0.2852783
# secondRFc_firstRFb         59 0.2806017
# secondRFa_firstRFc         40 0.2849417
# secondRFb_firstRFc         59 0.2806017
# secondRFc_firstRFc        118 0.2802709

attr(test, "ase")
# [1] 0.9500938



# Similarly when providing quantiles
betaQuantiles <- qbeta(p = c(0.025, 0.5, 0.975), shape1 = 100, shape2 = 15)
betaQuantiles
# [1] 0.8025410 0.8717095 0.9244472
library(rriskDistributions)
get.beta.par(p = c(0.025, 0.5, 0.975), q = betaQuantiles, show.output = FALSE,
    plot = FALSE)
#   shape1   shape2 
# 98.31395 14.75198 


tse <- createRandom(distributionName = "beta",
    distributionParameters = c(probabilities = c(0.025, 0.5, 0.975), 
        quantiles = betaQuantiles), isQuantiles = TRUE)

head(
    createRandom(distributionName = "norm",
        distributionParameters = c(probabilities = c(0.5, 0.975), 
            quantiles = betaQuantiles[-1]), isQuantiles = TRUE)
)
# [1] 0.8548535 0.8766508 0.8492252 0.9146337 0.8805756 0.8496331


outputWrapper <- wrapFunction(wrappedFunction = "optimizeSampleSizeGse", 
    tse = tse,
    targetAse = 0.95, totalN = NA, dp = 0.01, method = "binom", 
    isRiskbased = TRUE, 
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), 
            secondRF = c(0.4,0.4,0.2))))

summaryWrapper(outputWrapper = outputWrapper, percentile = 0.95, 
    parameterName = "sampleSize")
#                    sampleSize       gse
# secondRFa_firstRFa         14 0.2943093
# secondRFb_firstRFa         21 0.2932849
# secondRFc_firstRFa         42 0.2922704
# secondRFa_firstRFb         21 0.2932849
# secondRFb_firstRFb         32 0.2964838
# secondRFc_firstRFb         63 0.2919344
# secondRFa_firstRFc         42 0.2922704
# secondRFb_firstRFc         63 0.2919344
# secondRFc_firstRFc        126 0.2915995


# When the distribution is unknown:


# (1) Get the best distributional fit & its parameters to the given quantiles
bestDistribution <- getBestDistribution(probabilities = c(0.025, 0.5, 0.975), 
    quantiles = qbeta(p = c(0.025, 0.5, 0.975), shape1 = 100, shape2 = 15),
    withGUI = FALSE)

# (2) Use this info to create random values and apply the wrapFunction()
tse <- createRandom(distributionName = bestDistribution$bestName,
    distributionParameters = bestDistribution$bestParameters)

outputWrapper <- wrapFunction(wrappedFunction = "optimizeSampleSizeGse", 
    tse = tse,
    targetAse = 0.95, totalN = NA, dp = 0.01, method = "binom", 
    isRiskbased = TRUE, 
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), 
            secondRF = c(0.4,0.4,0.2))))

summaryWrapper(outputWrapper = outputWrapper, percentile = 0.5, 
    parameterName = "sampleSize")
#                    populationSize sampleSize       gse
# secondRFa_firstRFa             NA         13 0.2811220
# secondRFb_firstRFa             NA         20 0.2861556
# secondRFc_firstRFa             NA         40 0.2851409
# secondRFa_firstRFb             NA         20 0.2861556
# secondRFb_firstRFb             NA         30 0.2854780
# secondRFc_firstRFb             NA         59 0.2807982
# secondRFa_firstRFc             NA         40 0.2851409
# secondRFb_firstRFc             NA         59 0.2807982
# secondRFc_firstRFc             NA        118 0.2804670






# When input values are constant
outputWrapper <- wrapFunction(wrappedFunction = "optimizeSampleSizeGse", 
    tse = createRandom(distributionName = "constant", 
        distributionParameters = c(value = 0.85)),
    targetAse = 0.95, totalN = NA, dp = 0.01, method = "binom", 
    isRiskbased = TRUE, 
    relativeRisks = expandRisks(list(firstRF = 3:1, secondRF = 3:1)),
    proportionsRiskGroups = expandRisks(list(firstRF = c(a = 0.1, b = 0.2, c = 0.7), 
            secondRF = c(a = 0.4, b = 0.4, c = 0.2))))

summaryWrapper(outputWrapper = outputWrapper, percentile = 0.5, 
    parameterName = "sampleSize")


#Compare these results with resultBin
resultBin
#                    sampleSize       gse
# secondRFa_firstRFa         14 0.2968032
# secondRFb_firstRFa         20 0.2839042
# secondRFc_firstRFa         40 0.2829053
# secondRFa_firstRFb         20 0.2839042
# secondRFb_firstRFb         30 0.2832372
# secondRFc_firstRFb         60 0.2825745
# secondRFa_firstRFc         40 0.2829053
# secondRFb_firstRFc         60 0.2825745
# secondRFc_firstRFc        119 0.2802584


# Some random risk levels
firstRF <- cbind(createRandom(distributionName = "constant", 
        distributionParameters = c(value = 3)),
    createRandom(distributionName = "unif",
        distributionParameters = c(min = 2, max = 3)),
    createRandom(distributionName = "constant",
        distributionParameters = c(value = 1)))
attr(firstRF, "random") <- TRUE

secondRF <- cbind(createRandom(distributionName = "constant", 
        distributionParameters = c(value = 1)),
    createRandom(distributionName = "constant",
        distributionParameters = c(value = 2)),
    createRandom(distributionName = "norm",
        distributionParameters = c(mean = 3, sd = 0.2)))
attr(secondRF, "random") <- TRUE

expandedRisks <- expandRisks(unexpanded = list(firstRF = firstRF, secondRF = secondRF))


outputWrapper <- wrapFunction(wrappedFunction = "optimizeSampleSizeGse", 
    tse = 0.85, targetAse = 0.95, totalN = NA, dp = 0.01, method = "binom", 
    isRiskbased = TRUE, 
    relativeRisks = expandedRisks,
    proportionsRiskGroups = expandRisks(list(firstRF = c(0.1,0.2,0.7), 
            secondRF = c(0.4,0.4,0.2))))

summaryWrapper(outputWrapper = outputWrapper, percentile = 0.5, 
    parameterName = "sampleSize")
#                    sampleSize       gse
# secondRFa_firstRFa         35 0.2800441
# secondRFb_firstRFa         18 0.2879124
# secondRFc_firstRFa         13 0.2981877
# secondRFa_firstRFb         40 0.2854938
# secondRFb_firstRFb         20 0.2865109
# secondRFc_firstRFb         14 0.2889809
# secondRFa_firstRFc        106 0.2815490
# secondRFb_firstRFc         53 0.2819206
# secondRFc_firstRFc         37 0.2831511


#calculatePfree
pIntro <- cbind(createRandom(distributionName = "norm", 
        distributionParameters = c(mean = 0.3, sd = 0.1)),
    createRandom(distributionName = "norm", 
        distributionParameters = c(mean = 0.3, sd = 0.1)))
attr(pIntro, "random") <- TRUE

outputPFree <- wrapFunction(wrappedFunction = "calculatePfree", 
    pIntro = pIntro, targetPFree = createRandom(distributionName = "beta",
        distributionParameters = c(shape1 = 10, shape2 = 1)),
    sse = 0.95, pFreePrior = 0.5)

summaryWrapper(outputWrapper = outputPFree, percentile = 0.5, 
    parameterName = "pFreeAdjusted")
#             pFreePrior     pFree  sse    pIntro pFreeAdjusted
# timePeriod1  0.5000000 0.9523810 0.95 0.3398106     0.6287518
# timePeriod2  0.6287518 0.9941301 0.95 0.3398106     0.6563142



betaQuantiles <- qbeta(p = c(0.025, 0.5, 0.975), shape1 = 10, shape2 = 1)
get.beta.par(p = c(0.025, 0.5, 0.975), q = betaQuantiles, show.output = FALSE,
    plot = FALSE)
#    shape1    shape2 
# 10.000087  1.000007 

outputWrapper <- wrapFunction(wrappedFunction = "calculatePfree", 
    pIntro = rep(0.2, 2),
    targetPFree = createRandom(distributionName = "beta",
        distributionParameters = c(probabilities = c(0.025, 0.5, 0.975), 
            quantiles = betaQuantiles), isQuantiles = TRUE),
    sse = 0.95, pFreePrior = 0.5)

summaryWrapper(outputWrapper = outputWrapper, percentile = 0.5, 
    parameterName = "pFreeAdjusted")
#             pFreePrior     pFree  sse pIntro pFreeAdjusted
# timePeriod1  0.5000000 0.9523810 0.95    0.2     0.7619048
# timePeriod2  0.7619048 0.9275362 0.75    0.2     0.7420290


## histogramWrapper() ## 

histogramWrapper(outputWrapper = outputPFree,
    varNames = c("pIntro", "pFreePrior"))

histogramWrapper(outputWrapper = outputPFree,
    varNames = "pIntro", timePeriods = "timePeriod1")

histogramWrapper(outputWrapper = outputSampleSize,
    varNames = "sampleSize")


## Specific setting with bug

firstRF <- cbind(createRandom(distributionName = "constant", 
        distributionParameters = c(value = 1)),
    exp(createRandom(distributionName = "norm",
        distributionParameters = c(mean = 1.3, sd = 0.11))))
attr(firstRF, "random") <- TRUE

secondRF <- cbind(exp(createRandom(distributionName = "norm", 
        distributionParameters = c(mean = 0.96, sd = 0.17))),
    createRandom(distributionName = "constant",
        distributionParameters = c(value = 1)))
attr(secondRF, "random") <- TRUE

expandedRisks <- expandRisks(unexpanded = list(firstRF = firstRF, secondRF = secondRF))

outputWrapper <- wrapFunction(wrappedFunction = "optimizeSampleSizeGse", 
    targetAse = 0.95, totalN = 9412, dp = 0.001,
#    tse = 0.93,
    tse = createRandom(distributionName = "beta",
        distributionParameters = c(shape1 = 10, shape2 = 1)), 
    isRiskbased = TRUE, 
#    relativeRisks = c(2.62117,1,9.5896,3.6585),
    relativeRisks = expandedRisks,
    proportionsRiskGroups = c(0.716426,0.006906077,0.238632,0.038036549))

summaryWrapper(outputWrapper = outputWrapper, percentile = 0.9, 
    parameterName = "sampleSize")