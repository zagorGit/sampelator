# Project: sampelator_git
# 
# Author: mvarewyck
###############################################################################


#' Take the ceiling of a numeric unless the tolerance level is not exceeded
#' @param toRound numeric value that should be rounded
#' @param delta numeric; tolerance level; default is 0.1 
#' @return numeric value 
#' @export
roundUp <- function(toRound, delta = 0.1){
  
  ids <- which(!is.na(toRound))
  
  newValue <- toRound
  newValue[ids] <- trunc(newValue[ids])
  toChange <- (abs(newValue - toRound) > delta)
      
  newValue[toChange[!is.na(toChange)]] <- 
        ceiling(toRound[toChange[!is.na(toChange)]])
    
  return(newValue)
      
}

#' Calculate (unrounded) sample size, desired difference or statistical power
#' using a general formula 
#' @param purpose character string, the purpose of the study is one of 
#' "estimation" or "testing"
#' @param sampleSize integer, the sample size; default is NA
#' @param desiredDifference numeric, for "estimation" the desired margin of error,
#' i.e. half width of the desired confidence interval or for "testing" the 
#' true difference in means that is tested; default is NA
#' @param power numeric, statistical power to detect the predefined difference 
#' "desiredDifference" when purpose is "testing"; default is NA
#' @param typeIerror numeric, the type I error
#' @param variance numeric, e.g. estimated element variance in simple random
#' sampling design
#' @param populationSize numeric, the population size; default is NA
#' @param adjustFinitePopulation boolean, adjust for finite population?; 
#' default is FALSE
#' @param inflationFactor numeric, the inflation factor with which the uncorrected
#' sample size should be multiplied to account e.g. for missingness;
#' default is NA
#' @return a numeric; when purpose is estimation, one of sampleSize or 
#' desiredDifference is missing and the returned value; when purpose is testing,
#' one of sampleSize, desiredDifference or power is missing and the returned value
#' @export
getRawCalculation <- function(purpose = c("estimation", "testing"), 
    sampleSize = NA, desiredDifference = NA, power = NA, typeIerror, variance,
    populationSize = NA, adjustFinitePopulation = FALSE, inflationFactor = NA) {
  
  
  if( purpose == "estimation" ) {
    
    if( is.na(sampleSize) ) {
      
      outputValue <- qnorm(typeIerror/2)^2 / desiredDifference^2 * variance
      outputValue <- adjustSampleSize(sampleSize = outputValue, 
          populationSize = populationSize, 
          adjustFinitePopulation = adjustFinitePopulation, 
          inflationFactor = inflationFactor)
      
    } else if( is.na(desiredDifference) ) {
      
      outputValue <- sqrt( qnorm(typeIerror/2)^2 * variance / sampleSize )
      
    }    
    
  } else if ( purpose == "testing" ) {
    
    if( is.na(sampleSize) ) {
      
      outputValue <- variance * (qnorm(1 - typeIerror/2) + qnorm(power))^2 /
          desiredDifference^2
      outputValue <- adjustSampleSize(sampleSize = outputValue, 
          populationSize = populationSize, 
          adjustFinitePopulation = adjustFinitePopulation, 
          inflationFactor = inflationFactor)
      
    } else if(is.na(desiredDifference) ) {
      
      outputValue <- sqrt( variance / sampleSize * 
              (qnorm(1 - typeIerror/2) + qnorm(power))^2 )
      
    } else if( is.na(power) ) {
      
      outputValue <- pnorm(q = desiredDifference * sqrt( sampleSize / variance ) - 
              qnorm(1 - typeIerror/2))
      
    } 
    
  }
  
  return( outputValue ) 
  
}



#' Check which of the input parameters is missing
#' @param purpose character string, the purpose of the study is one of 
#' "estimation" or "testing"
#' @param inputList list with input parameters
#' @return boolean vector, which of the variables in inputList is missing;
#' error message if not exactly one necessary parameter of inputList is missing
#' @export 
getMissing <- function(purpose = c("estimation", "testing"), inputList) {
  
  isMissing <- sapply(inputList, function(x) is.na(x))
  
  if( purpose == "estimation" & sum(isMissing[-3]) != 1 ) {
    
    namesInput <- paste(names(inputList)[-3], collapse = ", ") 
    stop( paste0("Exactly one of ", namesInput, " should be missing") )
    
    
  } else if(purpose == "testing" & sum(isMissing) != 1 ) {
    
    namesInput <- paste(names(inputList), collapse = ", ") 
    stop( paste0("Exactly one of ", namesInput, " should be missing") )
    
  }
  
  return(isMissing)
  
}



#' Adjust sample size value for finite population or expected missingness
#' @param sampleSize numeric, the uncorrected estimated sample size
#' @param populationSize numeric, the population size; default is NA
#' @param adjustFinitePopulation boolean, adjust for finite population?; 
#' default is FALSE
#' @param inflationFactor numeric, the inflation factor with which the uncorrected
#' sample size should be multiplied to account e.g. for missingness;
#' default is NA
#' @return numeric, the corrected estimated sample size
#' @export
adjustSampleSize <- function(sampleSize, populationSize = NA,
    adjustFinitePopulation = FALSE, inflationFactor = NA) {
  
  if( adjustFinitePopulation ) {
    
    sampleSize <- populationSize*sampleSize / (populationSize + sampleSize - 1)
    
  }
  
  if( !is.na(inflationFactor)){
    
    sampleSize <- sampleSize * inflationFactor
    
  }
  
  return( sampleSize )
  
}



#' Calculate design statistics for simple random sampling
#' @param purpose character string, the purpose of the study is one of 
#' "estimation" or "testing"
#' @param sampleSize integer, the total sample size; default is NA
#' @param desiredDifference numeric, for "estimation" the desired margin of error,
#' i.e. half width of the desired confidence interval or for "testing" the 
#' true difference in means that is tested; default is NA
#' @param power numeric, statistical power to detect the predefined difference 
#' "desiredDifference" when purpose is "testing"; default is NA
#' @param typeIerror numeric, the type I error
#' @param sampleVariance numeric, estimated element variance in simple random
#' sampling design
#' @param populationSize numeric, the population size; default is NA
#' @param adjustFinitePopulation boolean, adjust for finite population?; 
#' default is FALSE
#' @param inflationFactor numeric, the inflation factor with which the uncorrected
#' sample size should be multiplied to account e.g. for missingness;
#' default is NA
#' @return a dataframe with sampleSize, desiredDifference and power
#' @export
getSimpleRandom <- function(purpose = c("estimation", "testing"), 
    sampleSize = NA, desiredDifference = NA, power = NA, typeIerror, 
    sampleVariance, populationSize = NA, adjustFinitePopulation = FALSE,
    inflationFactor = NA) {
  
  inputList <- list(sampleSize = sampleSize, 
      desiredDifference = desiredDifference, power = power)
  isMissing <- getMissing(purpose = purpose, inputList = inputList)
  
  calculatedMissing <- do.call( "getRawCalculation", 
      list(purpose = purpose, sampleSize = sampleSize, 
          desiredDifference = desiredDifference, power = power, 
          typeIerror = typeIerror, variance = sampleVariance, 
          populationSize = populationSize, 
          adjustFinitePopulation = adjustFinitePopulation, 
          inflationFactor = inflationFactor))
  assign( (names(inputList)[isMissing])[1], calculatedMissing )
  
  returnData <- data.frame( sampleSize = ceiling(sampleSize), 
      desiredDifference = round(desiredDifference, 3),
      power = round(power, 3) )
  
  return( returnData ) 
  
}



#' Calculate design statistics for cluster sampling
#' @param purpose character string, the purpose of the study is one of 
#' "estimation" or "testing"
#' @param sampleSize integer, the total sample size; default is NA
#' @param desiredDifference numeric, for "estimation" the desired margin of error,
#' i.e. half width of the desired confidence interval or for "testing" the 
#' true difference in means that is tested; default is NA
#' @param power numeric, statistical power to detect the predefined difference 
#' "desiredDifference" when purpose is "testing"; default is NA
#' @param typeIerror numeric, the type I error
#' @param clusterSize integer, (average) size of the clusters
#' @param clusterVariance numeric, estimated variance between the cluster means 
#' @param designEffect numeric (vector), the design effect, i.e. the ratio of 
#' the sample mean variances under simple random sampling and cluster sampling;
#' default is NA 
#' @param sampleVariance numeric, estimated element variance in simple random
#' sampling design
#' @param conditionalPrevalence numeric, conditional prevalence of the outcome
#' given a random effect of zero (to calculate designEffect); default is NA
#' @param correlation numeric, the within-cluster correlation; default is NA.
#' if NA, correlation is estimated based on approximation using clusterVariance
#' and clusterSize (to calculate designEffect)
#' @param populationSize numeric, the population size; default is NA
#' @param adjustFinitePopulation boolean, adjust for finite population?; 
#' default is FALSE
#' @param inflationFactor numeric, the inflation factor with which the uncorrected
#' sample size should be multiplied to account e.g. for missingness;
#' default is NA
#' @param exactProduct boolean, if TRUE the sampleSize is exactly the product of
#' numberClusters and clusterSize; if FALSE the sampleSize may be smaller than
#' the product of numberClusters and clusterSize; default is TRUE
#' @return a list with two dataframes: one with sampleAllocation info 
#' (availableClusters, numberClusters); one with design statistics
#' (numberClusters, sampleSize, desiredDifference, power)
#' @export
getClustered <- function(purpose = c("estimation", "testing"), 
    sampleSize = NA, desiredDifference = NA, power = NA, 
    typeIerror, clusterVariance, clusterSize, 
    designEffect = NA, sampleVariance, conditionalPrevalence = NA, 
    correlation = NA, populationSize = NA, adjustFinitePopulation = FALSE,
    inflationFactor = NA, exactProduct = TRUE) {
  
#  if( !is.na(sampleSize) ) {
#    
#    if( !is.na(numberClusters) & numberClusters*clusterSize != sampleSize) {
#      
#      stop( paste0("numberClusters * clusterSize (", 
#              numberClusters*clusterSize, ") does not equal sample size (", 
#              sampleSize, "). \n Please provide correct input values") )
#      
#    }
#    
#  } else {
#    
#    sampleSize <- numberClusters * clusterSize
#    
#  }
  
  if( all(is.na(designEffect)) ) {
    
    designEffect <- getDesignEffect(clusterVariance = clusterVariance, 
        conditionalPrevalence = conditionalPrevalence,
        correlation = correlation, clusterSize = clusterSize)
    
  }
  
  if( all(is.na(designEffect)) ) {
    
    clusterVarianceB <- clusterVariance * clusterSize
    
  } else{
    
    clusterVarianceB <- designEffect * rep(sampleVariance, times = length(designEffect))
    
  }
  
  inputList <- list(sampleSize = sampleSize,
      desiredDifference = desiredDifference, power = power)
  isMissing <- getMissing(purpose = purpose, inputList = inputList)
  
  calculatedMissing <- do.call( "getRawCalculation", 
      list(purpose = purpose, sampleSize = sampleSize, 
          desiredDifference = desiredDifference, power = power, 
          typeIerror = typeIerror, variance = clusterVarianceB,
          populationSize = populationSize, 
          adjustFinitePopulation = adjustFinitePopulation,
          inflationFactor = inflationFactor))
  assign( (names(inputList)[isMissing])[1], calculatedMissing )
  
  numberClusters <- ceiling(sampleSize / clusterSize)
  
  if(adjustFinitePopulation){
    
    availableClusters <- floor(populationSize / clusterSize)
    
  } else {
    
    availableClusters <- floor(populationSize)
    
  }
  
  if(exactProduct){
    
    sampleSize <- numberClusters * clusterSize
    
  }
  
  
  sampleAllocation <-  data.frame( availableClusters = availableClusters, 
      numberClusters = numberClusters )
  
  designStatistics <- data.frame(numberClusters = numberClusters,
      sampleSize = ceiling(sampleSize), 
      desiredDifference = round(desiredDifference, 3),
      power = round(power, 3) )
  
  return( list(sampleAllocation = sampleAllocation,
          designStatistics = designStatistics) )
  
}



#' Calculate the design effect
#' @param clusterVariance numeric, estimated variance between the cluster means  
#' @param conditionalPrevalence numeric, conditional prevalence of the outcome
#' given a random effect of zero; default is NA
#' @param correlation numeric, the intra-class correlation; default is NA
#' if NA correlation is estimated based on approximation using clusterVariance
#' and clusterSize
#' @param clusterSize integer, (average) size of the clusters
#' @return numeric, the design effect
#' @export
getDesignEffect <- function(clusterVariance, conditionalPrevalence = NA, 
    correlation = NA, clusterSize) {
  
  if( is.na(correlation) ) {
    
    correlation <- clusterVariance / ( clusterVariance +  
          1 / (conditionalPrevalence * (1 - conditionalPrevalence)) )
    
  }
  
  designEffect <- 1 + correlation*(clusterSize - 1)
  
  return(designEffect)
  
}



#' Calculate design statistics and sample allocation for stratified sampling
#' @param purpose character string, the purpose of the study is one of 
#' "estimation" or "testing"
#' @param sampleSize integer, the total sample size; default is NA
#' @param desiredDifference numeric, for "estimation" the desired margin of error,
#' i.e. half width of the desired confidence interval or for "testing" the 
#' true difference in means that is tested; default is NA
#' @param power numeric, statistical power to detect the predefined difference 
#' "desiredDifference" when purpose is "testing"; default is NA
#' @param typeIerror numeric, the type I error
#' @param allocation method to allocate the total sample size over the strata;
#' one of "proportional" or "neyman"
#' @param stratumVariances numeric vector, estimated variance within each stratum
#' @param stratumProportions numeric vector, the expected proportion of the 
#' population in each stratum
#' @param populationSize numeric, the population size; default is NA
#' @param adjustFinitePopulation boolean, adjust for finite population?; 
#' default is FALSE
#' @param inflationFactor numeric, the inflation factor with which the uncorrected
#' sample size should be multiplied to account e.g. for missingness;
#' default is NA
#' @param exactSum boolean, if TRUE the total sampleSize is exactly the sum of
#' all stratumSizes; if FALSE the sampleSize may be smaller than the the sum of 
#' all stratumSizes; default is TRUE
#' @param roundEndResult boolean, if FALSE raw calculation numbers are given as 
#' output; default is TRUE
#' @return a list with two dataframes: one with sampleAllocation info 
#' (stratumVariances, stratumProportions, availableSamples, stratumSize); 
#' one with design statistics (sampleSize, desiredDifference, power)
#' @export
getStratified <- function(purpose = c("estimation", "testing"), sampleSize = NA,
    desiredDifference = NA, power = NA, typeIerror, 
    allocation = c("proportional", "neyman"), stratumVariances, stratumProportions,
    populationSize = NA, adjustFinitePopulation = FALSE, inflationFactor = NA, 
    exactSum = TRUE, roundEndResult = TRUE) {
  
  
  # TODO can we calculate power when stratumProportions are given but sample size not?
  
  allocation <- match.arg(allocation)
  
  if( length(stratumVariances) == 1 ) {
    
    stratumVariances <- rep(stratumVariances,
        length.out = length(stratumProportions))
    
  }
  
  if( length(stratumProportions) == 1) {
    
    stratumProportions <- rep(stratumProportions, 
        length.out = length(stratumVariances))
    
  }
  
  inputList <- list(sampleSize = sampleSize,
      desiredDifference = desiredDifference, power = power)
  isMissing <- getMissing(purpose = purpose, inputList = inputList)
  
  if( allocation == "proportional" ) {
    
    sampleVariance <- sum( stratumProportions*stratumVariances )
    
    calculatedMissing <- do.call( "getRawCalculation", 
        list(purpose = purpose, sampleSize = sampleSize, 
            desiredDifference = desiredDifference, power = power, 
            typeIerror = typeIerror, variance = sampleVariance,
            populationSize = populationSize, 
            adjustFinitePopulation = adjustFinitePopulation, 
            inflationFactor = inflationFactor))
    assign( (names(inputList)[isMissing])[1], calculatedMissing )
    
    stratumSize <- stratumProportions*sampleSize
    
  } else {
    
    denominator <- sum( stratumProportions*sqrt(stratumVariances) )
    sampleVariance <- denominator^2
    
    calculatedMissing <- do.call( "getRawCalculation", 
        list(purpose = purpose, sampleSize = sampleSize, 
            desiredDifference = desiredDifference, power = power, 
            typeIerror = typeIerror, variance = sampleVariance,
            populationSize = populationSize, 
            adjustFinitePopulation = adjustFinitePopulation,
            inflationFactor = inflationFactor))
    assign( (names(inputList)[isMissing])[1], calculatedMissing )
    
    stratumSize <- stratumProportions*sqrt(stratumVariances) /
        denominator*sampleSize
    
  }
  
  if(roundEndResult){
    
    stratumSize <- roundUp(stratumSize)
#  stratumSize <- ceiling(stratumSize)
    
    if(exactSum){
      
      sampleSize <- sum(stratumSize)
      
    } else {
      
      sampleSize <- ceiling(sampleSize)
      
    }
    
  }
  
  availableSamples <- floor(populationSize*stratumProportions)
  
  
  
  sampleAllocation <-  data.frame( stratumProportions = stratumProportions,
      stratumVariances = stratumVariances,
      availableSamples = availableSamples,
      stratumSize = stratumSize )
  
  designStatistics <- data.frame( sampleSize = sampleSize, 
      desiredDifference = round(desiredDifference, 3),
      power = round(power, 3) )
  
  return( list(sampleAllocation = sampleAllocation,
          designStatistics = designStatistics) )
  
}



#' Calculate design statistics and sample alllocation for 
#' clustered sampling design within strata
#' @param purpose character string, the purpose of the study is one of 
#' "estimation" or "testing"
#' @param sampleSize integer, the total sample size; default is NA
#' @param desiredDifference numeric, for "estimation" the desired margin of error,
#' i.e. half width of the desired confidence interval or for "testing" the 
#' true difference in means that is tested; default is NA
#' @param power numeric, statistical power to detect the predefined difference 
#' "desiredDifference" when purpose is "testing"; default is NA
#' @param typeIerror numeric, the type I error
#' @param allocation method to allocate the total sample size over the strata;
#' one of "proportional" or "neyman"
#' @param clusterSize integer, (average) size of the clusters
#' @param clusterVariances numeric vector, estimated variance between cluster means
#' within each stratum
#' @param stratumProportions numeric vector, the expected proportion of the 
#' population in each stratum
#' @param designEffect numeric vector, the design effect for each stratum,
#' i.e. the ratio of the stratum mean variances under multistage sampling and 
#' stratified sampling; default value is NA 
#' @param stratumVariances numeric vector, estimated variance within each stratum
#' @param populationSize numeric, the population size; default is NA
#' @param adjustFinitePopulation boolean, adjust for finite population?; 
#' default is FALSE
#' @param inflationFactor numeric, the inflation factor with which the uncorrected
#' sample size should be multiplied to account e.g. for missingness;
#' default is NA
#' @param exactProduct boolean, if TRUE the sampleSize is exactly the product of
#' numberClusters and clusterSize; if FALSE the sampleSize may be smaller than
#' the product of numberClusters and clusterSize; default is TRUE
#' @param exactSum boolean, if TRUE the total sampleSize is exactly the sum of
#' all stratumSizes; if FALSE the sampleSize may be smaller than the the sum of 
#' all stratumSizes; default is TRUE
#' @return a list with two dataframes: one with sampleAllocation info 
#' (stratumVariances, stratumProportions, availableClusters, numberClusters,
#' stratumSize); one with design statistics (numberClusters, sampleSize, 
#' desiredDifference, power)
#' @export
getTwoStage <- function(purpose = c("estimation", "testing"), sampleSize = NA,
    desiredDifference = NA, power = NA, typeIerror, allocation = "proportional",
    clusterSize = NA, clusterVariances, stratumProportions,
    designEffect = NA, stratumVariances,
    populationSize = NA, adjustFinitePopulation = FALSE, inflationFactor = NA, 
    exactProduct = TRUE, exactSum = TRUE) {
  
  
#  # My version 
#  if( all(is.na(designEffect)) ) {
#    
#    multiStageVariances <- clusterSize*clusterVariances
#    
#  } else {
#    
#    multiStageVariances <- designEffect*stratumVariances
#    
#  }
#  
#  stratifiedValue <- getStratified(purpose = purpose, sampleSize = sampleSize,
#      desiredDifference = desiredDifference, power = power, 
#      typeIerror = typeIerror, allocation = allocation, 
#      stratumVariances = multiStageVariances,
#      stratumProportions = stratumProportions,
#      populationSize = populationSize, inflationFactor = inflationFactor,
#      exactSum = FALSE)
# 
#
#  numberClusters <- ceiling(stratifiedValue$sampleAllocation[,"stratumSize"]/clusterSize)
  
  
  # Jose's version
  if(is.na(inflationFactor)){
    
    inflationFactor <- 1
    
  }
  
  if( all(is.na(designEffect)) ) {
    
    multiStageVariances <- clusterSize*clusterVariances
    
    stratifiedValue <- getStratified(purpose = purpose, sampleSize = sampleSize,
        desiredDifference = desiredDifference, power = power, 
        typeIerror = typeIerror, allocation = allocation, 
        stratumVariances = multiStageVariances,
        stratumProportions = stratumProportions,
        populationSize = populationSize, 
        adjustFinitePopulation = adjustFinitePopulation,
        inflationFactor = 1,
        roundEndResult = FALSE)
    
      sampleSize <- stratifiedValue$designStatistics[,"sampleSize"]*inflationFactor
    
  } else {
    
    stratifiedValue <- getStratified(purpose = purpose, sampleSize = sampleSize,
        desiredDifference = desiredDifference, power = power, 
        typeIerror = typeIerror, allocation = allocation, 
        stratumVariances = stratumVariances,
        stratumProportions = stratumProportions,
        populationSize = populationSize, 
        adjustFinitePopulation = adjustFinitePopulation,
        inflationFactor = 1,
        roundEndResult = FALSE)
      
    
    sampleSize <- stratifiedValue$designStatistics[,"sampleSize"]*
          designEffect*inflationFactor
    
  }
  
  if( allocation == "proportional" ) {
    
    stratumSize <- stratumProportions*sampleSize
    
  } else {
    
    denominator <- sum( stratumProportions*sqrt(stratumVariances) )
    
    stratumSize <- stratumProportions*sqrt(stratumVariances) /
        denominator*sampleSize
    
  }
 
  numberClusters <- roundUp(sampleSize*stratumProportions/clusterSize)
#  numberClusters <- ceiling(sampleSize*stratumProportions/clusterSize)
  
  if(adjustFinitePopulation){
    
    availableClusters <- floor( populationSize * stratumProportions / clusterSize )
    
  } else {
    
    availableClusters <- floor( populationSize * stratumProportions )
    
  }
  
  if(exactProduct){
    
    stratumSize <- numberClusters*clusterSize
    
  }
  
  if(exactSum){
    
    sampleSize <- sum(stratumSize)
    
  }
  
  if( all(is.na(designEffect)) ) {
    
    sampleAllocation <- data.frame(stratumProportions = stratumProportions, 
        clusterVariances = clusterVariances, 
        availableClusters = availableClusters, 
        numberClusters = numberClusters,
        stratumSize = stratumSize)
    
  } else {
    
    sampleAllocation <- data.frame(stratumProportions = stratumProportions,
        stratumVariances = stratumVariances, 
        availableClusters = availableClusters, 
        numberClusters = numberClusters,
        stratumSize = stratumSize)
    
  }
  
  designStatistics <- data.frame(numberClusters = sum(numberClusters),
      sampleSize = sampleSize, 
      desiredDifference = stratifiedValue$designStatistics[,"desiredDifference"],
      power = stratifiedValue$designStatistics[,"power"] )
  
  
  return( list(sampleAllocation = sampleAllocation,
          designStatistics = designStatistics) )
  
}



#' Calculate design statistics for three stage sampling
#' @param purpose character string, the purpose of the study is one of 
#' "estimation" or "testing"
#' @param sampleSize integer, the total sample size; default is NA
#' @param desiredDifference numeric, for "estimation" the desired margin of error,
#' i.e. half width of the desired confidence interval or for "testing" the 
#' true difference in means that is tested; default is NA
#' @param power numeric, statistical power to detect the predefined difference 
#' "desiredDifference" when purpose is "testing"; default is NA
#' @param typeIerror numeric, the type I error
#' @param marginalPrevalence0 numeric, marginal prevalence under the 
#' null hypothesis; default is NA
#' @param marginalPrevalenceA numeric, marginal prevalence under the
#' alternative hypothesis; default is NA  
#' @param conditionalPrevalence0 numeric, conditional prevalence under the 
#' null hypothesis; default is NA
#' @param conditionalPrevalenceA numeric, conditional prevalence under the 
#' alternative hypothesis; default is NA
#' @param sizeLevel3 integer, the number of level 3 units (highest level); default is NA
#' @param sizeLevel2 integer, the number of level 2 units (middle level); default is NA
#' @param sizeLevel1 integer, the number of level 1 units (lowest level); default is NA
#' @param varianceLevel3 numeric, the variance between level 3 units (highest level)
#' @param varianceLevel2 numeric, the variance between level 2 units (middle level)
#' @param varianceLevel1 numeric, the variance between level 1 units (lowest level);
#' default is NA, if missing a value for conditionalPrevalenceA should be provided
#' @param populationSize numeric, the population size; default is NA
#' @param adjustFinitePopulation boolean, adjust for finite population?; 
#' default is FALSE
#' @param inflationFactor numeric, the inflation factor with which the uncorrected
#' sample size should be multiplied to account e.g. for missingness;
#' default is NA
#' @return a dataframe with sampleSize, desiredDifference, power,
#' sizeLevel3, sizeLevel2 and sizeLevel1
#' @export
getThreeStage <- function(purpose = c("estimation", "testing"), 
    sampleSize = NA, desiredDifference = NA, power = NA, typeIerror, 
    marginalPrevalence0 = NA, marginalPrevalenceA = NA,
    conditionalPrevalence0, conditionalPrevalenceA = NA,
    sizeLevel3 = NA, sizeLevel2 = NA, sizeLevel1 = NA,
    varianceLevel3, varianceLevel2, varianceLevel1 = NA,
    populationSize = NA, adjustFinitePopulation = FALSE, inflationFactor = NA){
  
  if(is.na(desiredDifference) | is.na(varianceLevel1)){
    
    desiredResult <- getDesiredDifference(varianceLevel3 = varianceLevel3,
        varianceLevel2 = varianceLevel2, 
        marginalPrevalence0 = marginalPrevalence0,
        marginalPrevalenceA = marginalPrevalenceA,
        conditionalPrevalence0 = conditionalPrevalence0,
        conditionalPrevalenceA = conditionalPrevalenceA)
    
    if(!is.na(desiredDifference) &
        round(desiredDifference, 3) != round(desiredResult$desiredDifference, 3)){
      
      stop(paste0("Conflict between input values for desiredDifference (", 
              round(desiredDifference, 2), ") versus input values for 
                  marginalPrevalence0 and marginalPrevalenceA or 
                  conditionalPrevalence0 and conditionalPrevalenceA,
                  (", round(desiredResult$desiredDifference, 2),"). 
                  \n Please provide correct input values"))
      
    } else {
      
      desiredDifference <- desiredResult$desiredDifference
      
    }
    
    if(is.na(varianceLevel1)) {
      
      if(!is.na(conditionalPrevalenceA) &
          round(conditionalPrevalenceA, 2) != 
          round(exp(desiredResult$randomEffectA)/( 1 + exp(desiredResult$randomEffectA)), 2)){
        
        stop("Conflict between input values for desiredDifference versus input values for 
                marginalPrevalence0 and marginalPrevalenceA or conditionalPrevalence0 and conditionalPrevalenceA.
                \n Please provide correct input values")
        
      } else {
        
        conditionalPrevalenceA <- exp(desiredResult$randomEffectA)/( 1 + exp(desiredResult$randomEffectA))
        
      }
      
      varianceLevel1 <- 1/(conditionalPrevalenceA * (1 - conditionalPrevalenceA))
      
    }
    
  }
  
  
  inputList <- list(sampleSize = sampleSize, desiredDifference = desiredDifference, 
      power = power)
  isMissing <- getMissing(purpose = purpose, inputList = inputList)
  
  if(is.na(sampleSize)){
    
    inputListSize <- list(sizeLevel3 = sizeLevel3, sizeLevel2 = sizeLevel2,
        sizeLevel1 = sizeLevel1)
    sizeMissing <- getMissing(purpose = "testing", inputList = inputListSize)
    
  } else if( sampleSize != sizeLevel3 * sizeLevel2 * sizeLevel1) {
    
    stop( paste0("sizeLevel3 * sizeLevel2 * sizeLevel1 (", 
            sizeLevel3 * sizeLevel2 * sizeLevel1, ") does not equal sample size (", 
            sampleSize, "). \n Please provide correct input values") )
    
  }
  
  
  if(!is.na(sampleSize)){
    
#    for(i in 1:3){
#      
#      iName <- paste0("sizeLevel", i)
#      assign(iName, do.call("adjustSampleSize",
#              list(sampleSize = eval(parse(text = iName)), populationSize = populationSize, 
#        adjustFinitePopulation = adjustFinitePopulation, 
#        inflationFactor = inflationFactor)))
#      
#    }
    
    variance <- varianceLevel3/sizeLevel3 + varianceLevel2/(sizeLevel3 *sizeLevel2) + 
        varianceLevel1/(sizeLevel3 * sizeLevel2 * sizeLevel1) 
    newSampleSize <- 1
    
    calculatedMissing <- do.call( "getRawCalculation", 
        list(purpose = purpose, sampleSize = newSampleSize, 
            desiredDifference = desiredDifference, power = power, 
            typeIerror = typeIerror, variance = variance,
            populationSize = populationSize, 
            adjustFinitePopulation = adjustFinitePopulation, 
            inflationFactor = inflationFactor))
    
    assign( (names(inputList)[isMissing])[1], calculatedMissing )
    
  } else {
    
    if(is.na(sizeLevel2)){
      
      numerator <- varianceLevel2 + varianceLevel1/sizeLevel1
      denominator <- sizeLevel3 * desiredDifference^2 - 
          (qnorm(1 - typeIerror/2) + qnorm(power))^2 * varianceLevel3
      variance <- numerator / denominator
      newDesiredDifference <- 1
      
    } else if(is.na(sizeLevel1)){
      
      variance <- varianceLevel1 /( sizeLevel3 * sizeLevel2 * desiredDifference^2 -
            (qnorm(1 - typeIerror/2) + qnorm(power))^2 * 
            (sizeLevel2 * varianceLevel3 + varianceLevel2))
      newDesiredDifference <- 1
      
    } else if(is.na(sizeLevel3)){
      
      variance <- varianceLevel3 + varianceLevel2/sizeLevel2 + 
          varianceLevel1/(sizeLevel2 * sizeLevel1) 
      newDesiredDifference <- desiredDifference
      
    }
    
    calculatedMissing <- do.call( "getRawCalculation", 
        list(purpose = purpose, sampleSize = NA, 
            desiredDifference = newDesiredDifference, power = power, 
            typeIerror = typeIerror, variance = variance,
            populationSize = populationSize,
            adjustFinitePopulation = adjustFinitePopulation, 
            inflationFactor = inflationFactor))
    
    assign( (names(inputListSize)[sizeMissing])[1], calculatedMissing )
    
    sampleSize <- sizeLevel3 * sizeLevel2 * sizeLevel1
    
  }
  
  
  returnData <- data.frame(sampleSize = ceiling(sampleSize),      
      desiredDifference = round(desiredDifference, 3),
      power = round(power, 3), sizeLevel3 = ceiling(sizeLevel3), 
      sizeLevel2 = ceiling(sizeLevel2), sizeLevel1 = ceiling(sizeLevel1))
  
  
  return( returnData )  
  
}


#' Calculate the desired difference for three stage sampling design
#' @param varianceLevel3 numeric, the variance between level 3 units (highest level)
#' @param varianceLevel2 numeric, the variance between level 2 units (middle level)
#' @param marginalPrevalence0 numeric, marginal prevalence under the 
#' null hypothesis; default is NA
#' @param marginalPrevalenceA numeric, marginal prevalence under the 
#' alternative hypothesis; default is NA
#' @param conditionalPrevalence0 numeric, conditional prevalence under the 
#' null hypothesis; default is NA
#' @param conditionalPrevalenceA numeric, conditional prevalence under the 
#' alternative hypothesis; default is NA
#' @return dataframe with desired difference, randomEffect0 i.e. conditional 
#' prevalence on the logodds scale under the null hypothesis, randomEffectA i.e.
#' conditional prevalence on the logodds scale under the alternative hypothesis
#' @export
getDesiredDifference <- function(varianceLevel3, varianceLevel2, 
    marginalPrevalence0 = NA, marginalPrevalenceA = NA,
    conditionalPrevalence0, conditionalPrevalenceA = NA){
  
  if(!is.na(marginalPrevalence0) & !is.na(marginalPrevalenceA)){
    
    constant <- sqrt(1 + 3*(varianceLevel3 + varianceLevel2)/pi^2)
    randomEffect0 <- log(marginalPrevalence0/(1 - marginalPrevalence0))*constant
    randomEffectA <- log(marginalPrevalenceA/(1 - marginalPrevalenceA))*constant
    
    if(!is.na(conditionalPrevalenceA) & 
        round(conditionalPrevalenceA, 2) != exp(randomEffectA)/( 1 + exp(randomEffectA))){
      
      stop("Conflict between input values for conditionalPrevalenceA and marginalPrevalenceA. 
              \n Please provide correct input values")
      
    }
    
  } else {
    
    randomEffect0 <- log(conditionalPrevalence0/(1 - conditionalPrevalence0))
    randomEffectA <- log(conditionalPrevalenceA/(1 - conditionalPrevalenceA))
    
  }
  
  desiredDifference <- randomEffectA - randomEffect0
  
  return(data.frame(desiredDifference = desiredDifference,
          randomEffect0 = randomEffect0, randomEffectA = randomEffectA))
  
}


#' Construct variance-covariance matrix
#' @param varianceCorrelation numeric matrix with variance at the diagonal and
#' pairwise correlation values at non-diagonal 
#' @param varianceSingle a single value for the variance; default is NA
#' @param correlationSingle a single value for the correlation; default is NA
#' @param dimension integer, determines the dimension (nrows = ncols) of the
#' output matrix if varianceSingle and correlationSingle are provided
#' @return variance-covariance matrix
#' @export
getVarianceCovariance <- function(varianceCorrelation, 
    varianceSingle = NA, correlationSingle= NA, dimension = NA) {
  
  if( !is.na(varianceSingle) & !is.na(correlationSingle) ) {
    
    varianceCovariance <- matrix(correlationSingle*varianceSingle, 
        nrow = dimension, ncol = dimension)
    diag(varianceCovariance) <- rep(varianceSingle, times = dimension)
    
  } else {
    
    variance <- diag(as.matrix(varianceCorrelation))
    varianceCovariance <- matrix(NA, nrow = nrow(varianceCorrelation),
        ncol = ncol(varianceCorrelation))
    
    for( iRow in 1:nrow(varianceCovariance) ) {
      
      for( jColumn in 1:ncol(varianceCovariance) ) {
        
        if(iRow == jColumn){
          
          varianceCovariance[iRow, jColumn] <- variance[iRow]
          
        } else {
          
          varianceCovariance[iRow, jColumn] <- varianceCorrelation[iRow, jColumn] *
              sqrt(variance[iRow] * variance[jColumn])
          
        }
        
      }
      
    }
    
  }
  
  return(varianceCovariance = varianceCovariance)
  
}



#' Calculate sample size for measuring change over time design
#' @param purpose character string, the purpose of the study is one of 
#' "estimation" or "testing"
#' @param sampleSize integer, the total sample size; default is NA
#' @param desiredDifference numeric, for "estimation" the desired margin of error,
#' i.e. half width of the desired confidence interval or for "testing" the 
#' true difference in means that is tested; default is NA
#' @param power numeric, statistical power to detect the predefined difference 
#' "desiredDifference" when purpose is "testing"; default is NA
#' @param typeIerror numeric, the type I error
#' @param varianceCovariance variance-covariance matrix for the sample means 
#' over time
#' @param populationSize numeric, the population size; default is NA
#' @param adjustFinitePopulation boolean, adjust for finite population?; 
#' default is FALSE
#' @param inflationFactor numeric, the inflation factor with which the uncorrected
#' sample size should be multiplied to account e.g. for missingness;
#' default is NA
#' @return a dataframe with sampleSize, desiredDifference and power
#' @export
getChangeOverTime <- function(purpose = c("estimation", "testing"), 
    sampleSize = NA, desiredDifference = NA, power = NA, 
    typeIerror, varianceCovariance, populationSize = NA, 
    adjustFinitePopulation = FALSE, inflationFactor = NA) {
  
  varianceDifference <- 2*sum(diag(varianceCovariance)) - sum(varianceCovariance) 
  
  returnValue <- getSimpleRandom(purpose = purpose, sampleSize = sampleSize, 
      desiredDifference = desiredDifference, power = power, 
      typeIerror = typeIerror, sampleVariance = varianceDifference,
      populationSize = populationSize, 
      adjustFinitePopulation = adjustFinitePopulation,
      inflationFactor = inflationFactor)
  
  return( returnValue )
  
}



#' Wrapper function around one of the functions in this package 
#' to account for uncertainty on some input parameters
#' @param wrappedFunction function that needs to be wrapped
#' @param randomInput list of input parameters that are random; default is NULL
#' @param fixedInput list of input parameters that are non-random;
#' default is NULL
#' @return a list with output elements as in the original wrappedFunction
#' @export
wrapFunctionMilanzi <- function(wrappedFunction = c("getSimpleRandom", 
        "getDesignEffect", "getClustered", "getStratified", "getTwoStage",
        "getThreeStage", "getChangeOverTime"), 
    randomInput = NULL, fixedInput = NULL) {
  
  
  if( is.null(randomInput) ){
    
    nRuns <- 1
    
  } else {
    
    nRuns <- max( sapply(randomInput, length) )
    
  }
  
  result <- list()
  
  for(iRun in 1:nRuns) {
    
    iRandomInput <- lapply(randomInput, function(x) x[iRun])
    result[[iRun]] <- do.call(wrappedFunction, c(iRandomInput, fixedInput))
    
  }
  
  return(result)    
  
}