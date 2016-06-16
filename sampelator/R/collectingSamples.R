#' Select a random sample from the data
#' @param data data frame from which the sample should be taken
#' @param seed integer, to assure reproducibility of the sampling
#' @param size integer, the size of the random sample
#' @param weights numeric vector, used for weighted sampling; default is NA  
#' @return data frame, random subsample of data
#' @export
selectSample <- function(data, seed, size, weights = NA){
  
  set.seed(seed)
  
  if(is.na(weights)){
    
    centersSampled <- sort(sample(1:dim(data)[1], size = size))
    
  } else{
    
    
    
  }  
  
  return(data[centersSampled,])
  
}

#' Simulate the random sampling of data
#' @param data data frame from which the sample should be taken
#' @param seed integer, to assure reproducibility of the sampling
#' @param size integer, the size of the random sample
#' @param nSimulations the number of random samples that has to be drawn
#' @return a list of data frames, each a random subsample of data
#' @export
simulateSampling <- function(data, seed, size, nSimulations){
  
  result <- list()
  
  for (i in 1:nSimulations){
    
    result[[i]] <- selectSample(data = data, seed = (seed + i), size = size)
    
  }
  
  return(result)
  
}


#' Grid of histogram plots comparing simulated and selected samples  
#' @param selectedSample data frame, as given by selectSample()
#' @param simulatedSamples list of data frames, as given by simulateSampling()
#' @param varName character, the variable name for which histograms have to be 
#' plotted
#' @return ggplot figure 
#' @import ggplot2
#' @export
summarizeSampling <- function(selectedSample, simulatedSamples, varName){
  
  size <- dim(selectedSample)[1]
  simulation <- rep(1:length(simulatedSamples), each = size)
  bindedSimulations <- cbind(do.call(rbind, simulatedSamples), 
      simulation = simulation)
  
  proportionsSimulated <- round(100*table(bindedSimulations$simulation,
          bindedSimulations[,varName])/size,2)
  dataSimulated <- data.frame(proportionsSimulated)
  names(dataSimulated) <- c("simulation", "variable", "frequency")
  
  proportionsSelected <- round(100*table(selectedSample[,varName])/size,2)
  levelNames <- unique(dataSimulated$variable)
  dataSelected <- data.frame(variable = levelNames)
  dataSelected$frequency <- 0
  dataSelected$frequency[which(dataSelected$variable %in% names(proportionsSelected))] <- 
      proportionsSelected
  
  ggplot(dataSimulated, aes(x = frequency)) + 
      geom_histogram(binwidth=.2, colour="black", fill="white") + 
      facet_wrap(~variable, ncol = 4) + 
      geom_vline(data = dataSelected, aes(xintercept = frequency),
          linetype = "dashed", size = 1, colour = "red") +
      xlab("Observed frequency in the simulated sample (%)") +
      ylab("Percentage (%)")
  
  
}


#' Collapse data according to the user-defined variable
#' @param inputData data frame, the original data
#' @param collapseVariable character, variable name in inputData with respect
#'  to which the data should be collapsed
#' @return data frame, cbind (1) the unique rows of the inputData excluding 
#' collapseVariable, (2) the number of registrations per level of collapseVariable
#'  (instead of the column for collapseVariable) and (3) the total number of 
#' registrations
#' @export
collapseData <- function(inputData, collapseVariable){
  
  inputData[,collapseVariable] <- as.factor(inputData[,collapseVariable])
  
  centerNames <- unique(inputData[,1])
  uniqueInputData <- unique(inputData[, !names(inputData) %in% collapseVariable])
  outputData <- c()
  
  for(i in seq_along(centerNames)){
    
    idRows <- which(inputData[,1] == centerNames[i])
    collapseTable <- t(data.frame(table(inputData[idRows,collapseVariable]))$Freq)
    colnames(collapseTable) <- levels(inputData[,collapseVariable])
    
    if(is.null(dim(uniqueInputData))){
      
      originalData <- uniqueInputData[i]
      
    } else {
      
      originalData <- uniqueInputData[i,]
      
    }
    
    outputData <- rbind(outputData,
        cbind(originalData, collapseTable, "NumberOfRegistrations" = length(idRows)))
    
  }
  
  rownames(outputData) <- NULL
  
  return(outputData)
  
}
