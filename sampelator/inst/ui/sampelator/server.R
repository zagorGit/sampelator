#http://walkerke.github.io/2014/03/tfr-in-europe/
#install.packages("base64enc")
#devtools::install_github('rCharts', 'ramnathv', ref='dev')
library(rCharts) #for interactive plots
options(RCHART_WIDTH = 800)

#library(shinyIncubator) #for matrixInput
library(sampelator)
library(shinysky) #for hotable
library(ggplot2)


`%then%` <- shiny:::`%OR%`

Sys.setlocale("LC_CTYPE", "C")



serverEstimatingGeneralParameters <- function(input, output, session){
  
  ## To debug
  output$print2 <- renderPrint({
        
        results$plotDesignStatistics()
        
      })
  
  
  output$designParameters <- renderUI({
        
        switch(input$samplingDesign,
            
            "getSimpleRandom" = {
              
              numericInput( inputId = "sampleVariance", 
                  label = "Sample variance", value = NA)
              
            },
            
            "getClustered" = {
              
              
              list(
                  numericInput( inputId = "clusterSize", 
                      label = "(Average) number sampled in each cluster", value = NA),
                  
                  selectInput( inputId = "optionVariance", label = "",
                      choices = c("Sample variance" = "sampleVariance",
                          "Variability of cluster means from the overall mean" = "clusterVariance")),
                  
                  conditionalPanel("input.optionVariance == 'clusterVariance'",
                      
#                      tabsetPanel(
#                          tabPanel("Option 1",
                      list(
                          numericInput( inputId = "clusterVariance", 
                              label = "Variability of cluster means from the overall mean", value = NA),
                          uiOutput("formulaClusterVariance")
                      )
#                          ), tabPanel("Option 2",
#                              list(
#                                  numericInput("conditionalPrevalence", "Conditional prevalence of the outcome", NA),
#                                  numericInput( inputId = "clusterVariance",
#                                      label = "Variance between cluster means on log-odds scale", value = NA)    
#                              )
#                          )
#                      )
                  ),
                  
                  conditionalPanel("input.optionVariance == 'sampleVariance'",
                      
                      list(
                          
                          numericInput( inputId = "sampleVarianceClustered", 
                              label = "Sample variance", value = NA),
                          
                          tabsetPanel( id = "clusterPanel",
                              
                              tabPanel("Option 1",
                                  numericInput( inputId = "designEffect", 
                                      label = "Design effect", value = NA)
                              ),
                              
                              tabPanel("Option 2",
                                  numericInput("correlation", "Within-cluster correlation", NA)
                              
                              )
                          )
                      )
                  )                 
              )
              
            },
            
            "getStratified" = {
              
              list(
                  selectInput( inputId = "allocation",
                      label = "Allocation of sample size over strata", 
                      choices = c("Proportional to population stratum size" = "proportional",
                          "Proportional to stratum variance (Neyman allocation)" = "neyman")),
                  
                  tabsetPanel( id = "loadedStratified",
                      
                      tabPanel("Enter data",
                          
                          sliderInput("nStrata", "Number of strata", min = 2, max = 40, value = 2),
                          
                          p(strong("Stratum proportion:"), "expected population proportion in each stratum"),
                          p(strong("Stratum variance:"), "variance within each stratum"),
                          
                          hotable("stratifiedInfo"),
                          tags$br(),
                          shiny::actionButton("fillMatrix", "Fill matrix")
                      
                      ),
                      
                      tabPanel("Load data",
                          
                          fileInput('dataStratified', 'Load data',
                              accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
                          
                          radioButtons('sepStratified', 'Separator for loaded data',
                              c("Comma (.csv file)"=',',
                                  "Semicolon (.csv file)"=';',
                                  "Tab (.txt file)"='\t'),
                              ','),
                          
                          checkboxInput("booleanStratumName", "First column with stratum names", value  = TRUE),
                          
                          tags$hr(),
                          
                          p(strong("Stratum proportion:"), "expected population proportion in each stratum"),
                          p(strong("Stratum variance:"), "variance within each stratum"),
                          
                          tableOutput("dataStratified")
                      
                      )
                  
                  )
              
              )
              
            },
            
            "getTwoStage" = {
              list(
                  
                  numericInput( inputId = "clusterSize", 
                      label = "(Average) number sampled in each cluster", value = NA),
                  
                  selectInput("twoStagePanel", label = "", 
                      choices = list("Stratum proportion and Cluster variance" = "option13",
                          "Stratum proportion and Stratum variance" = "option12")),
                  
                  conditionalPanel("input.twoStagePanel == 'option13'",
                      
                      list(
                          p(strong("Stratum proportion:"), "expected population proportion in each stratum"),
                          p(strong("Cluster variance:"), "variance between cluster means within each stratum")
                      )
                  ),
                  
                  conditionalPanel("input.twoStagePanel == 'option12'",
                      
                      list(
                          p(strong("Stratum proportion:"), "expected population proportion in each stratum"),
                          p(strong("Stratum variance:"), "variance within each stratum"),
                          
                          numericInput( inputId = "designEffectMS", 
                              label = "Design effect", value = NA)
                      
                      )
                  ),
                  
                  tabsetPanel( id = "loadedTwoStage",
                      
                      tabPanel("Enter data",
                          
                          sliderInput("nStrata2", "Number of strata", min = 2, max = 40, value = 2),
                          
                          conditionalPanel("input.twoStagePanel == 'option13'",
                              
                              list(
                                  hotable("twoStage13Info"),
                                  tags$br(),
                                  shiny::actionButton("fillMatrix2", "Fill matrix")
                              )
                          ),
                          
                          conditionalPanel("input.twoStagePanel == 'option12'",
                              
                              list(
                                  
                                  hotable("twoStage12Info"),
                                  tags$br(),
                                  shiny::actionButton("fillMatrix3", "Fill matrix")
                              
                              )
                          )
                      
                      ),
                      
                      tabPanel("Load data",
                          
                          fileInput('dataTwoStage', 'Load data',
                              accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
                          
                          radioButtons('sepTwoStage', 'Separator for loaded data',
                              c("Comma (.csv file)"=',',
                                  "Semicolon (.csv file)"=';',
                                  "Tab (.txt file)"='\t'),
                              ','),
                          
                          checkboxInput("booleanStratumName2", "First column with stratum names", value  = TRUE),
                          
                          tags$hr(),
                          
                          tableOutput("dataTwoStage")
                      
                      )
                  
                  )
              
              )
              
            },
            
            "getThreeStage" = {
              
              list(
                  
                  list(
                      
                      fluidRow(
                          column(6, numericInput(inputId = "sizeLevel3", "Number of level III items sampled", value = NA)),
                          column(6, numericInput(inputId = "varianceLevel3", "Variance between units of level III", value = NA))
                      ),
                      fluidRow(
                          column(6, numericInput(inputId = "sizeLevel2", "(Average) number sampled at level II", value = NA)),
                          column(6, numericInput(inputId = "varianceLevel2", "Variance between units of level II", value = NA))
                      ),
                      
                      numericInput(inputId = "sizeLevel1", "(Average) number sampled at level I", value = NA,
                          width = "48%")
                  
                  ),
                  
                  uiOutput("threeStagePanel"),
                  shiny::actionButton("update", "Update 'General parameters'")
              
              )
              
            },
            
            "getChangeOverTime" = {
              
              list(
#                  sliderInput( inputId = "dimension", 
#                      label = "Number of time points", value = 2,
#                      min = 2, max = 10),
#                  
                  p(strong("Diagonal values:"), "variance at a given time point"),
                  p(strong("Non-diagonal values:"), "correlation between two time points"),
                  
                  hotable("correlationInfo"),
                  tags$br(),
                  shiny::actionButton(inputId = "fillMatrixCorrelation", label = "Fill matrix")
              
              )
              
            }
        )
        
      })
  
  
  output$formulaClusterVariance <- renderUI({
        
        withMathJax(helpText("Given by $$ s^2_c = \\sum_{t=1}^a \\frac{(\\bar{y_t}-\\bar{y})^2}{a-1},$$
                    where \\(a\\) is the number of clusters, \\(\\bar{y_t}\\) is the mean for cluster \\(t\\) and \\(\\bar{y}\\) is the overall mean."))
        
      })
  
  
  observeEvent(input$update, {
        
        if(!all(is.null(c(input$marginalPrevalence0, input$conditionalPrevalence0)))) {
          
          desiredResult <- getDesiredDifference(varianceLevel3 = input$varianceLevel3,
              varianceLevel2 = input$varianceLevel2,
              marginalPrevalence0 = input$marginalPrevalence0,
              marginalPrevalenceA = input$marginalPrevalenceA,
              conditionalPrevalence0 = input$conditionalPrevalence0,
              conditionalPrevalenceA = input$conditionalPrevalenceA)
          
          if(!is.na(desiredResult$desiredDifference)){
            
            observe(
                updateNumericInput(session, "desiredDifference", 
                    value = round(desiredResult$desiredDifference, 3))
            )     
          }
          
        }
        
        if(!is.null(input$sizeLevel1)){
          
          allSizes <- c(input$sizeLevel1, input$sizeLevel2, input$sizeLevel3)
          missingSize <- sapply(as.list(allSizes), is.na)
          
          if(is.na(input$sampleSize) & all(!missingSize)) {
            
            observe(
                updateNumericInput(session, "sampleSize", value = prod(allSizes))
            )
            
          }
          
        }
        
        
      })
  
  
  output$threeStagePanel <- renderUI({
        
        if( (input$range != 'sampleSize' && is.na(input$sampleSize)) ||
            (input$range == 'sampleSize' && is.na(input$minsampleSize) && is.na(input$maxsampleSize)) ){
          
          tabsetPanel(
              
              tabPanel("Marginal level",
                  
                  list(
                      
                      p(em("i.e. population level")),
                      
                      numericInput(inputId = "marginalPrevalence0", "Marginal prevalence under the null hypothesis", value = NA),
                      numericInput(inputId = "marginalPrevalenceA", "Marginal prevalence under the alternative hypothesis", value = NA)
                  
                  )
              
              ),
              
              tabPanel("Conditional level",
                  
                  list(
                      
                      numericInput(inputId = "conditionalPrevalence0", "Conditional prevalence under the null hypothesis", value = NA),
                      numericInput(inputId = "conditionalPrevalenceA", "Conditional prevalence under the alternative hypothesis", value = NA)
                  
                  )
              )
          )
          
          
        } else {
          
          numericInput(inputId = "varianceLevel1", "Variance between units of level I", value = NA,
              width = "48%")
          
        }
        
        
      })
  
  
  results <- reactiveValues(correlationUpdated = data.frame(matrix("", nrow = 2, 
              ncol = 2), stringsAsFactors = FALSE))
  
  
  results$dataStratified <- reactive({
        
        inFile <- input$dataStratified
        
        if (is.null(inFile))
          return(NULL)
        
        dataStratified <- read.table(inFile$datapath, header = TRUE, 
            sep = input$sepStratified, quote = '"')
        
        if(!input$booleanStratumName){
          
          dataStratified <- cbind(letters[1:nrow(dataStratified)], dataStratified)
          
        }
        
        colnames(dataStratified) <- c("Stratum name", "Stratum proportion", "Stratum variance")
        
        dataStratified
        
      })
  
  
  output$dataStratified <- renderTable({
        
        validate(need(results$dataStratified(), "No data loaded"))
        results$dataStratified()
        
      }, include.rownames=FALSE)
  
  
  results$dataTwoStage <- reactive({
        
        inFile <- input$dataTwoStage
        
        if (is.null(inFile))
          return(NULL)
        
        dataTwoStage <- read.table(inFile$datapath, header = TRUE, 
            sep = input$sepTwoStage, quote = '"')
        
        if(!input$booleanStratumName2){
          
          dataTwoStage <- cbind(letters[1:nrow(dataTwoStage)], dataTwoStage)
          
        }
        
        if(input$twoStagePanel == 'option13'){
          
          colnames(dataTwoStage) <- c("Stratum name", "Stratum proportion", "Cluster variance")
          
        } else {
          
          colnames(dataTwoStage) <- c("Stratum name", "Stratum proportion", "Stratum variance")
          
        }
        
        dataTwoStage
        
      })
  
  
  output$dataTwoStage <- renderTable({
        
        validate(need(results$dataTwoStage(), "No data loaded"))
        results$dataTwoStage()
        
      }, include.rownames=FALSE)
  
  
  
# Matrices and their updates
  
  makeHotable <- function(name, nStrata){
    
    if(!is.null(nStrata)){
      
      initDataFrame <- data.frame(stratumName = letters[1:nStrata],
          stratumProportion = rep("", nStrata), 
          variance = rep("", nStrata), stringsAsFactors = FALSE)
      initDataFrame[,-1] <- as.data.frame(sapply(initDataFrame[,-1], as.numeric))
      
      colnames(initDataFrame) <- switch(name,
          "stratified" = c("Stratum name", "Stratum proportion", "Stratum variance"),
          "twoStage13" = c("Stratum name", "Stratum proportion", "Cluster variance"),
          "twoStage12" = c("Stratum name", "Stratum proportion", "Stratum variance")) 
      
      if(!is.null(input[[paste0(name, "Info")]])){
        
        if(nStrata >= nrow(hot.to.df(input[[paste0(name, "Info")]]))){
          
          initDataFrame[1:nrow(hot.to.df(input[[paste0(name, "Info")]])),] <- 
              hot.to.df(input[[paste0(name, "Info")]])
          
        } else {
          
          initDataFrame <- hot.to.df(input[[paste0(name, "Info")]])[1:nStrata,]
          
        }
      }
      
      results[[paste0(name,"Updated")]] <- initDataFrame
      
    }
    
    
  }
  
  
  updateData <- function(name){
    
    initData <- results[[paste0(name,"Updated")]]
    initData[,-1] <- as.data.frame(sapply(initData[,-1], as.numeric))
    
    missingProportions <- which(is.na(initData[,2]))
    currentSum <- sum(as.numeric(initData[,2]), na.rm = TRUE)
    
    if(length(missingProportions) > 0) {
      
      initData[missingProportions,2] <- 
          round((1 - currentSum) / length(missingProportions), 3)
      
    }
    
    missingVariances <- which(is.na(initData[,3]))
    if(length(missingVariances) > 0) {
      
      initData[missingVariances,3] <- 
          as.numeric(initData[1,3])
      
    }
    
    results[[paste0(name,"Updated")]] <- initData
  }
  
  
  # stratified
  observe({
        
        makeHotable(name = "stratified", nStrata = input$nStrata)
        
      })
  
  observeEvent(input$fillMatrix,{
        
        updateData(name = "stratified")
        
      })
  
  output$stratifiedInfo <- renderHotable({
        
        results$stratifiedUpdated
        
      }, readOnly = FALSE)
  
  
  # twoStage13  
  observe({
        
        makeHotable(name = "twoStage13", nStrata = input$nStrata2)
        
      })
  
  observeEvent(input$fillMatrix2,{
        
        updateData(name = "twoStage13")
        
      })
  
  output$twoStage13Info <- renderHotable({
        
        results$twoStage13Updated
        
      }, readOnly = FALSE)
  
  
  # twoStage12  
  observe({
        
        makeHotable(name = "twoStage12", nStrata = input$nStrata2)
        
      })
  
  observeEvent(input$fillMatrix3,{
        
        updateData(name = "twoStage12")
        
      })
  
  output$twoStage12Info <- renderHotable({
        
        results$twoStage12Updated
        
      }, readOnly = FALSE) 
  
  
  
  
  # correlation
  observeEvent(input$fillMatrixCorrelation, {
        
        currentData <- hot.to.df(input$correlationInfo)
        
        nDimension <- 2
        
        variance <- currentData[1,1]
        correlation <- currentData[1,2]
        
        currentData[1:nDimension, 1:nDimension] <- correlation
        diag(currentData) <- rep(variance, nDimension)
        
        results$correlationUpdated <- currentData
        
      })
  
  
  
  output$correlationInfo <- renderHotable({
        
        colnames(results$correlationUpdated) <- paste("Time point", 1:2)
        rownames(results$correlationUpdated) <- paste("Time point", 1:2)
        results$correlationUpdated
        
      }, readOnly = FALSE)
  
  
  
  results$designEffect <- reactive({
        
        value <- NA
        
        if( !is.null(input$samplingDesign)){
          
          if(input$samplingDesign == "getClustered" & !is.null(input$clusterPanel)){
            
            if(input$clusterPanel == "Option 1"){
              
              value <- input$designEffect
              
            }
            
          } else if(input$samplingDesign == "getTwoStage" & !is.null(input$twoStagePanel)){
            
            if(input$twoStagePanel == "option12"){
              
              value <- input$designEffectMS
              
            } 
            
          }
          
        }
        
        value
        
      })
  
  results$clusterVariance <- reactive({
        
        if( !is.null(input$samplingDesign)){
          
          if(input$samplingDesign == "getClustered"){
            if(input$optionVariance == "clusterVariance"){
              
              input$clusterVariance
              
            } else {
              
              NA
              
            }
          }
        }
      })
  
  
  results$inflationFactor <- reactive({
        
        if(is.null(input$inflation))
          return( NA )
        
        
        if(input$inflation){
          
          input$inflationFactor
          
        } else {
          
          NA
          
        }
        
      })
  
  
  threeStageParameters <- c("varianceLevel1", "marginalPrevalence0", "marginalPrevalenceA",
      "conditionalPrevalence0", "conditionalPrevalenceA")
  
  for(iParameter in threeStageParameters){
    
    local({
          
          iParameter <- iParameter
          
          results[[iParameter]] <- reactive({
                
                if(!is.null(input[[iParameter]])){
                  
                  input[[iParameter]]
                  
                } else {
                  
                  NA
                  
                }
                
              }) 
        })
  } 
  
  
  # Define range of values for general parameters
  # create results$sampleSize, results$desiredDifferenc and results$power
  generalNames <- c("Total sample size" = "sampleSize", 
      "Detectable difference" = "desiredDifference",
      "Power" = "power")
  
  
  for(iName in generalNames ){
    
    local({
          
          iName <- iName
          names(iName) <- names(generalNames)[which(iName == generalNames)]
          
          results[[iName]] <- reactive({
                
                if(input$range != iName){
                  
                  if(iName == 'sampleSize'){
                    
                    adjustSampleSize(sampleSize = input$sampleSize,
                        populationSize = input$populationSize,
                        adjustFinitePopulation = input$finite, 
                        inflationFactor = results$inflationFactor())
                    
                  } else {
                    
                    input[[iName]]
                    
                  }
                  
                } else {
                  
                  validate(need(input[[paste0("max",iName)]] & input[[paste0("min",iName)]],
                              paste0("Please provide values for miminum and maximum of '", names(iName), "'")) %then%
                          need(input[[paste0("max",iName)]] > input[[paste0("min",iName)]],
                              paste0("Maximum should be larger than minimum of '", names(iName), "'")))
                  
                  if(iName == "sampleSize"){
                    
                    adjustedMin <- adjustSampleSize(sampleSize = input$minsampleSize,
                        populationSize = input$populationSize,
                        adjustFinitePopulation = input$finite, 
                        inflationFactor = results$inflationFactor())
                    adjustedMax <- adjustSampleSize(sampleSize = input$maxsampleSize,
                        populationSize = input$populationSize,
                        adjustFinitePopulation = input$finite, 
                        inflationFactor = results$inflationFactor())
                    
                    seq(adjustedMin, adjustedMax, by = 1)
                    
                  } else {
                    
                    seq(input[[paste0("min",iName)]], input[[paste0("max",iName)]], by = 0.01)
                    
                  }
                  
                }
                
              })
          
        })
    
  }
  
  
  results$stratifiedInput <- reactive({
        
        if( is.null(input$loadedStratified) )
          return ( NULL )
        
        if(input$loadedStratified == "Enter data"){
          
          data <- hot.to.df(input$stratifiedInfo)
          
        } else {
          
          data <- results$dataStratified()
          
        }
        
        if(round(sum(data[,"Stratum proportion"], na.rm = TRUE), 1) != 1){
          
          populationSize <- sum(data[,"Stratum proportion"], na.rm  = TRUE)
          data[,"Stratum proportion"] <- data[,"Stratum proportion"]/
              populationSize
          
          observe(
              updateNumericInput(session, inputId = "populationSize",
                  value = populationSize)
          )
          
        }
        
        return(data)
        
      })
  
  
  results$twoStageInput <- reactive({
        
        if( is.null(input$loadedTwoStage) | is.null(input$twoStagePanel) )
          return( NULL )
        
        
        if(input$twoStagePanel == "option13") {
          
          if(input$loadedTwoStage == "Enter data"){
            
            if(is.null(input$twoStage13Info))
              return( NULL )
            
            twoStageInput <- hot.to.df(input$twoStage13Info)
            
          } else {
            
            if( is.null(results$dataTwoStage()))
              return( NULL )
            
            twoStageInput <- results$dataTwoStage()
            
          }
          
          twoStageInput[,"Stratum variance"] <- NA
          
        } else {
          
          if(input$loadedTwoStage == "Enter data"){
            
            if(is.null(input$twoStage12Info))
              return( NULL )
            
            twoStageInput <- hot.to.df(input$twoStage12Info)
            
          } else {
            
            if( is.null(results$dataTwoStage()))
              return( NULL )
            
            twoStageInput <- results$dataTwoStage()
            
          }
          
          twoStageInput[,"Cluster variance"] <- NA
          
        }
        
        
        if(round(sum(twoStageInput[,"Stratum proportion"], na.rm = TRUE), 1) != 1){
          
          populationSize <- sum(twoStageInput[,"Stratum proportion"], na.rm  = TRUE)
          twoStageInput[,"Stratum proportion"] <- twoStageInput[,"Stratum proportion"]/
              populationSize
          
          observe(
              updateNumericInput(session, inputId = "populationSize",
                  value = populationSize)
          )
          
        }
        
        return( twoStageInput )
        
      })
      
      
    
  results$fixedInput <- reactive({
        
        parametersListActive <- 
            switch(input$samplingDesign,
                
                "getSimpleRandom" = {
                  
                  list(sampleVariance = input$sampleVariance,
                      populationSize = input$populationSize,
                      adjustFinitePopulation = input$finite,
                      inflationFactor = results$inflationFactor() )
                  
                },
                
                "getClustered" = {
                  
                  list( clusterSize = input$clusterSize,
                      sampleVariance = input$sampleVarianceClustered,
                      designEffect = results$designEffect(),
                      correlation = input$correlation,
                      clusterVariance = results$clusterVariance(), 
                      conditionalPrevalence = input$conditionalPrevalence,
                      populationSize = input$populationSize,
                      adjustFinitePopulation = input$finite,
                      inflationFactor = results$inflationFactor() )
                  
                },
                
                "getStratified" = {
                  
                  list( allocation = input$allocation,
                      stratumProportions = results$stratifiedInput()[,"Stratum proportion"],
                      stratumVariances = results$stratifiedInput()[,"Stratum variance"],
                      populationSize = input$populationSize,
                      adjustFinitePopulation = input$finite,
                      inflationFactor = results$inflationFactor() )
                  
                  
                },
                
                "getTwoStage" = {
                  
                  if( !is.null(results$twoStageInput()) ){
                    
                    list( clusterSize = input$clusterSize,
                        stratumProportions = results$twoStageInput()[,"Stratum proportion"],
                        stratumVariances = results$twoStageInput()[,"Stratum variance"],
                        clusterVariances = results$twoStageInput()[,"Cluster variance"],
                        designEffect = results$designEffect(),
                        populationSize = input$populationSize,
                        adjustFinitePopulation = input$finite,
                        inflationFactor = results$inflationFactor() )
                    
                  } else {
                    
                    list()
                    
                  }
                  
                },
                
                "getThreeStage" = {
                  
                  list( marginalPrevalence0 = results$marginalPrevalence0(),
                      marginalPrevalenceA = results$marginalPrevalenceA(),
                      conditionalPrevalence0 = results$conditionalPrevalence0(),
                      conditionalPrevalenceA = results$conditionalPrevalenceA(),
                      sizeLevel3 = input$sizeLevel3,
                      sizeLevel2 = input$sizeLevel2,
                      sizeLevel1 = input$sizeLevel1,
                      varianceLevel3 = input$varianceLevel3,
                      varianceLevel2 = input$varianceLevel2,
                      varianceLevel1 = results$varianceLevel1(),
                      populationSize = input$populationSize,
                      adjustFinitePopulation = input$finite,
                      inflationFactor = results$inflationFactor())
                  
                },
                
                "getChangeOverTime" = {
                  
                  list(varianceCovariance = getVarianceCovariance(
                          varianceCorrelation = as.data.frame(sapply(hot.to.df(input$correlationInfo), as.numeric))),
                      populationSize = input$populationSize,
                      adjustFinitePopulation = input$finite,
                      inflationFactor = results$inflationFactor() )
                }
            )
        
        return( c(purpose = input$purpose, typeIerror = input$typeIerror, 
                parametersListActive) )
        
      })
  
  results$randomInput <- reactive({
        
        as.list(expand.grid(
                list( sampleSize = results$sampleSize(),
                    desiredDifference = results$desiredDifference(), 
                    power = results$power() ) 
            )
        )
        
      })
  
  
  results$isMissing <- reactive({
        
        allMissing <- sapply(results$randomInput(), function(x) any(is.na(x)))
        
        if(input$purpose == "estimation") {
          
          allMissing[1:2]
          
        } else {
          
          allMissing
          
        }
        
      })
  
  
  
  results$allSampling <- eventReactive(input$submit, {
        
        isolate({
              
              tryCatch({
                    
                    wrapFunctionMilanzi( wrappedFunction = input$samplingDesign,
                        randomInput = results$randomInput(),
                        fixedInput = results$fixedInput())
                    
                  }, error = function(err) {
                    
                    return(err)
                    
                  })
              
            })
        
      })
  
  
  
  results$tableHeader <- reactive({
        
        if(!is.null( results$allSampling() )){
          
          isolate(
              
              switch(input$samplingDesign,
                  
                  "getSimpleRandom" = {
                    
                    list(designStatistics = names(generalNames))
                    
                  },
                  
                  "getClustered" = {
                    
                    list(designStatistics = c("Total number of clusters", names(generalNames)),
                        sampleAllocation = c("Number of clusters in population", 
                            "Number of clusters to sample"))
                    
                  },
                  "getStratified" = {
                    
                    list(designStatistics = names(generalNames),
                        sampleAllocation = c("Stratum proportion", "Stratum variance",
                            "Stratum size in population", "Stratum size to sample"))
                    
                  },
                  
                  "getTwoStage" = {
                    
                    if(input$twoStagePanel == "option13") {
                      
                      list(designStatistics = c("Total number of clusters", names(generalNames)),
                          sampleAllocation = c("Stratum proportion", "Cluster variance", 
                              "Number of clusters in population", "Number of clusters to sample",
                              "Stratum size to sample"))
                      
                    } else if(input$twoStagePanel == "option12") {
                      
                      list(designStatistics = c("Total number of clusters", names(generalNames)),
                          sampleAllocation = c("Stratum proportion", "Stratum variance",
                              "Number of clusters in population", "Number of clusters to sample",
                              "Stratum size to sample"))
                      
                    }
                    
                  },
                  
                  "getThreeStage" = {
                    
                    list(designStatistics = c(names(generalNames), 
                            "Level III units", "Level II units", "Level I units"))
                    
                  },
                  
                  "getChangeOverTime" = {
                    
                    list(designStatistics = names(generalNames))
                    
                  }
              )
          )
        }
      })
  
  
  output$info <- renderUI({
        
        validate(need(!is.null( results$allSampling() ), ""),
            need( !any(attr(results$allSampling(), "class") == "error"), 
                paste("Error:", (results$allSampling())$message)))
        
        
        if(length(results$allSampling()) > 1){
          
          list(
              h3("General parameters"),
              numericInput("nRows", "Number of steps", value = 5, min = 1, max = 50, step = 1)
          )
          
        } else {
          
          h3("General parameters")
          
        }
        
      })
  
  results$stratumNames <- reactive({
        
        if(input$samplingDesign == "getStratified"){
          
          results$stratifiedInput()[,"Stratum name"]
          
        } else if(input$samplingDesign == "getTwoStage"){
          
          results$twoStageInput()[,"Stratum name"]
          
        }
        
      })
  
  
  results$designStatistics <- reactive({
        
        observe( updateNumericInput(session, "nRows", value = input$nRows) )
        
        if(is.null(results$allSampling()) )
          return( NULL )
        
        
        if( length(results$allSampling()) > 1 ){
          
          totalRows <- length(results$allSampling())
          validate(need(input$nRows, "Please provide a valid 'Number of steps'"))
          indices <- round(seq(1, totalRows, length.out = input$nRows))
          
          if(input$samplingDesign %in% c("getClustered", "getStratified", "getTwoStage")) {
            
            subSampling <- lapply(results$allSampling(), function(x) x$designStatistics)[indices]
            
          } else {
            
            subSampling <- results$allSampling()[indices]
            
          }
          
          helpOutput <- do.call(rbind.data.frame, subSampling)
          
        } else {
          
          if(input$samplingDesign %in% c("getClustered", "getStratified", "getTwoStage")) {
            
            helpOutput <- results$allSampling()[[1]]$designStatistics
            
          } else {
            
            helpOutput <- results$allSampling()[[1]]
            
          }
          
        }
        
        if( length(helpOutput) == 0 )
          return( NULL )
        
        
        if(length(results$tableHeader()$designStatistics) == ncol(helpOutput)){
          
          colnames(helpOutput) <- results$tableHeader()$designStatistics
          
        }
        
        if(input$purpose == "estimation") {
          helpOutput <- helpOutput[ , !colnames(helpOutput) %in% "Power"]
        }
        
        helpOutput
        
        
      })
  
  results$sampleAllocationMax <- eventReactive(input$submit, {
        
        if(is.null(results$allSampling()))
          return( NULL )
        
        if(input$samplingDesign %in% c("getClustered", "getStratified", "getTwoStage") &
            length(results$allSampling()) > 1){
          
          helpOutput <- results$allSampling()[[length(results$allSampling())]]$sampleAllocation
          colnames(helpOutput) <- results$tableHeader()$sampleAllocation
          
          if(is.na(input$populationSize)) {
            
            helpOutput <- helpOutput[, !colnames(helpOutput) %in% 
                    c("Number of clusters in population", 
                        "Stratum size in population")]
            
          }
          
          if(input$samplingDesign != "getClustered"){
            
            rownames(helpOutput) <- results$stratumNames()
            
          } 
          
          helpOutput
          
          
        } else {
          
          NULL
          
        }
        
      })
  
  results$sampleAllocationMin <- eventReactive(input$submit, { 
        
        if(is.null(results$allSampling()))
          return( NULL )
        
        if(input$samplingDesign %in% c("getClustered", "getStratified", "getTwoStage")){
          
          helpOutput <- results$allSampling()[[1]]$sampleAllocation
          colnames(helpOutput) <- results$tableHeader()$sampleAllocation
          
          if(is.na(input$populationSize)) {
            
            helpOutput <- helpOutput[, !colnames(helpOutput) %in% 
                    c("Number of clusters in population", 
                        "Stratum size in population")]
            
          }
          
          if(input$samplingDesign != "getClustered"){
            
            rownames(helpOutput) <- results$stratumNames()
            
          }
          
          helpOutput
          
        } else {
          
          NULL
          
        }
        
      })
  
  
  summarizeResults <- function(){
    
    
    #Clear previous output
    if( !is.null(input$submit)) {
      
      if(input$submit > 0){
        
        output$plotDesignStatistics <- renderPlot( NULL, height = "0px" )
        output$plotSampleAllocation <- renderPlot( NULL, height = "0px" )
        
      }
    }
    
    if(!is.null(results$allSampling())){
      if(!any(attr(results$allSampling(), "class") == "error")){
        
        output$warningTooSmall <- renderText({
              
#                  validate(need(!is.null(results$allSampling()), "No results available"),
#                      need(!any(attr(results$allSampling(), "class") == "error"),
#                          results$allSampling()$message))
              
              
              if(any( results$designStatistics()[,"Total sample size"] > input$populationSize,
                  na.rm = TRUE)){
                
                "Warning: The 'Total sample size' is larger than the 'Population size'"
                
              } else {
                
                NULL
                
              }
              
            })
        
        if(input$purpose == 'estimation'){
          
          documentation <- "Detectable difference: half width of the confidence interval"
          
        } else {
          
          documentation <- "Detectable difference: true difference in means that is tested for"
          
        }
        
        output$designStatistics <- renderTable({
              
              if(is.null(results$designStatistics()))
                return( NULL )
              
              toReport <- results$designStatistics()
              validate(need(!any(is.na(toReport)), 
                      "No results are shown: Please check whether valid input values are provided for all 'Design-specific parameters'."))
              toReport
              
            }, caption = documentation,
            caption.placement = getOption("xtable.caption.placement", "bottom"),
            digits = 3)
        
        
        output$downloadTableDesign <- downloadHandler(
            filename = function() { "sampelatorDesign.csv" },
            content = function(file) {
              write.csv(results$designStatistics(), file)
            })
        
        
        if(length(results$allSampling()) > 1){
          
          captionMin <- paste0("Table for minimum of '", 
              names(generalNames)[which(generalNames == input$range)], "'")
          
          captionMax <- paste0("Table for maximum of '", 
              names(generalNames)[which(generalNames == input$range)], "'")
          
        } else {
          
          captionMin <- ""
          captionMax <- ""
          
        }
        
        output$sampleAllocationMax <- renderTable({
              
              results$sampleAllocationMax()
              
            }, caption = captionMax,
            caption.placement = getOption("xtable.caption.placement", "top"), 
            caption.width = getOption("xtable.caption.width", NULL),
            digits = 3)
        
        output$downloadTableSampleAllocationMax <- downloadHandler(
            filename = function() { "sampelatorAllocationMax.csv" },
            content = function(file) {
              write.csv(results$sampleAllocationMax(), file)
            })
        
        
        
        output$sampleAllocationMin <- renderUI({
              
              if(is.null(results$sampleAllocationMin())){
                
                return( NULL )
                
              }
              
              list(
                  
                  h3("Sample allocation"),
                  renderTable({
                        
                        validate(need(!any(is.na(results$sampleAllocationMin())), 
                                "No results are shown: Please check whether valid input values are provided for all 'Design-specific parameters'."))
                        
                        results$sampleAllocationMin()
                        
                      }, caption = captionMin,
                      caption.placement = getOption("xtable.caption.placement", "top"), 
                      caption.width = getOption("xtable.caption.width", NULL),
                      digits = 3),
                  
                  downloadButton("downloadTableSampleAllocationMin", "Download")
              )
            })
        
        
        output$downloadTableSampleAllocationMin <- downloadHandler(
            filename = function() { "sampelatorAllocationMin.csv" },
            content = function(file) {
              write.csv(results$sampleAllocationMin(), file)
            })
        
        
#                  if( length(results$allSampling()) > 1 ){
        
        results$plotDesignStatistics <- eventReactive(input$submit, {
              
              if( input$range != "<none>" ){
                
                if(input$samplingDesign %in% c("getClustered", "getStratified", "getTwoStage")) {
                  
                  allDesignStatistics <- lapply(results$allSampling(), function(x) x$designStatistics)
                  allSampleAllocation <- lapply(results$allSampling(), function(x) x$sampleAllocation)
                  
                } else {
                  
                  allDesignStatistics <- results$allSampling()
                  
                }
                
                xLimits <- sapply(allDesignStatistics, function(x) x[,input$range])
                
                xValues <- xLimits
                yName <- generalNames[ which(results$isMissing()) ]
                yValues <- sapply(allDesignStatistics, function(x) x[,yName])
                group <- rep(names(yName), times = length(xLimits))
                
                if(input$samplingDesign == "getClustered"){
                  
                  xValues <- rep(xValues, times = 2)
                  yValues <- c(yValues,
                      sapply(allSampleAllocation, function(x) x[,"numberClusters"]))
                  group <- c(group, rep("Number of clusters to sample", times = length(xLimits)))
                  
                } else if(input$samplingDesign == "getThreeStage" & 
                    length(which(results$isMissing())) == 1){
                  if(names(which(results$isMissing())) == "sampleSize"){
                    
                    xValues <- rep(xValues, times = 4)
                    group <- c(group, rep(c("Level III units", "Level II units", 
                                "Level I units"), each = length(xLimits)))
                    toSelect <- c("sizeLevel3", "sizeLevel2", "sizeLevel1")
                    
                    for(iSelect in toSelect){
                      
                      iSelect <- iSelect
                      yValues <- c(yValues, 
                          sapply(allDesignStatistics, function(x) x[,iSelect]))
                      
                    }
                  }
                  
                }
                
                data.frame(xValues = xValues, yValues = yValues,
                    group = group)
                
              } else {
                
                NULL
                
              }
              
            })
        
        
        output$plotDesignStatistics <- renderChart2({
              
              validate(need(results$plotDesignStatistics(), ""))
              
              plotDesign <- nPlot(yValues ~ xValues, 
                  data = results$plotDesignStatistics(), 
                  group = "group", type = 'lineChart')
              
              if(input$range == "sampleSize"){
                plotDesign$xAxis(axisLabel = names(generalNames)[which(generalNames == input$range)])
                plotDesign$yAxis(tickFormat = "#! function(d) {return d3.format(',.2f')(d)} !#")
              } else {
                plotDesign$xAxis(axisLabel = names(generalNames)[which(generalNames == input$range)],
                    tickFormat = "#! function(d) {return d3.format(',.2f')(d)} !#")
              }
              
              plotDesign$save(file.path(tempdir(), "design.html"),
                  standalone = TRUE)
              
              return(plotDesign)
              
            })
        
        
        output$downloadPlotDesign <- downloadHandler(
            paste0("sampelatorDesign.html") ,
            content = function(file) {
              file.copy(file.path(tempdir(), "design.html"), file)
            }
        )
        
        output$commentDesignStatistics <- renderText({
              
              validate(need(results$plotDesignStatistics(), ""))
              
              dataDuplicated <- data.frame(matrix(results$plotDesignStatistics()$yValues, 
                      ncol = length(unique(results$plotDesignStatistics()$group)),
                      byrow = TRUE))
              
              idDuplicated <- which(duplicated(t(dataDuplicated)) | 
                      duplicated(t(dataDuplicated), fromLast = TRUE))
              
              if(length(idDuplicated) > 1){
                
                return( paste("Warning: The lines coincide for",
                        paste(unique(results$plotDesignStatistics()$group)[idDuplicated], 
                            collapse = ", ")) )    
                
                
                return( NULL )
                
              }
              
            })
        
        if(input$samplingDesign %in% c("getStratified", "getTwoStage")) {
          
          results$plotSampleAllocation <- eventReactive(input$submit, {
                
                if( input$range != "<none>" ){
                  
                  allDesignStatistics <- lapply(results$allSampling(), function(x) x$designStatistics)
                  xLimits <- sapply(allDesignStatistics, function(x) x[,input$range])
                  
                  allSampleAllocation <- lapply(results$allSampling(), function(x) x$sampleAllocation)
                  
                  yValues <- as.vector(sapply(allSampleAllocation, function(x) x[,"stratumSize"]))
                  nStrata <- dim(allSampleAllocation[[1]])[1]
                  main <- "Stratum size"
                  
                  data.frame(yValues = yValues, xValues = rep(xLimits, each = nStrata),
                      group = rep(results$stratumNames(), times = length(xLimits)))
                  
                } else {
                  
                  NULL
                  
                }
                
              })
          
          
          output$plotSampleAllocation <- renderChart2({
                
                validate(need(results$plotSampleAllocation(), ""))
                
                plotStrata <- nPlot( yValues ~ xValues, data = results$plotSampleAllocation(), group = "group",
                    type = "lineChart")
                
                plotStrata$yAxis(axisLabel = "Stratum size")
                if(input$range == "sampleSize"){
                  plotStrata$xAxis(axisLabel = names(generalNames)[which(generalNames == input$range)])
                } else {
                  plotStrata$xAxis(axisLabel = names(generalNames)[which(generalNames == input$range)],
                      tickFormat = "#! function(d) {return d3.format(',.2f')(d)} !#")
                }
                
#                      plotStrata$chart(tooltipContent = "#! function(key, x, y){
#                              return '<h3>' + key + '</h3>' + 
#                              '<p>' + y + ' out of ' + x + '</p>'
#                              } !#")
                plotStrata$save(file.path(tempdir(), "sampleAllocation.html"),
                    standalone = TRUE)
      
                return(plotStrata)
                
                
              })
          
          
          output$downloadPlotSampleAllocation <- downloadHandler(
              paste0("sampelatorSampleAllocation.html") ,
              content = function(file) {
                file.copy(file.path(tempdir(), "sampleAllocation.html"), file)
              }
          )
          
          output$commentSampleAllocation <- renderText({
                
                validate(need(results$plotSampleAllocation(), ""))
                
                dataDuplicated <- data.frame(matrix(results$plotSampleAllocation()$yValues, 
                        ncol = length(unique(results$plotSampleAllocation()$group)),
                        byrow = TRUE))
                
                idDuplicated <- which(duplicated(t(dataDuplicated)) | 
                        duplicated(t(dataDuplicated), fromLast = TRUE))
                
                if(length(idDuplicated) > 1){
                  
                  return( paste("Warning: The lines coincide for",
                          paste(unique(results$plotSampleAllocation()$group)[idDuplicated], 
                              collapse = ", ")) )    
                  
                } else {
                  
                  return( NULL )
                  
                }
                
              })
        }
        
      }
    }
    
  }
  
  
  
  
  
  # Output UI
  output$resultsToShow <- renderUI({
        
        input$submit
        
        summarizeResults()
        
        isolate({
              
              list(
                  
                  fluidRow(
                      column(6,
                          list(
                              uiOutput("info"),
                              tableOutput("designStatistics"),
                              textOutput("warningTooSmall"),
                              tags$head(tags$style("#warningTooSmall{color: red;
                                          font-style: italic;
                                          }")),
                              downloadButton("downloadTableDesign", "Download")
                          )
                      ),
                      column(6,
                          list(
                              showOutput("plotDesignStatistics", "nvd3"),
                              textOutput("commentDesignStatistics"),
                              downloadButton("downloadPlotDesign", "Download")
                          )
                      )
                  ),
                  
                  tags$br(),
                  tags$hr(),
                  
                  fluidRow(
                      column(6, 
                          list(
                              uiOutput("sampleAllocationMin"),
                              tableOutput("sampleAllocationMax"),
                              downloadButton("downloadTableSampleAllocationMax", "Download"),
                              tags$br()
                          )
                      ),
                      column(6,
                          list(
                              showOutput("plotSampleAllocation", "nvd3"),
                              textOutput("commentSampleAllocation"),
                              downloadButton("downloadPlotSampleAllocation", "Download"),
                              tags$br()
                          )
                      )
                  )
              
              )
              
            })
      })
  
  
  output$warnings <- renderUI({
        
        if(input$samplingDesign == "getThreeStage"){
          
          validate(need( input$purpose == "testing", "This sampling approach does not support 'Purpose of the study: Estimation'"))
          
        }
        
        messageMissing <- "Exactly one of 'General parameters' should be missing and is then estimated"
        validate(need( sum( results$isMissing() ) == 1, messageMissing)) 
        
#        if(input$samplingDesign == "getClustered"){
#          
#          if( ((input$range != "sampleSize" & is.na(input$sampleSize)) |
#                input$range == "sampleSize" & is.na(input$minsampleSize) & is.na(input$minsampleSize)) &
#              !is.null(input$numberClusters) ){
#            
#            validate(need( is.na(input$numberClusters), "If 'Total sample size' is missing, the 'Number of clusters' should also be missing"))
#            
#          }
#          
#        }
        
        if(input$finite){
          
          validate(need(input$populationSize,
                  "Please provide a valid input value for 'Population size'"))
          
        }
        
        if(input$inflation){
          
          validate(need(input$inflationFactor,
                  "Please provide a valid input value for 'Inflation factor'"))
          
        }
        
        
        if(input$samplingDesign == "getThreeStage"){
          
          if((input$range != "sampleSize" && is.na(input$sampleSize)) ||
              (input$range == "sampleSize" && is.na(input$minsampleSize) && is.na(input$minsampleSize))){
            
            sizeMissing <- sapply(list(input$sizeLevel3, input$sizeLevel2, input$sizeLevel1), is.na)
            validate(need( sum(sizeMissing) == 1, "If 'Total sample size' is missing, exactly one of 'Number of level III items sampled', '(Average) number sampled at level II' or 'at level I' should also be missing"))
            
          }
          
        }
        
        list(
            busyIndicator("In progress", wait = 0),
            actionButton(inputId = "submit", label = "Submit", 
                styleclass="primary", size="mini")
        )
        
      })
  
  
  source("serverCollectingSamples.R", local = TRUE)
  
}
