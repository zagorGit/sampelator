#http://walkerke.github.io/2014/03/tfr-in-europe/
#install.packages("base64enc")
#devtools::install_github('rCharts', 'ramnathv', ref='dev')
# library(sampelator)

scriptdir = '../../../R'
list.files(scriptdir)
source(file.path(scriptdir, 'collectingSamples.R'))
source(file.path(scriptdir, 'ribessFunctions.R'))
source(file.path(scriptdir, 'runRibess.R'))
source(file.path(scriptdir, 'runSampelator.R'))
source(file.path(scriptdir, 'sampelatorFunctions.R'))

library(rCharts) #for interactive plots
options(RCHART_WIDTH = 800)

library(shinyIncubator)  #for matrixInput

library(shinysky)  #for hotable

# `%then%` <- shiny:::`%OR%`

quantileStart <- data.frame(matrix(c(0.25, 0.75, "", ""), nrow = 2, ncol = 2))






serverFunction <- function(input,output,session){
  
  
  ## for debugging
  output$print <- renderPrint({
        
        
      })
  
  output$titlePanel <- renderUI({
        
        
        titlePanel(title = div(img(src = "/inst/images/EFSA_logo.jpg",
                    float = "top", height = "60px", hspace = "50px"),
                "RiBESS+"), windowTitle = "RiBESS+")
        
      })
  
  
  ## Random input
  createParametersTable <- function(parameterName, distributionName){
    
    output[[paste0("parametersTable", parameterName)]] <-  
        renderUI({ 
              
              if (is.null(distributionName)) return()
              
              switch(distributionName,
                  
                  "unif" = {
                    list(
                        numericInput(paste0("min", parameterName), "Minimum", 0),
                        numericInput(paste0("max", parameterName), "Maximum", 1)
                    )
                  },
                  
                  "norm" = {
                    list(
                        numericInput(paste0("mean", parameterName), "Mean", 0),
                        numericInput(paste0("sd", parameterName), "Standard Deviation", 1)
                    )
                  }, 
                  
                  "beta" = {
                    list(
                        numericInput(paste0("shape1", parameterName), "Shape 1", NA),
                        numericInput(paste0("shape2", parameterName), "Shape 2", NA)
                    )
                  },
                  
                  "gamma" = {
                    list(
                        numericInput(paste0("shape", parameterName), "Shape", NA),
                        numericInput(paste0("rate", parameterName), "Rate", 1)
                    )
                    
                  },
                  
                  "reference" = {
                    
                  },
                  
                  "expNorm" = {
                    list(
                        numericInput(paste0("mean", parameterName), "Mean", 0),
                        numericInput(paste0("sd", parameterName), "Standard Deviation", 1),
                        p(em("Values will be shown on exponential scale"))
                    )
                  }, 
                  
                  "expitNorm" = {
                    list(
                        numericInput(paste0("mean", parameterName), "Mean", 0),
                        numericInput(paste0("sd", parameterName), "Standard Deviation", 1),
                        p(em("Values will be shown on expit scale"))
                    )
                  } 
              )
              
            })
    
  }
  
  getDistributionParameters <- function(parameterName, distributionName){
    
    results[[paste0("distributionParameters", parameterName)]] <- reactive({
          
          switch(distributionName,
              
              "unif" = {
                c(min = input[[ paste0("min", parameterName) ]],
                    max = input[[ paste0("max", parameterName) ]])
              },
              
              "norm" = {
                c(mean = input[[ paste0("mean", parameterName) ]],
                    sd = input[[ paste0("sd", parameterName) ]])
              }, 
              
              "beta" = {
                c(shape1 = input[[ paste0("shape1", parameterName) ]],
                    shape2 = input[[ paste0("shape2", parameterName) ]])
              },
              
              "gamma" = {
                c(shape = input[[ paste0("shape", parameterName) ]],
                    rate = input[[ paste0("rate", parameterName) ]])
              },
              
              "reference" = {
                c(value = NA)
              },
              
              "expNorm" = {
                c(mean = input[[ paste0("mean", parameterName) ]],
                    sd = input[[ paste0("sd", parameterName) ]])
              },
              
              "expitNorm" = {
                c(mean = input[[ paste0("mean", parameterName) ]],
                    sd = input[[ paste0("sd", parameterName) ]])
              },
          )
          
        })
    
  }  
  
  
  createRandomValues <- function(parameterName, baseName, seed){
    
    results[[ paste0("randomValues", parameterName) ]] <- reactive({
          
          validate(need(results$nMC(), 
                  "Please provide valid input value for 'Number of random values'"))
          
          if(input[[paste0("random", baseName)]] == "fixed") {
            
            input[[ parameterName ]]
            
          } else {
            
            if(input[[paste0("random", baseName)]] == "randomKnownQ"){
              
              bestResult <- results[[ paste0("q", parameterName, "Info") ]]()
              
              distributionName = bestResult$bestName
              distributionParameters = bestResult$bestParameters
              linkFunction <- function(x) x
              
            } else {
              
              if(input[[paste0("distribution", parameterName)]] == "reference") {
                
                if(baseName == "Rr"){
                  
                  return(1)
                  
                } else {
                  
                  return(NA)
                  
                }
                
              } else {
                
                if(input[[paste0("distribution", parameterName)]] == "expNorm") {
                  
                  distributionName <- "norm"
                  linkFunction <- function(x) exp(x)
                  
                } else if(input[[paste0("distribution", parameterName)]] == "expitNorm") {
                  
                  distributionName <- "norm"
                  linkFunction <- function(x) {exp(x)/(1 + exp(x))}
                  
                } else {
                  
                  distributionName <- input[[paste0("distribution", parameterName)]]
                  linkFunction <- function(x) x
                  
                }
                
                distributionParameters <- getDistributionParameters(parameterName = parameterName,
                    distributionName = distributionName)()
                if(!is.null(distributionParameters)){
                  validate(need(!any(sapply(distributionParameters, is.na)), 
                          "Please enter distribution parameters"))
                  
                }
              }
            }
            
            randomValues <- tryCatch({
                  
                  linkFunction(createRandom( distributionName = distributionName, 
                          distributionParameters = distributionParameters,
                          nMC = results$nMC(), seed = seed)) 
                  
                }, error = function(err) {
                  
                  return(err)
                  
                })
            
            return(randomValues)
            
            
          }
          
        })
  }
  
  results <- reactiveValues()
  
  results$submitSample <- reactive({
        
        if(input$toCalculate != 'pFree'){
          
          input$submit
          
        } else {
          
          NULL
          
        }
      })
  
  results$submitPfree <- reactive({
        
        if(input$toCalculate == 'pFree'){
          
          input$submit
          
        } else {
          
          NULL
          
        }
      })
  
  results$pIntroNames <- reactive({
        
        paste0(rep("Pintro", input$nPeriods), 1:input$nPeriods)
        
      })
  
  
  results$parameterNames <- reactive({
        
        if(!input$numRF > 0){
          
          c("N", "TSe", "Pfree0", results$pIntroNames())
          
        } else {
          
          baseNames <- paste0(rep("Rr", input$numRF), 1:input$numRF)
          
          relativeRiskNames <- sapply(as.list(1:input$numRF), 
              function(x){
                if(!is.null(input[[paste0('numLevels',x)]])){
                  
                  times <- input[[paste0('numLevels',x)]]
                  return(paste0(rep(baseNames[x], times), '_', 1:times))
                  
                }
              } 
          )
          
          baseNames <- paste0(rep("Proportion", input$numRF), 1:input$numRF)
          
          proportionNames <- sapply(as.list(1:input$numRF), 
              function(x){
                if(!is.null(input[[paste0('numLevels',x)]])){
                  
                  times <- input[[paste0('numLevels',x)]]
                  return(paste0(rep(baseNames[x], times), '_', 1:times))
                  
                }
              }
          )
          
          unlist(c("N", "TSe", "Pfree0", results$pIntroNames(),
                  relativeRiskNames, proportionNames))
          
        }      
        
      })
  
  
  results$baseNames <- reactive({
        
        baseNames <- c()
        
        for(i in seq_along(results$parameterNames()) ){
          
          baseNames[i] <- local({
                
                iParameter <- results$parameterNames()[i]
                if(iParameter %in% c("N", "TSe", "Pfree0", results$pIntroNames())){
                  iParameter
                } else {
                  substr(iParameter, start = 1, stop = (nchar(iParameter) - 3))
                }
                
              })
          
        }
        
        baseNames
        
      })
  
  
  results$isRandom <- reactive({
        
        if(input$toCalculate != 'pFree'){
          
          baseNames <- results$baseNames()[ -which(results$baseNames() %in% c("Pfree0", results$pIntroNames())) ]
          isRandom <- unlist(sapply(paste0('random', baseNames), function(x) input[[x]] != 'fixed'))
          
        } else {
          
          isRandom <- unlist(sapply(paste0('random', c("Pfree0", results$pIntroNames())), function(x) input[[x]] != 'fixed'))
          
        }
        
        any(isRandom)
        
      })
  
  
  observeEvent( results$isRandom(), {
        
        output$isRandom <- renderUI({ 
              
              if(results$isRandom()){
                
                list(
                    numericInput("nMC", "Number of random values", value = 100, width = "50%"),
                    
                    conditionalPanel("input.nMC > 1000",
                        p(em("Warning: A large 'number of random values' will seriously slow down calculations"))
                    )
                )
                
              } else {
                
                NULL
                
              }
              
            })
        
        observe( updateNumericInput( session, "nMC", value = input$nMC ) )
        
      })
  
  
  
  observe({
        
        for(i in seq_along(results$parameterNames()) ){
          
          local({
                
                i <- i
                iParameter <- results$parameterNames()[i]
                iBaseName <- results$baseNames()[i]
                
                #Create random values
                createRandomValues(parameterName = iParameter, 
                    baseName = iBaseName, seed = i)
                
                #Create parameter tables for random input
                observeEvent( input[[paste0("distribution", iParameter)]], {
                      
                      createParametersTable( parameterName = iParameter, 
                          distributionName = input[[paste0("distribution", iParameter)]] )
                      
                    })
                
                #Plot the random values that will be generated
                output[[ paste0("plot", iParameter) ]] <- renderPlot({
                      
                      if(input[[ paste0("random", iBaseName) ]] != "fixed"){
                        
                        if(input[[paste0("distribution", iParameter)]] == "reference"){
                          
                          if(iBaseName == "Proportion") {
                            
                            message <- "Please push 'Complete risk proportions' for a plot to be shown"
                            
                          } else {
                            
                            message <- "No plot shown"
                            
                          }
                          
                        } else {
                          
                          message <- "Please enter valid distribution quantiles or parameters. \n Make sure that quantiles are ordered from small to large."
                          
                        }
                        
                        validate(need( !any(is.na(results[[ paste0("randomValues", iParameter) ]]())) &
                                    length(results[[ paste0("randomValues", iParameter) ]]()) > 2, 
                                message))
                        par(mar=c(4.1,4.1,1.1,1.1))
                        plot(density(results[[ paste0("randomValues", iParameter) ]]() ),
                            main = "", las = 1)
                        
                      }
                      
                    })
                
                #Info on distribution for quantiles
                results[[ paste0("q", iParameter, "Info") ]] <- reactive({
                      
                      if(input[[paste0("random", iBaseName)]] == "randomKnownQ") {
                        
                        quantileMatrix <- input[[paste0("q", iParameter)]]
                        validate(need(!any(is.na(quantileMatrix)), "Please enter distribution quantiles.") | # %then%
                                need(quantileMatrix[,1] == sort(quantileMatrix[,1]), "Please sort probabilities from small to large.") | # %then%
                                need(quantileMatrix[,2] == sort(quantileMatrix[,2]), "Please sort quantiles from small to large."))
                        
                        return( getBestDistribution(probabilities = quantileMatrix[,1], 
                                quantiles =  quantileMatrix[,2]) )
                        
                      } else {
                        
                        return( NULL )
                        
                      }
                      
                    })
                
                output[[ paste0("q", iParameter, "Info") ]] <- renderUI({
                      
                      if(input[[paste0("random", iBaseName)]] == "randomKnownQ") {
                        
                        bestResult <- results[[ paste0("q", iParameter, "Info") ]]()
                        
                        list(
                            strong("Distribution with largest log-likelihood"),
                            tags$br(),
                            HTML(paste('<b> out of', paste(names(bestResult$allNames), collapse = ", "), '</b>')),
                            HTML(paste('<br/>', "Distribution:", names(bestResult$bestName), 
                                    '<br/>', "Parameters:", 
                                    paste(paste0(signif(bestResult$bestParameters, 5), " (", names(bestResult$bestParameters), ")"), collapse = ", "),
                                    '<br/>', "Log-likelihood:", signif(bestResult$logLikelihood, 5)))
                        )
                        
                      } else {
                        
                        NULL
                        
                      }
                      
                    })
                
              })
        }
        
      })
  
  
  
  #Make risk factor tables 
  #toExpand: 'Rr' or 'Proportion' or 'names'
  makeRiskList <- function(toExpand){
    
    if(input$numRF == 0) return()
    
    riskList <- list()
    
    for(iFactor in 1:input$numRF){
      
      if(is.null(input[[paste0('numLevels',iFactor)]])) return()
      
      rfName <- input[[paste0('rf',iFactor)]]
      rfName[which(rfName == "")] <- paste("risk factor", iFactor)
      
      levelNames <- c()
      
      if(toExpand == 'names'){
        
        rfValues <- c()
        
      } else if(input[[paste0('random', toExpand)]] == "fixed"){
        
        rfValues <- matrix(NA, nrow = 1, ncol = input[[paste0('numLevels',iFactor)]])
        
      } else {
        
        rfValues <- matrix(NA, nrow = results$nMC(), ncol = input[[paste0('numLevels',iFactor)]])
        attr(rfValues, "random") <- TRUE
        
      }
      
      for(jLevel in 1:input[[paste0('numLevels',iFactor)]]) {
        
        if(toExpand == 'names'){
          
          rfValues[jLevel] <- 1
          
        } else if(input[[paste0('random', toExpand)]] == "fixed"){
          
          if(!is.null(input[[paste0(toExpand,iFactor,'_',jLevel)]])){
            
            rfValues[jLevel] <- input[[paste0(toExpand,iFactor,'_',jLevel)]]
            
          }
          
        } else {
          
          rfValues[,jLevel] <- results[[paste0("randomValues", toExpand, iFactor,'_',jLevel)]]()
          
        }   
        
        if(!is.null(input[[paste0('level',iFactor,'_',jLevel)]])){
          
          levelNames[jLevel] <- input[[paste0('level',iFactor,'_',jLevel)]]
          
        }
        
      }
      
      if(!all(levelNames == "")){
        
#        if(toExpand == 'names'){
        
        names(rfValues) <- levelNames
        
#        } else {
#          
#          colnames(rfValues) <- levelNames
#          
#        }
      }
      
      riskList[[rfName]] <- rfValues
      
    }
    
    return(riskList)
    
  }
  
  
  results$columnsRF <- reactive({
        
        inputList <- makeRiskList(toExpand = 'names')
        
        if(is.null(inputList)){
          
          riskGroupsNames <- data.frame("All") 
          names(riskGroupsNames) <- "Group"
          
        } else {
          
          riskGroupsNames <- as.data.frame(helpExpandNames(inputList = inputList, toPaste = FALSE))
          
          if(input$numRF == 1){
            
            names(riskGroupsNames) <- input$rf1
            
          }
          
        }
        
        return( riskGroupsNames )
        
      })
  
  
  
  #Update input proportions
  observeEvent(input$completeRisks, {
        
        if(input$asDataFrame == "none"){
          
          for (i in 1:input$numRF) {
            
            local({
                  
                  i <- i
                  
                  if(input$randomProportion == "fixed"){
                    
                    currentSum <- 0
                    riskIds <- c()
                    
                    for(j in 1:input[[paste0('numLevels',i)]]) {
                      
                      if(input[[paste0('Proportion',i,'_',j)]] == 0 |
                          is.na(input[[paste0('Proportion',i,'_',j)]])) {
                        
                        riskIds <- c(riskIds, j)
                        
                      } else {
                        
                        currentSum <- currentSum + input[[paste0('Proportion',i,'_',j)]]
                        
                      }
                      
                    }
                    
                  } else {
                    
                    currentSum <- rep(0, results$nMC())
                    riskIds <- c()
                    
                    for(j in 1:input[[paste0('numLevels',i)]]) {
                      
                      if(input[[paste0('distributionProportion',i,'_',j)]] == 'reference') {
                        
                        riskIds <- c(riskIds, j)
                        
                      } else {
                        
                        currentSum <- currentSum + results[[paste0("randomValuesProportion", i,'_',j)]]()
                        
                      }
                      
                    }
                    
                  }
                  
                  totalZero <- length(riskIds)
                  
                  if(totalZero > 0){
                    
                    for(k in 1:totalZero){
                      
                      if(input$randomProportion == "fixed"){
                        
                        updateNumericInput( session, paste0('Proportion',i,'_',riskIds[k]),
                            value = ((1 - currentSum)/ totalZero) )
                        
                      } else {
                        
                        results[[paste0("randomValuesProportion", i,'_',riskIds[k])]] <- 
                            reactive((1 - currentSum)/ totalZero)
                        
                      }
                      
                    }
                  }
                  
                })
          }  
          
        }
        
      })
  
  
  
  
  observeEvent(input$numRF, {
        
        if(input$numRF == 0)
          return( NULL )
        
        
        for (i in 1:input$numRF) {
          
          output[[paste0('levelTable',i)]] <- local({
                
                i <- i
                
                renderUI({
                      
                      tags <- list()
                      
                      for(j in 1:input[[paste0('numLevels',i)]]) {
                        
                        local({   
                              j <- j
                              observe({
                                    updateTextInput(session, paste0('level',i,'_',j), value = input[[paste0('level',i,'_',j)]])
                                    updateNumericInput( session, paste0('Rr',i,'_',j), value = input[[paste0('Rr',i,'_',j)]] )                                    
                                  })
                            })
                        
                        levelNames <- column(2,textInput(paste0('level',i,'_',j),'Level name',letters[j]),offset=2)
                        levelValues <- NULL
                        
                        if(input$asDataFrame == 'none'){
                          
                          
                          levelValues <- list(
                              
                              column(2,
                                  
                                  conditionalPanel("input.randomRr == 'fixed'",
                                      numericInput(paste0('Rr',i,'_',j),'Value', value = 1, min = 1)
                                  ),
                                  
                                  conditionalPanel("input.randomRr == 'randomKnownPar'",
                                      list(
                                          selectInput(paste0("distributionRr", i,'_',j), label = "Distribution",
                                              choices = c("reference", "normal" = "expNorm")), 
                                          uiOutput(paste0('parametersTableRr', i,'_',j))
                                      )
                                  )
                              
                              ),
                              
                              column(2, conditionalPanel( paste0("input.randomRr != 'fixed' &
                                              input.distributionRr", i, '_', j, " != 'constant'"),
                                      plotOutput(paste0("plotRr", i, '_',j), height = "250px")
                                  )),
                              
                              column(2,
                                  
                                  conditionalPanel("input.randomProportion == 'fixed'",
                                      numericInput(paste0('Proportion',i,'_',j),'Value', value = NA)
                                  ),
                                  
                                  conditionalPanel("input.randomProportion == 'randomKnownPar'",
                                      list(
                                          selectInput(paste0("distributionProportion", i,'_',j), label = "Distribution",
                                              choices = c("reference", "normal" = "expitNorm")),
                                          uiOutput(paste0('parametersTableProportion', i,'_',j))
                                      )
                                  )
                              
                              ),
                              
                              column(2, conditionalPanel( paste0("input.randomProportion != 'fixed' &
                                              input.distributionProportion", i, '_', j, " != 'constant'"),
                                      plotOutput(paste0("plotProportion", i, '_',j), height = "250px")
                                  ))
                          )
                          
                        } else if (input$asDataFrame == "one"){
                          
                          levelValues <- list(
                              
                              column(2,
                                  
                                  conditionalPanel("input.randomRr == 'fixed'",
                                      numericInput(paste0('Rr',i,'_',j),'Value', value = 1, min = 1)
                                  ),
                                  
                                  conditionalPanel("input.randomRr == 'randomKnownPar'",
                                      list(
                                          selectInput(paste0("distributionRr", i,'_',j), label = "Distribution",
                                              choices = c("reference", "normal" = "expNorm")),
                                          uiOutput(paste0('parametersTableRr', i,'_',j))
                                      )
                                  )
                              
                              ),
                              
                              column(2, conditionalPanel( paste0("input.randomRr != 'fixed' &
                                              input.distributionRr", i, '_', j, " != 'constant'"),
                                      plotOutput(paste0("plotRr", i, '_',j), height = "250px")
                                  ))
                          )
                          
                        }
                        
                        tags[[j]] <- fluidRow(list(levelNames, levelValues))
                        
                        
                      }
                      
                      tagList(tags) 
                      
                    })
                
              })
        }
      })
  
  
  
  output$rfTable <- renderUI({
        
        if(input$numRF == 0)
          return( NULL )
        
        tags <- list()
        for (i in 1:input$numRF) {
          tags[[i*2-1]] <- fluidRow(column(2,textInput(paste0('rf',i),'Risk Factor',
                      paste('risk factor', i))),
              column(2, sliderInput(paste0('numLevels',i),'# levels', min = 2, max = 10, value = 2, step = 1)))
          tags[[i*2]] <- uiOutput(paste0('levelTable',i))
          
        }
        tagList(tags)
        
      })
  
  
  
  # Construct reactive matrices
  makeHotable <- function(name){
    
    nGroups <- nrow(results$columnsRF())
    
    if(name == "rfDataFrame"){
      
      if(input$asDataFrame == "none" | any(results$columnsRF() == "All")){
        
        results[[paste0(name,"Updated")]] <- NULL
        return( results[[paste0(name,"Updated")]] )
        
      } else if(input$asDataFrame == "one"){
        
        initDataFrame <- data.frame(proportionsRiskGroups = rep("", nGroups), 
            stringsAsFactors = FALSE)
        inputNames <- "Proportion"
        
      } else {
        
        initDataFrame <- data.frame(relativeRisks = rep(1, nGroups), 
            proportionsRiskGroups = rep("", nGroups), stringsAsFactors = FALSE)
        inputNames <- c("Relative risk", "Proportion")
        
      }
      
      
    } else if (name == "convenience") {
      
      initDataFrame <- data.frame(convenience = rep(1, nGroups))
      inputNames <- "Convenience"
      
    } else if (name == "gse") {
      
      initDataFrame <- data.frame(sampleSize = rep(1, nGroups))
      inputNames <- "Sample Size"
      
    }
    
    names(initDataFrame) <- inputNames
    
    if(name == "rfDataFrame"){
      
      if(!is.null(input[[name]])){
        
        if(all(inputNames %in% names(hot.to.df(input[[name]])))){
          
          if(nGroups >= nrow(hot.to.df(input[[name]]))){
            
            initDataFrame[1:nrow(hot.to.df(input[[name]])),] <- 
                hot.to.df(input[[name]])[,inputNames]
            
          } else {
            
            initDataFrame[,inputNames] <- 
                hot.to.df(input[[name]])[1:nGroups,inputNames]
            
          }  
          
        }
        
      }
      
    }
    
    if(length(inputNames) > 1){
      
      initDataFrame[,inputNames] <- 
          as.data.frame(sapply(initDataFrame[,inputNames], as.numeric))
      
    } else {
      
      initDataFrame[,inputNames] <- 
          as.numeric(initDataFrame[,inputNames])
      
    }
    
    bindedDataFrame <- cbind(results$columnsRF(), initDataFrame)
    
    results[[paste0(name,"Updated")]] <- bindedDataFrame
    
  }
  
  
  updateData <- function(name){
    
    inputNames <- switch(input$asDataFrame,
        "none" = { return(NULL) },
        "two" = { c("Relative risk", "Proportion") },
        "one" = { "Proportion" }
    )
    
    initDataFrame <- hot.to.df(input[[name]])
#    initDataFrame <- results[[paste0(name,"Updated")]]
    
    if(length(inputNames) > 1){
      
      initDataFrame[,inputNames] <- 
          as.data.frame(sapply(initDataFrame[,inputNames], as.numeric))
      
    } else {
      
      initDataFrame[,inputNames] <- 
          as.numeric(initDataFrame[,inputNames])
      
    }
    
    missingProportions <- which(is.na(initDataFrame[,"Proportion"]))
    currentSum <- sum(as.numeric(initDataFrame[,"Proportion"]), na.rm = TRUE)
    
    if(length(missingProportions) > 0) {
      
      initDataFrame[missingProportions,"Proportion"] <- 
          (1 - currentSum) / length(missingProportions)
      
    }
    
    results[[paste0(name,"Updated")]] <- initDataFrame
    
  }
  
  
  
  ##rfDataFrame
  observe({
        
        makeHotable(name = "rfDataFrame")
        
      })
  
  observeEvent(input$completeRisks,{
        
        updateData(name = "rfDataFrame")
        
      })
  
  
  output$rfDataFrame <- renderHotable({ 
        
        results$rfDataFrameUpdated
        
      }, readOnly = FALSE)  
  
  
  # Convenience
  output$convDataFrame <- renderHotable({ 
        
        makeHotable(name = "convenience")
        results$convenienceUpdated
        
      }, readOnly = FALSE)  
  
  
  # Group Sensitivity
  output$gseDataFrame <- renderHotable({ 
        
        makeHotable(name = "gse")
        results$gseUpdated
        
      }, readOnly = FALSE)
  
  
  
  
  
#Define arguments for the ribess functions
  
  results$isRiskbased <- reactive({        
        if(input$numRF >= 1) TRUE else FALSE
      }) 
  
  results$maxn <- reactive({
        if(input$convenienceSampling %in% c("no", "auto")) NA else input$maxn
      })
  
  results$isSampledMaxn <- reactive({
        if(input$convenienceSampling == "atMostNin1") FALSE else TRUE
      })  
  
  
  results$nMC <- reactive({
        
        if(any(results$isRandom())){
          
          input$nMC
          
        } else {
          
          1
          
        }
      })
  
  
  results$relativeRisks <- reactive({
        
        if(input$numRF > 0){
          
          listRisks <- makeRiskList(toExpand = 'Rr')
          
          if(input$asDataFrame != 'both'){
            
            expandRisks(listRisks)
            
          } else {
            
            values <- as.numeric(hot.to.df(input$rfDataFrame)[,"Relative risk"])
            newNames <- helpExpandNames(inputList = listRisks)
            
            if(length(newNames) == length(values)){
              
              names(values) <- newNames
              
            }
            
            values
            
          }
          
        }        
        
      })
  
  results$proportionsRiskGroups <- reactive({
        
        if(input$numRF > 0){
          
          listProportions <- makeRiskList(toExpand = 'Proportion')
          
          if(input$asDataFrame == 'none'){
            
            expandRisks(listProportions)
            
          } else {
            
            if(is.null(input$rfDataFrame)) return()
            
            values <- as.numeric(hot.to.df(input$rfDataFrame)[,"Proportion"])
            newNames <- helpExpandNames(inputList = listProportions)
            
            if(length(newNames) == length(values)){
              
              names(values) <- newNames
              
            }
            
            values
            
          } 
        }
        
      })
  
  
  
  
# Run ribess functions
  results$optimalBinom <- eventReactive(results$submitSample(), {
        
        tryCatch({
              
              if(input$toCalculate != "sensitivity"){
                
                if(input$convenienceSampling != "no"){
                  
                  validate("No results, unless 'No convenience sampling'")
                  return( NULL )
                  
                } else {
                  
                  isolate( 
                      wrapFunction(wrappedFunction = "optimizeSampleSizeGse",
                          targetAse = input$Conf, 
                          totalN = results$randomValuesN(), dp = input$DP,
                          tse = results$randomValuesTSe(),
                          method = "binom", isRiskbased = results$isRiskbased(),
                          relativeRisks = results$relativeRisks(),
                          proportionsRiskGroups = results$proportionsRiskGroups(),
                          nMC = results$nMC()) 
                  )
                  
                }                  
                
              } else {
                
                isolate( 
                    wrapFunction(wrappedFunction = "getSampleSizeGse",
                        sse = NA, 
                        n = as.numeric(hot.to.df(input$gseDataFrame)[,"Sample Size"]), 
                        totalN = results$randomValuesN(), dp = input$DP,
                        tse = results$randomValuesTSe(),
                        method = "binom", isRiskbased = results$isRiskbased(),
                        relativeRisks = results$relativeRisks(),
                        proportionsRiskGroups = results$proportionsRiskGroups(),
                        nMC = results$nMC() ) 
                )
                
              }
              
            }, error = function(err) {
              
              return(err)
              
            })
        
      })
  
  
  
  results$optimalHyper <- eventReactive(results$submitSample(), {
        
        tryCatch({
              
              if(input$toCalculate != "sensitivity"){
                
                if(input$convenienceSampling == "no") {
                  
                  isolate( 
                      wrapFunction(wrappedFunction = "optimizeSampleSizeGse",
                          targetAse = input$Conf, 
                          totalN = results$randomValuesN(), dp = input$DP,
                          tse = results$randomValuesTSe(),
                          method = "hyper", isRiskbased = results$isRiskbased(),
                          relativeRisks = results$relativeRisks(),
                          proportionsRiskGroups = results$proportionsRiskGroups(),
                          nMC = results$nMC() ) 
                  )
                  
                } else {
                  
                  isolate( 
                      wrapFunction(wrappedFunction = "getConvenience",
                          targetAse = input$Conf, 
                          inputConvenience = as.numeric(hot.to.df(input$convDataFrame)[,"Convenience"]), 
                          maxn = results$maxn(), isSampledMaxn = results$isSampledMaxn(), 
                          totalN = results$randomValuesN(), dp = input$DP,
                          tse = results$randomValuesTSe(),
                          method = "hyper", isRiskbased = results$isRiskbased(),
                          relativeRisks = results$relativeRisks(),
                          proportionsRiskGroups = results$proportionsRiskGroups(),
                          nMC = results$nMC() ) 
                  )
                  
                }
                
              } else {
                
                isolate( 
                    wrapFunction(wrappedFunction = "getSampleSizeGse",
                        sse = NA, 
                        n = as.numeric(hot.to.df(input$gseDataFrame)[,"Sample Size"]), 
                        totalN = results$randomValuesN(), dp = input$DP,
                        tse = results$randomValuesTSe(),
                        method = "hyper", isRiskbased = results$isRiskbased(),
                        relativeRisks = results$relativeRisks(),
                        proportionsRiskGroups = results$proportionsRiskGroups(),
                        nMC = results$nMC() ) 
                )
                
              }
              
            }, error = function(err) {
              
              return(err)
              
            })
        
      })
  
  
  
  
  results$summarizeInfo <- eventReactive(input$submit, {
        
        if(input$toCalculate != 'pFree'){
          
          randomParams <- c("Population size" = "populationSize",
              "Sample size" = "sampleSize", 
              "Group sensitivity" = "gse")
          selected <- randomParams[2]
          
        } else {
          
          randomParams <- c("Prior probability of freedom" = "pFreePrior",
              "Unadjusted prob. of freedom" = "pFree", 
              "Global sensitivity" = "sse",
              "Prob. of disease introduction" = "pIntro",
              "Adjusted prob. of freedom" = "pFreeAdjusted")
          selected <- randomParams[5]
          
        } 
        
        if(results$nMC() > 1){
          
          list(
              
              fluidRow(
                  column(2, p(h4("Summarize results according to:"))),
                  column(2, selectInput("drawParam", "parameter",
                          choices = randomParams, selected = selected)),
                  column(2, sliderInput("percentile", "Show table for percentile",
                          min = 0, max = 1, value = 0.5))),
              
              tags$hr()
          )
          
        } else {
          
          NULL
          
        }
        
      })
  
  
  results$parameterSelected <- reactive({
        
        if(is.null(input$drawParam)){
          
          if(input$toCalculate != "pFree"){
            
            "sampleSize"
            
          } else {
            
            "pFreePrior"
            
          }
          
        } else {
          
          input$drawParam
          
        }
      })
  
  
  results$percentile <- reactive({
        
        if(is.null(input$percentile)){
          
          0.5
          
        } else {
          
          input$percentile
          
        }
      })
  
  
  summarizeResults <- function(method){
    
    if(method != "Pfree"){
      
      randomParams <- c("Population size" = "populationSize",
          "Sample size" = "sampleSize", 
          "Group sensitivity" = "gse")
      submitValue <- results$submitSample()
      
    } else {
      
      randomParams <- c("Prior probability of freedom" = "pFreePrior",
          "Unadjusted prob. of freedom" = "pFree", 
          "Global sensitivity" = "sse",
          "Prob. of disease introduction" = "pIntro",
          "Adjusted prob. of freedom" = "pFreeAdjusted")
      submitValue <- results$submitPfree()
      
    }
    
    results[[paste0("documentation", method)]] <- eventReactive(submitValue, {
          
          validate(need(!is.null( results[[paste0("optimal", method)]]() ), "No results available"),
              need( !any(attr(results[[paste0("optimal", method)]](), "class") == "error"), 
                  (results[[paste0("optimal", method)]]())$message))
          
          
        })
    
    
    #Clear previous output
    if(!is.null(submitValue)){
      if(submitValue > 0){
        
        results[[paste0("table", method)]] <- reactive( NULL )
        output[[paste0("drawHist", method)]] <- renderUI( NULL )
        output[[paste0("commentHist", method)]] <- renderText( NULL )
        
      }
    }
    
    if(!is.null( results[[paste0("optimal", method)]]() )){
      if( !any(attr(results[[paste0("optimal", method)]](), "class") == "error")){
        
        if(results$nMC() > 1){
          
          reportedResults <- summaryWrapper( outputWrapper = results[[paste0("optimal", method)]](),
              parameterName = results$parameterSelected(), 
              percentile = results$percentile())
          
        } else {
          
          reportedResults <- results[[paste0("optimal", method)]]()[[1]]
          
        }
        
        colnames(reportedResults) <- names(randomParams)
        
        if(method != "Pfree"){
          
          rownames(reportedResults) <- NULL
          
          if(method == "Binom"){
            
            correctColumns <- 
                reportedResults[,-which(colnames(reportedResults) %in% "Population size")]
            
          } else {
            
            correctColumns <- reportedResults
            
            results$choicesInitSse <- reactive({
                  reportedResults[,which(colnames(reportedResults) %in% "Group sensitivity")]
                })
            
          }
          
          if(nrow(correctColumns) > 1){
            
            toReport <- cbind(results$columnsRF(), correctColumns)
            
          } else {
            
            toReport <- correctColumns
            
          }
          
        } else {
          
          rownames(reportedResults) <- paste0("Time period ", 1:nrow(reportedResults))
          toReport <- reportedResults
          
        }
        
        results[[paste0("table", method)]] <- reactive({
              
              list(
                  renderTable(
                        
                        toReport
                        
                      ),
                  
                  renderText({
                        
                        if(method != "Pfree"){
                          
                          paste("Total sample size:", 
                              sum(reportedResults$sampleSize))
                          
                        } else {
                          
                          NULL
                          
                        }
                        
                      }),
                  
                  renderText({
                        
                        if(method != "Pfree"){
                          
                          paste("Global sensitivity:", 
                              round(attr(reportedResults, "ase"), 2) )
                          
                        } else {
                          
                          NULL
                          
                        }
                        
                      }),
                  
                  downloadButton(paste0("downloadTable", method), "Download")
              )
              
            })
            
            # Download data table
            output[[paste0("downloadTable", method)]] <- downloadHandler(
                filename = function() { paste0("ribess", method, ".csv") },
                content = function(file) {
                  write.csv(toReport, file)
                })
        
        
        if( results$nMC() > 1 ){
          
          results[[paste0("dataPlot", method)]] <- reactive({
                
                tryCatch({
                      
                      nGroups <- max(1, nrow(results[[paste0("optimal", method)]]()[[1]]))
                      
                      outputValues <- matrix(sapply(results[[paste0("optimal", method)]](), 
                              function(x) x[[ results$parameterSelected() ]]), nrow = nGroups)
                      
                      if(method != "Pfree" & nrow(outputValues) > 1){
                        outputValues <- rbind(outputValues, apply(outputValues, 2, sum))
                      } 
                      
                      densityValues <- apply(outputValues, 1, 
                          function(x){
                            if(length(unique(x)) == 1) {
                              data.frame(x = rep(unique(x), 2), y = c(0,NA))
                            } else {
                              density(x, na.rm = TRUE)
                            }
                          })
                      
                      yValues <- lapply(densityValues, function(element) element$y)
                      xValues <- unlist(lapply(densityValues, function(element) element$x))
                      groupSize <- sapply(yValues, length)
                      yValues <- unlist(yValues)
                      integratedValues <- unlist(lapply(densityValues, function(element){
                                nLength <- length(element$y)
                                c(0, cumsum((element$y[-1] + element$y[-nLength]) * 
                                            (element$x[-1] - element$x[-nLength]) / 2))
                              }))
                      
                      if(any(is.na(yValues))){
                        
                        yValues[which(is.na(yValues))] <- max(yValues, na.rm = TRUE)
                        
                      }
                      
                      if(nGroups > 1){
                        if(method != "Pfree"){
                          groupNames <- c(paste("Risk group", 1:nGroups), "Total")
                        } else {
                          groupNames <- paste("Time period", 1:nGroups) 
                        }
                        
                      } else {
                        groupNames <- "Total"
                      }
                      group <- rep(groupNames, times = groupSize)
                      
                      return( 
                          data.frame(yValues = yValues, xValues = xValues, group = group,
                              integratedValues = round(integratedValues, 2))
                      ) 
                      
                    }, error = function(err) {
                      
                      return(NULL)
                      
                    })
                
              })
          
          
         output[[paste0("drawHist", method)]] <- renderChart2({
                
                plotRandom <- tryCatch({
                      
                      plotRandom <- nPlot( integratedValues ~ xValues, 
                          data = results[[paste0("dataPlot", method)]](), 
                          group = "group", type = "lineChart")
                      
                      plotRandom$xAxis(axisLabel = names(randomParams)[which(randomParams == results$parameterSelected())],
                          tickFormat = "#! function(d) {return d3.format(',.2f')(d)} !#")
                      plotRandom$yAxis(tickFormat = "#! function(d) {return d3.format(',.3f')(d)} !#")
                      
                      plotRandom$save(file.path(tempdir(), paste0("plot", method, ".html")),
                          standalone = TRUE)
                      #Area under the curve: seriously slows down -> not shown
#                plotRandom$chart(tooltipContent = "#! function(key, x, y, e){
#                        return '<h3>' + key + '</h3>' + 
#                        '<p>' 'Area under the curve: ' + e.point.integratedValues + ' in ' + x + '</p>'
#                        } !#")
                      
                      plotRandom                      
                      
                    }, error = function(err) {
                      
                      NULL
                      
                    })
                
              })
          
          output[[paste0("downloadPlot", method)]] <- downloadHandler(
              paste0("ribess", method, ".html") ,
              content = function(file) {
                file.copy(file.path(tempdir(), paste0("plot", method, ".html")),
                    file)
              }
          )
          
          
          
          output[[paste0("commentHist", method)]] <- renderText({
                
                tryCatch({
                      
                      dataDuplicated <- data.frame(matrix(results[[paste0("dataPlot", method)]]()$yValues, 
                              ncol = length(unique(results[[paste0("dataPlot", method)]]()$group))))
                      
                      idDuplicated <- which(duplicated(t(dataDuplicated)) | 
                              duplicated(t(dataDuplicated), fromLast = TRUE))
                      
                      if(length(idDuplicated) > 1){
                        
                        return( paste("Warning: The lines coincide for",
                                paste(unique(results[[paste0("dataPlot", method)]]()$group)[idDuplicated], 
                                    collapse = ", ")) )    
                        
                      } else {
                        
                        return( NULL )
                        
                      }
                      
                    }, error = function(err) {
                      
                      return(NULL)
                      
                    })
                
              })
          
        }
        
      }
    }
    
  }
  
  
# Output UI
  output$resultsTable <- renderUI({
        
        results$submitSample()
        
        isolate({
              
              summarizeResults(method = "Binom")
              summarizeResults(method = "Hyper")
              
              figuresList <- list(
                  binom = tryCatch({
                        
                        showOutput("drawHistBinom", "nvd3")
                        
                      }, error = function(err) {
                        
                        NULL
                        
                      }),
                  
                  hyper = tryCatch({
                        
                        showOutput("drawHistHyper", "nvd3")
                        
                      }, error = function(err) {
                        
                        NULL
                        
                      })
              )
              
              list(
                  
                  renderUI(results$summarizeInfo()),
                  
                  fluidRow(
                      column(6, 
                          list(
                              
                              h3("Infinite population"),
                              renderUI(results$documentationBinom()),
                              renderUI(results$tableBinom())
                          )
                      ), 
                      
                      column(6, 
                          list(
                              
                              h3("Finite population"),
                              renderUI(results$documentationHyper()),
                              renderUI(results$tableHyper())
                          )
                      )
                  ),
                  
                  
                  if( (results$nMC() > 1) ){
                        
                        list(
                            
                            tags$br(), 
                            tags$br(),    
                            
                            fluidRow(column(6, 
                                    list(
                                        figuresList[["binom"]],
                                        textOutput("commentHistBinom"),
                                        downloadButton("downloadPlotBinom", "Download")
                                    )
                                ),
                                column(6, 
                                    list(
                                        figuresList[["hyper"]],
                                        textOutput("commentHistHyper"),
                                        downloadButton("downloadPlotHyper", "Download")
                                    )
                                )
                            ),                                
                            
                            tags$br()  
                        )
                        
                      } else {
                        
                        tags$br()
                        
                      }
              
              )
              
            })
        
      })
  
  
  output$warnings <- renderUI({
        
        if(input$toCalculate != "pFree"){
          
          if(input$randomN == 'fixed'){
            validate(need( input$N,
                    "Please provide valid input value for 'Population size'"))
          }
          
          if(input$randomTSe == 'fixed'){
            validate(need( input$TSe,
                    "Please provide valid input value for 'Test sensitivity'"))
          }
          
          validate(need( input$DP,
                  "Please provide valid input value for 'Design prevalence'"))
          
          
          if(input$numRF > 0){
            
            if(!is.null(results$proportionsRiskGroups())){
              
              if(input$randomProportion == "fixed"){
                
                validate(need( !any(is.na(results$proportionsRiskGroups())),
                        "Please provide valid input values for 'Proportion': no missing values are allowed."))
                
                validate(need( round(sum(results$proportionsRiskGroups()), 2) == 1,
                        "Please provide valid input values for 'Proportion': the sum should be 1 per risk factor."))
                
              } else {
                
                validate(need( !any(apply(as.matrix(results$proportionsRiskGroups()), 1, is.na)),
                        "Please provide valid input values for 'Proportion': no missing values are allowed."))
                
                validate(need( all(round(apply(as.matrix(results$proportionsRiskGroups()), 1, sum), 2) == 1),
                        "Please provide valid input values for 'Proportion': the sum should be 1 per risk factor"))
                
              }
            }
            
            if(!is.null(results$relativeRisks())){
              
              if(input$randomRr == "fixed"){
                
                validate(need( !any(is.na(results$relativeRisks())),
                        "Please provide valid input values for 'Relative risk': no missing values are allowed."))
                
                if(input$asDataFrame != 'none'){
                  
                  validate(need( sum(results$relativeRisks() == 1) == 1,
                          "Please provide valid input values for 'Relative risk': exactly one risk factor combination can have a relative risk of 1."))
                  
                } else {
                  
                  validate(need( sum(results$relativeRisks() == 1) == 1,
                          "Please provide valid input values for 'Relative risk': per risk factor exactly one level can have a relative risk of 1."))
                  
                }
                
              } else {
                
                validate(need( !any(apply(as.matrix(results$relativeRisks()), 1, is.na)),
                        "Please provide valid input values for 'Relative risk': no missing values are allowed."))
                
                for(i in 1:input$numRF){
                  
                  local({
                        
                        i <- i
                        distributionTmp <- c()
                        
                        for(j in 1:input[[paste0('numLevels',i)]]) {
                          
                          distributionTmp[j] <- input[[paste0("distributionRr", i,'_',j)]]
                          
                        }
                        
                        validate(need( sum(distributionTmp == "reference") == 1,
                                "Please define exactly one reference relative risk per risk factor"))
                        
                      }) 
                }
              }
            }
            
          }
          
        } else {
          
          validate(need(!is.na(results$initSse()), 
                  "Please provide valid input value for 'Global sensitivity in time period 1'"), 
              need(!any(is.na(results$randomValuesPfree0())), 
                  "Please provide valid input value for 'Prior probability of freedom'"),
              need(!any(is.na(results$randomValuesPintro())), 
                  "Please provide valid input values for 'Probability of disease introduction'"))
        }
        
        validate(need(results$nMC(), 
                "Please provide valid input value for 'Number of random values'"))
        
        
        list(
            busyIndicator("In progress",wait = 0),
            actionButton(inputId = "submit", label = "Submit", 
                styleclass="primary", size="mini")
        )
        
        
      })
  
  
  
  
  
  
#Calculate pFree
  
  
  output$pIntroTable <- renderUI({
        
        validate(need(input$nPeriods, "Please provide a value for 'Number of time periods'"))
        
        pTimes <- list()
        
        for (i in seq_len(input$nPeriods)) {
          
          pTimes[[i]] <- fluidRow(
              
              column(3, selectInput(paste0("randomPintro", i), label = paste0('Time period ', i), 
                      choices = c("fixed",
                          "random - enter distribution quantiles" = "randomKnownQ",
                          "random - enter distribution parameters" = "randomKnownPar")),
                  offset = 1),
              
              column(3,
                  
                  conditionalPanel(paste0("input.randomPintro", i, " == 'fixed'"),
                      numericInput(inputId = paste0('Pintro', i), label = "Value",
                          value = 0.2)
                  ),
                  
                  conditionalPanel(paste0("input.randomPintro", i, " == 'randomKnownQ'"),
                      list(matrixInput(inputId = paste0("qPintro", i), label = "Probability - Quantile", 
                              data = quantileStart),
                          uiOutput(paste0("qPintro", i, "Info"))
                      )
                  ),
                  
                  conditionalPanel(paste0("input.randomPintro", i, " == 'randomKnownPar'"),
                      list(
                          selectInput(paste0("distributionPintro", i), label = "Distribution",
                              choices = c("uniform" = "unif", "normal" = "norm", "beta", "gamma")),
                          uiOutput(paste0("parametersTablePintro", i))
                      )
                  )
              
              ),
              
              column(4, conditionalPanel(paste0("input.randomPintro", i, " != 'fixed'"),
                      plotOutput(paste0("plotPintro", i))
                  )
              )
          )
          
        }
        
        pTimes
        
      })
  
  
  results$randomValuesPintro <- reactive({
        
        local({
              
              randomValues <- matrix(NA, nrow = results$nMC(), ncol = input$nPeriods)
              isFixed <- rep(FALSE, input$nPeriods)
              
              for(i in 1:input$nPeriods) {
                
                i <- i
                iName <- results$pIntroNames()[i]
                
                if(input[[paste0('random', iName)]] == "fixed"){
                  
                  randomValues[,i] <- rep(results[[paste0("randomValues", iName)]](), times = results$nMC())
                  isFixed[i] <- TRUE
                  
                } else {
                  
                  randomValues[,i] <- results[[paste0("randomValues", iName)]]()
                  
                }
              }
              
              if(all(isFixed)) {
                
                randomValues[1,]
                
              } else {
                
                attr(randomValues, "random") <- TRUE
                randomValues
                
              }
              
            })
        
      })
  
  results$initSse <- reactive({
        
        if(!input$sampleFromResults){
          
          input$initSse
          
        } else {
          
          results$selectedInitSse()
          
        }
        
      })
  
  
  
  results$optimalPfree <- eventReactive(results$submitPfree(), { 
        
        tryCatch({
              isolate( 
                  wrapFunction( wrappedFunction = "calculatePfree",
                      targetPFree = input$Conf, 
                      pFreePrior = results$randomValuesPfree0(),
                      pIntro = results$randomValuesPintro(), 
                      sse = results$initSse(), 
                      timePoints = input$nPeriods,
                      nMC = results$nMC() ))
            },  error = function(err) {
              
              return(err)
              
            })
        
      })
  
  
  
  
  
  output$resultsPfree <- renderUI({
        
        results$submitPfree()
        
        isolate({
              
              summarizeResults(method = "Pfree")
              
              list(
                  
                  renderUI(results$summarizeInfo()),
                  
                  renderUI(results$documentationPfree()),
                  
                  renderUI(results$tablePfree()),
                  
                  
                  if( (results$nMC() > 1) ){
                        
                        list(
                            
                            tags$br(), 
                            tags$br(),    
                            
                            showOutput("drawHistPfree", "nvd3"),
                            
                            textOutput("commentHistPfree"),
                            downloadButton("downloadPlotPfree", "Download"),
                            
                            tags$br()  
                        )
                        
                      } else {
                        
                        tags$br()
                        
                      }
              )
              
            })
        
      })
  
  
  output$riskGroupNames <- renderUI({
        
        inputList <- makeRiskList(toExpand = 'names')
        riskGroupsNames <- helpExpandNames(inputList = inputList, toPaste = TRUE)
        
        if(length(riskGroupsNames) == 0){
          
          NULL  
          
        } else {
          
          selectInput("selectInitSse", "for selected risk group:",
              choices = riskGroupsNames)
          
        }
        
      })
  
  # TODO Option <all> to calculate for all risk groups at once?
  
  results$selectedInitSse <- reactive({
        
        inputList <- makeRiskList(toExpand = 'names')
        riskGroupsNames <- helpExpandNames(inputList = inputList, toPaste = TRUE)
        
        if(length(riskGroupsNames) == 0){
          
          results$choicesInitSse()
          
        } else {
          
          selectedInitSse <- results$choicesInitSse()[which(riskGroupsNames == input$selectInitSse)]
          selectedInitSse
          
        }        
        
      })
  
  
  output$selectedInitSse <- renderText({
        
        
        selectedInitSse <- tryCatch({
              
              round(results$selectedInitSse(), 2)
              
            }, error = function(err) {
              
              return("No values available")
              
            })
        
        return(selectedInitSse)
        
      })
  
  
}

serverFunction
