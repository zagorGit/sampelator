# Project: sampelator_git
# 
# Author: mvarewyck
###############################################################################

# To debug
output$print <- renderPrint({  
      
      
      
    })


# Read data
results$loadedData <- reactive({
      
      # input$dataCountry will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$dataCountry
      
      if (is.null(inFile))
        return(NULL)
      
      read.table(inFile$datapath, header = TRUE, sep = input$sep, 
          quote = '"', stringsAsFactors = FALSE)
      
      
    })

results$collapsedData <- reactive({
      
      if(is.null(input$collapseData))
        return( NULL )
      
      
      if(input$collapseData == "<none>"){
        
        return( results$loadedData() )
        
      } else {
        
        return( collapseData(results$loadedData(), input$collapseData) )
        
      }
      
    })

results$columnsCollapsed <- reactive({
      
      colnames(results$collapsedData())[-1]
      
    })

output$collapseDataUI <- renderUI({
      
      selectInput("collapseData", "Collapse loaded data according to:",
          choices = c("<none>", colnames(results$loadedData())[-1]))
      
    })


output$tableChoices <- renderUI({
      
      list(
          h4("Define sampling procedure"),
          selectInput("samplingVariables", "Convenience sampling for:",
              choices = results$columnsCollapsed(), multiple = TRUE)
      )
      
    })


output$figureChoices <- renderUI({
      
      selectInput("summaryName", "Show results according to:",
          choices = results$columnsCollapsed())
      
    })



# Define convenience matrices
createHotable <- function(iName){
  
  if(is.null(iName) | is.null(results$totalN()) |
      is.null(results$collapsedData())){
    return( NULL )
  }
  
  if(length(results$totalN()) < 1)
    return( NULL )
  
  
  originalTable <- table(results$collapsedData()[,iName])
  originalSizes <- data.frame(as.numeric(originalTable))
  originalProportions <- data.frame(as.numeric(prop.table(originalTable)))
  
  
  if(input$sampleFromResults){
    
    if(all(names(originalTable) %in% rownames(results$sampleAllocationMin()))){
      
      results$stratumName <- iName
      
    }
    
  }
  
  
  initTable <- hot.to.df(input[[paste0(iName, "Table")]])
  
  
  if(iName %in% input$samplingVariables){
    
    condition <- FALSE
    if(is.null(initTable)){
      condition <- TRUE
    } else if(ncol(initTable) != 6){
      condition <- TRUE
    }
    
    if(condition){
      
      initTable <- cbind(data.frame(names(originalTable), stringsAsFactors = FALSE),
          100*originalProportions, originalSizes, 
          convenience = 1, 100*originalProportions, 
          ceiling(originalProportions*results$totalN()))
      
    } else {
      
      sampledProportions <- (as.numeric(initTable[,4])/
            sum(as.numeric(initTable[,4]), na.rm = TRUE))
      sampledSizes <- sampledProportions*results$totalN()
      
      initTable[,5] <- 100*sampledProportions
      initTable[,6] <- ceiling(sampledSizes)
      
    }
    
  } else if(is.character(results$stratumName)){
    
    if(iName == results$stratumName & input$sampleFromResults & 
        !is.null(input$copyN)){
      
      if(input$samplingDesign == "getStratified"){
        
        indexSize <- which(sapply(results$allSampling(), 
                function(x) x$designStatistics[,"sampleSize"]) == input$copyN)
        
        if(length(indexSize) == 1){
          
          numberToSample <- data.frame(sapply(results$allSampling(), 
                  function(x) x$sampleAllocation[,"stratumSize"])[,indexSize])
          
        } else {
          
          return( NULL )
          
        }
        
      } else if(input$samplingDesign == "getTwoStage"){
        
        indexSize <- which(sapply(results$allSampling(), 
                function(x) sum(x$sampleAllocation[,"numberClusters"])) == input$copyN)
        
        if(length(indexSize) == 1){
          
          numberToSample <- data.frame(sapply(results$allSampling(), 
                  function(x) x$sampleAllocation[,"numberClusters"])[,indexSize])
          
        } else {
          
          return( NULL )
        }
        
      } else {
        
        numberToSample <- input$copyN
        
      }     
      
      initTable <- cbind(data.frame(names(originalTable), stringsAsFactors = FALSE),
          100*originalProportions, originalSizes, 
          convenience = 1, 100*(numberToSample/sum(numberToSample)), numberToSample)
      
    } else {
      
      initTable <- cbind(data.frame(names(originalTable), stringsAsFactors = FALSE),
          100*originalProportions, originalSizes, 
          convenience = 1, 100*originalProportions, 
          ceiling(originalProportions*results$totalN()))
      
    }
    
  } else {
    
    initTable <- cbind(data.frame(names(originalTable), stringsAsFactors = FALSE),
        100*originalProportions, originalSizes, 
        convenience = 1, 100*originalProportions, 
        ceiling(originalProportions*results$totalN()))
    
  }
  
  colnames(initTable) <- c(iName, "Original proportion (%)", "Original size", 
      "Convenience", "Proportion to sample (%)", "Number to sample")
  results[[paste0(iName, "Table")]] <- initTable
  
}

observe({
      
      for(iName in results$columnsCollapsed()){
        
        local({
              
              iName <- iName
              createHotable(iName)
              
              output[[paste0(iName,"Table")]] <- renderHotable({
                    
                    results[[paste0(iName,"Table")]]
                    
                  }, readOnly = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
              
            })
      }
      
    })



output$defineConvenience <- renderUI({
      
      tableList <- lapply(input$samplingVariables, function(iName) {
            list(
                hotable(paste0(iName, "Table")),
                tags$br()
            )
          })
      
      do.call(tagList, tableList)
      
    })


output$convenienceSizes <- renderUI({
      
      if(!is.null(results$originalN()) && !is.null(results$sampleN())){
        
        validate(need(!any(is.na(results$sampleN())), 
                "Please provide a value for the estimated number to sample"))
        
        
        if(nrow(results$originalN()) == nrow(results$sampleN())){
          
          outputTable <- cbind(results$originalN(), results$sampleN()[,1])
          names(outputTable)[ncol(outputTable)] <- "Number to sample"
          outputTable <- rbind(outputTable, c(rep(NA, times = ncol(outputTable) - 2),
                  sum(results$originalN()[,"Original size"]), 
                  sum(results$sampleN()[,1])))
          
          list(
              
              h4("Number to sample per group"),
              DT::renderDataTable({
                    
                    invalid <- (outputTable[,"Original size"] <
                          outputTable[,"Number to sample"])
                    
                    if(sum(invalid) > 0){
                      
                      outputTable$Warning <- ifelse(invalid, 
                          "Number to sample too large", "")
                      
                      datatable(outputTable, rownames = FALSE, options = list(
                                  lengthMenu = list(c(100, -1), c('100', 'All')),
                                  pageLength = 100)) %>%
                          
                          formatStyle("Warning",
                              backgroundColor = styleEqual(
                                  c("Number to sample too large", ""),
                                  c("red", "white")))
                      
                    } else {
                      
                      datatable(outputTable, rownames = FALSE, options = list(
                              lengthMenu = list(c(100, -1), c('100', 'All')),
                              pageLength = 100))
                      
                    }
                    
                  })
          )
          
        }
        
      }
      
    })




# Sample data
results$selectedSample <- eventReactive(input$collectSample, {
      
      isolate({
            
            tryCatch({
                  
                  if(nrow(results$sampleN()) == 1){
                    
                    selectSample(data = results$collapsedData(), seed = input$seed, 
                        size = results$sampleN()[,"numberToSample"])
                    
                  } else {
                    
                    covariates <- names(results$sampleN())[-1]
                    selectedSample <- c()
                    
                    for(iLevel in 1:nrow(results$sampleN())){
                      
                      toSelect <- paste(paste0(covariates, "== '", 
                              results$sampleN()[iLevel,-1], "'"), collapse = " & ")
                      subData <- subset(results$collapsedData(), eval(parse(text = toSelect)))
                      
                      subSample <- selectSample(data = subData,
                          seed = input$seed,  
                          size = results$sampleN()[iLevel,"numberToSample"])
                      selectedSample <- rbind(selectedSample, subSample)
                      
                    }
                    
                    selectedSample
                    
                  }
                  
                }, error = function(err) {
                  
                  return(err)
                  
                })
            
          })
      
    })


output$dataCountry <- DT::renderDataTable({
      
      validate(need(results$collapsedData(), "No data loaded"))
      
      DT::datatable(results$collapsedData(), options = list(
              lengthMenu = list(c(15, 100, -1), c('15', '100', 'All')),
              pageLength = 15))
      
    })


output$samplingResults <- renderUI({
      
      validate(need(results$allSampling(), "No sampling results available") %then%
              need(input$submit > 0 & !any(attr(results$allSampling(), "class") == "error"), 
                  "No sampling results available"))
      
      if(input$purpose == 'estimation'){
        
        documentation <- "Detectable difference: half width of the confidence interval"
        
      } else {
        
        documentation <- "Detectable difference: true difference in means that is tested for"
        
      }
      
      tableDesignStatistics <- renderTable({
            
            if(is.null(results$designStatistics()))
              return( NULL )
            
            toReport <- results$designStatistics()
            validate(need(!any(is.na(toReport)), 
                    "No results are shown: Please check whether valid input values are provided for all 'Design-specific parameters'."))
            toReport
            
          }, caption = documentation,
          caption.placement = getOption("xtable.caption.placement", "bottom"),
          digits = 3)
      
      
      if(length(results$allSampling()) > 1){
        
        captionMin <- paste0("Table for minimum of '", 
            names(generalNames)[which(generalNames == input$range)], "'")
        
        captionMax <- paste0("Table for maximum of '", 
            names(generalNames)[which(generalNames == input$range)], "'")
        
      } else {
        
        captionMin <- ""
        captionMax <- ""
        
      }
      
      sampleAllocationMax <- renderUI({
            
            if(is.null(results$sampleAllocationMax()))
              return( NULL )
            
            list(
                
                renderTable({
                      
                      results$sampleAllocationMax()
                      
                    }, caption = captionMax,
                    caption.placement = getOption("xtable.caption.placement", "top"), 
                    caption.width = getOption("xtable.caption.width", NULL),
                    digits = 3),
                
                downloadButton("downloadMax", "Download")
            
            )
            
          })
      
      
      sampleAllocationMin <- renderUI({
            
            if(is.null(results$sampleAllocationMin()))
              return( NULL )
            
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
                
                downloadButton("downloadMin", "Download")
            
            )
          })
      
      return(list(h3("General parameters"),
              tableDesignStatistics,
              downloadButton("downloadDesignStatistics", "Download"),
              sampleAllocationMin,              
              sampleAllocationMax)
      )
      
    })

output$downloadDesignStatistics <- downloadHandler(
    filename = function() { "designStatistics.csv" },
    content = function(file) {
      write.csv(results$designStatistics(), file, row.names = FALSE)
    })

output$downloadMin <- downloadHandler(
    filename = function() { "sampleAllocationMin.csv" },
    content = function(file) {
      write.csv(results$sampleAllocationMin(), file, row.names = FALSE)
    })

output$downloadMax <- downloadHandler(
    filename = function() { "sampleAllocationMax.csv" },
    content = function(file) {
      write.csv(results$sampleAllocationMax(), file, row.names = FALSE)
    })

output$stratumName <- renderUI({
      
      if(input$samplingDesign %in% c("getStratified", "getTwoStage")) {
        
        validate(need(results$sampleAllocationMin(), "No results available"))
        
        selectInput("stratumSample", "Stratum name to sample",
            choices = c("<all>", rownames(results$sampleAllocationMin())))
        
      }
      
    })


output$selectN <- renderUI({
      
      validate(need(results$allSampling(), "No results available"))      
      
      message <- ""
      
      
      
      if(input$samplingDesign %in% c("getStratified", "getTwoStage")) {
        
        strataChoices <- rownames(results$sampleAllocationMin())
        
        if(is.null(input$stratumSample)){
          
          indexStratum <- 1
          
        } else if (input$stratumSample != "<all>") {
          
          indexStratum <- which(input$stratumSample == strataChoices)
          
        } else {
          
          indexStratum <- NULL
          
        }
        
        if(input$samplingDesign == "getStratified"){
          
          toSelect <- c("number of samples" = "sampleSize")
          
        } else {
          
          toSelect <- c("number of clusters" = "numberClusters")
          
        }
        
        if(is.null(indexStratum)){
          
          if(input$samplingDesign == "getStratified"){
            
            sampleChoices <- sapply(results$allSampling(), 
                function(x) x$designStatistics[,toSelect])
            
          } else {
            
            sampleChoices <- sum(sapply(results$allSampling(), 
                    function(x) x$sampleAllocation[,toSelect]))
            
          }
          
          label <- paste("Total estimated", names(toSelect))    
          
          if(is.character(results$stratumName)){
            
            if(is.null(input$samplingVariables)){
              
              message <- paste0("Note: For each stratum of '", results$stratumName,
                  "' the estimated ", names(toSelect), ", as shown in 'View Estimated General Parameters', will be sampled")
              
            } else if(!results$stratumName %in% input$samplingVariables){
              
              message <- paste0("Note: For each stratum of '", results$stratumName,
                  "' at least the estimated ", names(toSelect), ", as shown in 'View Estimated General Parameters', will be sampled")
              
            }
            
          }
          
        } else {
          
          if(input$samplingDesign == "getStratified"){
            
            sampleChoices <- sapply(results$allSampling(), 
                function(x) x$sampleAllocation[indexStratum,"stratumSize"])
            
          } else {
            
            sampleChoices <- sapply(results$allSampling(), 
                function(x) x$sampleAllocation[indexStratum,"numberClusters"])
            
          }
          
          label <- paste("Estimated", names(toSelect))
          
        }
        
      } else if(input$samplingDesign == "getClustered") {
        
        sampleChoices <- sapply(results$allSampling(), 
            function(x) x$sampleAllocation[,"numberClusters"])
        label <- "Estimated number of clusters"
        
        
      } else {
        
        sampleChoices <- sapply(results$allSampling(), 
            function(x) x$sampleSize)
        label <- "Estimated number of samples"
        
      }
      
      
      list(
          p(em(message)),
          selectInput("copyN", label = label, choices = sampleChoices)
      )
      
    })


results$totalN <- reactive({
      
      if(input$sampleFromResults){
        
        suppressWarnings(as.numeric(input$copyN))
        
      } else {
        
        input$N
        
      }
      
    })



results$sampleN <- reactive({
      
      if(is.null(input$samplingVariables)){
        
        if(is.character(results$stratumName)){
          
          toSample <- cbind(results[[paste0(results$stratumName, "Table")]][,"Number to sample"],
              data.frame(results[[paste0(results$stratumName, "Table")]][,1], stringsAsFactors = FALSE))
          names <- c("numberToSample", results$stratumName)
          
          
        } else {
          
          toSample <- data.frame(numberToSample = results$totalN())
          names <- c("numberToSample")
          
        }
        
      } else {
        
        if(input$sampleFromResults){
          
          variableNames <- unique(c(results$stratumName, input$samplingVariables))
          
        } else {
          
          variableNames <- input$samplingVariables
          
        }        
        
        totalPercentage <- sapply(variableNames, function(x)
              results[[paste0(x, "Table")]][,"Proportion to sample (%)"]/100)
        totalSize <- ceiling(apply(expand.grid(totalPercentage), 1, prod)*
                results$totalN())
        totalNames <- sapply(variableNames, function(x)
              results[[paste0(x, "Table")]][,1])
        
        toSample <- cbind(totalSize, 
            expand.grid(totalNames, stringsAsFactors = FALSE))
        names <- c("numberToSample", variableNames)      
        
      }
      
      
      if(ncol(toSample) == length(names)){
        
        names(toSample) <- names
        
      }
      
      toSample
      
    })


results$originalN <- reactive({
      
      if(is.null(results$collapsedData()))
        return( NULL )
      
      
      variableNames <- unique(c(results$stratumName, input$samplingVariables))
      
      if(!is.null(variableNames)) {
        
        originalN <- data.frame(table(subset(results$collapsedData(), select = variableNames)),
            stringsAsFactors = FALSE)
        
      } else {
        
        originalN <- data.frame(nrow(results$collapsedData()))
        
      }
      
      names(originalN) <- c(variableNames, "Original size")
      originalN
      
    })



output$sampledCenters <- DT::renderDataTable({
      
      DT::datatable(results$selectedSample(), options = list(
              lengthMenu = list(c(25, 100, -1), c('25', '100', 'All')),
              pageLength = 25))
      
    })

# Representativeness of sample
results$simulatedSamples <- eventReactive(input$collectSample, {
      
      isolate({
            
            simulateSampling(data = results$collapsedData(), seed = input$seed, 
                size = sum(results$sampleN()$numberToSample), nSimulations = 100)
            
          })
      
    })

output$summarizeSampling <- renderPlot({
      
      if(is.null(input$summaryName)){
        
        varName <- results$columnsCollapsed()[1]
        
      } else {
        
        varName <- input$summaryName
        
      }
      
      tryCatch({
            
            summarizeSampling(selectedSample = results$selectedSample(), 
                simulatedSamples = results$simulatedSamples(), 
                varName = varName)
            
          }, error = function(err) {
            
            return(err)
            
          })
      
      
    }, height = 1000)

# Buttons at the bottom

output$collectSample <- renderUI({
      
      validate(need( results$totalN(),
              "Please provide valid input for 'Number to sample'"))
      
      validate(need( results$collapsedData(),
              "Please Load Data to select sample"))
      
      
      if(is.null(input$samplingVariables)){
        
        validate(need(nrow(results$collapsedData()) >= results$sampleN(),
                "The number to sample is larger than the original size"))
        
      } else {
        
        invalid <- which(results$originalN()[,"Original size"] < results$sampleN()$numberToSample)
        validate(need(length(invalid) == 0,
                "The number to sample is larger than the original size for some groups"))
        
      }      
      
      output$message <- renderText( NULL )
      difference <- sum(results$sampleN()$numberToSample) - results$totalN()
      
      if(!is.na(difference)){
        
        if(difference > 0){
          
          output$message <- renderText({
                "Note: The number of sampling units will be larger than the estimated number to sample."
              }) 
          
        } else if(difference < 0){
          
          output$message <- renderText({
                "Note: The number of sampling units will be smaller than the estimated number to sample."
              }) 
          
        }
        
      }
      
      list(
          busyIndicator("In progress", wait = 0),
          textOutput("message"),
          tags$br(),
          actionButton(inputId = "collectSample", label = "Select sample",
              styleclass="primary", size="mini")
      )
      
    })

output$downloadSample <- renderUI({
      
      validate(need(results$selectedSample(), "No data to download"))
      downloadButton('downloadData', 'Download')
      
    })


output$downloadData <- downloadHandler(
    
    filename = function() {
      paste0("results_", (input$dataCountry)$name)
    },
    
    content = function(file) {
      write.table(results$selectedSample(), file, sep = input$sep,
          row.names = FALSE)
    }
)

