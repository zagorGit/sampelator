library(shinyIncubator)  #for matrixInput
library(shinysky)  #for hotable


quantileStart <- data.frame(matrix(c(0.25, 0.75, "", ""), nrow = 2, ncol = 2))

fluidPage(
    
    helpText(
        h5(a(href="https://efsa-models.openanalytics.eu/projects/ribess/wiki", 
                target="_blank", "About"), align = "right"),
        h5(a(href="https://efsa-models.openanalytics.eu/projects/ribess/issues/new", 
                target="_blank", "Report new issue"), align = "right")),
    
  
    titlePanel(title = div(img(src = "EFSA_logo.JPG", 
                float = "top", height = "60px", hspace = "50px"),
            "RiBESS+"), windowTitle = "RiBESS+"),
    
   
    fluidRow(
        
        column(4,
            
            wellPanel(
                
                selectInput(inputId = "toCalculate", label = "What would you like to estimate?", 
                    choices = list("Sample Size" = "sampleSize", 
                        "Global (and Group) Sensitivity" = "sensitivity",
                        "Probability of Freedom from Disease" = "pFree")),
                
                conditionalPanel("input.toCalculate != 'sensitivity'",
                    
                    sliderInput('Conf','Target confidence of freedom',
                        0.95,min=0.01,max=0.99,step=0.01)
                
                ),
                
                conditionalPanel("input.toCalculate == 'sampleSize'",
                    
                    list(
                        
                        fluidRow(column(6,
                                selectInput("convenienceSampling", "Convenience sampling approach", 
                                    choices = list("No convenience sampling" = "no",
                                        "Convenience" = "auto",
                                        "Convenience with lower limit" = "sampleNin1",
                                        "Convenience with upper limit" = "atMostNin1"))
                            ), 
                            column(6, 
                                conditionalPanel("input.convenienceSampling == 'sampleNin1' || input.convenienceSampling == 'atMostNin1'",
                                    
                                    numericInput("maxn","Limit", 0)
                                
                                )
                            )
                        ),
                        
                        conditionalPanel("input.convenienceSampling == 'auto'",
                            
                            p(em("The number sampled per group is proportional to the convenience value"))
                        
                        ),
                        
                        conditionalPanel("input.convenienceSampling == 'sampleNin1'",
                            
                            p(em("At least 'Limit' samples are taken in groups with convenience different from 0. 
                                        The remaining samples are taken proportional to the convenience values larger than 1."))
                        
                        ),
                        
                        conditionalPanel("input.convenienceSampling == 'atMostNin1'",
                            
                            p(em("The number sampled per group is proportional to the convenience value.
                                        At most 'Limit' samples are taken in groups with convenience 1."))
                        
                        ),
                        
                        conditionalPanel("input.convenienceSampling != 'no'",
                            
                            hotable("convDataFrame")
                        
                        )
                    
                    
                    )
                
                ),
                
                conditionalPanel("input.toCalculate == 'sensitivity'",
                    
                    hotable("gseDataFrame")
                
                ),
                
                conditionalPanel("input.toCalculate == 'pFree'",
                    
                    list(
                        
                        p(strong("Global sensitivity in time period 1")),
                        
                        conditionalPanel("input.sampleFromResults",
                            
                            list(
                                textOutput("selectedInitSse"),
                                uiOutput("riskGroupNames")
                            )
                        
                        ),
                        
                        conditionalPanel("!input.sampleFromResults",
                            
                            numericInput("initSse", "", value = 0.95)
                        
                        ), 
                        
                        checkboxInput("sampleFromResults", 
                            "Copy from Estimation Results")
                    
                    )
                
#                    sliderInput('initSse', 'Global sensitivity in time period 1', 0.95, min=0.01, max=0.99, step=0.01)
                
                ),
                
                uiOutput("isRandom")
            
            )
        
        ),
        
        column(8,
            
            conditionalPanel("input.toCalculate != 'pFree'",
                
                tabsetPanel(
                    tabPanel("Parameters",
                        
                        p(h4("Population size")),
                        
                        fluidRow(
                            
                            column(3, selectInput("randomN", label = "", 
                                    choices = c("fixed",  
                                        "random - enter distribution quantiles" = "randomKnownQ",
                                        "random - enter distribution parameters" = "randomKnownPar")),
                                offset = 1),
                            
                            column(3,
                                
                                conditionalPanel("input.randomN == 'fixed'",
                                    numericInput('N','Value',1000,0)
                                ),
                                
                                conditionalPanel("input.randomN == 'randomKnownQ'",
                                    list(
                                        matrixInput(inputId = "qN", label = "Probability - Quantile", 
                                            data = quantileStart),
                                        uiOutput("qNInfo")
                                    )
                                ),
                                
                                conditionalPanel("input.randomN == 'randomKnownPar'",
                                    list(
                                        selectInput("distributionN", label = "Distribution",
                                            choices = c("uniform" = "unif", "normal" = "norm", "beta", "gamma")),
                                        uiOutput('parametersTableN')
                                    )
                                )
                            
                            ),
                            
                            column(4, conditionalPanel("input.randomN != 'fixed'",
                                    plotOutput("plotN")
                                )
                            )
                        
                        ),
                        
                        p(h4("Test sensitivity")),
                        
                        fluidRow(
                            
                            column(3, selectInput("randomTSe", label = "", 
                                    choices = c("fixed", 
                                        "random - enter distribution quantiles" = "randomKnownQ",
                                        "random - enter distribution parameters" = "randomKnownPar")),
                                offset = 1),
                            
                            column(3,
                                
                                conditionalPanel("input.randomTSe == 'fixed'",
                                    numericInput('TSe','Value',0.6,0.01,1,0.01)
                                ),
                                
                                conditionalPanel("input.randomTSe == 'randomKnownQ'",
                                    list(
                                        matrixInput(inputId = "qTSe", label = "Probability - Quantile", 
                                            data = quantileStart),
                                        uiOutput("qTSeInfo")
                                    )
                                ),
                                
                                conditionalPanel("input.randomTSe == 'randomKnownPar'",
                                    list(
                                        selectInput("distributionTSe", label = "Distribution",
                                            choices = c("uniform" = "unif", "normal" = "norm", "beta", "gamma")),
                                        uiOutput('parametersTableTSe')
                                    )
                                )
                            ),
                            
                            column(4, conditionalPanel("input.randomTSe != 'fixed'",
                                    plotOutput("plotTSe")
                                )
                            )
                        ),
                        
                        p(h4("Design prevalence")),
                        
                        fluidRow(
                            column(3, selectInput("randomDP", label = "", 
                                    choices = c("fixed")), offset = 1),
                            
                            column(4, numericInput('DP','Value',0.01,0,1,0.01, width="75%")))
                    
                    
                    ),
                    
                    tabPanel("Risk factors",
                        
                        selectInput("asDataFrame", "Enter as data frame", 
                            choices = c("<none>" = "none", "Proportion" = "one",
                                "Relative risk and Proportion" = "both")),
                        
                        sliderInput('numRF','Number of Risk factors', min = 0, max = 20, value = 0, width = '100%'),
                        
                        conditionalPanel("input.numRF >= 1",
                            list(
                                fluidRow(
                                    column(2, actionButton("completeRisks", "Complete risk proportions", 
                                            styleclass="primary", size="mini")),
                                    
                                    conditionalPanel("input.asDataFrame != 'two'",
                                        
                                        column(2, selectInput("randomRr", label = "Relative risk:", 
                                                choices = c("fixed",
                                                    "random" = "randomKnownPar")), offset = 2),
                                        column(2, selectInput("randomProportion", label = "Proportion:", 
                                                choices = c("fixed",
                                                    "random" = "randomKnownPar")), offset = 2)
                                    )
                                ),
                                uiOutput('rfTable')
                            )
                        
                        ),
                        
                        conditionalPanel("input.asDataFrame != 'none'",
                            
                            hotable("rfDataFrame")
                        
                        )
                    
                    )
                
                ) #end tabsetPanel
            
            ), # end conditionalPanel
            
            conditionalPanel("input.toCalculate == 'pFree'",
                
                p(h4("Prior probability of freedom (for time period 1)")),
                
                fluidRow(
                    
                    column(3, selectInput("randomPfree0", label = "", 
                            choices = c("fixed", 
                                "random - enter distribution quantiles" = "randomKnownQ",
                                "random - enter distribution parameters" = "randomKnownPar")),
                        offset = 1),
                    
                    column(3,
                        
                        conditionalPanel("input.randomPfree0 == 'fixed'",
                            numericInput('Pfree0','Value', 0.5, 0,1, 0.1)
                        ),
                        
                        conditionalPanel("input.randomPfree0 == 'randomKnownQ'",
                            list(
                                matrixInput(inputId = "qPfree0", label = "Probability - Quantile", 
                                    data = quantileStart),
                                uiOutput("qPfree0Info")
                            )
                        ),
                        
                        conditionalPanel("input.randomPfree0 == 'randomKnownPar'",
                            list(
                                selectInput("distributionPfree0", label = "Distribution",
                                    choices = c("uniform" = "unif", "normal" = "norm", "beta", "gamma")),
                                uiOutput('parametersTablePfree0')
                            )
                        )
                    
                    ),
                    
                    column(4, conditionalPanel("input.randomPfree0 != 'fixed'",
                            plotOutput("plotPfree0")
                        )
                    )      
                
                ),
                
                p(h4("Probability of disease introduction")),
                
                fluidRow(
                    column(3, sliderInput('nPeriods', '# periods', 
                            value = 2, min = 2, max = 10), offset = 1)),
                
                # critical piece
                uiOutput('pIntroTable')
            
            
            ) #end conditionalPanel
        
        ) #end column(8,)
    
    ), #end fluidRow
    
    tags$hr(),
    
    uiOutput("warnings"),
    
    tags$br(),
    tags$br(),
    
#    verbatimTextOutput("print"),
    
    conditionalPanel("input.toCalculate != 'pFree'",
        
        uiOutput("resultsTable")
    
    ),
    
    conditionalPanel("input.toCalculate == 'pFree'",
        
        uiOutput("resultsPfree"),
        
        plotOutput("plotPfree")
    
    )
)