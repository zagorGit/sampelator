library(rCharts) #for interactive plots
options(RCHART_LIB = 'polycharts')

#library(shiny)
library(DT) #for renderDataTable
#library(shinyIncubator)  #for matrixInput


fluidPage(
    
    helpText(
        h5(a(href = "https://efsa-models.openanalytics.eu/projects/sampelator/wiki", 
                target = "_blank", "About"), align = "right"),
        h5(a(href = "https://efsa-models.openanalytics.eu/projects/sampelator/issues/new", 
                target = "_blank", "Report new issue"), align = "right")
    ),
    
    navbarPage(title = div(img(src = "EFSA_logo.JPG",
                float = "top", height = "50px", hspace = "60px",
                style = 'margin-top:-15px'),
            "Sampelator"),
        windowTitle = "Sampelator",
        
        tabPanel("Estimating General Parameters",
            
            fluidRow(
                
                column(3, wellPanel(
                        
                        selectInput( inputId = "samplingDesign", label = "Sampling design",
                            choices = list("Simple random sampling" = "getSimpleRandom", 
                                "Clustered sampling" = "getClustered",
                                "Stratified sampling" = "getStratified", 
                                "Two-stage sampling" = "getTwoStage",
                                "Three-stage sampling" = "getThreeStage",
                                "Measure change over time" = "getChangeOverTime")),
                        
                        radioButtons( inputId = 'purpose', "Purpose of the study", 
                            choices = c("Two-sided testing" = "testing", "Estimation" = "estimation")),
                        
                        sliderInput( inputId = "typeIerror", label = "Type I error", 
                            value = 0.05, min = 0, max = 1, step = 0.01 ),
                        
                        numericInput("populationSize", "Population size", 10000)
                    
                    )   
                
                ),
                
                column(3,
                    
                    h3("General parameters"),
                    
                    wellPanel(                    
                        
                        h4("Total sample size"),
                        
                        conditionalPanel("input.range != 'sampleSize'",
                            
                            numericInput( inputId = "sampleSize", label = NULL, value = NA )),
                        
                        conditionalPanel("input.range == 'sampleSize'",
                            
                            fluidRow( column(6, numericInput( inputId = "minsampleSize", 
                                        label = "Minimum", value = NA )),
                                column(6, numericInput( inputId = "maxsampleSize", 
                                        label = "Maximum", value = NA )) )
                        ),
                        
                        fluidRow(column(10, offset = 1, checkboxInput("finite", "Adjust for finite population"))),
                        fluidRow(column(10, offset = 1, checkboxInput("inflation", "Inflate sample size (account for missingness)"))),
                        
                        conditionalPanel("input.inflation",
                            
                            fluidRow(column(10, offset = 1, 
                                    numericInput("inflationFactor", "Inflation factor", value = 1, step = 0.1)))
                        
                        ),
                        
                        h4("Desired difference"),
                        
                        conditionalPanel("input.purpose == 'estimation'",
                            p(em("i.e. half width of the confidence interval for the mean"))
                        ),
                        conditionalPanel("input.purpose == 'testing'",
                            p(em("true difference in means that is tested for"))
                        ),
                        
                        conditionalPanel("input.range != 'desiredDifference'",
                            
                            numericInput( inputId = "desiredDifference", label = NULL, value = NA, step = 0.01)),
                        
                        conditionalPanel("input.range == 'desiredDifference'",
                            
                            fluidRow( column(6, numericInput( inputId = "mindesiredDifference", 
                                        label = "Minimum", value = NA, step = 0.01 )),
                                column(6, numericInput( inputId = "maxdesiredDifference", 
                                        label = "Maximum", value = NA, step = 0.01 )) )
                        ),
                        
                        conditionalPanel("input.purpose == 'testing'",
                            
                            h4("Power"),
                            
                            conditionalPanel("input.range != 'power'",
                                
                                numericInput( inputId = "power", label = NULL, value = NA, step = 0.01)),
                            
                            conditionalPanel("input.range == 'power'",
                                
                                fluidRow( column(6, numericInput( inputId = "minpower", 
                                            label = "Minimum", value = NA, step = 0.01 )),
                                    column(6, numericInput( inputId = "maxpower", 
                                            label = "Maximum", value = NA, step = 0.01 )) )
                            )
                        )
                    ),
                    
                    selectInput("range", "Define range for:", 
                        choices = list("<none>", "Total sample size" = "sampleSize",
                            "Desired difference" = "desiredDifference",
                            "Power" = "power"))
                
                
                ),
                
                column(6,
                    
                    h3("Design-specific parameters"),
                    
                    wellPanel(
                        uiOutput("designParameters")
                    )
                
                )
            ),
            
            tags$hr(),
            
            
            uiOutput("warnings"),
            #for debugging
            #verbatimTextOutput("print2"),
            
            tags$br(),
            
            uiOutput("resultsToShow"),
            
            tags$br()
        
        ),
        
        tabPanel("Selecting Sampling Units",
            
            fluidRow(
                
                column(4, wellPanel(
                        
                        fileInput('dataCountry', 'Load data',
                            accept=c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv')),
                        
                        radioButtons('sep', 'Separator for data',
                            c("Comma (.csv file)"=',',
                                "Semicolon (.csv file)"=';',
                                "Tab (.txt file)"='\t')),
                        
                        uiOutput("collapseDataUI"), 
                        
                        tags$hr(),
                        
                        conditionalPanel("input.sampleFromResults",
                            
                            list(
                                uiOutput("stratumName"),
                                uiOutput("selectN")
                            )
                        
                        ),
                        
                        conditionalPanel("!input.sampleFromResults",
                            
                            numericInput("N", "Estimated number to sample", value = NA)
                        
                        ),
                        
                        checkboxInput("sampleFromResults", 
                            "Copy from 'Estimated General Parameters'"),
                        
                        
                        #p(strong("Actual number to sample"), uiOutput("actualSample")),
                        
                        tags$hr(),
                        numericInput("seed", "Seed for sampling (reproducibility)", value = 1)
                    
                    )
                
                ),
                
                column(8, 
                    
                    tabsetPanel(
                        tabPanel("View Loaded Data", 
                            DT::dataTableOutput('dataCountry')
                        ),
                        tabPanel("View 'Estimated General Parameters'", 
                            uiOutput("samplingResults")
                        ),
                        tabPanel("Define Sampling Procedure", 
                            list(
                                uiOutput("tableChoices"),
                                uiOutput("defineConvenience"),
                                uiOutput("convenienceSizes")
                            )
                        )                        
                    )
                )
            
            ),
            
            tags$hr(),
            
            #for debugging
#            verbatimTextOutput("print"),
            
            uiOutput("collectSample"),
            
            tags$br(),
            
            tabsetPanel(
                tabPanel("Selected sample",
                    list(
                        tags$br(),
                        uiOutput("downloadSample"),
                        tags$br(),
                        DT::dataTableOutput("sampledCenters")
                    )
                ),
                tabPanel("Check representativeness", 
                    list(
                        tags$br(),
                        p(strong("Based on 100 random samples drawn from the Loaded Data")),
                        p(strong("The relative frequency in the 100 samples (histogram) is compared with the relative frequency in the selected sample (red dashed line)")),
                        uiOutput("figureChoices"),
                        plotOutput("summarizeSampling")
                    )
                )
            
            
            )
        )        
    )
)
