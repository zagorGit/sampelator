
#' Run the Sampelator Application
#' @return no return value
#' @import shiny
#' @export
runSampelator <- function(){
  
  if (FALSE){
    remove.packages(c("ggplot2", "shinysky", "plyr", "sampelator", "shinyIncubator",
            "DT", "rCharts", "shiny"))
    
    install.packages(c("shiny", "ggplot2"))
    
  }  
  
  if(!require(ggplot2)){
    
    install.packages("ggplot2")
    
  }
  
  if(!require(devtools)){
    
    install.packages("devtools")
    
  }
  
  if(!require(shinyIncubator)){
    
    devtools::install_github("rstudio/shiny-incubator")
    
  }
  
  if(!require(rCharts)){
    
    install.packages("base64enc")
    devtools::install_github('rCharts', 'ramnathv', ref='dev')
    
  }
  
  if(!require(DT)){
    
    install.packages("DT")
    
  }
  
  if(!require(shinysky)){
    
    devtools::install_github("AnalytixWare/ShinySky")
    
  }
  
  sampelatorTmpDir <- tempdir()
  
  setwd(sampelatorTmpDir)
  
  # Copy server.R and ui.R (not folder www)
  sampelatorUiDir <- system.file("ui", "sampelator", package = "sampelator")
  sampelatorUiFiles <- list.files(path = sampelatorUiDir, full.names = TRUE)
  sampelatorUiFiles <- sampelatorUiFiles[!grepl("www", sampelatorUiFiles)
  ]
  sapply(sampelatorUiFiles, function(x){
        file.copy(from = x, to = file.path(sampelatorTmpDir, basename(x)),
            overwrite = TRUE)}
  )
  
  
  # Make www directory and copy its files
  dir.create(path = paste0(sampelatorTmpDir, "/www"))
  wwwFiles <- list.files(path = paste0(sampelatorUiDir, "/www"), full.names = TRUE)
  
  sapply(wwwFiles, function(x){
        file.copy(from = x, to = file.path(paste0(sampelatorTmpDir, "/www"), basename(x)),
            overwrite = TRUE)}
  )
  
  
  runApp(appDir = sampelatorTmpDir, host = "0.0.0.0")
  
}
