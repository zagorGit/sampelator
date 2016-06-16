
#' Run the Ribess+ Application
#' @return no return value
#' @import shiny
#' @export
runRibess <- function(){
  
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
  
  if(!require(shinysky)){
    
    devtools::install_github("AnalytixWare/ShinySky")
    
  }
  
  ribessTmpDir <- tempdir()
  
  setwd(ribessTmpDir)
  
  # Copy server.R and ui.R (not folder www)
  ribessUiDir <- system.file("ui", "ribess", package = "sampelator")
  ribessUiFiles <- list.files(path = ribessUiDir, full.names = TRUE)
  ribessUiFiles <- ribessUiFiles[!grepl("www", ribessUiFiles)]
  
  sapply(ribessUiFiles, function(x){
        file.copy(from = x, to = file.path(ribessTmpDir, basename(x)),
            overwrite = TRUE)}
  )
  
  # Make www directory and copy its files
  dir.create(path = paste0(ribessTmpDir, "/www"))
  wwwFiles <- list.files(path = paste0(ribessUiDir, "/www"), full.names = TRUE)

  sapply(wwwFiles, function(x){
        file.copy(from = x, to = file.path(paste0(ribessTmpDir, "/www"), basename(x)),
            overwrite = TRUE)}
  )
  
  runApp(appDir = ribessTmpDir, host = "0.0.0.0")

}
