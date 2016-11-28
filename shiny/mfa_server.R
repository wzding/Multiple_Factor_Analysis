# shource the file that support this app
setwd("/Users/Shiying/Dropbox/BERKELEY_study/2016_Fall/02_STAT243/243_proj/Stats-243/shiny")
source("../mfa_fntion.R")

# or add the library when the package is ready
library(shiny)
#library(mfa)

# Define server logic required to draw the required plots

shinyServer(function(input, output) {
# 1) Run the necessary function to generate a MFA object with the necessary fields
  # Reactive: will be automatically re-executed when inputs change.
  plotChoice <- reactive({
    # define a new mfa object, then computes
    # 1.eigenvalues
    # 2.common factor scores(compromise factor score) matrix
    # 3.partial factor score
    # 4.loadings
    # result <-
    switch(input$Plot,
           "Eigenvalue Bar Chart"=mfa@plotEig,
           "Common Factor Score"=mfa@plotFactor,
           "Partial Factor Score"=mfa@plotPfs,
           "Loadings"=mfa@plotLoad)
    
  })
  output$result_plot<-renderPlot({
    plotChoice(mfa1)
  })
  
})

