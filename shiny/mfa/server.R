# shource the file that support this app

# or add the library when the package is ready
library(shiny)
source("/Users/Shiying/Dropbox/BERKELEY_study/2016_Fall/02_STAT243/243_proj/Stats-243/mfa_fntion.R")
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
    Fmat=matrix(c(-0.98,0.163,-0.809,0.033,-0.761,-0.454,-1.115,-0.166,1.373,-0.128),nrow=5,ncol=2,byrow=TRUE)
    mfa1=new(Class ="mfa",F=Fmat)
    switch(input$Plot,
           "Eigenvalue Bar Chart"=plotEig(mfa1),
           "Common Factor Score"=plotFactor(mfa1),
           "Partial Factor Score"=plotPfs(mfa1),
           "Loadings"=plotLoad(mfa1))
    
  })
  
  output$text<-renderText({
    paste("You have selected to plot ",input$Plot)
  })
  
  output$result_plot<-renderPlot({
    plotChoice()
  })
  
})

