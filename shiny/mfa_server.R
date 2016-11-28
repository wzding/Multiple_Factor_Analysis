# shource the file that support this app
#source("../mfa_fntion.R")

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
           "Eigenvalue Bar Chart"=mfa@eig_plot,
           "Common Factor Score"=mfa@cfs_plot,
           "Partial Factor Score"=mfa@pfs_plot,
           "Loadings"=mfa@loading_plot)
    
  })
  output$result_plot<-renderPlot({
    plot(plotChoice())
  })
  
})

