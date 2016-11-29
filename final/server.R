# source the file that support this app

# or add the library when the package is ready
library(shiny)
<<<<<<< HEAD
source("/Users/Shiying/Dropbox/BERKELEY_study/2016_Fall/02_STAT243/243_proj/Stats-243/final/MFA01/install.R")
#source("E:/Stats-243/final/install.R")
=======
# source("/Users/Shiying/Dropbox/BERKELEY_study/2016_Fall/02_STAT243/243_proj/Stats-243/final/install.R")
source("C:/Users/Emma/Downloads/stats-243/final/MFA01/install.R")
>>>>>>> 007f232fe73b84cc6df706e447af340f90a8652e

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

    MFA()
  })

  output$text<-renderText({
    paste("You have selected to plot ",input$Plot,":")
  })

  output$text_table<-renderText({
    paste("Table: Eigenvalues and Percentage of Explained Inertia of the MFA. ")
  })

  output$cont_table<-renderText({
    paste("Table: Contribution Table")
  })

  output$result_plot<-renderPlot({
    mfa1=MFA()
    switch(input$Plot,
           "Eigenvalue Bar Chart"=plot(mfa1,FALSE,eig=TRUE),
           "Common Factor Score"=plot(mfa1),
           "Partial Factor Score"=plot(mfa1,FALSE,pfs=TRUE,num=input$tester),
           "Factor Loading"=plot(mfa1,FALSE,pfl=TRUE,num=input$tester))
  })

  output$eigTbl<-renderDataTable({
    mfa1=plotChoice()
    ev.summary(mfa1)
  })

})



