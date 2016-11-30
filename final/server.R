# source the file that support this app

# or add the library when the package is ready
library(shiny)

# source("/Users/Shiying/Dropbox/BERKELEY_study/2016_Fall/02_STAT243/243_proj/Stats-243/final/install.R")
source("E:/Stats-243/final/MFA01/install_Emma.R")


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
    paste(input$Plot," (number on top corresponds to ith observation)" )  
  })

  output$text_table<-renderText({
    paste("Table: Eigenvalues and Percentage of Explained Inertia of the MFA. ")
  })

  output$cont_table<-renderText({
    paste("Table: Contribution Table")
  })

  output$eign_plot <- renderPlot({
    # plot of eigenvalues
    plot(plotChoice(),FALSE,eig=TRUE)
  })
  
  output$result_plot<-renderPlot({
    switch(input$Plot,
           "Common Factor Score"=plot(plotChoice()),
           "Partial Factor Score"=plot(plotChoice(),FALSE,pfs=TRUE,num=input$tester),
           "Factor Loading"=plot(plotChoice(),FALSE,pfl=TRUE,num=input$tester),
           "Partial Factor Score and Factor Loading" = plot(plotChoice(),FALSE,pfs=TRUE,pfl=TRUE,num=input$tester))
  })

  output$tbl <- renderTable({
    switch(input$Plot,
           "Common Factor Score"= plotChoice()$cfs[,1:input$num],
           "Partial Factor Score"= plotChoice()$pfs[[input$tester]][,1:input$num],
           "Factor Loading"= plotChoice()$pfl[[input$tester]][,1:input$num],
           "Partial Factor Score and Factor Loading" = 
             list(plotChoice()$pfs[[input$tester]][,1:input$num], 
                  plotChoice()$pfl[[input$tester]][,1:input$num]))
  })  
  
  output$eigTbl<-renderDataTable({
    ev.summary(plotChoice())
  })

})



