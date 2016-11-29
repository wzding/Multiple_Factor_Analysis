# source the file that support this app

# or add the library when the package is ready
library(shiny)
source("/Users/Shiying/Dropbox/BERKELEY_study/2016_Fall/02_STAT243/243_proj/Stats-243/final/MFA01/install.R")

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
    #Fmat=matrix(c(-0.98,0.163,-0.809,0.033,-0.761,-0.454,-1.115,-0.166,1.373,-0.128),nrow=5,ncol=2,byrow=TRUE)
    #sv=c(0.878,0.351,0.301,0.276,0.244,0.198,0.176,0.158,0.137,0.116,0.106)
    #eigs=sv^2
    #mfa1=new(Class ="mfa",F=Fmat,sv=sv,eig=eigs)
    MFA()
  })

  output$text<-renderText({
    paste("You have selected to plot ",input$Plot,":")
  })

  output$text_table<-renderText({
    paste("Table: Eigenvalues and Percentage of Explained Inertia of the MFA. ")
  })
  
  output$cont_table<-renderText({
    paste("Table: Controbution Table")
  })

  output$result_plot<-renderPlot({
    mfa1=MFA()
    switch(input$Plot,
           "Eigenvalue Bar Chart"=plot(mfa1),
           "Common Factor Score"=plot(mfa1),
           "Partial Factor Score"=plot(mfa1,FALSE,pfs=TRUE,num=1),
           "Loadings"=plot(mfa1,FALSE,pfl=TRUE,num=1))
  })

  output$eigTbl<-renderTable({
    mfa1=plotChoice()
    ev.summary(mfa1)
  })

})

