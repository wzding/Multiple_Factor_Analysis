library(shiny)

# Define UI for application that graphs relative freqs
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Multiple Factor Analysis Application"),
  
  # Sidebar with a slider input for number of tosses 
  sidebarLayout(
    sidebarPanel(
      selectInput("Plot","Choose a figure to plot:",
                  choices = c("Eigenvalue Bar Chart","Common Factor Score",
                              "Partial Factor Score","Loadings"),
                  selected = "Eigenvalue Bar Chart")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text"),
      plotOutput("result_plot"),
      textOutput("text_table"),
      dataTableOutput("eigTbl")
    )
  )
))
