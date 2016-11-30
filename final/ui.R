library(shiny)

x <- paste(cat("Which table to analyze", "\n","when selecting partial factor score or factor loading"))

# Define UI for application that graphs relative freqs
shinyUI(fluidPage(

  # Application title
  titlePanel("Multiple Factor Analysis Application"),

  # Sidebar with a slider input for number of tosses
  sidebarLayout(
    sidebarPanel(
      selectInput("Plot","Choose a figure to plot:",
                  choices = c("Eigenvalue Bar Chart","Common Factor Score",
                              "Partial Factor Score","Factor Loading",
                              "Partial Factor Score and Factor Loading"),
                  selected = "Common Factor Score"),
      sliderInput("tester",paste("Which table to analyze for partial factor score or factor loading"),
                  min=1,max=10,value=1)

    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text"),
      textOutput("text_sub"),
      plotOutput("result_plot"),
      textOutput("text_table"),
      #tableOutput("eigTbl")
      dataTableOutput("eigTbl")
    )
  )
))
