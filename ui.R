#ui.R for pot randomisation based on randomised block desing
#Load shiny package
library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Randomisation of pots"),
  
  # Numeric input of variables used in pot design
  sidebarLayout(
    sidebarPanel(
      textInput("Treatments", "List your treatments seperated with comma:", "0 mM, 75 mM, 125 mM"),
      textInput("Genotypes", "List your genotypes seperated with comma:", "Col-0, 5A, 5B, 5D"),
      numericInput("nRep", "Number of replicates per genotype:", 20),
      numericInput("nPots", "Number of pots per tray:", 40),
      numericInput("DimensionR", "Number of rows per tray:", 8),
      textInput("ExpName", "Name of your experiment:", "Exp1")
    ),
    
    # Show the pot design
    mainPanel(
      tableOutput("potdesign")
    )
  )
))