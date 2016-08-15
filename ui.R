#ui.R for pot randomisation based on randomised block desing
#Load shiny package
library(shiny)

# Define UI for dataset viewer application
shinyUI(navbarPage("RandomisationApps",
  
  #Tab with explanation
  tabPanel("About",
           "These apps are developed to ease the randomized design of an experiment. Currently it contains two apps: 
           A pot randomisation app and a plate randomisation app. The pot randomisation is meant for randomizing different plants over trays.
           For each treatment the same randomisation is used (which is necessary for treatments only possible to apply to a whole tray, such as salt stress.
           The app directly shows the design in the table and you can download both the design and the setup (with a row for each
           plant). The plate randomisation app is designed for experiments with two different genotypes on 1 plate."),
  
  # Numeric input of variables used in pot design
  tabPanel("Pot randomisation", 
      sidebarLayout(
      sidebarPanel(
      textInput("Treatments", "List your treatments seperated with comma:", "0 mM, 75 mM, 125 mM"),
      textInput("Genotypes", "List your genotypes seperated with comma:", "Col-0, 5A, 5B, 5D"),
      numericInput("nRep", "Number of replicates per genotype:", 20),
      textInput("Dimension", "Dimensions of tray (rows,columns)", "8,5"),
      textInput("ExpName", "Name of your experiment:", "Exp1"),
      submitButton(text = "Apply changes"),
      downloadButton("downloadDesign", "Download the potdesign"),
      downloadButton("downloadSetup", "Download the setup")
  ),
    # Show the pot design
    mainPanel(
      tableOutput("potdesign")
      )
  )),
  
  tabPanel("Plate randomisation",
           sidebarLayout(
             sidebarPanel(
               textInput("TM", "List your treatments seperated with comma:", "0 mM, 75 mM, 125 mM"),
               textInput("GT", "List your genotypes seperated with comma:", "Col-0, 5A, 5B, 5D"),
               numericInput("SpP", "Number of seedlings per plate", 4),
               numericInput("nRep", "Number of replicates per genotype:", 20),
               textInput("Exp", "Name of your experiment:", "Exp1"),
               submitButton(text = "Apply changes"),
               downloadButton("downloadD", "Download the potdesign"),
               downloadButton("downloadS", "Download the setup")
             ),
             # Show the pot design
             mainPanel(tableOutput("platedesign"))
             )
           )
  
))