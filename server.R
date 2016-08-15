#server.R for pot randomisation based on Randomised block design

#Load the package
library(shiny)

#Load Randomisation function
source("Randomise.R")
source("RandomisePlates.R")

# Define server logic required to calculate the positions and produce a nice excel sheet with all samples
shinyServer(function(input, output){
  output$potdesign <- renderTable({
    Randomise(input$Treatments, input$Genotypes, input$nRep, input$Dimension)[[1]]
    })
  output$downloadDesign <- downloadHandler(
    filename = function(){paste(input$ExpName,"_Design.csv", sep = "")},
    content= function(file) {
      write.csv(Randomise(input$Treatments, input$Genotypes, input$nRep, input$Dimension)[[1]], file)
    }
  )
  output$downloadSetup <- downloadHandler(
    filename = function(){paste(input$ExpName, "_setup.csv", sep = "")},
    content= function(file) {
      write.csv(Randomise(input$Treatments, input$Genotypes, input$nRep, input$Dimension)[[2]], file)
    }
  )
  output$platedesign <- renderTable({
    RandomisePlates(input$TM, input$GT, input$SpP, 2, input$nRep)[[1]]
  })
  output$downloadD <- downloadHandler(
    filename = function(){paste(input$Exp,"_Design.csv", sep = "")},
    content= function(file) {
      write.csv(RandomisePlates(input$TM, input$GT, input$SpP, 2, input$nRep)[[1]], file)
    })
  output$downloadS <- downloadHandler(
    filename = function(){paste(input$Exp,"_setup.csv", sep = "")},
    content= function(file) {write.csv(RandomisePlates(input$TM, input$GT, input$SpP, 2, input$nRep)[[2]], file)})
})
