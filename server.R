#server.R for pot randomisation based on Randomised block design

#Load the package
library(shiny)

#Load Randomisation function
Randomise <- function(Treats, Genos, PotsPerTray, Replicates, Dimension, Experiment_Name) {
  ##Define data
  Genotypes <- strsplit(Genos, ",")[[1]]
  Treatments <- strsplit(Treats, ",")[[1]]
  nTreat <- length(Treatments)
  nGeno <- length(Genotypes)
  nTrays <- ceiling((nGeno*Replicates)/PotsPerTray)
  nPlants <- Replicates*nGeno*nTreat
  SeedlingsPerTray <- (Replicates*nGeno)/nTrays
  while(round(SeedlingsPerTray) != SeedlingsPerTray){
    Genotypes <- c(Genotypes, "empty")
    nGeno <- length(Genotypes)
    nTrays <- ceiling((nGeno*Replicates)/PotsPerTray)
    nPlants <- Replicates*nGeno*nTreat
    SeedlingsPerTray <- (Replicates*nGeno)/nTrays
  }
  nCol <- PotsPerTray/Dimension
  pre <- matrix(ncol=nCol, nrow=nPlants)
  colnames(pre) <- c("ID", "Tray", "Position", "Treatment", "Genotype")
  setup <- data.frame(pre)
  setup$ID <- 1:nPlants
  
  ##Define positions of seedlings
  Position <- NULL
  Tray <- NULL
  for(i in 1:nTrays){
    Tray <- c(Tray, rep(i,SeedlingsPerTray))
    Position <- c(Position, sample.int(PotsPerTray, SeedlingsPerTray, replace = FALSE))
  }
  setup$Tray <- rep(Tray, nTreat)  
  setup$Position <- rep(Position, nTreat)
  
  #Repeat this for every treatment
  nPperT <- SeedlingsPerTray*nTrays
  for(i in 1:nTreat){
    setup$Treatment[(i*nPperT-nPperT+1):(i*nPperT)]<- Treatments[i]
  }
  
  #Replace number with genotype
  setup$Genotype <- rep(Genotypes, (length(setup$ID)/nGeno))
  
  #Save set-up
  #filename1 <- paste(Experiment_Name, "_TOLsetup.csv", sep="")
  #write.csv(setup, file = filename1, row.names=FALSE)
  
  #setup a table for the potdesign
  pots <- matrix(nrow=Dimension, ncol=nCol)
  for(i in 1:nCol){
    start <- 1+((i-1)*Dimension)
    end <- i*Dimension
    pots[1:Dimension, i] <- start:end
  }
  
  #make potdesign
  PotDesign <- NULL
  for(i in 1:nTrays){
    D <- pots
    for(a in 1:Dimension){
      for(b in 1:nCol){
        e = 0
        tryCatch({D[a,b] <- setup$Genotype[which(setup$Position == D[a,b] & setup$Tray == i & setup$Treatment == Treatments[1])]
        }, error = function(e){D[a,b] <- NA})
      }
    }
    PotDesign <- rbind(PotDesign, D)
    PotDesign <- rbind(PotDesign, NA)
    
  }
  
  #Save design
  #filename2 <- paste(Experiment_Name, "_potdesign.csv", sep="")
  #write.csv(PotDesign, file =filename2, row.names =FALSE
  print(PotDesign)
}

# Define server logic required to calculate the positions and produce a nice excel sheet with all samples
shinyServer(function(input, output){
  
  output$potdesign <- renderTable({
    Randomise(input$Treatments, input$Genotypes, input$nPots, input$nRep, input$DimensionR, input$ExpName)
    })
})