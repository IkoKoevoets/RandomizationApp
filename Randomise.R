Randomise <- function(Treats, Genos, Replicates, Dimensions) {
  ##Define data
  Genotypes <- strsplit(Genos, ",")[[1]]
  Treatments <- strsplit(Treats, ",")[[1]]
  Design <- as.numeric(strsplit(Dimensions, ",")[[1]])
  nTreat <- length(Treatments)
  nGeno <- length(Genotypes)
  PotsPerTray <- Design[1]*Design[2]
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
  pre <- matrix(ncol=5, nrow=nPlants)
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
  
  #setup a table for the potdesign
  pots <- matrix(nrow=Design[1], ncol=Design[2])
  for(i in 1:Design[2]){
    start <- 1+((i-1)*Design[1])
    end <- i*Design[1]
    pots[1:Design[1], i] <- start:end
  }
  
  #make potdesign
  PotDesign <- NULL
  for(i in 1:nTrays){
    D <- pots
    for(a in 1:Design[1]){
      for(b in 1:Design[2]){
        e = 0
        tryCatch({D[a,b] <- setup$Genotype[which(setup$Position == D[a,b] & setup$Tray == i & setup$Treatment == Treatments[1])]
        }, error = function(e){D[a,b] <- NA})
      }
    }
    PotDesign <- rbind(PotDesign, D)
    PotDesign <- rbind(PotDesign, NA)
  }
  return(list(PotDesign, setup))
}