##randomisation function
RandomisePlates <- function(Treats, Genos, SeedlingsPerPlate, GenotypesPerPlate, Replicates) {
  Genotypes <- strsplit(Genos, ",")[[1]]
  Treatments <- strsplit(Treats, ",")[[1]]
  nTreat <- length(Treatments)
  nGeno <- length(Genotypes)
  nPlates <- (nTreat*nGeno*Replicates)/SeedlingsPerPlate
  pre <- matrix(ncol=5, nrow=nPlates)
  colnames(pre) <- c("ID", "Plate", "Treatment", "Genotype_left", "Genotype_right")
  pre_setup <- data.frame(pre)
  pre_setup$ID <- 1:nPlates
  pre_setup$Plate <- sample(nPlates, nPlates, replace = FALSE)
  nPperT <- nPlates/nTreat
  for(i in 1:nTreat){
    pre_setup$Treatment[(i*nPperT-nPperT+1):(i*nPperT)]<- Treatments[i]
  }
  
  LEFT <- 0
  RIGHT <- 0
  MAX <- ceiling(nPperT/nGeno)
  
  require(ibd)
  bibd <- ibd(nGeno, nPperT, GenotypesPerPlate)
  bibd <- bibd$design
  
  for(i in 1:nGeno){
    bibd[which(bibd== i)] <- Genotypes[i]
  }
  
  for(i in 1:nrow(bibd)){
    if(length(which(LEFT == bibd[i,1])) == MAX|length(which(RIGHT == bibd[i,2])) == MAX){
      LEFT[i] <- bibd[i,2]
      RIGHT[i] <- bibd[i,1]
    }
    else{
      LEFT[i] <- bibd[i,1]
      RIGHT[i] <- bibd[i,2]
    }
  }
  
  pre_setup$Genotype_left <- rep(LEFT, nTreat)
  pre_setup$Genotype_right <- rep(RIGHT, nTreat)
  genotypes <- c(rep(as.character(pre_setup$Genotype_left),SeedlingsPerPlate/2), rep(as.character(pre_setup$Genotype_right), SeedlingsPerPlate/2))
  Root <- 0
  count <- 0
  for(i in 1:SeedlingsPerPlate){
    for(a in 1:nPlates){
      count <- count + 1
      Root[count] <- i-1
    }
  }
  ID <- 1:(nPlates*SeedlingsPerPlate)
  Plate <- rep(pre_setup[,2], SeedlingsPerPlate)
  Treatment <- rep(pre_setup[,3], SeedlingsPerPlate)
  setup <- data.frame(ID, Plate, Treatment,genotypes,Root)
  return(list(pre_setup, setup))
}

