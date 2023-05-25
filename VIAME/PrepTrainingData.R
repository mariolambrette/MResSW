

PrepTrainingData <- function(EMDB, EMsep = '\t', Vdir, Vfilename, CleanData = T,
                             Summary = T){
  
  # Convert EM database output to VIAME csv file
  source('C:/Annotations/PipelineScripts/Rscripts/EM2VIAME.R')
  
  Tr <- EM2VIAME(EMlocation = EMDB,
                 EMtype = EMsep,
                 VIAMElocation = Vdir,
                 VIAMEfilename = Vfilename,
                 annotations = T,
                 lengths = F,
                 fps = 1)
  
  # Clean Data - remove point annotations
  
  if(CleanData){
    print('Removing point annotations...')
    
    TotalAnnotations <- nrow(Tr)
    
    # Remove rows with point annotations
    Tr <- subset(Tr, Tr$TL_y != Tr$`BR_y)` | Tr$TL_y == "")
    
    # Calculate number of point annotations removed
    Removed <- TotalAnnotations-nrow(Tr)
    print(paste('Removed ', Removed, ' point annotations'))
  }
  
  # Create a summary of the number of annotations for each species
  if(Summary){
    Sum <- as.data.frame(table(Tr$`10-11+: Repeated Species`))
    colnames(Sum) <- c('Species', 'Frequency')
    Sum <- subset(Sum, Species != "")
    
    print(Sum)
  }
  
  # Determine if a minimum number of annotations is needed and slect for those species
  # Which meet the threshold
  MinAnnotations <- readline("Impose a minimum number of annotations per species (y/n)? ")
  if(MinAnnotations == 'y'){
    num <- readline("Minumum number of annotations per species: ")
    num <- as.numeric(num)
    
    KeepSum <- subset(Sum, Sum$Frequency > num)
    KeepSp <- KeepSum$Species
    
    Tr <- subset(Tr, Tr$`10-11+: Repeated Species` %in% KeepSp | Tr$`10-11+: Repeated Species` == "")
  }
  
  # Save the final annotation file
  write.csv(Tr, file = paste(Vdir, Vfilename,sep='/'),
            row.names = F)
  
  # Create a list of items to return
  ret <- list(Tr, Sum, KeepSum)
  names(ret) <- c('Saved VIAME annotation file', 'Pre limit species summary', 'Exported file species summary')
  
  return(ret)
}



