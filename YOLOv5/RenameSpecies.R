
RenameSpecies <- function(EMDB, verbose = T, rm.species = NULL, group.names = NULL,
                          replace.species = NULL, savepath = NULL){
  
  EM <- read.csv(EMDB, sep = '\t')
  
  if(verbose == T){
    species <- unique(EM$Species)
    
    print('The following species labels are present in the DB: ')
    print(species)
    
    rm <- readline('Remove any species? (y/n): ')
    
    if(rm == 'y'){
      
      sp_rm <- readline('Species to remove: ')
      sp_rm <- strsplit(sp_rm, ' ')[[1]]
      
      EM <- EM %>%
        filter(!(Species %in% sp_rm))
    }
    
    NewNames <- readline('Enter the new species/group names to be used separated by a space: ')
    NewNames <- strsplit(NewNames, ' ')[[1]]
    
    nNames <- length(NewNames)
    
    for(i in 1:nNames){
      newname <- NewNames[i]
      
      replace <- readline(paste0("Which labels should be replaced by '", newname, "'? Enter Species separated by a space: "))
      replace <- strsplit(replace, ' ')[[1]]
      
      EM <- EM %>%
        mutate(Species = case_when(Species %in% replace ~ newname, T~Species))
    }
    
    savepath <- readline('Enter the filepath giving the location where you would like the updated DB to be saved: ')
  } else{
    EM <- EM %>%
      filter(!(Species %in% rm.species))
    
    for(i in 1:length(group.names)){
      NewName <- group.names[[i]]
      
      EM <- EM %>%
        mutate(Species = case_when(Species %in% replace.species[[i]] ~ NewName, T~Species))
    }
    
    
  }
  
  write.table(EM, file = savepath, sep = '\t', row.names = F)
}
