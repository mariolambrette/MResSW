#' Rename 'Species' in an EventMeasure database
#'
#' This function takes an EventMeasure database output and changes values in the
#' Species column to values specified by the user. The function can be run either
#' interatively (default) or not.
#' 
#' When run interactively, the user only specifies the location of the EventMeasure
#' database and is lead through the steps of renaming/removing any species of
#' insterest. This is most useful when the user is unsure of all the species
#' contained in the database.
#' 
#' Alternatively, the user can specify the species names that are to be replaced
#' and what the replacement should be in the function call. This is useful when
#' multiple databases need to be converted or the user is sure of all the species
#' contained in a database.
#' 
#' The function saves an updated version of the database in the specified location.
#' 
#' .
#' @param EMDB Path to an EventMeasure databse output.
#' @param verbose Boolean. Default = T. Specifies whether the function is to be run interactively or not (see above). TRUE (default) runs the funtion interactively while FALSE does not. All parameters other than EMDB need only be specified if verbose = F.
#' @param rm.species A character vector containing the names of species that are to be removed from the database all together.
#' @param new.names A list of the new names to be applied to species in the database. List elements whould be character strings.
#' @param replace.species A list containing the names of Species that are to be replaced. This list must be the same length as new.names. List elements can be either character strings, or in the case where multiple species are to be renamed the same thing (e.g. to put them into groups for training) they may be character vectors containing the names of all species within that group. It is vital that the list order here matches that in new.names. i.e. element 1 in this list will be renamed using the string specified in element 1 of new.names.
#' @param save.path A filepath specifying where to save the modified database. The file name must carry a .txt extension.
#' @return Function returns the modified database as a dataframe.
#' @export
#' 
#' @examples
#' 
#' # Interactive run - in this case the user is shown all the species in the database,
#' # then lead through subsequent steps
#' 
#' RenameSpecies(EMDB = 'path/to/EM/Database.txt')
#' 
#' # Non-Interactive run - in this case species1 and species2 will both be renamed
#' # 'group1' and species3 and species4 will be renamed 'group2'
#' 
#' RenameSpecies(EMDB = 'path/to/EM/Database.txt',
#'               verbose = F,
#'               rm.species = c('species1'),
#'               new.names = list('group1', 'group2'),
#'               replace.species = list(c('species1', 'species2'), c('species3', 'species4')),
#'               save.path = 'path/to/modified/EM/Database.txt')




RenameSpecies <- function(EMDB, verbose = T, rm.species = NULL, new.names = NULL,
                          replace.species = NULL, save.path = NULL){
  
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
    
    save.path <- readline('Enter the filepath giving the location where you would like the updated DB to be saved: ')
  } else{
    EM <- EM %>%
      filter(!(Species %in% rm.species))
    
    for(i in 1:length(new.names)){
      NewName <- new.names[[i]]
      
      EM <- EM %>%
        mutate(Species = case_when(Species %in% replace.species[[i]] ~ NewName, T~Species))
    }
    
    
  }
  
  write.table(EM, file = save.path, sep = '\t', row.names = F)
  
  return(EM)
}
