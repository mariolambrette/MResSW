
EMCreateYAML <- function(path.EM, path.YOLO, path.train, path.val, path.test, remove.anno = NULL, path.yaml){
  library(dplyr)
  library(yaml)
  
  EMDBs <- list.files(path.EM, full.names = T)
  EMs <- lapply(EMDBs, read.csv, sep = '\t')
  EMs <- do.call(rbind, EMs)
  
  if(!is.null(remove.anno)){
    EMs <- EMs %>%
      filter(!(Species %in% remove.anno))
  }
  
  species <- unique(EMs$Species)
  
  sptable <- tibble(species, id = seq(1:(length(species)))) %>%
    mutate(id = id-1)
  
  class.list <- as.list(species)
  names(class.list) <- as.numeric(sptable$id)
  
  ylist <- list(path = path.YOLO, train = path.train, val = path.val, test = path.test, names = class.list)
  
  write_yaml(ylist, path.yaml)
  
  return(sptable)
}




t1 <- read_yaml('C:/YOLOv5/Training/config.yaml')



