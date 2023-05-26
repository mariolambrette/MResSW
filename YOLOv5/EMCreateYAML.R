#' Create a YAML configuration file giving the path to YOLO format training,
#' validation and testing images.
#'
#' This function takes a directory of EventMeasure Database outputs and uses the 
#' Species named within it to create the YAML configuration file needed to train
#' a YOLO model. The function creates a zero-based index for the species detailed
#' within the exported databases and returns a dataframe showing that index. This
#' dataframe is needed to run the EM2YOLO() function properly. the YAML configuration
#' is saved at path.yaml and is essential for training YOLO models.
#' 
#' For information on reccomended directory structure see Training_YOLOv5.rmd
#' .
#' @param path.EM Path to a directory containing all the EventMeasure database exports of interest for model training. This directory must contain no .txt files other than the EventMeasure databases.
#' @param path.YOLO The path to the directory containing the YOLO training data. This directory needs to have a specific structure, containing two subdirectories named 'images' and 'labels'. Each of these must then have three subdirectories named 'Train', 'Val' and 'Test'.
#' @param path.train The path to the directory containing YOLO format training images. Should take the format 'path.YOLO/images/Train'.
#' @param path.val The path to the directory containing YOLO format validation images. Should take the format 'path.YOLO/images/Val'.
#' @param path.test The path to the directory containing YOLO format teasting images. Should take the format 'path.YOLO/images/Test'.
#' @param remove.anno An optional character vector of species names that are not to be included in model trainig, and as such will not be included in the .yaml file. This vector should be indetical to that used when remove.anno is specified in other functions, though is not neccesary if data has been prepared with the RenameSpecies() function. It is vital that species names appear exactly as they would in the EventMeasure database 'Species' column.
#' @param path.yaml Filepath giving the location where the .yaml configuration file is to be saved. The recoomendation is to put it next to the 'image' and 'label' directories: 'path.YOLO/config.yaml'. It is vital that the file name is specified with a .yaml extension.
#' @return A dataframe with two columns, 'species' and 'index', detailing the species index used in the .yaml configuration file. This dataframe is required for the EM2YOLO() function and should be saved.
#' @export
#' 
#' @examples
#' EMCreateYAML(path.EM = 'path/to/EM/Ouputs',
#'              path.YOLO = 'path/to/YOLO/data',
#'              path.train = 'path/to/training/images',
#'              path.val = 'path/to/validation/images',
#'              path.test = 'path/to/validation/images',
#'              remove.anno = c('species1', 'species2'),
#'              path.YAML = 'path.YOLO/config.yaml')



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