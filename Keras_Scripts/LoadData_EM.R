## Load annotations and images

LoadData_EM <- function(ImgDir, EMFile, ImageHeight, ImageWidth, TargetHeight, TargetWidth,
                        KeepSpecies = NULL, test){
  
  library(dplyr)
  
  # Load annotations
  source("C:/Annotations/PipelineScripts/Rscripts/Keras_Scripts/LoadAnnotations_EM.R")
  annotations <- LoadAnnotations_EM(ImgDir = ImgDir,
                                    EMFile = EMFile,
                                    ImageHeight = ImageHeight,
                                    ImageWidth = ImageWidth,
                                    TargetHeight = TargetHeight,
                                    TargetWidth = TargetWidth)
  
  # Remove point annotations from the data
  print('Removing point annotations...')

  annotations <- annotations %>%
    filter(boxheight != 0)
  
  if(!is.null(KeepSpecies)){  
    print('Selecting Target Species...')
    
    annotations <- annotations %>%
      filter(sp_name %in% KeepSpecies)
    }

  
  # Create a summary of the number of annotations for each species
  Sum <- annotations %>%
    summarise(sp_name) %>%
    table() %>%
    as.data.frame() %>%
    rename(Species = sp_name,
           Frequency = Freq)
  
  print('Number of annotations per species:')
  print(' ')
  print(Sum)
  
  MinAnnotations <- readline("Impose a minimum number of annotations per species (y/n)? ") # Suggest a sensible minimum threshold
  if(MinAnnotations == 'y'){
    num <- readline("Minumum number of annotations per species: ") %>%
      as.numeric()
    
    KeepSum <- Sum %>% filter(Frequency > num)
    KeepSp <- KeepSum$Species
    
    FiltAnnotations <- annotations %>%
      filter(sp_name %in% KeepSp)
  } else(
    FiltAnnotations <- annotations
  )
  
  save <- readline("Save the final annotations (y/n)? ")
  if(save == 'y'){
    loc <- readline("Save location: ")
    write.csv(FiltAnnotations, loc) # Make this more foolproof - i.e. must be a full path to the folder then append file name?
  }
  
  ########
  
  # Further pre processing for multiple object detection
  source("C:/Annotations/PipelineScripts/Rscripts/Keras_Scripts/FormatAnnotations.R")
  KerasAnnotations <- FormatAnnotations(FiltAnnotations, Testing = test, comb_rows = F)



}
