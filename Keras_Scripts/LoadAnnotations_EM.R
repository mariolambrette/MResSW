#' Load EventMeasure Annotations into R environment
#'
#' This function loads an EventMeasure database export into the R environment and
#' converts it into the correct format for use in later anlyses
#'
#' .
#' @param ImgDir The file path to the folder containing jpegs exported from EventMeasure.
#' This can be either a relative or full file path.
#' @param EMFile The location of the text file exported by EventMeasure
#' @param ImageHeight The height (in pixels) of the images that will be used in 
#' the analysis
#' @param ImageWidth The width (in pixels) of the images that will be used in 
#' the analysis
#' @param TargetHeight The height (in pixels) of the scaled images that will be
#' passed to the subsequent model
#' @param TargetWidth The width (in pixels) of the scaled images that will be
#' passed to the subsequent model
#' @examples
#' LoadAnnotations_EM(ImgDir = 'C:/TrainingImages',
#'                    EMFile = 'C:/EMExportFile.txt',
#'                    ImageHeight = 1080,
#'                    ImageWidth = 1920,
#'                    TargetHeight = 224,
#'                    TargetWidth = 224)
#'

LoadAnnotations_EM <- function(ImgDir, EMFile, ImageHeight, ImageWidth, TargetHeight, TargetWidth){
  
  library(dplyr)
  
  # Read the exported EM annotations into R
  EM <- read.csv(EMFile, sep = '\t')
  
  # Create an index of categories
  EM <- EM %>%
    mutate(Species = as.factor(Species)) %>%
    mutate(sp_id = unclass(Species))
  
  # Create an index of image names
  EM <- EM %>%
    mutate(Filename = as.factor(Filename)) %>%
    mutate(img_id = as.numeric(unclass(Filename))) %>%
    mutate(Filename = as.character(Filename))
  

  # Transform the EM databse output to the correct format
  annotations <- tibble::tibble(
    id = EM$img_id,
    filepath = ImgDir,
    filename = EM$Filename,
    imgheight = ImageHeight,
    imgwidth = ImageWidth,
    sp_id = EM$sp_id,
    x_left = floor(EM$ImageCol),
    y_top = floor(EM$ImageRow),
    boxheight = EM$RectHeight,
    boxwidth = EM$RectWidth,
    y_bottom = floor(EM$ImageRow)+ceiling(EM$RectHeight),
    x_right = floor(EM$ImageCol) + ceiling(EM$RectWidth),
    sp_name = EM$Species %>% as.character()
  )
  
  # Create the correct scaled bounding box coordinates
  annotations <- annotations %>%
    mutate(x_left_scaled = (x_left / imgwidth * TargetWidth) %>% round(digits = 7),
           x_right_scaled = (x_right / imgwidth * TargetWidth) %>% round(digits = 7),
           y_top_scaled = (y_top / imgheight * TargetHeight) %>% round(digits = 7),
           y_bottom_scaled = (y_bottom / imgheight * TargetHeight) %>% round(digits = 7),
           boxwidth_scaled =  (boxwidth / imgwidth * TargetWidth) %>% round(digits = 7),
           boxheight_scaled = (boxheight / imgheight * TargetHeight) %>% round(digits = 7)
    )

  return(annotations)
  
}