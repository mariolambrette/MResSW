#' Subset YOLO training dataset
#'
#' Once a YOLO training dataset has been created it may be useful to subset the
#' images and associated annotations, for example to train a model on a smaller
#' selection of images to understand the influence of dataset size on model
#' performance. This function allows the user to randomly select a specified number
#' of images from a YOLO dataset and copy both the images and asociated labels
#' into new target directories.
#' 
#' Note that this function is designed to work on a dataset that has been prepared to
#' the specified YOLO directory structure
#' .
#' @param img.dir The directory containing the full set of images to be subsetted
#'                (may be the training, validation or testing directory of the YOLO dataset).
#' @param label.dir The directory containing the label associated to images in img.dir.
#' @param sample.size The number of images to sample from the full set.
#' @param target.img.dir The directory that selected images should be copied in to.
#' @param target.label.dir The directory where labels for selected images should be copied.
#' @export
#' 
#' @examples
#' SubsetAnnotations(img.dir = 'Training/images/Train',
#'                   label.dir = 'Training/labels/Train',
#'                   sample.size = 200,
#'                   target.img.dir = 'Training/Subset/images/Train',
#'                   target.label.dir = 'Training/Subset/labels/Train')

SubsetAnnotations <- function(img.dir, label.dir, sample.size, target.img.dir, target.label.dir){
  library(fs)
  
  img.files <- list.files(img.dir, full.names = TRUE)
  
  # Select random sample of desired size
  selected <- sample(img.files, size = sample.size)
  
  # Get the base filenames with no extension
  base.names <- tools::file_path_sans_ext(basename(selected))
  
  # Get the vector of target annotation filepaths
  label.files <- file.path(label.dir, paste0(base.names, '.txt'))
  
  # create the character vectors showing destination for images and labels
  img.dest <- file.path(target.img.dir, paste0(base.names, '.jpg'))
  label.dest <- file.path(target.label.dir, paste0(base.names, '.txt'))
  
  # Copy the files across
  invisible(file.copy2(selected, img.dest))
  invisible(file.copy2(label.files, label.dest))
}




