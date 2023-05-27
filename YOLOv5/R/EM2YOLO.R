#' Convert an EventMeasure Database to YOLO annotation format
#'
#' This function takes an EventMeasure database export and converts it to the
#' YOLO annotation format. Images are randomly sampled and copied in to the 
#' relevant YOLO training/validation/testing directories with an 80/10/10 split.
#' .
#' @param EMDB The file path to the EventMeasure database. This may be a modified version of the EventMeasure export created with RenameSpecies()
#' @param pic.dir The path to the directory containg the images reffered to in the specied EventMeasure database
#' @param group.name The identifier to give this set of images. This must be a character string and will form the filename for the images copied in to the YOLO image directories. Image filenames will be 'NUMBER_group.name.jpg'.
#' @param species.index A dataframe containing each unique value in the EventMeasure database 'Species' column and an associated index value. This should be created with the EMCreateYAML() function so as to be consistent with the YAML configuration file for the YOLO model.
#' @param img.height The height (in pixels) of annotated images (required to transform bounding box coordinates to YOLO format).
#' @param img.width The width (in pixels) of annotated images (required to transform bounding box coordinates to YOLO format).
#' @param YOLO.dir The path to the directory containing the YOLO training data. This directory needs to have a specific structure, containing two subdirectories named 'images' and 'labels'. Each of these must then have three subdirectories named 'Train', 'Val' and 'Test'.
#' @param remove.anno An optional character vector containing the name of any species that should not be included in the YOLO annotations. The species names must appear exactly as they do in the 'Species' Column of the EventMeasure databse. This can be useful when, for example, multiple species are annotated but a detector is required for only a small number of those.
#' @export
#' 
#' @examples
#' EM2YOLO(EMDB = 'path/to/EMdatabase.txt',
#'         pic.dir = 'path/to/EM/images',
#'         group.name = 'Set1',
#'         species.index = sptable,
#'         img.height = 1080,
#'         img.width = 1920,
#'         YOLO.dir = path/to/YOLO/data,
#'         remove.anno = c('sp1', 'sp2'))


EM2YOLO <- function(EMDB, pic.dir, group.name, species.index, img.height, img.width, 
                    YOLO.dir, remove.anno = NULL){
  
  # Load in the EM DB file
  EM <- read.csv(EMDB, sep = '\t')
  
  # Create the new individual file names to prevent overwriting later one
  imgs <- list.files(pic.dir)
  
  imgindex <- as.character(seq(1:length(imgs)))
  imgindex <- sapply(imgindex, paste0, '_', group.name, '.jpg')
  
  imgs <- cbind(imgs, imgindex) %>%
    as.data.frame()
  
  # Randomly select training and validation images
  nimgs <- nrow(imgs)
  ntrain <- floor(nimgs*0.8)
  set.seed(123)
  trainimgs <- imgs[sample(nimgs, size = ntrain),]
  testimgs <- imgs %>%
    filter(!(imgindex %in% trainimgs$imgindex))
  ntest <- nrow(testimgs)
  valimgs <- testimgs[sample(ntest, size = ntest*0.5),]
  testimgs <- testimgs %>%
    filter(!(imgindex %in% valimgs$imgindex))
  
  # set colnames of imgs for later
  colnames(imgs) <- c('EMimg', 'YOLOimg')
  
  # Create a class index - first filtering out empty images
  
  index <- function(x, spid = species.index){
    comp <- species.index %>%
      filter(species == x)
    return(comp$id)
  }
  
  if(!is.null(remove.anno)){
    EMfilt <- EM %>%
      filter(!(Species %in% remove.anno))
  }
  
  EMfilt <- EM %>%
    rowwise() %>%
    mutate(sp_id = index(Species))
  
  # Split the annotations for each image
  imgannots <- EMfilt %>%
    split(f = as.factor(.$Filename))
  
  # Transform the annotations into YOLO format and save into the relevant directory
  imgTransform <- function(imgannot, imheight = img.height, imwidth = img.width){
    # split into individual annotations
    annots <- imgannot %>%
      rowwise %>%
      group_split()
    
    # Calculate coordinates for each annotation
    reformat <- function(anno, img.width = imwidth, img.height = imheight){
      class <- anno$sp_id
      x_centre <- (anno$ImageCol+(anno$RectWidth/2))/img.width
      y_centre <- (anno$ImageRow+(anno$RectHeight/2))/img.height
      width <- anno$RectWidth/imwidth
      height <- anno$RectHeight/imheight
      
      annot <- cbind(class, x_centre, y_centre, width, height) %>%
        as.data.frame()
    }
    
    annots <- lapply(annots, reformat)
    YOLOannots <- do.call(rbind, annots) %>%
      mutate(class = as.numeric(class))
    
    # Set the filename for saving annotations
    EMfilename <- as.character(imgannot$Filename[[1]])
    comp <- parent.frame(n=2)$imgs %>%
      filter(EMimg == EMfilename)
    filename <- tools::file_path_sans_ext(comp$YOLOimg)
    filename <- paste0(filename, '.txt')

    # Determine if the image for testing or training
    if(imgannot$Filename[[1]] %in% parent.frame(n=2)$trainimgs$imgs){
      # Image is for training
      savepath <- paste0(parent.frame(n=2)$YOLO.dir, '/labels/Train/', filename)
      write.table(YOLOannots, file = savepath, sep = ' ', col.names = F, row.names = F)
    } else if(imgannot$Filename[[1]] %in% parent.frame(n=2)$testimgs$imgs){
      # Image is for testing
      savepath <- paste0(parent.frame(n=2)$YOLO.dir, '/labels/Test/', filename)
      write.table(YOLOannots, file = savepath, sep = ' ', col.names = F, row.names = F)
    } else if(imgannot$Filename[[1]] %in% parent.frame(n=2)$valimgs$imgs){
      # Image is for testing
      savepath <- paste0(parent.frame(n=2)$YOLO.dir, '/labels/Val/', filename)
      write.table(YOLOannots, file = savepath, sep = ' ', col.names = F, row.names = F)
    } else(
      print('Error: Image not found in testing or training set')
    )
    
    return(imgannot)
    
  }
  
  imgannots <- lapply(imgannots, imgTransform)
  
  # Change the image datasets to contain the full relevant filepaths
  testimgs$imgs <- paste0(pic.dir, '/', testimgs$imgs)
  trainimgs$imgs <- paste0(pic.dir, '/', trainimgs$imgs)
  valimgs$imgs <- paste0(pic.dir, '/', valimgs$imgs)
  testimgs$imgdest <- paste0(YOLO.dir, '/images/Test/', testimgs$imgindex)
  trainimgs$imgdest <- paste0(YOLO.dir, '/images/Train/', trainimgs$imgindex)
  valimgs$imgdest <- paste0(YOLO.dir, '/images/Val/', valimgs$imgindex)
  
  # copy the images into the YOLO directories
  file.copy(from = testimgs$imgs, to = testimgs$imgdest)
  file.copy(from = trainimgs$imgs, to = trainimgs$imgdest)
  file.copy(from = valimgs$imgs, to = valimgs$imgdest)
}




