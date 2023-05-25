
EMDB <- 'C:/YOLOv5/Training/EMOutputs/1000_MEOW_Points.txt'
picdir <- 'C:/Annotations/1000Frames_MEOW/Training'
group_name <- '1000MEOW'
imgheight <- 1080
imgwidth <- 1920
YOLOdir <- 'C:/YOLOv5/Training'
SpeciesIndex <- sptable
remove.anno = c('spp', 'limanda')
label.type = 'group'
groups <- list(benthic = list('albida', 'bernhardus', 'pagarus', 'rubens', 'undatum', 'vulgaris'),
               benthopelagic = list('gurnadus', 'intermedius', 'limanda', 'limanda-flesus', 'lyra', 'platessa', 'scorpius'),
               pelagic = list('aeglefinus', 'canicula', 'capillata', 'merlangus', 'minutus', 'molva', 'morhua'))


EM2YOLO <- function(EMDB, picdir, group_name, SpeciesIndex, imgheight, imgwidth, 
                    YOLOdir, remove.anno = NULL, label.type = 'original', groups = NULL){
  
  # Load in the EM DB file
  EM <- read.csv(EMDB, sep = '\t')
  
  # Create the new individual file names to prevent overwriting later one
  imgs <- list.files(picdir)
  
  imgindex <- as.character(seq(1:length(imgs)))
  imgindex <- sapply(imgindex, paste0, '_', group_name, '.jpg')
  
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
  
  index <- function(x, spid = SpeciesIndex){
    comp <- SpeciesIndex %>%
      filter(species == x)
    return(comp$id)
  }
  
  if(label.type == 'original'){
    EMfilt <- EM %>%
      filter(!(Species %in% remove.anno)) %>% 
      rowwise() %>%
      mutate(sp_id = index(Species))
  }
  
  # Split the annotations for each image
  imgannots <- EMfilt %>%
    split(f = as.factor(.$Filename))
  
  # Transform the annotations into YOLO format and save into the relevant directory
  imgTransform <- function(imgannot, imheight = imgheight, imwidth = imgwidth){
    # split into individual annotations
    annots <- imgannot %>%
      rowwise %>%
      group_split()
    
    # Calculate coordinates for each annotation
    reformat <- function(anno, imgwidth = imwidth, imgheight = imheight){
      class <- anno$sp_id
      x_centre <- (anno$ImageCol+(anno$RectWidth/2))/imgwidth
      y_centre <- (anno$ImageRow+(anno$RectHeight/2))/imgheight
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
      savepath <- paste0(parent.frame(n=2)$YOLOdir, '/labels/Train/', filename)
      write.table(YOLOannots, file = savepath, sep = ' ', col.names = F, row.names = F)
    } else if(imgannot$Filename[[1]] %in% parent.frame(n=2)$testimgs$imgs){
      # Image is for testing
      savepath <- paste0(parent.frame(n=2)$YOLOdir, '/labels/Test/', filename)
      write.table(YOLOannots, file = savepath, sep = ' ', col.names = F, row.names = F)
    } else if(imgannot$Filename[[1]] %in% parent.frame(n=2)$valimgs$imgs){
      # Image is for testing
      savepath <- paste0(parent.frame(n=2)$YOLOdir, '/labels/Val/', filename)
      write.table(YOLOannots, file = savepath, sep = ' ', col.names = F, row.names = F)
    } else(
      print('Error: Image not found in testing or training set')
    )
    
    return(imgannot)
    
  }
  
  imgannots <- lapply(imgannots, imgTransform)
  
  # Change the image datasets to contain the full relevant filepaths
  testimgs$imgs <- paste0(picdir, '/', testimgs$imgs)
  trainimgs$imgs <- paste0(picdir, '/', trainimgs$imgs)
  valimgs$imgs <- paste0(picdir, '/', valimgs$imgs)
  testimgs$imgdest <- paste0(YOLOdir, '/images/Test/', testimgs$imgindex)
  trainimgs$imgdest <- paste0(YOLOdir, '/images/Train/', trainimgs$imgindex)
  valimgs$imgdest <- paste0(YOLOdir, '/images/Val/', valimgs$imgindex)
  
  # copy the images into the YOLO directories
  file.copy(from = testimgs$imgs, to = testimgs$imgdest)
  file.copy(from = trainimgs$imgs, to = trainimgs$imgdest)
  file.copy(from = valimgs$imgs, to = valimgs$imgdest)
}




