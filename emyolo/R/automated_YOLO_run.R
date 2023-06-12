## Batch train YOLO models

# Empty image directories and ensure they are ready for new round of training
# Create random sample of test/train/val images
# Prepare all config files
# run training and validation for each sub sample (250, 500, 750 images etc)
# Save training outputs so results can be extracted



## Prep image directories

EmptySubfolders <- function(rootDir) {
  # Get the list of subdirectories
  subDirs <- list.dirs(rootDir, recursive = TRUE, full.names = TRUE)
  
  # Remove all the files within the subdirectories
  file.remove(unlist(lapply(subDirs, list.files, full.names = TRUE)))
}

EmptySubfolders("C:/YOLOv5/Training/Subset")


# Distribute the images randomly into subsets

setwd("C:/YOLOv5")

SampleSizes <- c(250, 500, 750, 1000, 1250, 1750)

applySubsetAnnotations <- function(sampleSize) {
  subsetDir <- paste0("Training/Subset/", sampleSize)
  
  SubsetAnnotations(
    img.dir = file.path("Training/images/Train"),
    label.dir = file.path("Training/labels/Train"),
    sample.size = sampleSize * 0.8,
    target.img.dir = file.path(subsetDir, "images/Train"),
    target.label.dir = file.path(subsetDir, "labels/Train")
  )
  
  SubsetAnnotations(
    img.dir = file.path("Training/images/Test"),
    label.dir = file.path("Training/labels/Test"),
    sample.size = sampleSize * 0.1,
    target.img.dir = file.path(subsetDir, "images/Test"),
    target.label.dir = file.path(subsetDir, "labels/Test")
  )
  
  SubsetAnnotations(
    img.dir = file.path("Training/images/Val"),
    label.dir = file.path("Training/labels/Val"),
    sample.size = sampleSize * 0.1,
    target.img.dir = file.path(subsetDir, "images/Val"),
    target.label.dir = file.path(subsetDir, "labels/Val")
  )
}

lapply(sampleSizes, applySubsetAnnotations)

system('conda run -n YOLOv52 python val.py --weights runs/train/exp27/weights/best.pt --data C:/YOLOv5/Training/config.yaml --task test --save-txt')







