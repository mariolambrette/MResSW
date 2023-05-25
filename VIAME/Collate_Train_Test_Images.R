
library(dplyr)

# Define the model directory
model_dir <-'C:/Annotations/TrainedModels/VIAME/7_Beijing'

# List all the image files in the relevant directories
imgs <- list.files(path = 'C:/Annotations', pattern = '*.jpg', full.names = T,
                   recursive = T)

# Check for duplicate file names and remove them from the image list
nrm <- 0

for(i in 1:length(imgs)){
  if(basename(imgs[i]) %in% basename(imgs[-i])){
    imgs <- imgs[-i]
    nrm <- nrm+1
  }
}

# List all the EM DB outputs
EMOutputs <- list.files(path = 'C:/Annotations/TrainedModels/EM_DB_Outputs',
                        pattern = '*_Points.txt',
                        full.names = T)


# Convert the EM DBs to VIAME csv files
source('C:/Annotations/PipelineScripts/Rscripts/EM2VIAME.R')
Viamecsvs <- lapply(EMOutputs, EM2VIAME, VIAMElocation = 'C:/Annotations/TrainedModels/VIAMEcsvFiles')

AllAnnotations <- do.call(rbind, Viamecsvs)
rm(Viamecsvs)


# Create a summary of the annotations by species and verify the benthic/pelagic species lists
Sum <- as.data.frame(table(AllAnnotations$`10-11+: Repeated Species`))
Sum <- Sum %>%
  rename(Species = Var1,
         Frequency = Freq)
Sum <- Sum %>%
  filter(Species != "") %>%
  filter(Species != 'spp')
Sum

benthic <- list('albida', 'bernhardus', 'pagurus', 'rubens', 'undatum', 'vulgaris', 'linearis')
benthopelagic <- list('gurnardus', 'intermedius', 'limanda', 'limanda-flesus', 'lyra', 'platessa', 'scorpius')
pelagic <- list('aeglefinus', 'canicula', 'capillata', 'merlangus', 'minutus', 'molva', 'morhua')

aeglefinus canicula capillata merlangus minutus molva morhua
gurnadus intermedius limanda limanda-flesus lyra platessa scorpius
albida bernhardus pagarus rubens undatum vulgaris

# Select annotations belonging to each category
BenthicAnnotations <- AllAnnotations %>%
  filter(`10-11+: Repeated Species` %in% benthic)
BenthopelagicAnnotations <- AllAnnotations %>%
  filter(`10-11+: Repeated Species` %in% benthopelagic)
PelagicAnnotations <- AllAnnotations %>%
  filter(`10-11+: Repeated Species` %in% pelagic)
EmptyAnnotations <- AllAnnotations %>%
  filter(`10-11+: Repeated Species` == 'spp')


BenthicImages <- unique(BenthicAnnotations$`2: Video or Image Identifier`)
BenthoPelagicImages <- unique(BenthopelagicAnnotations$`2: Video or Image Identifier`)
PelagicImages <- unique(PelagicAnnotations$`2: Video or Image Identifier`)
EmptyImages <- unique(EmptyAnnotations$`2: Video or Image Identifier`)


# Create a function to split into training and validation datasets
TrainTestSplit <- function(imgs, train.size){
  smp_size <- floor(train.size*length(imgs))
  
  set.seed(123)
  train_ind <- sample(seq_len(length(imgs)), size = smp_size)
  
  train <- imgs[train_ind]
  test <- imgs[-train_ind]
  
  return(list(train = train, test = test))
}

SplitBenthic <- TrainTestSplit(BenthicImages, 0.9)
SplitBenthoPelagic <- TrainTestSplit(BenthoPelagicImages, 0.9)
SplitPelagic <- TrainTestSplit(PelagicImages, 0.9)
SplitEmpty <- TrainTestSplit(EmptyImages, 0.9)

# Create text files containing the image locations for each dataset
setwd(model_dir)

BenthicTrainImgs <- subset(imgs, unique(basename(imgs)) %in% SplitBenthic$train) %>%
  writeLines('ImageLists/BenthicTrain.txt')
BenthicTestImgs <- subset(imgs, unique(basename(imgs)) %in% SplitBenthic$test) %>%
  writeLines('ImageLists/Benthictest.txt')
BenthoPelagicTrainImgs <- subset(imgs, unique(basename(imgs)) %in% SplitBenthoPelagic$train) %>%
  writeLines('ImageLists/BenthoPelagicTrain.txt')
BenthoPelagicTestImgs <- subset(imgs, unique(basename(imgs)) %in% SplitBenthoPelagic$test) %>%
  writeLines('ImageLists/BenthoPelagicTest.txt')
PelagicTrainImgs <- subset(imgs, unique(basename(imgs)) %in% SplitPelagic$train) %>%
  writeLines('ImageLists/PelagicTrain.txt')
PelagicTestImgs <- subset(imgs, unique(basename(imgs)) %in% SplitPelagic$test) %>%
  writeLines('ImageLists/PelagicTest.txt')
EmptyTrainImgs <- subset(imgs, unique(basename(imgs)) %in% SplitEmpty$train) %>%
  writeLines('ImageLists/EmptyTrain.txt')
EmptyTestImgs <- subset(imgs, unique(basename(imgs)) %in% SplitEmpty$test) %>%
  writeLines('ImageLists/EmptyTest.txt')


# Select the relevant annotations for each of the image datasets
BenthicAnnotationsTrain <- BenthicAnnotations %>%
  filter(`2: Video or Image Identifier` %in% SplitBenthic$train) %>%
  write.csv('Annotations/BenthicTrain.csv')
BenthicAnnotationsTest <- BenthicAnnotations %>%
  filter(`2: Video or Image Identifier` %in% SplitBenthic$test) %>%
  write.csv('Annotations/BenthicTest.csv')
BenthopelagicAnnotationsTrain <- BenthopelagicAnnotations %>%
  filter(`2: Video or Image Identifier` %in% SplitBenthoPelagic$train) %>%
  write.csv('Annotations/BenthoPelagicTrain.csv')
BenthopelagicAnnotationsTest <- BenthopelagicAnnotations %>%
  filter(`2: Video or Image Identifier` %in% SplitBenthoPelagic$test) %>%
  write.csv('Annotations/BenthoPelagicTest.csv')
PelagicAnnotationsTrain <- PelagicAnnotations %>%
  filter(`2: Video or Image Identifier` %in% SplitPelagic$train) %>%
  write.csv('Annotations/PelagicTrain.csv')
PelagicAnnotationsTest <- PelagicAnnotations %>%
  filter(`2: Video or Image Identifier` %in% SplitPelagic$test) %>%
  write.csv('Annotations/PelagicTest.csv')


# Images to remove
'01_RGX010003.MP4_0008981'
'01_RGX010002.MP4_0005375'































