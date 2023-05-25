
### Run Script for Keras

# Set key varibales
TrainImgDir <- 'C:/Annotations/PreAnnotatedMethod/Training'
ImageHeight <- 1080
ImageWidth <- 1920


source("C:/Annotations/PipelineScripts/Rscripts/Keras_Scripts/LoadData_EM.R")

KerasAnnotations <- LoadData_EM(ImgDir = TrainImgDir,
                                EMFile = 'C:/Annotations/TrainedModels/3_Lisbon/DBOutputs/Train_ScotW_Meow_Improved_Points.txt',
                                ImageHeight = ImageHeight,
                                ImageWidth = ImageWidth,
                                TargetHeight = 224,
                                TargetWidth = 224,
                                test = T)

########## Train and save model ##########

































