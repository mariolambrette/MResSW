### PERFORMANCE STATISTICS - these are from the perspective of HOW GOOD IS THE DETECTOR ###

#setwd()
setwd("c:/users/mjw205/onedrive - university of exeter/projects/viame")

#results data.frames from COMPARE_VIAME.R
detectorOutput <- read.csv("data/detectorOutput_results.csv")
annotatedTest  <- read.csv("data/annotatedTest_results.csv")

#intersection over union
detectorOutput$iou <- detectorOutput$polyOverlap / detectorOutput$union_area

#confidence threshold
cf <- 0.9

#apply detection threshold to results data.frames
detectorOutput <- subset(detectorOutput, conf >= cf)
annotatedTest  <- subset(annotatedTest,  dO_conf >= cf)

### TRUE POSITIVES DETECTIONS [model correctly predicts presence] ###
#where the detector identified an animal and this is confirmed by the annotated test dataset
#select rows of the annotatedTest dataset where a polyOverlap area exists
TP <- nrow(subset(annotatedTest, polyOverlap > 0))


### FALSE POSITIVE DETECTIONS [model incorrectly predicts presence] ### 
#where the detector suggests an animal is present, but where this is not confimed by an animal being present
FP <- nrow(subset(detectorOutput, is.na(aT_conf)))


### FALSE NEGATIVE DETECTIONS ###
#where the detector has not indicated the presence of an animal appearing in the annotated test dataset
#select rows of the annotatedTest dataset that have NO entry (i.e. NA) in the polyOverlap column (i.e. those rows that have NA)
FN <- nrow(subset(annotatedTest, is.na(polyOverlap)))


### RECALL ###
TP / (TP + FN)


### PRECISION ###
TP / (TP + FP)