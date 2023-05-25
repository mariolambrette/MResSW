### CALCULATIONS of a viame model detecting flapper skates in novel video data
library(spatstat.utils)
library(matlab)
library(polyclip)

#setwd()
setwd("c:/users/mjw205/onedrive - university of exeter/projects/viame")

#detector output(s)
detectorOutput <- read.csv("data/clip-20180906_WEC_P1S_W1_fs_detector2_output.csv", header=T, skip=2)
colnames(detectorOutput) <- c("trackID","videoID","frameID","TL_x","TL_y","BR_x","BR_y","conf","targetlength","species","attributes")
detectorOutput <- detectorOutput[order(detectorOutput$frameID),]

#validation/test output(s)
annotatedTest  <- read.csv("data/clip-20180906_WEC_P1S_N1.csv", header=T, skip=2)
colnames(annotatedTest)  <- c("trackID","videoID","frameID","TL_x","TL_y","BR_x","BR_y","conf","targetlength","species","attributes")
annotatedTest <- annotatedTest[order(annotatedTest$frameID),]
annotatedTest <- annotatedTest[!duplicated(annotatedTest$frameID),]

#simplify detectorOutput dataset to single frameIDs with highest confidence 
#[as multiple instances of the same frame.id exist]
D <- list()
frameid <- unique(detectorOutput$frameID)
for (i in 1:length(frameid)){
  S <- subset(detectorOutput, frameID == frameid[i])
  if (nrow(S) > 1){
    D[[i]] <- S[which.max(S$conf),]
  }
  else {
    D[[i]] <- S
  }
}
detectorOutput <- do.call(rbind, D)

#### COMPARE DETECTION RECTANGLES (detectorOutput as MASTER, annotatedTest as LOOKUP)
#create results column(s)
detectorOutput$frame_area      <- rep(NA,nrow(detectorOutput))
detectorOutput$aT_frameID      <- rep(NA,nrow(detectorOutput))
detectorOutput$aT_rowN         <- rep(NA,nrow(detectorOutput))
detectorOutput$aT_TLx          <- rep(NA,nrow(detectorOutput))
detectorOutput$aT_TLy          <- rep(NA,nrow(detectorOutput))
detectorOutput$aT_BRx          <- rep(NA,nrow(detectorOutput))
detectorOutput$aT_BRy          <- rep(NA,nrow(detectorOutput))
detectorOutput$aT_conf         <- rep(NA,nrow(detectorOutput))
detectorOutput$aT_area         <- rep(NA,nrow(detectorOutput))
detectorOutput$polyOverlap     <- rep(NA,nrow(detectorOutput))
detectorOutput$union_area      <- rep(NA,nrow(detectorOutput))

#perform comparison
for (i in 1:nrow(detectorOutput)){
  
  #find rows within annotatedTest dataset that TIME match to relevant detectorOutput detection being considered
  rowN <- which(detectorOutput$frameID[i] == annotatedTest$frameID)
  
  #if frameID match identified, next consider spatial overlap of bounding polygons
  if (length(rowN)>0){
    aTx <- c(annotatedTest$TL_x[rowN],annotatedTest$TL_x[rowN],annotatedTest$BR_x[rowN],annotatedTest$BR_x[rowN])
    aTy <- c(annotatedTest$TL_y[rowN],annotatedTest$BR_y[rowN],annotatedTest$BR_y[rowN],annotatedTest$TL_y[rowN])
    
    dOx <- c(detectorOutput$TL_x[i],detectorOutput$TL_x[i],detectorOutput$BR_x[i],detectorOutput$BR_x[i])
    dOy <- c(detectorOutput$TL_y[i],detectorOutput$BR_y[i],detectorOutput$BR_y[i],detectorOutput$TL_y[i])
    
    dO <- list(x=dOx,y=dOy)
    aT <- list(x=aTx,y=aTy)
    
    #evaluate whether detection polygons from a time matched event overlap in space
    if (!any(is.na(c(dO,aT)))){
      
      polyOverlap <- overlap.xypolygon(dO, aT)
      
      if (polyOverlap > 0){
        #print("--------")
        #print(i)
        #print(rowN)
        #print(detectorOutput$frameID[i])
        #print(detectorOutput$TL_x[i])
        #print(detectorOutput$TL_y[i])
        #print(detectorOutput$BR_x[i])
        #print(detectorOutput$BR_y[i])
        #print(polyOverlap)
        
        union_area                        <- polyclip(dO,aT,op="union",closed=TRUE)
        detectorOutput$frame_area[i]      <- abs(Area.xypolygon(dO))
        detectorOutput$aT_frameID[i]      <- annotatedTest$frameID[rowN]
        detectorOutput$aT_rowN[i]         <- rowN
        detectorOutput$aT_TLx[i]          <- annotatedTest$TL_x[rowN]
        detectorOutput$aT_TLy[i]          <- annotatedTest$TL_y[rowN]
        detectorOutput$aT_BRx[i]          <- annotatedTest$BR_x[rowN]
        detectorOutput$aT_BRy[i]          <- annotatedTest$BR_y[rowN]
        detectorOutput$aT_conf[i]         <- annotatedTest$conf[rowN]
        detectorOutput$aT_area[i]         <- abs(Area.xypolygon(aT))
        detectorOutput$polyOverlap[i]     <- polyOverlap
        detectorOutput$union_area[i]      <- abs(Area.xypolygon(union_area[[1]]))
      }
    }
  }
}

#export the detectorOuput dataset, now includes information on time&space relevant matches from the annotatedTest
write.csv(detectorOutput,"data/detectorOutput_results2.csv", row.names=FALSE)

#### COMPARE DETECTION RECTANGLES (detectorOutput as LOOKUP, annotatedTest as MASTER)
#create results column(s)
annotatedTest$frame_area      <- rep(NA,nrow(annotatedTest))
annotatedTest$dO_frameID      <- rep(NA,nrow(annotatedTest))
annotatedTest$dO_rowN         <- rep(NA,nrow(annotatedTest))
annotatedTest$dO_TLx          <- rep(NA,nrow(annotatedTest))
annotatedTest$dO_TLy          <- rep(NA,nrow(annotatedTest))
annotatedTest$dO_BRx          <- rep(NA,nrow(annotatedTest))
annotatedTest$dO_BRy          <- rep(NA,nrow(annotatedTest))
annotatedTest$dO_conf         <- rep(NA,nrow(annotatedTest))
annotatedTest$dO_area         <- rep(NA,nrow(annotatedTest))
annotatedTest$polyOverlap     <- rep(NA,nrow(annotatedTest))
annotatedTest$union_area      <- rep(NA,nrow(annotatedTest))

#perform comparison
for (i in 1:nrow(annotatedTest)){
  
  #find rows within annotatedTest dataset that TIME match to relevant detectorOutput detection being considered
  rowN <- which(annotatedTest$frameID[i] == detectorOutput$frameID)

  #if frameID match identified, next consider spatial overlap of bounding polygons
  if (length(rowN)>0){
    aTx <- c(annotatedTest$TL_x[i],annotatedTest$TL_x[i],annotatedTest$BR_x[i],annotatedTest$BR_x[i])
    aTy <- c(annotatedTest$TL_y[i],annotatedTest$BR_y[i],annotatedTest$BR_y[i],annotatedTest$TL_y[i])
    
    dOx <- c(detectorOutput$TL_x[rowN],detectorOutput$TL_x[rowN],detectorOutput$BR_x[rowN],detectorOutput$BR_x[rowN])
    dOy <- c(detectorOutput$TL_y[rowN],detectorOutput$BR_y[rowN],detectorOutput$BR_y[rowN],detectorOutput$TL_y[rowN])
    
    dO <- list(x=dOx,y=dOy)
    aT <- list(x=aTx,y=aTy)
    
    #evaluate whether detection polygons from a time matched event overlap in space
    if (!any(is.na(c(dO,aT)))){
      
      polyOverlap <- overlap.xypolygon(dO, aT)
      
      if (polyOverlap > 0){
        #print("--------")
        #print(i)
        #print(rowN)
        #print(annotatedTest$frameID[i])
        #print(annotatedTest$TL_x[i])
        #print(annotatedTest$TL_y[i])
        #print(annotatedTest$BR_x[i])
        #print(annotatedTest$BR_y[i])
        #print(polyOverlap)
        
        union_area                       <- polyclip(aT,dO,op="union",closed=TRUE)
        annotatedTest$frame_area[i]      <- abs(Area.xypolygon(aT))
        annotatedTest$dO_frameID[i]      <- detectorOutput$frameID[rowN]
        annotatedTest$dO_rowN[i]         <- rowN
        annotatedTest$dO_TLx[i]          <- detectorOutput$TL_x[rowN]
        annotatedTest$dO_TLy[i]          <- detectorOutput$TL_y[rowN]
        annotatedTest$dO_BRx[i]          <- detectorOutput$BR_x[rowN]
        annotatedTest$dO_BRy[i]          <- detectorOutput$BR_y[rowN]
        annotatedTest$dO_conf[i]         <- detectorOutput$conf[rowN]
        annotatedTest$dO_area[i]         <- abs(Area.xypolygon(dO))
        annotatedTest$polyOverlap[i]     <- polyOverlap
        annotatedTest$union_area[i]      <- abs(Area.xypolygon(union_area[[1]]))
      }
    }
  }
}

#export the annotatedTest dataset, now includes information on time&space relevant matches from the detector output
write.csv(annotatedTest,"data/annotatedTest_results2.csv", row.names=FALSE)