### Evaluating model performance ###

# 1. Load Viame CSV files and format (see MW and vvipr)
  # - Create 
# 2. Calc IoU for each predicion and determine number of TP, FP and FN
# 3. Calc precision, recall and f1 score
# 4. Calc AP and mAP

# Packages/functions to look at
# polyoverlap
# sf

EvalModel <- function(truthfile, predfile){
  
  # Load csv files skipping metadata
  tcsv <- read.csv(truthfile, header = F, skip = 2)
  pcsv <- read.csv(predfile, header = F, skip = 2)
  
  # Name the columns
  colnames(tcsv) <- c("trackID","videoID","frameID","TL_x","TL_y","BR_x","BR_y","conf","targetlength","species","attributes")
  colnames(pcsv) <- c("trackID","videoID","frameID","TL_x","TL_y","BR_x","BR_y","conf","targetlength","species","attributes")
  
  # Order each df by frameID
  tcsv <- tcsv[order(tcsv$frameID),]
  pcsv <- pcsv[order(pcsv$frameID),]
  
  # Clean unneccesary columns - currently removes videoID column which may be useful in the future
  # if using multiple videos rather than a single video or image sequence
  tcsv <- tcsv[,-c(2,9,11)]
  pcsv <- pcsv[,-c(2,9,11)]
  
  # Create an ID column that combines track and frame ID
  tcsv$ID <- paste(tcsv$trackID, tcsv$frameID, sep = '_')
  pcsv$ID <- paste(pcsv$trackID, pcsv$frameID, sep = '_')
  
  # Add a truth and prediction for later use
  tcsv$type <- 'truth'
  pcsv$type <- 'pred'
  
  # Bind the two data sets
  acsv <- rbind(tcsv,pcsv)
  
  # Create a list of dataframes where each df describes one frame
  aFrames <- split(acsv, f = acsv$frameID)
  
  ## Convert each line from the dataframes into a spatial polygon
  
  CreatePolygons <- function(df){

   polydfs <- list()
   data <- list()

   for(i in 1:nrow(df)){

    r <- df[i,]

    polydfs[[i]] <- data.frame(trackID = r$trackID,
                               frameID = r$frameID,
                               ID = r$ID,
                               X = c(r$TL_x, r$BR_x, r$BR_x, r$TL_x, r$TL_x),
                               Y = -c(r$TL_y, r$TL_y, r$BR_y, r$BR_y, r$TL_y),
                               conf = r$conf,
                               species = r$species)
    
    
    
   }
   
   fPolys <- lapply(polygons, sfheaders::sf_polygon, x = 'X', y = 'Y', polygon_id = 'ID')
   



 #  polys <- sfheaders::sf_polygon(df, x = 'X', y = 'Y')

   return(fPolys)
 }

 aPolygons <- lapply(aFrames, CreatePolygons)
  
 return(aPolygons)
  
}


