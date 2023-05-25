### Evaluating model performance ###

# 1. Load Viame CSV files and format (see MW and vvipr)
# 2. Calc IoU for each predicion and determine number of TP, FP and FN
# 3. Calc precision, recall and f1 score
#  Steps 2 and 3 need to be iterated over multiple confidence thresholds
# 4. Calc AP and mAP

# Packages/functions to look at

EvalModel <- function(truthfile, predfile, Conf.thresholds = 'default', IoU.threshold){
  
  library(dplyr)
  
  # Load csv files skipping metadata and name columns
  tcsv <- read.csv(truthfile, header = F, skip = 2)
  colnames(tcsv) <- c("trackID","videoID","frameID","TL_x","TL_y","BR_x","BR_y","conf","targetlength","species","attributes")
  pcsv <- read.csv(predfile, header = F, skip = 2)
  colnames(pcsv) <- c("trackID","videoID","frameID","TL_x","TL_y","BR_x","BR_y","conf","targetlength","species","attributes")
  
  # Format the truth and prediction data
  tcsv <- tcsv %>%
    arrange(frameID) %>%
    select(-c('videoID', 'targetlength', 'attributes')) %>%
    mutate(ID = paste(trackID, frameID, sep = '_'),
           type = 'truth')
  
  pcsv <- pcsv %>%
    arrange(frameID) %>%
    select(-c('videoID', 'targetlength', 'attributes')) %>%
    mutate(ID = paste(trackID, frameID, sep = '_'),
           type = 'pred')
  
  # Bind the two data sets
  acsv <- rbind(tcsv,pcsv)
  
  ##  Create a list of filtered acsvs for each confidence level
  # Define confidence thresholds if it has been left as default
  if(Conf.thresholds == 'default'){
    Conf.thresholds <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.90, 0.95)
  }
  
  # Function to be applied over list of confidence intervals
  Filteracsv <- function(conf.interval, csv){
    Filtacsv <- csv %>%
      filter(conf > conf.interval) %>%
      mutate(conf.thresh = conf.interval)
    return(Filtacsv)
  }
  
  # Create the list of dataframes
  FiltData <- lapply(Conf.thresholds, Filteracsv, csv = acsv)
  names(FiltData) <- Conf.thresholds

  # Create a summary of all the species predictions at each confidence threshold
  ConfPredSummary <- function(FilteredData){
    preds <- FilteredData %>%
      filter(type == 'pred') %>%
      as.data.frame()
    
    conf.thresh <- preds$conf.thresh
    conf.thresh <- conf.thresh[[1]]
    
    predsum <- table(preds$species) %>%
      as.data.frame()
    
    predsum$conf <- conf.thresh
    
    colnames(predsum) <- c('species', 'frequency', 'conf.threshold')
    
    return(predsum)
  }
  
  PredSums <- lapply(FiltData, ConfPredSummary)
  
  # Create a single df with each prediction summary for export if required
  aPredSums <- do.call(rbind, PredSums)
  row.names(aPredSums) <- seq(1:nrow(aPredSums))
  
  print('The following species were detected at each confidence threshold:')
  print(aPredSums)
  
  savepredsum <- readline('Save a copy of the prediction summary? (T/F):')
  if(savepredsum){
    savelocation <- readline('Where should the file be saved?')
    write.csv(aPredSums, file = savelocation, row.names = F)
  }
  
  CalcResults <- function(FilteredData, IoU.thresh = IoU.threshold){
    
    conf <- FilteredData$conf.thresh[[1]]
    print(conf)

    # Create a list of dataframes where each df describes one frame
    aFrames <- split(FilteredData, f = FilteredData$frameID)
    
    # Split each frame df into predictions and annotations
    sFrames <- lapply(aFrames, dplyr::group_split, type)
    
    # Create the function to be iterated over each frame and calculate comparisons
    FrameComparisons <- function(FrameData, thresh = IoU.thresh){
      # This function needs to:
      # - For each prediction run through the truth annotations and for those of
      #   the same class calculate the IoU
      # - If a Match is found remove the truth annotation from future searches for
      #   subsequent predictions
      # - Count the number of TPs (IoU is above threshold)
      # - Count the number of FPs (IoU is below threshold)
      # - Count the number of FNs (truth annotations with no match)
      # - Return a df (Fdata) with 4 columns: TP, FP, FN, species and a row for
      #   species in the frame
      
      # Input is a list with two elements - a df of predictions and a df of truths
      
      # filter for scenario with no truths or predictions
      if(length(FrameData) != 2){
        if(length(FrameData) > 2){
          stop("Error in Frame data")
        } else{
          Fdata <- FrameData[[1]]
          if(Fdata$type[1] == 'pred'){
            Fdata <- Fdata %>%
              count(species) %>%
              rename(FP = n) %>%
              mutate(TP = 0, FN = 0) %>%
              relocate(TP, FP, FN, species)
            
            return(Fdata)
          } else {
            Fdata <- Fdata %>%
              count(species) %>%
              rename(FN = n) %>%
              mutate(TP = 0, FP = 0) %>%
              relocate(TP, FP, FN, species)
            
            return(Fdata)
          }
        }
      }
      
      # Define the truth and prediction data frames
      if(FrameData[[1]]$type[1] == 'pred'){
        preds <- FrameData[[1]]
        truths <- FrameData[[2]]
      } else{
        preds <- FrameData[[2]]
        truths <- FrameData[[1]]
      }
      
      # Find the species in the frame
      tsps <- unique(preds$species)
      psps <- unique(truths$species)
      sps <- unique(c(tsps, psps))
      
      # Define the empty Fdata df for the frame
      Fdata <- tibble(TP = 0,
                      FP = 0,
                      FN = 0,
                      species = sps)
      
      
      # Create a list of truths and predictions by ID and add the 'complete' column to
      # be used later to check whether the annotation has been successfuly matched
      predsl <- preds %>%
        mutate(complete = F) %>%
        group_split(ID)
      truthsl <- truths %>%
        mutate(complete = F) %>%
        group_split(ID)
      
      
      # Iterate over each prediction
      for(i in 1:length(predsl)){
        # Select the prediction data
        pred <- predsl[[i]]
        # identify the species
        sp <- pred$species
        
        # Iterate each prediction over each truth annotation
        for(j in 1:length(truthsl)){
          truth <- truthsl[[j]]
          
          if(sp == truth$species){
            # Check whether the truth annotation has already been matched
            if(truth$complete){
              next
            }
            
            ## DO COMPARISON ##
            # comparison returns match or not
            
            # define t and p polygon data frames
            tpolydf <- data_frame(ID = 'truth',
                                  X = c(truth$TL_x, truth$BR_x, truth$BR_x, truth$TL_x, truth$TL_x),
                                  Y = -c(truth$TL_y, truth$TL_y, truth$BR_y, truth$BR_y, truth$TL_y))
            ppolydf <- data_frame(ID = 'pred',
                                  X = c(pred$TL_x, pred$BR_x, pred$BR_x, pred$TL_x, pred$TL_x),
                                  Y = -c(pred$TL_y, pred$TL_y, pred$BR_y, pred$BR_y, pred$TL_y))
            
            # Merge the dataframes and create an st collection with two polygons
            polydf <- rbind(tpolydf,ppolydf)
            
            polys <- polydf %>%
              sf::st_as_sf(coords = c('X','Y')) %>%
              group_by(ID) %>%
              summarise(geometry = sf::st_combine(geometry)) %>%
              sf::st_cast('POLYGON')
            
            # check if the polygons intersect
            int <- sf::st_intersects(polys)
            
            # If polygons do not intersect set 'res' to false
            if(length(int[[1]]) == 1){
              res <- FALSE
              
            } else{
              # Define the polygon of the overlapping area and calculate area
              intersection <- sf::st_intersection(polys) %>%
                filter(n.overlaps == 2) %>%
                sf::st_area()
              
              # Define the polygon of the union area and calculate area
              union <- sf::st_union(polys) %>%
                sf::st_area()
              
              # Calculate IoU
              IoU <- intersection/union
              
              # Determine if IoU meets threshold and assign res accordingly
              if(IoU > thresh){
                res <- TRUE
              } else {
                res <- FALSE
              }
              
            }
            
            match <- res
            
            if(match){
              # Add one to the TP count for the relevant species
              Fdata[Fdata$species == sp, 'TP'] <- Fdata[Fdata$species == sp, 'TP']+1
              
              # Set the 'complete' fields for the annotations to T
              truthsl[[j]]$complete <- T
              pred$complete <- T
              predsl[[i]]$complete <- T
              
              # skip remaining truth comparisons and move to next prediction
              break
              
            } else{ # Comparison did not result in match
              if(j == length(truthsl)){
                # The final truth does not result in a match
                # Add one to the FP count for the relevant species
                Fdata[Fdata$species == sp, 'FP'] <- Fdata[Fdata$species == sp, 'FP']+1
                
                # Set the pred 'complete' field to T
                pred$complete <- T
                predsl[[i]]$complete <- T
              } else{
                # move on to the next truth annotation
                next
              }
            }
            
          } else{
            # Check if this is the last truth annotation - in which case the predicition
            # is a false positive
            # skip this truth if the species do not match
            
            # Check if this is the final truth annotation
            if(j == length(truthsl)){
              # Add one to the FP count for the relevant species
              Fdata[Fdata$species == sp, 'FP'] <- Fdata[Fdata$species == sp, 'FP']+1
              
              # Set the 'complete' field for the prediciton to T
              pred$complete <- T
              predsl[[i]]$complete <- T
            }
            
            next
          }
        }
      }
      
      # Count the number of FNs for each species in the ground truth data
      truthFN <- do.call(rbind, truthsl) %>%
        group_by(species) %>%
        summarise(complete) %>% ## This line produces the summarise() text in console
        filter(!complete) %>%
        table() %>%
        as.data.frame()
      
      # identify species with FNs
      FNsps <- as.vector(truthFN$species)
      
      for(i in 1:length(FNsps)){
        # Iterate over each species
        sp <- FNsps[[i]]
        
        # Find the number of false bnegatives for each species
        spFN <- truthFN[truthFN$species == sp, 'Freq']
        
        # Adjust Fdata accordingly
        Fdata[Fdata$species == sp, 'FN'] <- Fdata[Fdata$species == sp, 'FN']+spFN
      }
  
      return(Fdata)  
      
    }
   
   # Loop Comparisons over each frame
   # returns a list of 'Fdata' dataframes
   FrameSums <- lapply(sFrames, FrameComparisons, thresh = IoU.thresh)
   
   # Summarise the results of the comparisons for the whole dataset giving total 
   # TPs, FPs and FNs for each species and the whole data set
   results <- do.call(rbind, FrameSums) %>%
     group_by(species) %>%
     summarise(TP = sum(TP),
               FP = sum(FP),
               FN = sum(FN))
   results <- rbind(results, data.frame(species = 'total', t(colSums(results[,-1]))))
   
   
   # Calculate precision and recall for each species and in total
   results <- results %>%
     mutate(precision = TP/(TP+FP),
            recall = TP/(TP+FN))
   
   # calculate the f1 score
   results <- results %>%
     mutate(f1 = 2*((precision*recall)/(precision+recall)))
     
  
  ### Add a line to append a column showing the confidence level to each result  
    
   return(results)
  }
  
  results <- lapply(FiltData, CalcResults)
  
  return(results)
}

###############################################################################


