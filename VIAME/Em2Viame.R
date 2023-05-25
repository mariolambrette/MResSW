EM2VIAME <- function(EMlocation, EMtype = '\t', VIAMElocation, VIAMEfilename = NA, annotations = T, lengths = F,
                     fps = 1){

#### FORMAT EM FILE
#################

# Load EM output file
if(EMtype == 'csv'){
  EM <- read.csv(EMlocation)
  
  # Extract the column names from row 4 of the EM file
  titles <- as.vector(EM[4,])
  # Set them as dataframe column names
  colnames(EM) <- titles
  
  # Remove first 4 rows from EM file
  EM <- EM[-c(1,2,3,4),]
} else{
  EM <- read.csv(EMlocation, sep = EMtype)
}
# Create a vector of unique species that have been labelled
## Currently only using species name - could be modified to include genus/family
species <- unique(EM$Species)
# Count the number of labelled species
n_sp <- length(species)

#################

#### SET UP VIAME SKELETON FILES
#################


#Identify the number of rows required
rows <- 2+nrow(EM)

# Create the empty dataframe
V_data <- matrix(nrow = nrow(EM), ncol = 11)
V_data <- as.data.frame(V_data)

# Define column names
titles_v <- c('# 1: Detection or Track-id', '2: Video or Image Identifier', '3: Unique Frame Identifier',
              '4-7: Img-bbox(TL_x', 'TL_y', 'BR_x', 'BR_y)', '8: Detection or Length Confidence',
              '9: Target Length (0 or -1 if invalid)', '10-11+: Repeated Species', 
              'Confidence Pairs or Attributes')
colnames(V_data) <- titles_v

# Define the metadata matrix
V_met <- matrix(nrow = 2, ncol = 11) # Possible change number of columns back to cols if required - as above
V_met <- as.data.frame(V_met)
colnames(V_met) <- titles_v

V_met[1,] <- titles_v
V_met[2,1] <- '# metadata'
V_met[2,2] <- paste('fps: ', fps, sep = '')
V_met[2,3] <- 'exported_by: "dive:typescript"' # Understand what this part of the metadata is from documentation
V_met[2,4] <- 'exported_time: "6/26/2022, 12:26:15 PM"' # Understand what this part of the metadata is from documentation
V_met[2,c(5,6,7,8,9,10,11)] <- ''


#################


#### Populate V_data
#################

# Object IDs
IDs <- seq(0,nrow(EM)-1)
V_data[,1] <- IDs

# Time stamps

if(fps > 1){
  EMtimes <- as.numeric(EM$Time)
  
  timeconvert <- function(minutes){
    # Original minutes vector
   # minutes <- time
    
    # Get the number of whole minutes, save in `mins`
    mins <- floor(minutes)
    
    # Get the decimal from `minutes` and multiple by 60 to get
    # the number of seconds within a the minute. Save in `secs`
    secs <- format(round(minutes %% mins * 60, 2), nsmall = 2)
  
    # Paste them together in whatever format you would like
    paste0(mins, ":", secs)
  }
  
  Vtimes <- sapply(EMtimes, timeconvert)
  V_data[,2] <- Vtimes
} else{
  V_data[,2] <- EM$Filename
}

# Frame number

if(fps > 1){
  EMframe <- EM$Frame
  V_data[,3] <- EMframe ## Assuming that EM and Viame count frames in the same way - must verify
} else{
  ###
  EM_ID <- transform(EM, ID = as.numeric(factor(EM$Filename)))
  V_data[,3] <- EM_ID$ID
  ###
}


# Bounding box coordinates
# EM DB labels top left corner with width and height
# VIAME labels top left and bottom right

# TL_x
if(EMtype == 'csv'){
  V_data[,4] <- floor(EM[,7])
} else{
  V_data[,4] <- floor(EM$ImageCol)
}
#TL_y
if(EMtype == 'csv'){
  V_data[,5] <- floor(EM[,6])
} else{
  V_data[,5] <- floor(EM$ImageRow)
}
#BR_x
if(EMtype == 'csv'){
  V_data[,6] <- floor(as.numeric(EM[,7])+as.numeric(EM[,8]))
} else{
  V_data[,6] <- floor(EM$ImageCol+EM$RectWidth)
}
#BR_y
if(EMtype == 'csv'){
  V_data[,7] <- floor(as.numeric(EM[,6])-as.numeric(EM[,9]))
} else{
  V_data[,7] <- floor(EM$ImageRow+EM$RectHeight)
}

# Detection confidence
if(annotations){
  V_data[,8] <- 1
} else{
  stop('Data are not annotations')
}

# Length measurement
if(!lengths){
  V_data[,9] <- 0
} ## Add an else statement for if there are length measurmenets - need to understand how this
  ## would work with the viame file

# Species names
all_species <- EM$Species
V_data[,10] <- all_species

# Confidence pairs or attributes - ## need to understand what these are
# Just use 1 for now...
V_data[,11] <- 1

#### Bind V-data to the metadata
#############


if(is.na(VIAMEfilename)){
  f <- basename(EMlocation)
  VIAMEfilename <- gsub('txt', 'csv', f)
}
  
VIAMEcsv <- rbind(V_met, V_data)
VIAMEcsv <- VIAMEcsv[-1,]
write.csv(VIAMEcsv, file = paste(VIAMElocation, VIAMEfilename,sep='/'),
          row.names = F)

return(VIAMEcsv)

}
















