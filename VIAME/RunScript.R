## RUN SCRIPT - Preparing training data ##


source("C:/Annotations/PipelineScripts/Rscripts/PrepTrainingData.R")

TrainingData <- PrepTrainingData(EMDB = 'Train_1000_MEOW_1_Points.txt',
                                 Vdir = 'C:/Annotations/TrainedModels/6_Tokyo/VIAMEcsv',
                                 Vfilename = 'Train_1000Frames_MEOW1.csv',
                                 CleanData = T,
                                 Summary = T)


## Evaluate trained model

source("C:/Annotations/PipelineScripts/Rscripts/EvalModel_v2.R")

results <- EvalModel(predfile = 'C:/Annotations/TrainedModels/3_Lisbon/VIAMEcsv/Results_Lisbon_Test_ScotW_MEOW.csv',
                truthfile = 'C:/Annotations/TrainedModels/3_Lisbon/VIAMEcsv/Test_ScotW_MEOW.csv',
                IoU.thresh = 0.5)

# Plot model results
####################

library(ggplot2)
library(ggpubr)

# Prepare results for plotting
res2 <- mapply(`[<-`, results, 'Conf', value = names(results), SIMPLIFY = FALSE)

resultsConf <- do.call(rbind, res2)
row.names(resultsConf) <- seq(1:nrow(resultsConf))

# Find the number of species identified for coloring the plot
length(unique(resultsConf$species))

# Plot the precision-recall curve

PrecRecCurve <- ggplot(data = resultsConf %>% group_by(species), aes(x = recall, 
                                                                     y = precision,
                                                                     group = species,
                                                                     color = species)) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = c('#008b8b', '#ffa500', '#00ff00', '#0000ff', '#ff1493')) +
  labs(x = 'Recall',
       y = 'Precision') +
  theme_pubr() +
  theme(legend.position = 'left',
        axis.title = element_text(face = 'bold'))


PrecRecCurve

F1plot <- ggplot(data = resultsConf %>% group_by(species), aes(x = as.numeric(Conf), 
                                                               y = f1,
                                                               group = species,
                                                               color = species)) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = c('#008b8b', '#ffa500', '#00ff00', '#0000ff', '#ff1493')) +
  labs(x = 'Confidence Threshold',
       y = 'F1 Score') +
  theme_pubr() +
  theme(legend.position = 'left',
        axis.title = element_text(face = 'bold'))


F1plot

write.csv(resultsConf, file = 'C:/Annotations/TrainedModels/3_Lisbon/ModelOutcome/resultsConf.csv')
####################

# Compare MaxNs
####################

## Create a function to compare the MaxN of the model and the truth annotations




####################

