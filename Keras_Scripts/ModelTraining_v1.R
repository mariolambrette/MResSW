## Train Model following Import of annotation data with LoadData_EM()
train_datagen <- image_data_generator(rescale = 1./255,
                                      validation_split = 0.1)

train_batch_size <- 20
val_batch_size <- 20

train_data <- flow_images_from_dataframe(
  dataframe = KerasAnnotations$Annotations,
  generator = train_datagen,
  x_col = 'file',
  y_col = 'annot',
  target_size = c(224, 224),
  classes = KerasAnnotations$SpeciesIndex$sp_id,
  class_mode = 'categorical',
  batch_size = train_batch_size,
  shuffle = T,
  seed = 10,
  save_to_dir = 'C:/Dump/TestingR/kerasDataGenPhotos',
  save_prefix = 'NA',
  save_format = 'jpeg',
  subset = 'training'
)

val_data <- flow_images_from_dataframe(
  dataframe = KerasAnnotations$Annotations,
  generator = train_datagen,
  x_col = 'file',
  y_col = 'annot',
  target_size = c(224, 224),
  classes = KerasAnnotations$SpeciesIndex$sp_id,
  class_mode = 'categorical',
  batch_size = val_batch_size,
  shuffle = T,
  seed = 10,
  save_to_dir = 'C:/Dump/TestingR/kerasDataGenPhotos',
  save_prefix = 'NA',
  save_format = 'jpeg',
  subset = 'validation'
)

n_train <- train_data$n
n_val <- val_data$n

model <- keras_model_sequential()
model %>%
  application_densenet()
layer_conv_2d(filters = 32, kernel_size = c(3,3), input_shape = c(224, 224, 3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = nrow(KerasAnnotations$SpeciesIndex), activation = "softmax")

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(learning_rate = 0.001),
  metrics = c('accuracy')
)

model %>% fit(
  train_data,
  steps_per_epoch = ceiling(n_train/train_batch_size),
  epochs = 15,
  validation_data = val_data,
  validation_steps = ceiling(n_val/val_batch_size)
)

model %>%
  save_model_hdf5('C:/Dump/TestingR/TestModel.hdf5')
##########

########## Evaluate Model

TestImgDir <- 'C:/Annotations/PreAnnotatedMethod/Testing'

TestingAnnotations <- LoadData_EM(ImgDir = TestImgDir,
                                  EMFile = 'C:/Annotations/TrainedModels/3_Lisbon/DBOutputs/Test_ScotW_Meow_Improved_Points.txt',
                                  ImageHeight = ImageHeight,
                                  ImageWidth = ImageWidth,
                                  TargetHeight = 224,
                                  TargetWidth = 224,
                                  KeepSpecies = KerasAnnotations$SpeciesIndex$sp_name)


test_datagen <- image_data_generator(rescale = 1./255)

test_batch_size <- 1

test_datagen <- flow_images_from_dataframe(
  dataframe = TestingAnnotations$Annotations,
  generator = test_datagen,
  x_col = 'file',
  y_col = 'annot',
  target_size = c(224, 224),
  classes = TestingAnnotations$SpeciesIndex$sp_id,
  class_mode = 'categorical',
  batch_size = test_batch_size,
  shuffle = F,
  seed = 1
)

n_test <- test_datagen$n

predictions <- model %>%
  predict(test_datagen,
          verbose = 'auto',
          steps = n_test/test_batch_size)

eval <- model %>%
  evaluate(test_datagen,
           steps = n_test/test_batch_size,
           metrics = c('accuracy'))

eval


