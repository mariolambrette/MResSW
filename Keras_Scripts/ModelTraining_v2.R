library(keras)
library(reticulate)

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

# Define your model architecture
input_shape <- c(224, 224, 3)
base_model <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = input_shape
)
rpn <- layer_conv_2d(base_model$output, filters = 128, kernel_size = c(3, 3), padding = "same", activation = "relu") %>%
 layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same", activation = "relu") %>%
 layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same", activation = "relu") %>%
 layer_conv_2d(filters = 2 * 9, kernel_size = c(1, 1), activation = "softmax", name = "rpn_cls") %>%
 layer_conv_2d(filters = 4 * 9, kernel_size = c(1, 1), name = "rpn_reg")

# Define the ROI Pooling layer
pooling <- layer_lambda(
  function(x) {
    rois <- x[[1]]
    feature_maps <- x[[2]]
    crop_size <- 7
    output = list()
    for (i in 1:nrow(rois)) {
      roi <- rois[i,]
      batch_ind <- as.integer(roi[1])
      start_row <- as.integer(roi[2])
      start_col <- as.integer(roi[3])
      end_row <- as.integer(roi[4])
      end_col <- as.integer(roi[5])
      roi_map <- feature_maps[batch_ind,start_row:end_row,start_col:end_col,]
      output[[i]] <- keras::k_reshape(
        keras::k_max_pool2d(
          keras::k_constant(roi_map),
          pool_size = c(
            as.integer((end_row-start_row+1) / crop_size),
            as.integer((end_col-start_col+1) / crop_size)
          ),
          strides = c(
            as.integer((end_row-start_row+1) / crop_size),
            as.integer((end_col-start_col+1) / crop_size)
          )
        ),
        target_shape = c(crop_size, crop_size, dim(roi_map)[3])
      )
    }
    keras::k_concatenate(output, axis = 0)
  }
)

# Define the CNN model
input_layer <- layer_input(shape = c(224, 224, 3))
roi_input_layer <- layer_input(shape = c(5))
x <- vgg16$input[[1]](input_layer)
for (layer in vgg16$layers[-1]) {
  x <- layer(x)
}
x <- pooling(list(roi_input_layer, x))
x <- layer_flatten()(x)
x <- layer_dense(units = 4096, activation = "relu")(x)
x <- layer_dense(units = 4096, activation = "relu")(x)
output_layer <- layer_dense(units = 4, activation = "linear")(x)
model <- keras::keras_model(input_layer, roi_input_layer, output_layer)

# Compile the model
model %>% compile(
  loss = list(
    "categorical_crossentropy",
    "mse"
  ),
  optimizer = optimizer_sgd(lr = 1e-4, momentum = 0.9),
  metrics = c("accuracy")
)
# Train the model
model_fit <- fit_generator(
  generator = train_generator,
  steps_per_epoch = n_train_samples / batch_size,
  epochs = num_epochs,
  validation_data = val_generator,
  validation_steps = n_val_samples / batch_size
)


model2 %>%
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


