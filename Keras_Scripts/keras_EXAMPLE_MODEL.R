  # ## INSTALL REQUIRED PACKAGES
  # 
  # install.packages("tensorflow", "keras")
  # 
  # tensorflow::install_tensorflow()
  # keras::install_keras()

library(keras)
library(tidyverse)

# Load the dataset into R
fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat',
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

## Preprocess the data

# Plot the first image
image_1 <- as.data.frame(train_images[1,,])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

# Scale the pixel values from 0 to 1 (required for model training)
train_images <- train_images/255
test_images <- test_images/255

# Display the first 25 images from the dataset and verify they are in the correct
# format

par(mfcol = c(5,5))
par(mar = c(0,0,1.5,0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev))
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}

#### Set up the model layers
# layers extract a representation from the data fed to them which is more helpful for
# the problem at hand than the input data

model <- keras_model_sequential()

model %>%
  layer_flatten(input_shape = c(28,28)) %>% # This 'flattens' the image array - it converts the
                                            # matrix type layout of pixels to a single long line
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

# After flattening pixels the network consists of two 'dense' layers. These are
# fully-connected neural layers. The first layer contain 128 nodes (or neurons)
# the second is a 10-node softmax layer that returns an array of 10 probability scores that sum to 1
# each node contains a score that indicates the probability that the current image
# belongs to one of the 10 digit classes

#### Compile the model

# compiling the models involves defining some more parameters
model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# loss - this is the loss function used to calculate training and validation loss
# Optimizer - How the model is updated based on the data it sees and its loss function
# Metrics - used to monitor the training and testing steps (e.g. accuracy or precision)

#### Start training the model with fit()

fit <- model %>%
  fit(train_images, train_labels, epochs = 5, verbose = 1)

model

#### Evaluate model accuracy

score <- model %>% evaluate(test_images, test_labels, verbose = 1)

cat('Test loss:', score["loss"],"\n")
cat('Test accuracy:', score["accuracy"], "\n")

# This model performs slightly better on the training data than test data which is an
# example of overfitting

#### Make predictions

predictions <- model %>% predict(test_images)

predictions[1,] # A prediction is array of, in this case, 10 numbers describing the confidence
                # of the model that the image corresponds to each of the 10 categories
                # The number of categories is obviously customisable

# find the highest confidence
which.max(predictions[1,]) # this returns the class name of the highest confidence

# Plot several images with their predictions
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev))
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800'
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}


## Load images into keras

# optional data augmentation - train_data_gen is then used later on when loading in
# the images to augment the data
train_data_gen = image_data_generator(
  rescale = 1/255 #,
  #rotation_range = 40,
  #width_shift_range = 0.2,
  #height_shift_range = 0.2,
  #shear_range = 0.2,
  #zoom_range = 0.2,
  #horizontal_flip = TRUE,
  #fill_mode = "nearest"
)

# Validation data shouldn't be augmented! But it should also be scaled.
valid_data_gen <- image_data_generator(
  rescale = 1/255
)  

# training images
train_image_array_gen <- flow_images_from_directory(train_image_files_path, 
                                                    train_data_gen,
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = fruit_list,
                                                    seed = 42)

# validation images
valid_image_array_gen <- flow_images_from_directory(valid_image_files_path, 
                                                    valid_data_gen,
                                                    target_size = target_size,
                                                    class_mode = "categorical",
                                                    classes = fruit_list,
                                                    seed = 42)
