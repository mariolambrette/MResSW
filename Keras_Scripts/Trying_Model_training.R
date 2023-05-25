library(keras)
library(tensorflow)
library(rjson)
library(magick)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(abind)

Annotations <- KerasAnnotations$Annotations

Annotations <- Annotations %>%
  relocate(file, sp_id, sp_name, x_left_sc, y_top_sc, x_right_sc, y_bottom_sc, 
           xl_orig, yt_orig, xr_orig, yb_orig)


#### Fololwing tutorial ####

imageinfo4ssd <- Annotations %>%
  select(sp_id,
         file,
         sp_name,
         x_left_sc,
         y_top_sc,
         x_right_sc,
         y_bottom_sc,
         xl_orig,
         yt_orig,
         xr_orig,
         yb_orig)

imageinfo4ssd <- imageinfo4ssd %>%
  group_by(file) %>%
  summarise(
    categories = toString(sp_id),
    name = toString(sp_name),
    xl = toString(x_left_sc),
    yt = toString(y_top_sc),
    xr = toString(x_right_sc),
    yb = toString(y_bottom_sc),
    xl_orig = toString(xl_orig),
    yt_orig = toString(yt_orig),
    xr_orig = toString(xr_orig),
    yb_orig = toString(yb_orig),
    cnt = n()
  )


## Check the annotations by plotting the image
example <- imageinfo4ssd[10, ]
img <- image_read(example$file)
name <- (example$name %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl_orig %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr_orig %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt_orig %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb_orig %>% str_split(pattern = ", "))[[1]]

img <- image_draw(img)
for (i in 1:example$cnt) {
  rect(x_left[i],
       y_bottom[i],
       x_right[i],
       y_top[i],
       border = "white",
       lwd = 2)
  text(
    x = as.integer(x_right[i]),
    y = as.integer(y_top[i]),
    labels = name[i],
    offset = 1,
    pos = 2,
    cex = 1,
    col = "white"
  )
}
dev.off()
print(img)



### Creating the anchors for the rpn
# In this case there is a grid with one anchor per grid cell - 
# though in more complex models there may be multiple anchors per grid cell

# There are the centre coordinates for the grid cells
cells_per_row <- 4
gridsize <- 1/cells_per_row
anchor_offset <- 1 / (cells_per_row * 2) 

anchor_xs <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
  rep(each = cells_per_row)
anchor_ys <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
  rep(cells_per_row)

ggplot(data.frame(x = anchor_xs, y = anchor_ys), aes(x, y)) +
  geom_point() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(aspect.ratio = 1)

# The centre coordinates are suplemented by height and width
anchor_centers <- cbind(anchor_xs, anchor_ys)
anchor_height_width <- matrix(1 / cells_per_row, nrow = 16, ncol = 2)
# This gis us a representation of the centes of the anchors (interchangeable with grid cells in this instance)
anchors <- cbind(anchor_centers, anchor_height_width)
anchors

# We also need a representation of the corners of the grid cells
hw2corners <- function(centers, height_width) {
  cbind(centers - height_width / 2, centers + height_width / 2) %>% unname()
}

# cells are indicated by (xl, yt, xr, yb)
# successive rows first go down in the image, then to the right
anchor_corners <- hw2corners(anchor_centers, anchor_height_width)
anchor_corners

## Display this image again with the grid overlayed
## this displays the scaled image

example <- imageinfo4ssd[10, ]
name <- (example$name %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb %>% str_split(pattern = ", "))[[1]]


img <- image_read(example$file)
img <- image_resize(img, geometry = "224x224!")
img <- image_draw(img)

for (i in 1:example$cnt) {
  rect(x_left[i],
       y_bottom[i],
       x_right[i],
       y_top[i],
       border = "white",
       lwd = 2)
  text(
    x = as.integer(x_right[i]),
    y = as.integer(y_top[i]),
    labels = name[i],
    offset = 0,
    pos = 2,
    cex = 1,
    col = "white"
  )
}
for (i in 1:nrow(anchor_corners)) {
  rect(
    anchor_corners[i, 1] * 224,
    anchor_corners[i, 4] * 224,
    anchor_corners[i, 3] * 224,
    anchor_corners[i, 2] * 224,
    border = "cyan",
    lwd = 1,
    lty = 3
  )
}

dev.off()
print(img)


## Now for each grid cell we must find best matching ground truth object

# overlaps shape is: number of ground truth objects * number of grid cells
map_to_ground_truth <- function(overlaps) {
  
  # for each ground truth object, find maximally overlapping cell (crit. 1)
  # measure of overlap, shape: number of ground truth objects
  prior_overlap <- apply(overlaps, 1, max)
  # which cell is this, for each object
  prior_idx <- apply(overlaps, 1, which.max)
  
  # for each grid cell, what object does it overlap with most (crit. 2)
  # measure of overlap, shape: number of grid cells
  gt_overlap <-  apply(overlaps, 2, max)
  # which object is this, for each cell
  gt_idx <- apply(overlaps, 2, which.max)
  
  # set all definitely overlapping cells to respective object (crit. 1)
  gt_overlap[prior_idx] <- 1.99
  
  # now still set all others to best match by crit. 2
  # actually it's other way round, we start from (2) and overwrite with (1)
  for (i in 1:length(prior_idx)) {
    # iterate over all cells "absolutely assigned"
    p <- prior_idx[i] # get respective grid cell
    gt_idx[p] <- i # assign this cell the object number
  }
  
  # return: for each grid cell, object it overlaps with most + measure of overlap
  list(gt_overlap, gt_idx)
  
}

# compute IOU
jaccard <- function(bbox, anchor_corners) {
  bbox <- k_constant(bbox)
  anchor_corners <- k_constant(anchor_corners)
  intersection <- intersect(bbox, anchor_corners)
  union <-
    k_expand_dims(box_area(bbox), axis = 2)  + k_expand_dims(box_area(anchor_corners), axis = 1) - intersection
  res <- intersection / union
  res %>% k_eval()
}

# compute intersection for IOU
intersect <- function(box1, box2) {
  box1_a <- box1[, 3:4] %>% k_expand_dims(axis = 2)
  box2_a <- box2[, 3:4] %>% k_expand_dims(axis = 1)
  max_xy <- k_minimum(box1_a, box2_a)
  
  box1_b <- box1[, 1:2] %>% k_expand_dims(axis = 2)
  box2_b <- box2[, 1:2] %>% k_expand_dims(axis = 1)
  min_xy <- k_maximum(box1_b, box2_b)
  
  intersection <- k_clip(max_xy - min_xy, min = 0, max = Inf)
  intersection[, , 1] * intersection[, , 2]
  
}

box_area <- function(box) {
  (box[, 3] - box[, 1]) * (box[, 4] - box[, 2]) 
}


### Before these calculations can take place we need a data generator

batch_size <- 16
target_width <- 224
target_height <- 224
image_size <- 224 # same as height

threshold <- 0.4

class_background <- nrow(KerasAnnotations$SpeciesIndex)+1

# Preprocessing function
load_and_preprocess_image <- function(image_name, t_height, t_width) {
  img_array <- image_load(
    image_name,
    target_size = c(t_height, t_width)
  ) %>%
    image_to_array() %>%
    imagenet_preprocess_input() 
  dim(img_array) <- c(1, dim(img_array))
  img_array
}

ssd_generator <-
  function(data,
           target_height,
           target_width,
           shuffle,
           batch_size) {
    i <- 1
    function() {
      if (shuffle) {
        indices <- sample(1:nrow(data), size = batch_size)
      } else {
        if (i + batch_size >= nrow(data))
          i <<- 1
        indices <- c(i:min(i + batch_size - 1, nrow(data)))
        i <<- i + length(indices)
      }
      
      x <-
        array(0, dim = c(length(indices), target_height, target_width, 3))
      y1 <- array(0, dim = c(length(indices), 16))
      y2 <- array(0, dim = c(length(indices), 16, 4))
      
      for (j in 1:length(indices)) {
        x[j, , , ] <-
          load_and_preprocess_image(data[[indices[j], "file"]], target_height, target_width)
        
        class_string <- data[indices[j], ]$categories
        xl_string <- data[indices[j], ]$xl
        yt_string <- data[indices[j], ]$yt
        xr_string <- data[indices[j], ]$xr
        yb_string <- data[indices[j], ]$yb
        
        classes <-  str_split(class_string, pattern = ", ")[[1]]
        xl <-
          str_split(xl_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        yt <-
          str_split(yt_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        xr <-
          str_split(xr_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        yb <-
          str_split(yb_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        
        # rows are objects, columns are coordinates (xl, yt, xr, yb)
        # anchor_corners are 16 rows with corresponding coordinates
        bbox <- cbind(xl, yt, xr, yb)
        overlaps <- jaccard(bbox, anchor_corners)
        
        c(gt_overlap, gt_idx) %<-% map_to_ground_truth(overlaps)
        gt_class <- classes[gt_idx]
        
        pos <- gt_overlap > threshold
        gt_class[gt_overlap < threshold] <- 5
        
        # columns correspond to objects
        boxes <- rbind(xl, yt, xr, yb)
        # columns correspond to object boxes according to gt_idx
        gt_bbox <- boxes[, gt_idx]
        # set those with non-sufficient overlap to 0
        gt_bbox[, !pos] <- 0
        gt_bbox <- gt_bbox %>% t()
        
        y1[j, ] <- as.integer(gt_class) - 1
        y2[j, , ] <- gt_bbox
        
      }
      
      x <- x %>% imagenet_preprocess_input()
      y1 <- y1 %>% to_categorical(num_classes = class_background)
      list(x, list(y1, y2))
    }
  }


train_generator <- ssd_generator(
  imageinfo4ssd,
  target_height = target_height,
  target_width = target_width,
  shuffle = TRUE,
  batch_size = batch_size
)


batch <- train_generator()
c(x, c(y1, y2)) %<-% batch
dim(y1)
dim(y2)


## Ready to start building the model

# Use resnet50 as a feature extractor
feature_extractor <- application_resnet50(
  include_top = FALSE,
  input_shape = c(224, 224, 3)
)


# Then we append some conv layers. The first few are there for capacity,
# while the last one downsamples its input from 7x7 yto 4x4 which is what we need
# for the grid
input <- feature_extractor$input

common <- feature_extractor$output %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    padding = "same",
    activation = "relu",
    name = "head_conv1_1"
  ) %>%
  layer_batch_normalization() %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    padding = "same",
    activation = "relu",
    name = "head_conv1_2"
  ) %>%
  layer_batch_normalization() %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    padding = "same",
    activation = "relu",
    name = "head_conv1_3"
  ) %>%
  layer_batch_normalization() %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    strides = 2,
    padding = "same",
    activation = "relu",
    name = "head_conv2"
  ) %>%
  layer_batch_normalization() 

# Now we create two outputs - one for bounding boxes and one for classes

# first classes - there are 5 classes and 16 cells so we get an output size of 
# 16x5

class_output <-
  layer_conv_2d(
    common,
    filters = 5,
    kernel_size = 3,
    padding = "same",
    name = "class_conv"
  ) %>%
  layer_reshape(target_shape = c(16, 5), name = "class_output")

# Next the bounding box output - these computations are made in layer_lambda
# We start from the actual anchor box centers and move them around by a 
# sclaed down version of the activations. We then convert ehm to anchor corners

bbox_output <-
  layer_conv_2d(
    common,
    filters = 4,
    kernel_size = 3,
    padding = "same",
    name = "bbox_conv"
  ) %>%
  layer_reshape(target_shape = c(16, 4), name = "bbox_flatten") %>%
  layer_activation("tanh") %>%
  layer_lambda(
    f = function(x) {
      activation_centers <-
        (x[, , 1:2] / 2 * gridsize) + k_constant(anchors[, 1:2])
      activation_height_width <-
        (x[, , 3:4] / 2 + 1) * k_constant(anchors[, 3:4])
      activation_corners <-
        k_concatenate(
          list(
            activation_centers - activation_height_width / 2,
            activation_centers + activation_height_width / 2
          )
        )
      activation_corners
    },
    name = "bbox_output"
  )

# Now that the outputs are defined we define the final model

model <- keras_model(
  inputs = input,
  outputs = list(class_output, bbox_output)
)

# The last missing piece is the loss function
# shapes are batch_size * 16 * 5
class_loss <- function(y_true, y_pred, n_classes = class_background) {
  
  class_loss  <-
    tf$nn$sigmoid_cross_entropy_with_logits(labels = y_true, logits = y_pred)
  
  class_loss <-
    tf$reduce_sum(class_loss) / tf$cast(n_classes + 1, "float32")
  
  class_loss
}

# Focal class loss can be a better loss function as it helps to avoid biasing training
# towards dominant classes (e.g. background)

alpha <- 0.25
gamma <- 1

get_weights <- function(y_true, y_pred) {
  p <- y_pred %>% k_sigmoid()
  pt <-  y_true*p + (1-p)*(1-y_true)
  w <- alpha*y_true + (1-alpha)*(1-y_true)
  w <-  w * (1-pt)^gamma
  w
}

class_loss_focal  <- function(y_true, y_pred) {
  
  w <- get_weights(y_true, y_pred)
  cx <- tf$nn$sigmoid_cross_entropy_with_logits(labels = y_true, logits = y_pred)
  weighted_cx <- w * cx
  
  class_loss <-
    tf$reduce_sum(weighted_cx) / tf$cast(21, "float32")
  
  class_loss
}

# shapes are batch_size * 16 * 4
bbox_loss <- function(y_true, y_pred) {
  
  # calculate localization loss for all boxes where ground truth was assigned some overlap
  # calculate mask
  pos <- y_true[, , 1] + y_true[, , 3] > 0
  pos <-
    pos %>% k_cast(tf$float32) %>% k_reshape(shape = c(batch_size, 16, 1))
  pos <-
    tf$tile(pos, multiples = k_constant(c(1L, 1L, 4L), dtype = tf$int32))
  
  diff <- y_pred - y_true
  # mask out irrelevant activations
  diff <- diff %>% tf$multiply(pos)
  
  loc_loss <- diff %>% tf$abs() %>% tf$reduce_mean()
  loc_loss * 100
}

# Now we need to freeze the feature detector weights and compile the model

model %>% freeze_weights()
model %>% unfreeze_weights(from = "head_conv1_1")
model

model %>% compile(
  loss = list(class_loss, bbox_loss),
  optimizer = "adam",
  metrics = list(
    class_output = custom_metric("class_loss_focal", metric_fn = class_loss_focal),
    bbox_output = custom_metric("bbox_loss", metric_fn = bbox_loss)
  )
)

## Now we train the model - this may take along time and optimisations can be made

steps_per_epoch <- nrow(imageinfo4ssd) / batch_size

model %>% fit(
  train_generator,
  steps_per_epoch = steps_per_epoch,
  epochs = 15,
  callbacks = callback_model_checkpoint(
    "weights.{epoch:02d}-{loss:.2f}.hdf5", 
    save_weights_only = TRUE
  )
)


## Testing the model

TestImgDir <- 'C:/Annotations/PreAnnotatedMethod/Testing'

TestingAnnotations <- LoadData_EM(ImgDir = TestImgDir,
                                  EMFile = 'C:/Annotations/TrainedModels/3_Lisbon/DBOutputs/Test_ScotW_Meow_Improved_Points.txt',
                                  ImageHeight = ImageHeight,
                                  ImageWidth = ImageWidth,
                                  TargetHeight = 224,
                                  TargetWidth = 224,
                                  KeepSpecies = KerasAnnotations$SpeciesIndex$sp_name,
                                  test = T)


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




##############

# Modify the anchor generator and model to allow multiple anchor boxes per 
# grid square and allow anchor boxes with different aspect ratios

# Creat eanchor boxes as combinations of different sclaes and aspect ratios

anchor_zooms <- c(0.7,1,1.3)
anchor_ratios <- matrix(c(1,1,1,0.5,0.5,1), ncol = 2, byrow = T)

#In this example there are 9 different combinations:
anchor_scales <- rbind(
  anchor_ratios * anchor_zooms[1],
  anchor_ratios * anchor_zooms[2],
  anchor_ratios * anchor_zooms[3]
)

k <- nrow(anchor_scales)

anchor_scales

# Detectors are placed at 3 stages with resolution at 4x4 (like before), 2x2 and 1x1

anchor_grids <- c(4,2,1)

# Now we can compute the the coordinates of the box centres

anchor_offsets <- 1/(anchor_grids*2)

anchor_x <- map(
    1:3,
    function(x) rep(seq(anchor_offsets[x],
                       1 - anchor_offsets[x],
                        length.out = anchor_grids[x]),
                    each = anchor_grids[x])) %>%
  flatten() %>%
  unlist()

anchor_y <- map(
    1:3,
    function(y) rep(seq(anchor_offsets[y],
                        1 - anchor_offsets[y],
                        length.out = anchor_grids[y]),
                    times = anchor_grids[y])) %>%
  flatten() %>%
  unlist()

anchor_centers <- cbind(rep(anchor_x, each = k), rep(anchor_y, each = k))

anchor_sizes <- map(
    anchor_grids,
    function(x)
      matrix(rep(t(anchor_scales/x), x*x), ncol = 2, byrow = TRUE)
  ) %>%
  abind(along = 1)

grid_sizes <- c(rep(0.25, k * anchor_grids[1]^2),
                rep(0.5, k * anchor_grids[2]^2),
                rep(1, k * anchor_grids[3]^2)
)

anchors <- cbind(anchor_centers, anchor_sizes)

hw2corners <- function(centers, height_width) {
  cbind(centers - height_width / 2, centers + height_width / 2) %>% unname()
}

anchor_corners <- hw2corners(anchors[ , 1:2], anchors[ , 3:4])

# Plot the anchor grid
ggplot(data.frame(x = anchor_x, y = anchor_y), aes(x, y)) +
  geom_point() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  theme(aspect.ratio = 1)

# Plot the anchor grid over an example image

example <- imageinfo4ssd[10, ]
name <- (example$name %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb %>% str_split(pattern = ", "))[[1]]


img <- image_read(example$file)
img <- image_resize(img, geometry = "224x224!")
img <- image_draw(img)

for (i in 1:example$cnt) {
  rect(x_left[i],
       y_bottom[i],
       x_right[i],
       y_top[i],
       border = "white",
       lwd = 2)
  text(
    x = as.integer(x_right[i]),
    y = as.integer(y_top[i]),
    labels = name[i],
    offset = 0,
    pos = 2,
    cex = 1,
    col = "white"
  )
}
for (i in 1:nrow(anchor_corners)) {
  rect(
    anchor_corners[i, 1] * 224,
    anchor_corners[i, 4] * 224,
    anchor_corners[i, 3] * 224,
    anchor_corners[i, 2] * 224,
    border = "cyan",
    lwd = 1,
    lty = 3
  )
}

dev.off()
print(img)


## The model will also need to be modified to deal with this

# Start with a feature extractor
feature_extractor <- application_resnet50(
  include_top = FALSE,
  input_shape = c(224, 224, 3)
)

# Attach some conv layers (this is still the same as one anchor per grid box, like before)
input <- feature_extractor$input

common <- feature_extractor$output %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    padding = "same",
    activation = "relu",
    name = "head_conv1_1"
  ) %>%
  layer_batch_normalization() %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    padding = "same",
    activation = "relu",
    name = "head_conv1_2"
  ) %>%
  layer_batch_normalization() %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    padding = "same",
    activation = "relu",
    name = "head_conv1_3"
  ) %>%
  layer_batch_normalization()

## Now things begin to change as we want to attach detectors (i.e. output layers)
## to differetn stages in apipeline of successive downsampling.

# This is the downsampling pipeline
downscale_4x4 <- common %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    strides = 2,
    padding = "same",
    activation = "relu",
    name = "downscale_4x4"
  ) %>%
  layer_batch_normalization() 
downscale_2x2 <- downscale_4x4 %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    strides = 2,
    padding = "same",
    activation = "relu",
    name = "downscale_2x2"
  ) %>%
  layer_batch_normalization() 
downscale_1x1 <- downscale_2x2 %>%
  layer_conv_2d(
    filters = 256,
    kernel_size = 3,
    strides = 2,
    padding = "same",
    activation = "relu",
    name = "downscale_1x1"
  ) %>%
  layer_batch_normalization() 

# The bbox output definitions get mor ecomplicated as each output has to take in to accoount its relative anchor ox coordinates
create_bbox_output <- function(prev_layer, anchor_start, anchor_stop, suffix) {
  output <- layer_conv_2d(
    prev_layer,
    filters = 4 * k,
    kernel_size = 3,
    padding = "same",
    name = paste0("bbox_conv_", suffix)
  ) %>%
    layer_reshape(target_shape = c(-1, 4), name = paste0("bbox_flatten_", suffix)) %>%
    layer_activation("tanh") %>%
    layer_lambda(
      f = function(x) {
        activation_centers <-
          (x[, , 1:2] / 2 * matrix(grid_sizes[anchor_start:anchor_stop], ncol = 1)) +
          k_constant(anchors[anchor_start:anchor_stop, 1:2])
        activation_height_width <-
          (x[, , 3:4] / 2 + 1) * k_constant(anchors[anchor_start:anchor_stop, 3:4])
        activation_corners <-
          k_concatenate(
            list(
              activation_centers - activation_height_width / 2,
              activation_centers + activation_height_width / 2
            )
          )
        activation_corners
      },
      name = paste0("bbox_output_", suffix)
    )
  output
}

# Here each one is attached to it's respective stage of the pipeline
bbox_output_4x4 <- create_bbox_output(downscale_4x4, 1, 144, "4x4")
bbox_output_2x2 <- create_bbox_output(downscale_2x2, 145, 180, "2x2")
bbox_output_1x1 <- create_bbox_output(downscale_1x1, 181, 189, "1x1")

# The same applies to class outputs

create_class_output <- function(prev_layer, suffix) {
  output <-
    layer_conv_2d(
      prev_layer,
      filters = 21 * k,
      kernel_size = 3,
      padding = "same",
      name = paste0("class_conv_", suffix)
    ) %>%
    layer_reshape(target_shape = c(-1, 21), name = paste0("class_output_", suffix))
  output
}
class_output_4x4 <- create_class_output(downscale_4x4, "4x4")
class_output_2x2 <- create_class_output(downscale_2x2, "2x2")
class_output_1x1 <- create_class_output(downscale_1x1, "1x1")

# Then combine everything to get the model
model <- keras_model(
  inputs = input,
  outputs = list(
    bbox_output_1x1,
    bbox_output_2x2,
    bbox_output_4x4,
    class_output_1x1, 
    class_output_2x2, 
    class_output_4x4)
)


## Figuring out changes to generator
data <- imageinfo4ssd
indices <- 1:10 

j <- 10 # this is our image

class_string <- data[indices[j], ]$categories
xl_string <- data[indices[j], ]$xl
yt_string <- data[indices[j], ]$yt
xr_string <- data[indices[j], ]$xr
yb_string <- data[indices[j], ]$yb

classes <-  str_split(class_string, pattern = ", ")[[1]]
xl <- str_split(xl_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
yt <- str_split(yt_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
xr <- str_split(xr_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
yb <- str_split(yb_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)

classes
xl

bbox <- cbind(xl, yt, xr, yb)
bbox

anchor_corners

overlaps <- jaccard(bbox, anchor_corners)

c(gt_overlap, gt_idx) %<-% map_to_ground_truth(overlaps)
gt_overlap

gt_class <- classes[gt_idx]
gt_class

pos <- gt_overlap > threshold
gt_class[gt_overlap < threshold] <- class_background

gt_class

orig_boxes <- rbind(xl, yt, xr, yb)
# columns correspond to object boxes according to gt_idx
gt_bbox <- orig_boxes[, gt_idx]
# set those with non-sufficient overlap to 0
gt_bbox[, !pos] <- 0
gt_bbox <- gt_bbox %>% t()

gt_bbox

