#' Resize all images in a directory
#'
#' This function resizes all images in a given directory to a target size and reshapes
#' them to be square.
#' 
#' The resizing is done using ffmpeg. As such a local installation of ffmpeg is
#' essential. ffmpeg can be installed from: LINK TO FFMPEG INSTALL 
#' .
#' @param img.dir Path to the directory containing images to be resized.
#' @param target.size numeric, Default = 640. The size that the images should be resized to. YOLO models can only be trained on square images, as size is specified by only on parameter. The default is 640, the default YOLO training image size. Any change to this will neccesitate a complementary change in the YOLO training command issued.
#' @return NULL
#' @export
#' 
#' @examples
#' 
#' # Resize images to 640x640 pixels
#' YOLO_img_resize('path/to/image/folder')
#' 
#' # Resize images to 1280x1280 pixels
#' YOLO_img_resize(img.dir = 'path/to/image/folder', target.size = 1280)


YOLO_img_resize <- function(img.dir, target.size = 640){
  imgs <- list.files(img.dir, full.names = T)
  
  resize <- function(img, th = target.size, tw = target.size){
    cmd <- paste0('ffmpeg -y -i ', img, ' -vf scale=', tw, ':', th, ' ', img)
    system(cmd)
  }
  
  lapply(imgs, resize)
  
  return(NULL)
}





