
imgdir <- 'c:/YOLOv5/Training/images/Train'


YOLO_img_resize <- function(imgdir, targetsize = 640){
  imgs <- list.files(imgdir, full.names = T)
  
  resize <- function(img, th = targetsize, tw = targetsize){
    cmd <- paste0('ffmpeg -y -i ', img, ' -vf scale=', tw, ':', th, ' ', img)
    system(cmd)
  }
  
  lapply(imgs, resize)
  
}





