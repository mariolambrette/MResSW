

library(opencv)
library(av)
library(imager)
library(mmand)
library(magick)
library(image.ContourDetector)
library(purrr)
library(raster)

VidPath <- 'D:/PrePARED/2022/ScotW/SWR01/SWR01_01_BRUV_2/RGX030025.mp4'

av_video_images(video = VidPath, destdir = getwd(), format = 'jpg', fps = 0.25)


img1 <- imager::load.image('image_000079.jpg') %>%
  imager::grayscale()
img2 <- imager::load.image('image_000080.jpg') %>%
  imager::grayscale()

img1a <- as.array(img1)
img2a <- as.array(img2)

imgDiff <- img1a-img2a
imgDiff <- abs(imgDiff)

CimgDiff <- as.cimg(imgDiff)

summary(imgDiff)

imgThresh <- mmand::threshold(imgDiff, method = 'literal', level = 0.02, binarise = T)
summary(imgThresh)

CimgThresh <- as.cimg(imgThresh)
plot(CimgThresh)
plot(img1)

contThresh <- imager::contours(CimgThresh, nlevels = 2)
contDiff <- imager::contours(CimgDiff, nlevels = 4)

contDiff2 <- image.ContourDetector::image_contour_detector(CimgDiff)

plot(CimgDiff)
x <- purrr::walk(contDiff, function(v) lines(v$x, v$y, col = 'red'))
plot(x)
dev.off()

PximgDiff <- imager::as.pixset(CimgDiff)

boundary(PximgDiff, depth = .9, high = T) %>%
  plot(int = T, main = "8-point neighbourhood")


class(imgThresh)
imgThreshm <- matrix(imgThresh, ncol = 1920, nrow = 1080, byrow = T) %>%
  t()

testplot <- imager::as.cimg(imgThreshm) %>%
  plot()


imgThreshR <- as(imgThreshm, 'RasterLayer')

# sliding indow with focal

t1 <- runner::runner(imgThreshm, sum, k = 30 ,lag = 10)
t1

library(reticulate)
use_miniconda('c:/users/ml673/miniconda3')
py_config()
py_install('pandas')
