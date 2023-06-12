## Batch train YOLO models

# Empty image directories and ensure they are ready for new round of training
# Create random sample of test/train/val images
# Prepare all config files
# run training and validation for each sub sample (250, 500, 750 images etc)
# Save training outputs so results can be extracted



## Prep image directories

# clear all images and caches
unlink('C:/YOLOv5/Training/images/Test')
unlink('C:/YOLOv5/Training/images/Train')
unlink('C:/YOLOv5/Training/images/Val')
unlink('C:/YOLOv5/Training/labels/Test')
unlink('C:/YOLOv5/Training/labels/Train')
unlink('C:/YOLOv5/Training/labels/Val')
unlink('C:/YOLOv5/Training/Subset/250')
unlink('C:/YOLOv5/Training/Subset/500')
unlink('C:/YOLOv5/Training/Subset/750')
unlink('C:/YOLOv5/Training/Subset/1000')
unlink('C:/YOLOv5/Training/Subset/1250')
unlink('C:/YOLOv5/Training/Subset/1750')

# Recreate up folder structure
dir.create('C:/YOLOv5/Training/images/Test')
dir.create('C:/YOLOv5/Training/images/Train')
dir.create('C:/YOLOv5/Training/images/Val')
dir.create('C:/YOLOv5/Training/labels/Test')
dir.create('C:/YOLOv5/Training/labels/Train')
dir.create('C:/YOLOv5/Training/labels/Val')

dir.create('C:/YOLOv5/Training/Subset/250/images/Train')
dir.create('C:/YOLOv5/Training/Subset/250/images/Test')
dir.create('C:/YOLOv5/Training/Subset/250/images/Val')
dir.create('C:/YOLOv5/Training/Subset/250/labels/Train')
dir.create('C:/YOLOv5/Training/Subset/250/labels/Test')
dir.create('C:/YOLOv5/Training/Subset/250/labels/Val')

dir.create('C:/YOLOv5/Training/Subset/500/images/Train')
dir.create('C:/YOLOv5/Training/Subset/500/images/Test')
dir.create('C:/YOLOv5/Training/Subset/500/images/Val')
dir.create('C:/YOLOv5/Training/Subset/500/labels/Train')
dir.create('C:/YOLOv5/Training/Subset/500/labels/Test')
dir.create('C:/YOLOv5/Training/Subset/500/labels/Val')

dir.create('C:/YOLOv5/Training/Subset/750/images/Train')
dir.create('C:/YOLOv5/Training/Subset/750/images/Test')
dir.create('C:/YOLOv5/Training/Subset/750/images/Val')
dir.create('C:/YOLOv5/Training/Subset/750/labels/Train')
dir.create('C:/YOLOv5/Training/Subset/750/labels/Test')
dir.create('C:/YOLOv5/Training/Subset/750/labels/Val')

dir.create('C:/YOLOv5/Training/Subset/1000/images/Train')
dir.create('C:/YOLOv5/Training/Subset/1000/images/Test')
dir.create('C:/YOLOv5/Training/Subset/1000/images/Val')
dir.create('C:/YOLOv5/Training/Subset/1000/labels/Train')
dir.create('C:/YOLOv5/Training/Subset/1000/labels/Test')
dir.create('C:/YOLOv5/Training/Subset/1000/labels/Val')

dir.create('C:/YOLOv5/Training/Subset/1250/images/Train')
dir.create('C:/YOLOv5/Training/Subset/1250/images/Test')
dir.create('C:/YOLOv5/Training/Subset/1250/images/Val')
dir.create('C:/YOLOv5/Training/Subset/1250/labels/Train')
dir.create('C:/YOLOv5/Training/Subset/1250/labels/Test')
dir.create('C:/YOLOv5/Training/Subset/1250/labels/Val')

dir.create('C:/YOLOv5/Training/Subset/1750/images/Train')
dir.create('C:/YOLOv5/Training/Subset/1750/images/Test')
dir.create('C:/YOLOv5/Training/Subset/1750/images/Val')
dir.create('C:/YOLOv5/Training/Subset/1750/labels/Train')
dir.create('C:/YOLOv5/Training/Subset/1750/labels/Test')
dir.create('C:/YOLOv5/Training/Subset/1750/labels/Val')

system('conda run -n YOLOv52 python val.py --weights runs/train/exp27/weights/best.pt --data C:/YOLOv5/Training/config.yaml --task test --save-txt')