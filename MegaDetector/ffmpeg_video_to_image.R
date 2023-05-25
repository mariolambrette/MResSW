#ffmpeg video to screenshots

#source(s) FOLDER(S)
ff <- list.dirs("Y://Anguilla/AX_2021")
ff <- ff[-1]

for (j in 1:length(ff)){
  
  folderpath <- ff[1]
  print(folderpath)
  
  #source(s) FILE(S)
  fi <- list.files(folderpath, pattern="LGX", recursive=TRUE, full.names=TRUE)
  if (length(fi)==0){
    fi <- list.files(folderpath, pattern="RGX", recursive=TRUE, full.names=TRUE)
  }
  
  #iterate
  for (i in 1:length(fi)){
    print(fi[i])
    fn <- tools::file_path_sans_ext(basename(fi[i]))
    sourcedir <- file.path(dirname(fi[i]), fsep="//")
    
    #multiple images using iterative loop on vector of specified times
    #V  <- av::av_media_info(fi[i])
    #S  <- seq(from=1, to=floor(V$duration), by=1)
    #D  <- as.numeric(as.POSIXct(Sys.Date())+S)
    #E  <- format(lubridate::as_datetime(D), "%H:%M:%S")
    
    #for (j in 1:length(S)){
    #  C <- file.path(B, paste0(fn, "-", sprintf("%04d",j),".jpg"))
    #  system(paste0("c:/ffmpeg/bin/ffmpeg.exe -ss ", E[j] ," -i ", fi[ii], " -frames:v 1 -q:v 1 ", C))
    #}
    
    #extract multiple images from video using fps to declare picture subset sampling interval
    #ffmpeg -i myvideo.avi -vf fps=1/60 img%03d.jpg
    #fps=1/60 one photo every 60 seconds
    #fps=1 one photo every second
    
    savepath <- paste0("Y://images/",basename(folderpath))
    dir.create(savepath)
    C <- file.path(savepath, paste0(fn, "-%04d.jpg"))
    ffmpegstring <- paste0("c:/ffmpeg/bin/ffmpeg.exe -i ", fi[i], " -vf scale=-1:1080 -vf fps=15/60 ", C)
    print(ffmpegstring)
    system(ffmpegstring)
  }
  
}
