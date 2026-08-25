library(magick)


inDir <- 'C:/Projects/NSMM/Photos/Curated'
outDir <- 'C:/Projects/NSMM/Photos/AppPhotoLibrary/NSMM'

dirs <- list.dirs(inDir, recursive = F, full.names = F)


for (i in 1:length(dirs)) {
  
  d <- dirs[i]
  od <- paste0(outDir, '/', d, '/Thumbnails')
  if(!dir.exists(od)){dir.create(od, recursive = T)}
  
  fls <- list.files(paste0(inDir, '/', d), ".(jpg|jpeg|png)$", full.names = T )
  for (j in 1:length(fls)) {
    f <- fls[j]
    Image <- image_read(f)
    thmb <- image_resize(Image, "400x400")
    fname <- basename(f)
    ext <- tools::file_ext(fname)
    outname <- str_remove(fname, paste0('.', ext))
    
    image_write(thmb,  paste0(outDir,'/', d, '/Thumbnails/Thumbnail_', outname, '.jpg'))
    image_write(Image,  paste0(outDir,'/', d, '/', outname, '.jpg'))
  }
}