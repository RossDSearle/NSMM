library(magick)


inDir <- 'C:/Projects/NSMM/Photos/Curated'
outDir <- 'C:/Projects/NSMM/Photos/AppPhotoLibrary/NSMM'

dirs <- list.dirs(inDir, recursive = F, full.names = F)


for (i in 1:length(dirs)) {
  
  d <- dirs[i]
  print(d)
  od <- paste0(outDir, '/', d, '/Thumbnails')
  if(!dir.exists(od)){dir.create(od, recursive = T)}
  
  fls <- list.files(paste0(inDir, '/', d), ".(jpg|jpeg|png)$", full.names = T )
    if(length(fls)>0){
        for (j in 1:length(fls)) {
          f <- fls[j]
          Image <- image_read(f)
          img_width <- image_info(Image)$width
          img_height <- image_info(Image)$height
          
          if(img_width > img_height){
             thmb <- image_resize(Image, geometry = "200x")
          }else{
            
            thmb <- image_resize(Image, geometry = "x200")
          }
          
          
         
          fname <- basename(f)
          ext <- tools::file_ext(fname)
          outname <- str_remove(fname, paste0('.', ext))
          
          image_write(thmb,  paste0(outDir,'/', d, '/Thumbnails/Thumbnail_', outname, '.jpg'))
          image_write(Image,  paste0(outDir,'/', d, '/', outname, '.jpg'))
        }
    }
}



fls <- list.files('C:/Projects/NSMM/Photos/AppPhotoLibrary/NSMM/Kerang/Thumbnails', ".(jpg|jpeg|png)$", full.names = T )

for (i in 1:length(fls)) {
  img <- image_read(fls[i])
  info <- image_info(img)
  
  # Compare width and height
  if (info$width > info$height) {
    print("Landscape orientation")
  } else if (info$width < info$height) {
    print("Portrait orientation")
  } else {
    print("Square orientation")
  }
  
  fixed_img <- image_transform(img, orientation = NULL)
  
  image_write('c:/temp/Thumbnail_', outname, '.jpg'))
  
  
}
