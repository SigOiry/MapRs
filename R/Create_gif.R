# Function to create GIF from a list of images
create_gif <- function(image_list = NA, output_file = NA, fps = 10, loop = 0) {
  # Read the images into a magick image object
  
  if(is.na(image_list)){
    path <- rstudioapi::selectDirectory(caption = "Path to the folder where the images are")
    image_list <- list.files(path, full.names = T)
  }
  images <- magick::image_read(image_list)
  
  # Create the GIF
  gif <- magick::image_animate(images, fps = fps, loop = loop)
  
  # Write the GIF to the specified output file
  if(!is.na(output_file)){
    magick::image_write(gif, path = output_file)
  }
  return(gif)
}
