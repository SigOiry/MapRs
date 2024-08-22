# Function to create GIF from a list of images
create_gif <- function(image_list, output_file = NA, fps = 10, loop = 0) {
  # Read the images into a magick image object
  images <- magick::image_read(image_list)
  
  # Create the GIF
  gif <- magick::image_animate(images, fps = fps, loop = loop)
  
  # Write the GIF to the specified output file
  if(!is.na(output_file)){
    magick::image_write(gif, path = output_file)
  }
  return(gif)
}
