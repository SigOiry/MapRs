require(tidyverse)
require(terra)

S2_SHSI <- function(img, STD = T) {
  
  if (STD) {
    min_img <- min(img)
    max_img <- max(img)
    
    img <- (img - min_img)/(max_img - min_img)
  }
  
  Dgreen = (img$B02 + 0.18667*(img$B08A - img$B02))-img$B03
  Dre = (img$B02 + 0.66667*(img$B08A - img$B02))-img$B06
  
  SHSI = (Dgreen+Dre)
  
  return(SHSI)
  
}
