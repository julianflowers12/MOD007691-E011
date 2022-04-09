### get phylopic image

get_image <- function(search, size){
  require(magick)
  require(rphylopic)

  ## get uid

  im <- name_search(search, options = "namebankID")[1,]
  id <- name_images(uuid = im$uid[1])
  uid <- id$same[[1]]$uid

  img <- paste0("http://phylopic.org/assets/images/submissions/", uid, ".", size, ".png") %>%
    image_read()

  img

}
