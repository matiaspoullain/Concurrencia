ushuaia_restaurants <- function(){
  address <- "https://www.interpatagonia.com/ushuaia/comidas.html"
  txt <- getURL(address)
  entre <- ex_between(txt, '<span itemprop="name"> ', '</span> </p> <p class="')[[1]]
  nombre.lugar <- sub("</span>.*", "", entre)
  nombre.lugar <- nombre.lugar[2:length(nombre.lugar)]
  nombre.lugar <- trimws(nombre.lugar) #saca los espacios de adelante y atras
  nombre.lugar <- gsub('[[:punct:] ]+',' ',nombre.lugar)
  direccion.lugar <- sub(".*streetAddress\">", "", entre)
  direccion.lugar <- direccion.lugar[2:length(direccion.lugar)]
  direccion.lugar <- sub("-.*", "", direccion.lugar)
  direccion.lugar <- trimws(direccion.lugar)
  direccion.lugar <- gsub('[[:punct:] ]+',' ', direccion.lugar)
  nombre.lugar <- nombre.lugar[which(!duplicated(direccion.lugar))]
  direccion.lugar <- unique(direccion.lugar)
  lugares <- paste(nombre.lugar, direccion.lugar, "Ushuaia", sep = ", ")
}
