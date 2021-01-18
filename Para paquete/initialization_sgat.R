load_all()
initialization_sgat <- function(){
  intento <- c("1")
  class(intento) = "try-error"
  while("try-error" %in% class(intento)){
    intento <- try(driver_try(), silent = T)
  }
}