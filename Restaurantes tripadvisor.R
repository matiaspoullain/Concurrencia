#restaaurantes costa atlantica

restaurantes_general <- function(ciudad, n.resultados = Inf){
  remDr$open() # abre firefox
  remDr$navigate("https://www.google.com.ar")
  webElem <- remDr$findElement(using = "name", value = "q")
  webElem$sendKeysToElement(list(paste("restaurantes tripadvisor", ciudad, sep = " ")))
  webElem <- remDr$findElement(using = "css selector", value = "div.tfB0Bf:nth-child(7) > center:nth-child(2) > input:nth-child(2)")
  Sys.sleep(1)
  webElem$clickElement()
  tripadvisor <- as.character(remDr$getCurrentUrl()[[1]])
  n <- gregexpr(pattern = "-", tripadvisor)[[1]][2]
  restaurantes <- c()
  rest.num <- 30
  x <- 1
  while((remDr$getCurrentUrl()[[1]] != tripadvisor | x == 1) & length(restaurantes) < n.resultados){
    source <- c(1, 2)
    while(length(source) == 2){
      source <- remDr$getPageSource()[[1]]
    }
    restaurantes <- c(restaurantes, qdapRegex::ex_between(source, '<!-- -->. <!-- -->', "</a>")[[1]])
    remDr$navigate(paste(substr(tripadvisor, 1, n-1), "-oa", rest.num, substr(tripadvisor, n, nchar(tripadvisor)), sep = ""))
    x <- x + 1
    rest.num <- rest.num + 30
  }
  remDr$close()
  if(n.resultados < Inf){
    restaurantes[1:n.resultados]
  }else{
    restaurantes
  }
  
}
