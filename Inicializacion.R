library(RSelenium)

driver.try <- function(){
  driver <- try(rsDriver(browser = "firefox", geckover = "latest"), silent = T)
  if("try-error" %in% class(driver)){
    versiones <- binman::list_versions("geckodriver")[[1]]
    x <- 1
    while("try-error" %in% class(driver)){
      try(system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE), silent = T)
      ver <- versiones[length(versiones) - x]
      driver <- try(rsDriver(browser = "firefox", geckover = ver), silent = T)
      x <- x + 1
    }
  }
  
  driver$client$close()
  remDr <- driver[["client"]]
  remDr <- remoteDriver(remoteServerAddr = "localhost", 
                        port = 4567, 
                        browserName = "firefox")
}



initialization.sgat <- function(){
  intento <- c("1")
  class(intento) = "try-error"
  while("try-error" %in% class(intento)){
    intento <- try(driver.try(), silent = T)
  }
}

initialization.sgat()
