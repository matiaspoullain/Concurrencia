
google_mobility <- function(country.code){
  if (missing(country.code)) {
    stop('"country.code" must be specified')
  }
  if (sum(!(country.code %in% unicos$country_region_code)) > 0) {
    stop('"country.code" must be a valid country code, a vector of valid country codes or "All"')
  }
  URL <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
  temp <- tempfile()
  download.file(URL,temp)
  if(length(country.code > 1)){
    data <- data.frame()
    for (i in country.code){
      one.country <- read.csv(unz(temp, i), encoding = "UTF-8")
      data <- rbind(data, one.country)
    }
  }else if(country.code == "All"){
    nombres <- unzip(temp, list = TRUE)$Name
    data <- data.frame()
    for (i in nombres){
      one.country <- read.csv(unz(temp, i), encoding = "UTF-8")
      data <- rbind(data, one.country)
    }
  } else {
    data <- read.csv(unz(temp, paste("2020_", country.code,"_Region_Mobility_Report.csv", sep = "")), encoding = "UTF-8")
  }
  unlink(temp)
  data[is.na(data$country_region_code), "country_region_code"] <- "NA"
  data
}


todos <- google_mobility("All")

todos <- todos[,c("country_region_code", "country_region")]

unicos <- unique(todos)

unicos <- unicos[order(unicos$country_region),]

write.csv(unicos, "codigo pais.csv", row.names = FALSE)

google_mobility("AR")

google_mobility("hola")

google_mobility(c("LK", "BS"))


c("LK", "BS") %in% unicos$country_region_code

sum(!(c("LK", "BS") %in% unicos$country_region_code)) == 0
