source("codigo pais.R")
google_mobility <- function(country.code){
  if (missing(country.code)) {
    stop('"country.code" must be specified')
  }
  if (sum(!(country.code %in% countries.codes$country.code)) > 0) {
    stop('"country.code" must be a valid country code, a vector of valid country codes or "All"')
  }
  URL <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
  temp <- tempfile()
  download.file(URL,temp)
  if(length(country.code > 1)){
    data <- data.frame()
    for (i in country.code){
      one.country <- read.csv(unz(temp, paste("2020_", i,"_Region_Mobility_Report.csv", sep = "")), encoding = "UTF-8")
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
