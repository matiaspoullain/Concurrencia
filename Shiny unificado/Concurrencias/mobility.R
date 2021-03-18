mobility_var <- function(country.code){
  if (missing(country.code)) {
    stop('"country.code" must be specified')
  }
  if (sum(!(country.code %in% countries_codes$country.code)) > 0) {
    stop('"country.code" must be a valid country code, a vector of valid country codes or "All"')
  }
  URL <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
  temp <- "descargados.zip"
  utils::download.file(URL,temp, method = "curl")
  if(length(country.code > 1)){
    data <- data.frame()
    for (i in country.code){
      one.country <- utils::read.csv(unz(temp, paste("2020_", i,"_Region_Mobility_Report.csv", sep = "")), encoding = "UTF-8")
      data <- rbind(data, one.country)
    }
  }else if(country.code == "All"){
    nombres <- utils::unzip(temp, list = TRUE)$Name
    data <- data.frame()
    for (i in nombres){
      one.country <- utils::read.csv(unz(temp, i), encoding = "UTF-8")
      data <- rbind(data, one.country)
    }
  } else {
    data <- utils::read.csv(unz(temp, paste("2020_", country.code,"_Region_Mobility_Report.csv", sep = "")), encoding = "UTF-8")
  }
  data[is.na(data$country_region_code), "country_region_code"] <- "NA"
  data
}



load("countries_codes.rda")
