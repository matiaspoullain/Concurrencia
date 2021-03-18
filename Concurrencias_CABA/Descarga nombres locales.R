#Descarga de nombres de locales en buenos aires

library(sgat)

initialization_sgat()
caba <- tripadvisor_places("Buenos Aires")

length(caba)
caba
caba <- data.frame(1:length(caba), caba)

write.csv(caba, "Nombres bares CABA.csv")
