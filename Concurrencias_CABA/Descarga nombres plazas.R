library(htmltab)
wiki <- htmltab("https://es.wikipedia.org/wiki/Anexo:Plazas_de_la_ciudad_de_Buenos_Aires")

write.csv(wiki, "tabla.csv")

wiki <- read.csv("tabla.csv", fileEncoding = "UTF-8")

write.csv(wiki, "Nombres parques CABA.csv", fileEncoding = "UTF-8")



wiki$Nombre.de.la.plaza.o.plazoleta
