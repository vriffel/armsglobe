library(plyr)
library(dplyr)
library(tesseract)
setwd("/home/dsbd/Desktop/teste/")

d <- getwd()

dataextract <- list.files(path = d,pattern = "*.png",all.files = T,full.names = F)

Extraction <- llply(.data=dataextract, .fun=ocr)

write.csv(Extraction[[1]], file = paste0("/home/dsbd/Desktop/teste/",
                                         names(Extraction)[[1]],".txt"))

## Salva o arquivo como gato1.txt
names(Extraction) <- c("gato1","gato2","gato3")

write.csv(Extraction[[1]], file = paste0("/home/dsbd/Desktop/teste/",
                                         names(Extraction)[[1]],".txt"))
## Fazendo um loop
for(i in 1:length(Extraction)){
  write.csv(Extraction[[i]], file = paste0("/home/dsbd/Desktop/teste/",
                                           names(Extraction)[[i]],".txt"))
}
