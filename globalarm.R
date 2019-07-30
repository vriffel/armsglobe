library(tidyverse)
library(jsonlite)
library(stringr)

#myData <- fromJSON(url("https://raw.githubusercontent.com/dataarts/armsglobe/master/categories/All.json"))
#saveRDS(myData, file = "myData.RDS")
myData <- readRDS("myData.RDS")

da <- unlist(myData)
db <- as.vector(da)
## i importação
imp.idx <- str_detect(string = names(da), pattern = "\\.i")
## wc uso da arma (mil = militar, civ = civil, ammo = munição)
wc.idx <- str_detect(string = names(da), pattern = "\\.wc")
## v = valor
v.idx <- str_detect(string = names(da), pattern = "\\.v")
## e exportação
e.idx <- str_detect(string = names(da), pattern = "\\.e")
imp <- db[imp.idx]
wc <- db[wc.idx]
e <- db[e.idx]
v <- as.numeric(db[v.idx])
times.year <- numeric(19)
for (i in 1:19) {
    yr <- nrow(myData$timeBins$data[[i]])
    times.year[i] <- yr
}
year <- rep(c(1992:2010), times = times.year)
all <- data.frame("year" = year, "imp" = imp, "wc" = wc, "e" = e, "v" = v)
