library(tidyverse)
library(jsonlite)
library(stringr)

#mydata <- fromJSON(url("https://raw.githubusercontent.com/dataarts/armsglobe/master/categories/All.json"))
#saveRDS(mydata, file = "mydata.RDS")
mydata <- readRDS("mydata.RDS")
da <- unlist(mydata)
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
    yr <- nrow(mydata$timeBins$data[[i]])
    times.year[i] <- yr
}
year <- rep(c(1992:2010), times = times.year)
all <- data.frame("year" = year, "imp" = imp, "wc" = wc, "e" = e, "v" = v)

#-----------------------------------------------------------------------

br.idx <- which(all$i == "Brazil")
br <- all[br.idx, ]
head(br)
