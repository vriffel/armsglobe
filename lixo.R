library(tidyverse)
library(jsonlite)
library(stringr)

myData <- fromJSON(url("https://raw.githubusercontent.com/dataarts/armsglobe/master/categories/All.json"))

da <- unlist(myData)
db <- as.vector(da)
i.idx <- str_detect(string = names(da), pattern = "\\.i")
wc.idx <- str_detect(string = names(da), pattern = "\\.wc")
v.idx <- str_detect(string = names(da), pattern = "\\.v")
e.idx <- str_detect(string = names(da), pattern = "\\.e")
i <- db[i.idx]
wc <- db[wc.idx]
e <- db[e.idx]
v <- db[v.idx]
all <- c(i, wc, e, v)

ggplot() +
    geom_bin2d(aes(x = i, y = v))
