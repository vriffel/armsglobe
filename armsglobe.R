#-------------------------------------------------------------------------------

## Data Manipulation
require(tidyverse)

## Get the data
#require(jsonlite)

## Working with strings
require(stringr)

## Data visualization
require(shiny)
require(shinydashboard)

#-------------------------------------------------------------------------------
## Load Data

## Get the data from Google Data Arts Team's github
#mydata <- fromJSON(url("https://raw.githubusercontent.com/dataarts/armsglobe/master/categories/All.json"))
#saveRDS(mydata, file = "mydata.RDS")
## You can get the this rds file here:
##              https://github.com/vriffel/armsglobe/blob/master/mydata.RDS
mydata <- readRDS("mydata.RDS")

#-------------------------------------------------------------------------------
## Data Cleaning

da <- unlist(mydata)
db <- as.vector(da)
## i = i mport
imp.idx <- str_detect(string = names(da), pattern = "\\.i")
## wc = use (mil = military, civ = civil, ammo = ammunition)
wc.idx <- str_detect(string = names(da), pattern = "\\.wc")
## v = value
v.idx <- str_detect(string = names(da), pattern = "\\.v")
## e = export
e.idx <- str_detect(string = names(da), pattern = "\\.e")
imp <- db[imp.idx]
wc <- db[wc.idx]
e <- db[e.idx]
v <- as.numeric(db[v.idx])

#-------------------------------------------------------------------------------
## Create a column with years

times.year <- numeric(19)
for (i in 1:19) {
    yr <- nrow(mydata$timeBins$data[[i]])
    times.year[i] <- yr
}
year <- rep(c(1992:2010), times = times.year)

#-------------------------------------------------------------------------------
## Create a unique object with the data

all <- data.frame("year" = year, "imp" = imp, "wc" = wc, "e" = e, "v" = v)

#-------------------------------------------------------------------------------
