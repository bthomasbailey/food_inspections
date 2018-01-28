library(XML)
library(readr)

dat <- xmlToDataFrame("dinesafe.xml")

names(dat) <- tolower(names(dat))
write_csv(dat, "dinesafe.csv")

