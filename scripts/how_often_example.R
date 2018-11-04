source('./data.R')
source('./how_often.R')

library(data.table)
library(ggplot2)
library(stringi)

temp <- how_often(scripts, regex = "sam")

ggplot(data = temp, aes(x = char, y = average)) + geom_col() + coord_flip()
