library(tidyr)
library(dplyr)
library(ggplot2)

setwd('/home/rook/Projects/Code/r-source')

chf <- read.csv('chf.csv', header = FALSE, sep = ';')
names(chf)