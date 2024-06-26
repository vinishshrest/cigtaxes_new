# path file 

rm(list = ls())

library(haven)
library(dplyr)
library(tidyr)
library(tidyselect)
library(plm)
library(ggplot2)
library(maps)
library(ggpubr)
library(gridExtra)
#install.packages("bacondecomp")
library(data.table)
library(janitor)
library(fixest)
library(bacondecomp)
library(ggiplot)
#library(fuzzyjoin)
library(xtable)
library(did)
library(stargazer)


user <- 2
if (user == 1) {
        datapath <- file.path("/home/user1/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/home/user1/Dropbox/cigtaxes_new", "output")
}else{
        datapath <- file.path("/home/vinish/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/home/vinish/Dropbox/cigtaxes_new", "output")
}

