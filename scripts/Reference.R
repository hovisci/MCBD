rm(list=ls())                         # clear Console Window
options(show.error.locations = TRUE); # show line numbers on error

# test

### libraries ###
library("ggplot2")      # include all GGPlot2 functions
library("tidyverse")    # plots and working with complex tables
library("RColorBrewer") # colors for graphics
library("plyr")         # sort data
library("reshape")      # sort data
library("reshape2")     # sort data
library('tmap')         # visualizing maps
library("sp")           # working with maps
library("lubridate")    # time data
library("tm")           # text mining
library("wordcloud")    # word clouds
library("gplots")       # venn diagrams
library("stringr")
library("writexl")
library("rnaturalearthdata")
library("rnaturalearth")
library("sf")
library("psych")
library('classInt')
library('devtools')
library('rnaturalearthhires')
library('rgeos')
getwd()

### Directories & Set up ###

# Root directory
#  setwd('..')           # set directory by one folder up
  dir <- paste0(getwd()); dir

# Data directory
  dat.dir <- paste0(dir,'/data/')

# Final tables
  tab.dir <- paste0(dir,'/results/')

# Final figures
  fig.dir <- paste0(dir,'/images/')

# Tables for manual edits/error checks
 dir.create(paste0(dir,'/issues/',date(Sys.Date())),recursive=TRUE)
 tab.check.dir <- paste0(dir,'/issues/',date(Sys.Date()),'/')

# custom colors
  paper_col <- c('#DDCC77','#009988','#882255')
  effect_cols <- c('#9A709E','#999999','#A50026','#FDB366','#6EA6CD')
  sigeff_col <- c("#00CC00", "#FF0000","#FF9933", "#C0C0C0")
  majeff_col <- c("#00CC00","#FF0000","#FF9933","#2274CD","#C0C0C0")

## customized theme (I just copied this from app3) ##

# my_theme = function()
# {
# theme( axis.text.y = element_text(angle = 45),
#        plot.subtitle = element_text(color ="purple", size = 11),
#        axis.text.x = element_text(face = 'italic', color = 'purple')) 
# }

