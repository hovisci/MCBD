rm(list=ls())                         # clear Console Window
options(show.error.locations = TRUE); # show line numbers on error

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
library("stringr")
library("writexl")
library("rnaturalearthdata")
library("rnaturalearth")
library("sf")


### Directories & Set up ###

# Root directory
dir <- paste0(getwd())

# Data directory
dat.dir <- paste0(dir,'/data/')

# Final tables
tab.dir <- paste0(dir,'/tables/')

# Final figures
fig.dir <- paste0(dir,'/images/')

# Tables for manual edits/error checks
tab.check.dir <- paste0(tab.dir,'/manual_check_',date(Sys.Date()),'/')

# custom colors
paper_col <- c('#DDCC77','#009988','#882255')
effect_cols <- c('#9A709E','#999999','#A50026','#FDB366','#6EA6CD')

## customized theme (I just copied this from app3) ##

# my_theme = function()
# {
# theme( axis.text.y = element_text(angle = 45),
#        plot.subtitle = element_text(color ="purple", size = 11),
#        axis.text.x = element_text(face = 'italic', color = 'purple')) 
# }
