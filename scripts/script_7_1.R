rm(list=ls(all=TRUE))

dir <- 'C://Users/Ciara Hovis/Desktop/Research/Co-author papers/MetaBD/Analysis/survey_data/Clean_7.1/'

survey1 <- read.csv('C://Users/Ciara Hovis/Desktop/Research/Co-author papers/MetaBD/Analysis/survey_data/Clean_7.1/survey1_cleaned.csv')

survey2 <- read.csv('C://Users/Ciara Hovis/Desktop/Research/Co-author papers/MetaBD/Analysis/survey_data/Clean_7.1/survey2_cleaned_20200623.csv')

survey3 <- read.csv('C://Users/Ciara Hovis/Desktop/Research/Co-author papers/MetaBD/Analysis/survey_data/Clean_7.1/survey3_cleaned.csv')

# load packages
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
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

# Habitats

habitat <- count(survey2, 'habitat')
prop.table(table(survey2$habitat))

# taxa

desired_length <- 8 
empty_list <- vector(mode = "list", length = desired_length)

count(survey2$taxa)
taxa <- (survey2$taxa)
taxa.list <- c('Mammals', 'Plants', 'Reptiles', 'Amphibians', 'Birds', 'Fish', 'Invertebrates', 'Other')
taxa.count <- empty_list

for (i in 1:length(taxa.list)) {
  
  taxa.count[i] <- sum(str_count(taxa, taxa.list[i]))
    }

taxa.count <- rbind(taxa.list,taxa.count)
taxa.count

multiple <- str_count(taxa, ';')
multiple <- multiple != 0
multiple <- table(multiple) ["TRUE"]

# metrics

num.metric <- count(survey3$paper_id)
range(num.metric$freq)
sum(num.metric$freq)

# geo dist

biogeo <- table(survey2$biodiv_countries)

geo <- table(survey2$list_continents)

# TC flows

count(survey2$tele_cat)

tc.list <- c("Energy Transfer", "Investment", "Knowledge Transfer", "Migration (human)", 
             "Migration (non-human)","Other", "remove", "Species Dispersal", "Technology Transfer",
             "Tourism", "Trade", "Water Transfer", "Waste Transfer")

desired_length <- length(tc.list)
empty_list <- vector(mode = "list", length = desired_length)


tc.cat <- (survey2$tele_cat)
tc.count <- empty_list

for (i in 1:length(tc.list)) {
  
  tc.count[i] <- sum(str_count(tc.cat, tc.list[i]))
  
}

tc.count
tc.count <- as.data.frame(rbind(tc.list,tc.count))
tc.count

# Can't get loop to count the migration values for some reason, manual input code below

tc.table <- table(survey2$tele_cat)

tc.count[2,4] <- tc.table[4] + tc.table[15]      # Migration (human) n=6
tc.count[2,5] <- tc.table[5]                     # Migration (non-human) n-7

tc.count

gather(tc.count)


# Plot for habitat and impact

survey3$hab <- NA
hab.table <- as.data.frame(cbind(survey2$paper_id, survey2$habitat))
survey3$hab <- hab.table$V2[match(survey3$paper_id,hab.table$V1)]


# get summary
a <- ddply(survey3, .(hab, effect), summarize,
           # total count of entries     
           count=length(hab)) %>%
  # percents of effects within totals per hab
  group_by(hab) %>%
  nest() %>% 
  mutate(perc_per_hab=map(data, function(x) x$count/sum(x$count))) %>% 
  unnest()


# save as csv
# write.csv(a, paste0(tab.dir,'survey3_summary_effects_by_taxa.csv'),
#          row.names = TRUE)

# view

a

quick_plot <- ggplot(a, aes(fill=effect, y=perc_per_hab, x=hab)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("")

quick_plot



# Plot TC percent effect figure
# get summary

# subset the entries in survey 3 that have multiple metric types (prob could use if else but not sure how to code)


survey3$flag <- with(survey3, ifelse((!is.na(biodiv_cat_1sp)) &
                                       ((!is.na(biodiv_cat_multsp)) |
                                       (!is.na(biodiv_cat_habitat))),"Flag",
                                     ifelse((!is.na(biodiv_cat_multsp)) & 
                                              (!is.na(biodiv_cat_habitat)), "Flag", 
                                            ifelse((is.na(biodiv_cat_1sp)) &
                                                     (is.na(biodiv_cat_multsp)) &
                                                     (is.na(biodiv_cat_habitat)), "Flag", "NA" ))))                                                               

# subset flags column for yes

wrongo <- subset(survey3, survey3$flag == "Flag")

write.csv(wrongo, "C://Users/Ciara Hovis/Desktop/Research/Co-author papers/MetaBD/Analysis/survey_data/Clean_7.1/Survey3_FIX.csv")

