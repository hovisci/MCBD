data <- read.csv("C://Users/Ciara Hovis/Desktop/Research/Co-author papers/MetaBD/Analysis/pct_of_tc_by_country.csv")

table(data$tc)

library(ggplot2)

#############
### To Do ###
#############

# Figure out "Other" TC type, both occur for US & ZAF 

data[which(data$tc =="Other"),]

# How many countries? n = 73

length(unique(data$ctr))

# How many instances of TC total? n = 257

total_tc <- paste(data$ctr,data$total_tc, sep = "_")
total_tc <- unique(total_tc)
total_tc <- as.data.frame(total_tc)

total_tc$tcValue <- gsub(pattern = "[^0123456789]", replacement= "", x = total_tc$`unique(total_tc)`)
total_tc$tcValue <- as.numeric(total_tc$tcValue)
sum(total_tc$tcValue)

# Percent of total tc for each country, in descending order

total_tc$pct <- total_tc$tcValue/sum(total_tc$tcValue)

total_tc2 <- total_tc[order(-total_tc$pct),]

# What percentage is in first 20 countries? ANSWER:  ~60%

sum(total_tc2$pct[1:20])

# Pie chart time (only first 10 countries)

data2<-subset(data[1:10,])


ggplot(data2, aes(x="", y=n_tc, fill=tc)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  facet_wrap( facet= ~ctr)
  
