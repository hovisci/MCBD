# Source file
source('./scripts/Reference.R')

# Data directory
dat.dir


# Final tables
tab.dir


# Final figures
fig.dir

s23 <- read.csv(paste0(dat.dir,'s23.csv'))
taxEff<- read.csv(paste0(dat.dir,'tax_eff.csv'))
assign <- read.csv(paste0(dat.dir,'assignments_fixed.csv'))

final.ID <- unique(taxEff$tax_ID)
x <-as.data.frame(str_split_fixed(final.ID, '_', 2))
x <- x[order(x$V1),]

finalSamp <- data.frame()[1:length(x$V1), ]
rownames(finalSamp)<-1:length(x$V1)

finalSamp$paper_ID <- x$V1
finalSamp$title <- assign$TITLE[match(finalSamp$paper_ID,assign$STUDY_ID.1)]
finalSamp$author <- assign$AU[match(finalSamp$paper_ID,assign$STUDY_ID.1)]
finalSamp$year<- assign$PY[match(finalSamp$paper_ID,assign$STUDY_ID.1)]
finalSamp$abstract <- assign$ABSTRACT[match(finalSamp$paper_ID,assign$STUDY_ID.1)]

finalSamp$taxa <- x$V2
finalSamp$tc <- s23$tc[match(finalSamp$paper_ID,s23$paper_id)]

finalSamp$bd_ctr <- s23$biodiv_countries[match(finalSamp$paper_ID,s23$paper_id)]
finalSamp$bd_cont <- s23$bd_continents[match(finalSamp$paper_ID,s23$paper_id)]

finalSamp$study_id <- paste0(finalSamp$paper_ID,'_',finalSamp$taxa)
finalSamp$study_effect <- taxEff$maj_eff_taxa[match(finalSamp$study_id,taxEff$tax_ID)]

write.csv(finalSamp,'./data/finalSample.csv',row.names = FALSE)

