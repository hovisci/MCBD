## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = FALSE, cache.comments = FALSE,
                      warning = FALSE, message = FALSE, results='hold')


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# Source file
  source('./scripts/Reference.R')


## ---- echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
# Data directory
  dat.dir


## ---- echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
# Final tables
  tab.dir


## ---- echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
# Final figures
  fig.dir


## ---- echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
tab.check.dir


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# load previous workspace (if needed)
  #load("survey3Results.RData")

# survey
  survey3 <- read.csv(paste0(dat.dir,'survey_cleanup\\survey3_for_cleanup.csv'))

# 5 common surveys
  survey3.5 <- read.csv(paste0(dat.dir,
                       'common_papers\\survey3_5_common_paper_selection_090120.csv'))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop columns
  survey3.5 <- subset(survey3.5,
                      select=-c(entry,final_decision))

# change structure
  survey3.5$entry_id <- as.character(survey3.5$entry_id)
  survey3.5$p_value <- as.character(survey3.5$p_value)
  
# bind rows
  survey3 <- bind_rows(survey3,survey3.5)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# number entries
  paste('number of entries:',nrow(survey3))
  
# number columns
  paste('number of columns:',ncol(survey3))
  
# column names
  paste('column names:')
  colnames(survey3)

# number of unique papers surveyed
  paste('number of papers:',length(unique(survey3$paper_id)))
  
# number of observers
  paste('number of observers:',length(unique(survey3$coder_id)))
  
# list of observers
  paste('observer names:')
  paste(unique(survey3$coder_id),collapse="; ")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# check for blank paper ID number fields
  survey3[survey3$paper_id=='',]
  survey3[is.na(survey3$paper_id),]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# check for blank entry ID number fields
  survey3[survey3$entry_id=='',]
  survey3[is.na(survey3$entry_id),]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# check structure
  str(survey3$entry_id)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract non-numeric entry IDs
  subset(survey3, !grepl('^\\d+$', survey3$entry_id))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract non-numeric entry IDs
  nonnum <- subset(survey3, !grepl('^\\d+$', survey3$entry_id))
  
# antijoin from survey3
  survey3 <- anti_join(survey3,nonnum)
  
# move to notes
  nonnum$notes <- nonnum$entry_id
  
# assign numbers
  nonnum <- nonnum %>% 
              mutate(entry_id = group_indices(., entry_id))
  
# rejoin to survey 3
  survey3 <- rbind(survey3,nonnum)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# subset out paper ID 5905
  s3_sub <- survey3[survey3$paper_id==5905,]

# antijoin from survey3
  survey3 <- anti_join(survey3,s3_sub)
  
# assign numbers
  s3_sub <- s3_sub %>% 
              mutate(entry_id = seq_along(entry_id))
  
# rejoin to survey 3
  survey3 <- rbind(survey3,s3_sub)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# check for blank data fields
  survey3[survey3$taxon_or_func=='',]
  #survey3[is.na(survey3$taxon_or_func),]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract paper ID numbers
  tfIDs <- unique(survey3$paper_id[survey3$taxon_or_func==''])

# extract papers with at least one empty taxon_or_func
  taxfun <- subset(survey3, survey3$paper_id %in% tfIDs)

# antijoin from survey3
  survey3 <- anti_join(survey3,taxfun)
  
# visually inspect each paper
  # for (i in 1:length(tfIDs)){
  #   print(taxfun[taxfun$paper_id==tfIDs[[i]],])
  # }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# manual edits 
  taxfun$taxon_or_func[taxfun$paper_id==292 & taxfun$entry_id==2] <- 'Taxonomic'
  taxfun$taxon_or_func[taxfun$paper_id==479 & taxfun$entry_id==3] <- 'Taxonomic'
  taxfun$taxon_or_func[taxfun$paper_id==6845 & taxfun$entry_id==2] <- 'Taxonomic'
  taxfun$taxon_or_func[taxfun$paper_id==5844 & taxfun$entry_id==11] <- 'Functional'
  taxfun$taxon_or_func[taxfun$paper_id==713 & taxfun$entry_id==2] <- 'Functional'
  taxfun$taxon_or_func[taxfun$paper_id==479 & taxfun$entry_id==3] <- 'Taxonomic'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  taxfun$taxon_or_func[taxfun$paper_id==4087 & taxfun$entry_id==1] <- 'Functional'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# subset
  taxfun_NA <- taxfun[taxfun$taxon_or_func=='',]

# save as csv (conditional)
  if (nrow(taxfun_NA) >=1){
  write.csv(taxfun_NA,paste0(tab.check.dir,
                             'survey3_cleanup_tax_fun_missing_v2.csv'),
            row.names = FALSE)
  }

# preview
  taxfun_NA


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# rejoin to survey 3
  survey3 <- rbind(survey3,taxfun)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# check for blank data fields
  survey3[survey3$taxa==''|survey3$taxa=='Other',]
  #survey3[is.na(survey3$taxa),]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract paper ID numbers
  txIDs <- unique(survey3$paper_id[survey3$taxa==''|survey3$taxa=='Other'])

# extract papers with at least one empty taxon_or_func
  tax <- subset(survey3, survey3$paper_id %in% txIDs)

# antijoin from survey3
  survey3 <- anti_join(survey3,tax)
  
# visually inspect each paper
  for (i in 1:length(txIDs)){
    #print(tax[tax$paper_id==txIDs[[i]],])
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# manual edits 
  tax$taxa[tax$paper_id==6502 & tax$entry_id==18] <- 'Birds'
  tax$taxa[tax$paper_id==2050 & tax$entry_id==3] <- 'Plants/trees/shrubs'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  tax$taxa[tax$paper_id==3485 |tax$paper_id==5804] <- 'Invertebrates'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# rejoin to survey 3
  survey3 <- rbind(survey3,tax)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# check for blank data fields
  survey3[survey3$effect=='',]
  #survey3[is.na(survey3$effect),]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract paper ID numbers
  efIDs <- unique(survey3$paper_id[survey3$effect==''])

# extract papers with at least one empty effect
  eff <- subset(survey3, survey3$paper_id %in% efIDs)

# antijoin from survey3
  survey3 <- anti_join(survey3,eff)
  
# visually inspect each paper
  for (i in 1:length(efIDs)){
    #print(eff[eff$paper_id==efIDs[[i]],])
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# manual edits 
  eff$effect[eff$paper_id==6502 & eff$entry_id==13] <- 'Negative (detrimental)'  # EmilyD, paper 6502
  eff$effect[eff$paper_id==826 & eff$entry_id==2] <- 'Negative (detrimental)'  # CiaraH, paper 826 (CLH 9/23/2020)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# subset
  eff_NA <- eff[eff$effect=='',]

# save as csv (conditional)
  if (nrow(eff_NA)>=1){
  write.csv(eff_NA,paste0(tab.check.dir,'survey3_cleanup_effect_missing.csv'),
            row.names = TRUE)
  }

# preview
  eff_NA


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# rejoin to survey 3
  survey3 <- rbind(survey3,eff)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# check for blank data fields
  survey3[survey3$significant=='',]
  #survey3[is.na(survey3$significant),]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract paper ID numbers
  sigIDs <- unique(survey3$paper_id[survey3$significant==''])

# extract papers with at least one empty significant
  sig <- subset(survey3, survey3$significant=='')

# antijoin from survey3
  survey3 <- anti_join(survey3,sig)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# manual edits 
  sig$significant[sig$paper_id==3989 & sig$entry_id==5] <- 'TRUE'
  #sig$significant[sig$paper_id==411 & sig$entry_id==25] <- 'TRUE'
  sig$significant[sig$paper_id==5844 & sig$entry_id==10] <- 'TRUE'
  sig$significant[sig$paper_id==6502 & sig$entry_id==9] <- 'TRUE'
  sig$significant[sig$paper_id==713 & sig$entry_id==2] <- 'TRUE'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# subset
  sig_NA <- sig[sig$significant=='',]

# save as csv (conditional)
  if (nrow(sig_NA)>=1){
      write.csv(sig_NA,paste0(tab.check.dir,'survey3_cleanup_sig_missing.csv'),
                row.names = TRUE)
  }

# preview
  sig_NA


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# rejoin to survey 3
  survey3 <- rbind(survey3,sig)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change all blanks to NAs
  survey3$biodiv_cat_habitat[survey3$biodiv_cat_habitat==''] <- NA
# check for blank data fields
  #survey3[survey3$scale=='',]
  survey3[is.na(survey3$scale) & is.na(survey3$biodiv_cat_habitat),]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# extract and save as CSV (conditional)
  no_scale <- survey3[is.na(survey3$scale) & !is.na(survey3$biodiv_cat_habitat),]

# conditional save
  if (nrow(no_scale)>=1){
  write.csv(no_scale,paste0(tab.check.dir,'survey3_cleanup_scale_missing.csv'),
            row.names = TRUE)
  }

# preview
  no_scale


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get levels
  survey3$taxa <- as.factor(survey3$taxa)
  survey3$taxa <- droplevels(survey3$taxa)
  levels(survey3$taxa)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to character
  survey3$taxa <- as.character(survey3$taxa)

# make list of entries to change
  mult_list <- c("Mammals, Reptiles, Amphibians, Invertebrates",
                 "Mammals, Reptiles, Amphibians, Invertebrates, Plants/trees/shrubs",
                 "Multiple, unspecified",
                 "Reptiles;Amphibians",
                 "Birds, Mammals"
                  )

# change to multiple
  survey3$taxa[grepl(paste(mult_list,collapse="|"),survey3$taxa)] <- 'Multiple'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
  survey3$taxa[grepl('*plants*',
              survey3$taxa,ignore.case = TRUE)] <- 'Plants/trees/shrubs'

  survey3$taxa <- gsub('Plants/trees/shrubs','Plants/Trees/Shrubs',
                       survey3$taxa)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  mult_list <- c("echinoderm","Clams"
                  )

# change to inverts
  survey3$taxa[grepl(paste(mult_list,collapse="|"),survey3$taxa)] <- 'Invertebrates'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# replace semi-colon to comma
  survey3$taxa <- gsub('^Birds $','Birds',survey3$taxa)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# replace semi-colon to comma
  survey3$taxa <- gsub(';',', ',survey3$taxa)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of items
  taxa_list <- c("Other",
                 "not clear",
                 "bio-geodiversity",
                 "habitat"
                 )

# subset
  weird_tx <- survey3 %>%
              subset(grepl(paste(taxa_list,collapse="|"), 
                           taxa))

# antijoin from survey3
  survey3 <- anti_join(survey3,weird_tx)
  
# show entries
  weird_tx


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# save as csv (conditional)
  if (nrow(weird_tx)>=1){
  write.csv(weird_tx,paste0(tab.check.dir,'survey3_cleanup_weird_taxa.csv'),
            row.names = TRUE)
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# rejoin to survey 3
  survey3 <- rbind(survey3,weird_tx)

# show levels
  survey3$taxa <- as.factor(survey3$taxa)
  survey3$taxa <- droplevels(survey3$taxa)
  levels(survey3$taxa)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get levels
  survey3$taxon_or_func <- as.factor(survey3$taxon_or_func)
  survey3$taxon_or_func <- droplevels(survey3$taxon_or_func)
  levels(survey3$taxon_or_func)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# edit
  survey3$taxon_or_func <- gsub('Taxanomic','Taxonomic',survey3$taxon_or_func)

# get levels
  survey3$taxon_or_func <- as.factor(survey3$taxon_or_func)
  survey3$taxon_or_func <- droplevels(survey3$taxon_or_func)
  levels(survey3$taxon_or_func)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get levels
  survey3$biodiv_cat_1sp <- as.factor(survey3$biodiv_cat_1sp)
  levels(survey3$biodiv_cat_1sp)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to character
  survey3$biodiv_cat_1sp <- as.character(survey3$biodiv_cat_1sp)

# edit blanks
  survey3$biodiv_cat_1sp[survey3$biodiv_cat_1sp==''] <- NA 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("bird extinction risks")

# change
  survey3$biodiv_cat_1sp[grepl(paste(div_list,collapse="|"),
               survey3$biodiv_cat_1sp,
               ignore.case = TRUE)
               ] <- 'Population dynamics (survival, fitness, reproduction, mortality, etc.)'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("*Occurrence*","*detection*")

# change
  survey3$biodiv_cat_1sp[grepl(paste(div_list,collapse="|"),
               survey3$biodiv_cat_1sp,
               ignore.case = TRUE)
               ] <- 'Occurrence (presence, range, persistence, etc., NOT detection)'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change multiple field to character
  survey3$biodiv_cat_habitat <- as.character(survey3$biodiv_cat_habitat)

# change to habitat
  survey3$biodiv_cat_habitat[
    survey3$biodiv_cat_1sp=="Suitable habitat loss"] <- "Suitable habitat loss"
  
# erase
  survey3$biodiv_cat_1sp[
      survey3$biodiv_cat_1sp=="Suitable habitat loss"] <- NA


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("*number of*",
                 "*abundance*")

# change
  survey3$biodiv_cat_1sp[grepl(paste(div_list,collapse="|"),
               survey3$biodiv_cat_1sp,
               ignore.case = TRUE)
               ] <- as.character('Abundance/Density (number of individuals, individuals/unit area, biomass)')


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get levels
  survey3$biodiv_cat_1sp <- as.factor(survey3$biodiv_cat_1sp)
  survey3$biodiv_cat_1sp <- droplevels(survey3$biodiv_cat_1sp)
  levels(survey3$biodiv_cat_1sp)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to character
  survey3$biodiv_cat_multsp <- as.character(survey3$biodiv_cat_multsp)

# change blank to NA
  survey3$biodiv_cat_multsp[survey3$biodiv_cat_multsp==''] <- NA 

# get levels
  levels(as.factor(survey3$biodiv_cat_multsp))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("*taxonomic distinctness",
                "composite index*",
                "Tolerance to disturbance*")

# change to diversity index
  survey3$biodiv_cat_multsp[grepl(paste(div_list,collapse="|"),
               survey3$biodiv_cat_multsp,
               ignore.case = TRUE)] <- "Diversity index (Shannon-Weiner, Simpson's, Inverse Simpson's, etc.)"


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("^Abundance$",
                "all herbivore biomass",
                'all herbivores',
                "Herbivore biomass",
                "*biomass*",
                "*Pisci*",
                "*Non-Target*",
                '*Log10 of number of*',
                '^Percent cover$'
                )

# change to abundance/density
  survey3$biodiv_cat_multsp[grepl(paste(div_list,collapse="|"),
        survey3$biodiv_cat_multsp,
        ignore.case = TRUE)
        ] <- 'Abundance/Density (biomass, mass, number of individuals, individuals/unit area)'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("*richness*")

# change to richness
  survey3$biodiv_cat_multsp[grepl(paste(div_list,collapse="|"),
               survey3$biodiv_cat_multsp,
               ignore.case = TRUE)] <- 'Richness (number of species)'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("Mortality",
                "Total fisheries production",
                "reproduction",
                "*index of coral health*",
                "*regeneration*"
                )

# change to pop dynamics
  survey3$biodiv_cat_multsp[grepl(paste(div_list,collapse="|"),
               survey3$biodiv_cat_multsp)] <- 'Population dynamics (survival, fitness, reproduction, mortality, etc.)'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to character 
  survey3$biodiv_cat_multsp <- as.character(survey3$biodiv_cat_multsp)

# make list of entries to change
  div_list <- c("*occur*","*detection*")

# change to occurrence
  survey3$biodiv_cat_multsp[grepl(paste(div_list,collapse="|"),
               survey3$biodiv_cat_multsp,
               ignore.case = TRUE)] <- 'Occurrence'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# subset out paper ID 5905
  survey3 <- survey3[!(survey3$paper_id==2546 & survey3$entry_id==2),]


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("*Vulnearability*")

# change to other
  survey3$biodiv_cat_multsp[grepl(paste(div_list,collapse="|"),
               survey3$biodiv_cat_multsp)] <- "Diversity index (Shannon-Weiner, Simpson's, Inverse Simpson's, etc.)"


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# E_Xing entries
  survey3$biodiv_cat_multsp[survey3$biodiv_cat_multsp=='Other' & 
                            survey3$paper_id==3668] <- 'Population dynamics (survival, fitness, reproduction, mortality, etc.)'
  survey3$biodiv_cat_multsp[survey3$biodiv_cat_multsp=='Other' & 
                            survey3$paper_id==3485] <- 'Abundance/Density (biomass, mass, number of individuals, individuals/unit area)'
  
# 'coral reef size/age' fixes
  survey3$biodiv_cat_1sp[survey3$biodiv_cat_multsp=='coral reef size/age' & 
                         survey3$paper_id==1376 &
                         survey3$entry_id==3] <- 'With-in species diversity (genetic diversity, age structure, etc.)'
  survey3$biodiv_cat_1sp[survey3$biodiv_cat_multsp=='coral reef size/age' & 
                         survey3$paper_id==1376 &
                         survey3$entry_id==1] <- 'Abundance/Density (number of individuals, individuals/unit area, biomass)'
  survey3$biodiv_cat_multsp[survey3$biodiv_cat_multsp=='coral reef size/age' & 
                         survey3$paper_id==1376 &
                         (survey3$entry_id==1|survey3$entry_id==3)] <- NA


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get levels
  survey3$biodiv_cat_multsp <- as.factor(survey3$biodiv_cat_multsp)
  survey3$biodiv_cat_multsp <- droplevels(survey3$biodiv_cat_multsp)
  levels(survey3$biodiv_cat_multsp)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of items
  div_list <- c("Other"
                 )

# subset
  weird_div <- survey3 %>%
              subset(grepl(paste(div_list,collapse="|"),
                           biodiv_cat_multsp,
                           ignore.case = TRUE))

# show entries
  weird_div


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# save as csv (conditional on if there are entries)
  if (nrow(weird_div)>=1){
      write.csv(weird_div,paste0(tab.check.dir,
                                 'survey3_cleanup_weird_biodiv_multi.csv'),
                row.names = TRUE)
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to character
  survey3$biodiv_cat_habitat <- as.character(survey3$biodiv_cat_habitat)

# change blank to NA
  survey3$biodiv_cat_habitat[survey3$biodiv_cat_habitat==''] <- NA 

# get levels
  levels(as.factor(survey3$biodiv_cat_habitat))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("island area",
                "Suitable habitat loss")

# change
  survey3$biodiv_cat_habitat[grepl(paste(div_list,collapse="|"),
        survey3$biodiv_cat_habitat,
        ignore.case = TRUE)] <- 'Amount (e.g. land use change from non-habitat to habitat)'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change other
  survey3$biodiv_cat_habitat[survey3$biodiv_cat_habitat=='Other' &
      survey3$paper_id==44] <- 'Quality (pollution, connectence, disturbance, etc.)'



## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("habitat density",
                "habitat height",
                "habitat temperature")

# change
  survey3$biodiv_cat_habitat[grepl(paste(div_list,collapse="|"),
        survey3$biodiv_cat_habitat,
        ignore.case = TRUE)] <- 'Quality (pollution, connectence, disturbance, etc.)'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of entries to change
  div_list <- c("*compared birds*")

# change to notes
  note <- survey3$biodiv_cat_habitat[grepl(paste(div_list,collapse="|"),
              survey3$biodiv_cat_habitat,
              ignore.case = TRUE)]
  
  survey3$notes[grepl(paste(div_list,collapse="|"),
                survey3$biodiv_cat_habitat,
                ignore.case = TRUE)] <- paste0(note)
  
# change to NA
  survey3$biodiv_cat_habitat[grepl(paste(div_list,collapse="|"),
              survey3$biodiv_cat_habitat,
              ignore.case = TRUE)] <- NA


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to factor
  survey3$biodiv_cat_habitat <- as.factor(survey3$biodiv_cat_habitat)

# drop levels
  survey3$biodiv_cat_habitat <- droplevels(survey3$biodiv_cat_habitat)
  
# show levels
  levels(survey3$biodiv_cat_habitat)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of items
  div_list <- c("Other")

# subset
  weird_div <- survey3 %>%
                subset(grepl(paste(div_list,collapse="|"), 
                             biodiv_cat_habitat,
                             ignore.case = TRUE))
  
# show entries
  weird_div


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# save as csv (conditional)
  if (nrow(weird_div)>=1){
      write.csv(weird_div,paste0(tab.check.dir,
                                 'survey3_cleanup_weird_biodiv_hab.csv'),
                row.names = TRUE)
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# A_Herzberger errors
  # Paper ID 2546
    survey3$biodiv_cat_multsp[survey3$paper_id==2546 & 
                               survey3$entry_id==1] <- 'Abundance/Density (biomass, mass, number of individuals, individuals/unit area)'

  # Paper ID 1616
    survey3$biodiv_cat_habitat[survey3$paper_id==1616 & 
                              survey3$entry_id==1] <- NA
    
# A_Torres errors
  # Paper ID 357
    survey3$biodiv_cat_1sp[survey3$paper_id==357 & survey3$entry_id==5
                           ] <- 'Movement'

# K_Kapsar errors
  # Paper ID 3989
    survey3$biodiv_cat_1sp[survey3$paper_id==3989 & survey3$entry_id==8
                           ] <- 'Abundance/Density (biomass, mass, number of individuals, individuals/unit area)'

  # Paper ID 4552
    survey3$biodiv_cat_1sp[survey3$paper_id==4552 &
                             survey3$entry_id==14
                           ] <- 'Abundance/Density (biomass, mass, number of individuals, individuals/unit area)'

# M_Lei errors
  # Paper ID 523
    survey3$biodiv_cat_habitat[survey3$paper_id==523 & survey3$entry_id==1] <- NA
  # Paper ID 471
    survey3$biodiv_cat_habitat[survey3$paper_id==471] <- NA
    
  # Paper ID 6195
    survey3$biodiv_cat_habitat[survey3$paper_id==6195 & survey3$entry_id==1] <- NA
    
  # Paper ID 3247
    survey3$biodiv_cat_habitat[survey3$paper_id==3247] <- NA
    
# MG_Chung errors
  # Paper ID 654
    survey3$biodiv_cat_habitat[survey3$paper_id==654] <- NA
    
# Y_Li errors
  # Paper ID 2222
    survey3$biodiv_cat_habitat[survey3$paper_id==2222] <- NA
    
  # Paper ID 654
    survey3$biodiv_cat_habitat[survey3$paper_id==813] <- NA


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to character
  survey3$effect <- as.character(survey3$effect)

# change blank to NA
  survey3$effect[survey3$effect==''] <- NA 

# get levels
  levels(as.factor(survey3$effect))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# batch changes
  survey3$effect[grepl("*changed*",
                survey3$effect,
                ignore.case = TRUE)] <- 'Changed (e.g. species composition)'
  survey3$effect[grepl("*positive*",
                survey3$effect,ignore.case = TRUE)] <- 'Positive (beneficial)'
  survey3$effect[grepl("*Negative*",
                survey3$effect,ignore.case = TRUE)] <- 'Negative (detrimental)'
  survey3$effect[grepl("*Neutral*",
                survey3$effect,ignore.case = TRUE)] <-
    'Neutral (only when there is absolutely no difference in metrics)'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# show levels
  survey3$effect <- as.factor(survey3$effect)
  survey3$effect <- droplevels(survey3$effect)
  levels(survey3$effect)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# make list of items
  eff_list <- c("Unclear")

# subset
  weird_eff <- survey3 %>%
                subset(grepl(paste(eff_list,collapse="|"), 
                             effect,
                             ignore.case = TRUE))
  
# show entries
  weird_eff


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# save as csv (conditional)
  if (nrow(weird_eff)>=1){
      write.csv(weird_eff,paste0(tab.check.dir,
                                 'survey3_cleanup_weird_effects.csv'),
                row.names = TRUE)
  }


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to character
  survey3$significant <- as.character(survey3$significant)

# change blank to NA
  survey3$significant[survey3$significant==''] <- NA 

# get levels
  levels(as.factor(survey3$significant))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# batch changes
  survey3$significant[grepl("*True*|*yes*",
                survey3$significant,ignore.case = TRUE)] <- 'TRUE'
  survey3$significant[grepl("*False*|^no$",
                survey3$significant,ignore.case = TRUE)] <- 'FALSE'


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# show levels
  survey3$significant <- as.factor(survey3$significant)
  survey3$significant <- droplevels(survey3$significant)
  levels(survey3$significant)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# 4551
survey3$biodiv_cat_multsp[survey3$paper_id == '4551' & survey3$entry_id == 3] <- "Diversity index (Shannon-Weiner, Simpson's, Inverse Simpson's, etc.)"
survey3$biodiv_cat_multsp[survey3$paper_id == '4551' & survey3$entry_id == 4] <- "Diversity index (Shannon-Weiner, Simpson's, Inverse Simpson's, etc.)"

# 5905
survey3$effect[survey3$paper_id == '5905' & survey3$entry_id == 1] <- "Changed (e.g. species composition)"

# 5966
survey3$effect[survey3$paper_id == '5966' & survey3$entry_id == 1] <- "Changed (e.g. species composition)"

# 510
survey3$biodiv_cat_multsp[survey3$paper_id == '510' & survey3$entry_id == 2] <- "Abundance/Density (biomass, mass, number of individuals, individuals/unit area)"

# 4257
survey3 <- survey3[which(survey3$paper_id != '4257'),]                          # drop old entry for paper 4257
kelly_fix <- read.csv(paste0(dir,'/issues/dataFixes/KellySppCompEntryAdd.csv')) # read in Kelly's fixes
colnames(kelly_fix) <- colnames(survey3)                                        # make col names match
survey3 <- rbind(survey3, kelly_fix)                                            # Add kelly's entries (total in survey 3 should be 797)





## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get summary
  ddply(survey3, .(paper_id), summarize,
        # total count of entries     
        num_entries=length(entry_id),
        perc_dataset=length(entry_id)/nrow(survey3),
        num_taxa=length(unique(taxa))) %>% 
    arrange(desc(perc_dataset))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get summary
  a <- ddply(survey3, .(taxa), summarize,
        # total count of entries     
        num_entries=length(entry_id),
        perc_dataset=length(entry_id)/nrow(survey3),
        num_papers=length(unique(paper_id)),
        perc_papers=length(unique(paper_id))/length(unique(survey3$paper_id))) %>% 
      arrange(desc(perc_dataset))

# save as csv
  write.csv(a, paste0(tab.dir,'survey3_summary_number_taxa.csv'),
            row.names = TRUE)
  
# view
  a


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get summary
  a <- ddply(survey3, .(taxa), summarize,
          # total count of entries     
          num_taxonomic=sum(taxon_or_func=='Taxonomic'),
          perc_taxonomic=sum(taxon_or_func=='Taxonomic')/nrow(survey3),
          num_functional=sum(taxon_or_func=='Functional'),
          perc_functional=sum(taxon_or_func=='Functional')/nrow(survey3),
          num_1sp=sum(!is.na(biodiv_cat_1sp)),
          perc_1sp=sum(!is.na(biodiv_cat_1sp))/nrow(survey3),
          num_multsp=sum(!is.na(biodiv_cat_multsp)),
          perc_multsp=sum(!is.na(biodiv_cat_multsp))/nrow(survey3),
          num_hab=sum(!is.na(biodiv_cat_habitat)),
          perc_hab=sum(!is.na(biodiv_cat_habitat))/nrow(survey3))

# save as csv
  write.csv(a, paste0(tab.dir,'survey3_summary_biodiv_metrics.csv'),
            row.names = TRUE)
  
# view
  a


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get summary
  a <- ddply(survey3, .(effect, significant), summarize,
          # total count of entries     
          count=length(effect))

# Stack plot
  quick_plot <- ggplot(a, aes(fill=effect, y=count, x=significant)) + 
                geom_bar(position="stack", stat="identity") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                theme_classic() +
                xlab("")

# save image
  ggsave(filename=paste0(fig.dir,"Figure_significant_effects.png"),
         plot=quick_plot, height = 6, width = 11)
  
# save as csv
  write.csv(a, paste0(tab.dir,'survey3_summary_significant.csv'),
            row.names = TRUE)
  
# view
  a


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# get summary
  a <- ddply(survey3, .(taxa, effect), summarize,
          # total count of entries     
          count=length(taxa)) %>%
          # percents of effects within totals per taxa
          group_by(taxa) %>%
            nest() %>% 
             mutate(perc_per_taxa=map(data, function(x) x$count/sum(x$count))) %>% 
              unnest()
# save as csv
  write.csv(a, paste0(tab.dir,'survey3_summary_effects_by_taxa.csv'),
            row.names = TRUE)

# view
  a


## ---- fig.height = 6, fig.width = 11-------------------------------------------------------------------------------------------------------------------------
# Stack plot
  quick_plot <- ggplot(a, aes(fill=effect, y=perc_per_taxa, x=taxa)) + 
                geom_bar(position="stack", stat="identity") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                theme_classic() +
                xlab("")

# save image
  ggsave(filename=paste0(fig.dir,"Figure_Xtra_effects_by_taxa.png"),
         plot=quick_plot, height = 6, width = 11)
  
# view
  quick_plot


## ---- fig.height = 6, fig.width = 11-------------------------------------------------------------------------------------------------------------------------
# Stack plot
  quick_plot <- ggplot(a, aes(fill=effect, y=count, x=taxa)) + 
                geom_bar(position="stack", stat="identity") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                theme_classic() +
                xlab("")

# save image
  ggsave(filename=paste0(fig.dir,"Figure_Xtra_effects_by_taxa2.png"),
         plot=quick_plot, height = 6, width = 11)
  
# view
  quick_plot


## ------------------------------------------------------------------------------------------------------------------------------------------------------------
# export
  write.csv(survey3,paste0(dat.dir,'survey3_cleaned.csv'),row.names=FALSE)


