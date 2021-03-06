## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = FALSE, cache.comments = FALSE,
                      warning = FALSE, message = FALSE, results='hold')


## ------------------------------------------------------------------------------------------------
# Source file
  source('./scripts/Reference.R')


## ---- echo=FALSE---------------------------------------------------------------------------------
# Data directory
  dat.dir


## ---- echo=FALSE---------------------------------------------------------------------------------
# Final tables
  tab.dir


## ---- echo=FALSE---------------------------------------------------------------------------------
# Final figures
  fig.dir


## ---- echo=FALSE---------------------------------------------------------------------------------
tab.check.dir


## ------------------------------------------------------------------------------------------------
# load previous workspace (if needed)
  #load("Survey1Results.RData")

# survey
  survey1 <- read.csv(paste0(dat.dir,'survey_cleanup\\survey1_for_cleanup.csv'))

# 5 common surveys (no changes since version 4)
  survey1.5 <- read.csv(paste0(dat.dir,
                        'common_papers\\survey1_5_common_paper_selection_090120.csv'))

# assignment sheet
  assignments <- read.csv(paste0(dat.dir,
                                 'absScreening\\Coding_MergeAssign_R1.csv'))
  
# abstract screening
  ab_screen <- read.csv(paste0(dat.dir,
                               'absScreening\\AbsScreen1_Merge.csv'))
  ab_maybes <- read.csv(paste0(dat.dir,
                               'absScreening\\AbsScreen2_maybes_to_keep.csv'))
  ab_yeses <- read.csv(paste0(dat.dir,
                              'absScreening\\final_sample_for_coding.csv'))


## ------------------------------------------------------------------------------------------------
survey1 <- bind_rows(survey1,survey1.5)


## ------------------------------------------------------------------------------------------------
# number entries
  paste('number of entries:',nrow(survey1))
  
# number columns
  paste('number of columns:',ncol(survey1))
  
# column names
  paste('column names:')
  colnames(survey1)

# number of unique papers surveyed
  paste('number of papers:',length(unique(survey1$paper_id)))
  
# number of observers
  paste('number of observers:',length(unique(survey1$coder_id)))
  
# list of observers
  paste('observer names:')
  paste(unique(survey1$coder_id),collapse="; ")


## ------------------------------------------------------------------------------------------------
# change paper ID based on original ID and coder
  assignments$STUDY_ID.1[assignments$STUDY_ID.1==501 &
                           assignments$REVIEWERS=='EmilyX'] <-5010002
  assignments$STUDY_ID.1[assignments$STUDY_ID.1==501 &
                           assignments$REVIEWERS=='Yuqian'] <- 5010001
  assignments$STUDY_ID.1[assignments$STUDY_ID.1==379 &
                           assignments$REVIEWERS=='Ruishan'] <- 3790002


## ------------------------------------------------------------------------------------------------
# get list of values to replace
  pID <- unique(survey1$paper_id)

# replace years
  for (i in 1:length(pID)){
      survey1$year_pub[survey1$paper_id==pID[[i]]] <-
      assignments$PY[assignments$STUDY_ID.1==pID[[i]]]
  }


## ------------------------------------------------------------------------------------------------
# change to character
  assignments$AU <- as.character(assignments$AU)
  survey1$author <- as.character(survey1$author)

# replace author names
  for (i in 1:length(pID)){
      aut <- str_split(assignments$AU[assignments$STUDY_ID.1==pID[[i]]],";")[[1]][1]
      survey1$author[survey1$paper_id==pID[[i]]] <- aut
  }


## ------------------------------------------------------------------------------------------------
# drop rejects
  keep <-  subset(survey1, status=='accept')

# antijoin for rejects (to be combined later)
  reject <- anti_join(survey1,keep)


## ------------------------------------------------------------------------------------------------
# drop levels
  keep <- droplevels(keep)

# unique criteria 1s
  #unique(keep$C1_biodiv)
  length(unique(keep$C1_biodiv))


## ------------------------------------------------------------------------------------------------
# unique criteria 2s
  #unique(keep$C2_meta)
  length(unique(keep$C2_meta))


## ------------------------------------------------------------------------------------------------
# unique criteria 3s
  #unique(keep$C3_quant)
  length(unique(keep$C3_quant))


## ------------------------------------------------------------------------------------------------
# unique criteria 4s
  keep$C4_study <- as.factor(keep$C4_study)
  levels(keep$C4_study)
  length(unique(keep$C4_study))


## ------------------------------------------------------------------------------------------------
# "Experimental study" 
  keep[keep$C4_study==levels(keep$C4_study)[1],]


## ------------------------------------------------------------------------------------------------
# "Interview" 
  keep[keep$C4_study==levels(keep$C4_study)[2],]


## ------------------------------------------------------------------------------------------------
# Edit as per email with coder 
  keep$C4_study[keep$paper_id==5853] <- "Observational study"


## ------------------------------------------------------------------------------------------------
# "Mixed: both observational/experimental study and literature review" 
  keep[keep$C4_study==levels(keep$C4_study)[3],]


## ------------------------------------------------------------------------------------------------
# Edit as per email with coder 
  keep$C4_study[keep$paper_id==425] <- "Observational study"


## ------------------------------------------------------------------------------------------------
# "Modeling study (using empirical data and not projected)" 
  keep[keep$C4_study==levels(keep$C4_study)[4],]


## ------------------------------------------------------------------------------------------------
# "Observational study" 
  keep[keep$C4_study==levels(keep$C4_study)[5],]


## ------------------------------------------------------------------------------------------------
# "Observational/Modeling study"
  keep[keep$C4_study==levels(keep$C4_study)[6],]


## ------------------------------------------------------------------------------------------------
# drop levels
  keep$C4_study <- droplevels(keep$C4_study)


## ------------------------------------------------------------------------------------------------
# show NA rows
  keep[keep$explicit_distant_impacts=='',]


## ------------------------------------------------------------------------------------------------
# A_Herzberger errors
  keep$explicit_distant_impacts[keep$coder_id=='A_Herzberger' &
                               (keep$paper_id==332  | 
                                keep$paper_id==4087 |
                                keep$paper_id==5486 |
                                keep$paper_id==5905)] <- 'No'

# A_Torres errors
  keep$explicit_distant_impacts[keep$coder_id=='A_Torres' &
                (keep$paper_id==357 | keep$paper_id==411)] <- 'No'
  keep$explain_distant_impacts[
                keep$coder_id=='A_Torres' & keep$paper_id==357] <-
                'The link with tourism is not well developed'
  keep$explain_distant_impacts[
      keep$coder_id=='A_Torres' & keep$paper_id==411] <-
      'Shipping or recreation, nor the source of the ships (local, national, international) is mentioned. The scale of the entire system is unclear.'

# E_Dean errors
  keep$explicit_distant_impacts[
      keep$coder_id=='E_Dean' & (keep$paper_id==339)] <- 'Yes'
  keep$explicit_distant_impacts[
      keep$coder_id=='E_Dean' & (keep$paper_id==2704)] <- 'No'
  keep$explain_distant_impacts[keep$coder_id=='E_Dean' & keep$paper_id==339] <- 
    'they explained how distal market forces and institutions affect biodiversity; relates market forces and institutions to biodiversity loss using a statistical analysis with household survey data and environmental data'
  keep$explain_distant_impacts[keep$coder_id=='E_Dean' & keep$paper_id==2704] <- 
    'they discuss road networks but do not connect their development or use to any distant place or interaction.'
  
# M_Lei errors
  keep$explicit_distant_impacts[
    keep$coder_id=='M_Lei' & (keep$paper_id==897)] <- 'Yes'
  keep$explicit_distant_impacts[
    keep$coder_id=='M_Lei' & (keep$paper_id==3151)] <- 'No'
  keep$explain_distant_impacts[keep$coder_id=='M_Lei' &
      (keep$paper_id==3151)] <- 'The removal of the dam connected the upstream and downstream'
  
# MG_Chung errors
  keep$explicit_distant_impacts[keep$coder_id=='MG_Chung' &
                               (keep$paper_id==45  | 
                                keep$paper_id==654 |
                                keep$paper_id==5966 |
                                keep$paper_id==6396)] <- 'Yes'
  keep$explicit_distant_impacts[keep$coder_id=='MG_Chung' &
                               (keep$paper_id==4132  | 
                                keep$paper_id==4155 |
                                keep$paper_id==6493)] <- 'No'
  keep$explain_distant_impacts[
    keep$coder_id=='MG_Chung' & keep$paper_id==45] <-
    'The impacts of soybean trade (Cerrado-sourced soy) on endemic species'
  keep$explain_distant_impacts[
    keep$coder_id=='MG_Chung' & keep$paper_id==654] <-
    'They studied how roads and the associated traffic intensity due to recreational activities influence natural ecological processes within a scavenger guild.'
  keep$explain_distant_impacts[
    keep$coder_id=='MG_Chung' & keep$paper_id==5966] <-
    'biotic homogenization of fish fauna caused by the elimination of a natural barrier by a dam construction, aquacuture, aquarium trade'
  keep$explain_distant_impacts[
    keep$coder_id=='MG_Chung' & keep$paper_id==6396] <-
    'the exposures of Chernobyl-derived ionising radiation on (surrounding and distant) lakes'
  keep$explain_distant_impacts[
    keep$coder_id=='MG_Chung' & keep$paper_id==4132] <-
    'Bird migration from distant areas impacts grassland ecosystem'
  keep$explain_distant_impacts[
    keep$coder_id=='MG_Chung' & keep$paper_id==4155] <-
    'Oil and gas development can be caused by distant energy needs, and such development may affect bird biodiversity'
  keep$explain_distant_impacts[
    keep$coder_id=='MG_Chung' & keep$paper_id==6493] <-
    'Exurban development can be caused by population growth/migration'
  
# X_Wu errors
  keep$explicit_distant_impacts[keep$coder_id=='X_Wu' &
                               (keep$paper_id==992  | 
                                keep$paper_id==2345 |
                                keep$paper_id==2606 |
                                keep$paper_id==5309 |
                                keep$paper_id==6820 |
                                keep$paper_id==6845 |
                                keep$paper_id==302 |
                                keep$paper_id==4257 |
                                keep$paper_id==5746|
                                keep$paper_id==1368|
                                keep$paper_id==1799|
                                keep$paper_id==3165|
                                keep$paper_id==4551|
                                keep$paper_id==5125)] <- 'Yes'
  
# Y_Dou errors
  keep$explicit_distant_impacts[keep$coder_id=='Y_Dou' & (keep$paper_id==92)] <- 'No'
  keep$explain_distant_impacts[
    keep$coder_id=='Y_Dou' &
      (keep$paper_id==92)] <- "because authors were only comparing spots that were affected by tourists and less affected, they didn't mention the origin of the tourists"
  
# Y_Li errors
  keep$explicit_distant_impacts[
    keep$coder_id=='Y_Li' & keep$paper_id==2222] <- 'Yes'
  keep$explain_distant_impacts[
    keep$coder_id=='Y_Li' & keep$paper_id==2222] <- "wood trade"


## ------------------------------------------------------------------------------------------------
# show NA rows
  keep[keep$explicit_distant_impacts==''|
         keep$explicit_distant_impacts=='no such data in Chinese survey',]


## ------------------------------------------------------------------------------------------------
# identify coders to be contacted for edits
  unique(keep$coder_id[keep$explicit_distant_impacts==''|keep$explicit_distant_impacts=='no such data in Chinese survey'])


## ------------------------------------------------------------------------------------------------
# export missing entries
  missing_explicit <- keep[keep$explicit_distant_impacts==''|keep$explicit_distant_impacts=='no such data in Chinese survey',]

# conditional save
  if (nrow(missing_explicit)>=1){
  write.csv(missing_explicit,
            paste0(tab.check.dir,'survey1_cleanup_inferred_columns.csv'),
                   row.names=FALSE)
  }


## ------------------------------------------------------------------------------------------------
# get total numbers but also ensure both values here match
  nrow(survey1)
  length(unique(survey1$paper_id))


## ------------------------------------------------------------------------------------------------
# get summary
  ddply(survey1, .(status), summarize,
        # total count of entries     
        count=length(status))


## ------------------------------------------------------------------------------------------------
# all abstract screened papers (n=7307)
  ab_screen <- ab_screen[!ab_screen$PY==2020,] #remove 2020 studies
  ab_all <- ab_screen[!duplicated(ab_screen$TITLE),]

# remove the papers accepted in screening from the abstract screening list (n=6666)
  ab_reject <- anti_join(ab_all,ab_maybes,by=c("TITLE"))
  ab_reject <- anti_join(ab_reject,ab_yeses,by=c("TITLE"))
  ab_reject <- ab_reject[!duplicated(ab_reject$TITLE,ab_reject$PY),]
    
# all papers that were accepted from abstract screening (n=641)
  ab_accept <- ab_yeses[!duplicated(ab_yeses$TITLE),]
  
# accepted papers from the assignment list for full review (n=595)
  papers_reviewed <- assignments[!duplicated(assignments$TITLE),]

# papers with PDF not available or rejected because of language (n=46)
  paper_unavail <- anti_join(ab_accept,papers_reviewed,by=c("TITLE"))
  paper_unavail <- paper_unavail[!duplicated(paper_unavail$TITLE),]
  
# rejected papers or not yet reviewed (n=512)
  papers_rejected <- anti_join(papers_reviewed,keep,by=c("STUDY_ID.1"="paper_id"))
  papers_rejected <- papers_rejected[!duplicated(papers_rejected$TITLE),]
  
# show numbers:
  paste('original number of papers in WOS search:',nrow(ab_screen))
  paste('number of duplicates in WOS search:',nrow(ab_screen)-nrow(ab_all))
  paste('number of abstracts screened:',nrow(ab_all))
  paste('number of accepted abstracts:',nrow(ab_accept))
  paste('number of rejected abstracts:',nrow(ab_reject))
  paste('number of accepted papers unavailable or non-English:',
        nrow(paper_unavail))
  paste('number of papers available for full review:',
        nrow(papers_reviewed))
  paste('number of full papers accepted for synthesis:',
        nrow(keep)) #REMEMBER YOU NEED THE 5 COMMON PAPERS
  paste('number of full papers rejected from synthesis:',nrow(reject))
  paste('number of full papers not yet reviewed for synthesis:',
        nrow(papers_rejected)-nrow(reject)-5) #THE 5 COMMON PAPERS NOT IN DATASET YET
  
# test if they add up correctly
  paste('COUNT TESTS:')
  paste('total full papers:',
        nrow(papers_reviewed) == nrow(ab_accept) - nrow(paper_unavail))
  paste('total abstract screen:',
        nrow(ab_all) == nrow(ab_reject) + nrow(ab_accept))


## ---- eval=FALSE,echo=FALSE----------------------------------------------------------------------
##  # uniqueIDs <- unique(ab_maybes$STUDY_ID.1)
##  # ab_accept %>% subset(grepl(paste(uniqueIDs,collapse="|"),STUDY_ID))


## ------------------------------------------------------------------------------------------------
# select columns
  # papers found in WOS but don't meet criteria
    reject_abstract_level <- subset(ab_reject,select=c("STUDY_ID","PY"))
  # WOS papers that meet criteria but don't directly look at coupling effects on biodiv
    reject_paper_level <- subset(papers_rejected,select=c("STUDY_ID.1","PY"))
    reject_paper_level2 <- subset(paper_unavail,select=c("STUDY_ID","PY"))
  # papers that do look at coupling effects on biodiv
    accept_paper_level <- subset(keep,select=c("paper_id","year_pub"))
  
# rename columns
  colnames(reject_abstract_level) <- c('paper_id','year_pub')
  colnames(reject_paper_level) <- c('paper_id','year_pub')
  colnames(reject_paper_level2) <- c('paper_id','year_pub')
  
# add columns
  #papers that discuss tc effects on biodiv in...
  reject_abstract_level$type <- 'search but not abstract'
  reject_paper_level$type <- 'abstract but not paper'
  reject_paper_level2$type <- 'abstract but not paper'
  accept_paper_level$type <- 'abstract and paper'
  
# rbind (should be 7307 rows)
  papers <- rbind(reject_abstract_level,reject_paper_level,
                  reject_paper_level2, # <-- NOT SURE IF UNAVAILABLE PAPERS BELONG HERE
                  accept_paper_level)
  
# check
  nrow(papers)


## ---- fig.width=5,fig.height=5-------------------------------------------------------------------
# Get a count of records per year
  paper_ct <- ddply(papers, .(year_pub,type), summarize, count=length(year_pub))

# Change to factors
  paper_ct$year_pub <- as.factor(paper_ct$year_pub)
  paper_ct$type <- factor(paper_ct$type,
                        levels = c('search but not abstract',
                                   'abstract but not paper',
                                   'abstract and paper'))
              
  papers.fig <- ggplot(paper_ct, aes(x=year_pub, y=count)) + 
                  geom_area(position="identity",
                            aes(y=count, fill = type, group = type), alpha=0.6) +
                  geom_point(aes(y =count, color = type, group = type))+
                  geom_line(aes(y =count, color = type, group = type))+
                  geom_text(aes(label=count),size=3,
                            position=position_dodge(.7),
                            vjust=-.7,
                            check_overlap=TRUE)+
                  scale_color_manual(
                  name=expression(italic("telecoupling effects\non biodiversity in...")),
                  values=paper_col)+
                  scale_fill_manual(
                  name=expression(italic("telecoupling effects\non biodiversity in...")),
                  values=paper_col)+
                  xlab("publication year")+
                  ylab("number of articles") + ylim(0,860)+
                  scale_y_continuous(breaks=seq(0,850,100)) +
                  theme_classic()+
                  theme(legend.position = c(0.22,0.87))
  
# save image
  ggsave(filename=paste0(fig.dir,"Figure_2_papers_over_time.png"),
         plot=papers.fig, height = 5, width = 5)

# show here
  papers.fig


## ------------------------------------------------------------------------------------------------
# get total abstract acceptance rate
  sum(ab_all$INCLUDE=='YES')/nrow(ab_all)


## ------------------------------------------------------------------------------------------------
# get total paper acceptance rate
  sum(survey1$status=='accept')/nrow(survey1)


## ------------------------------------------------------------------------------------------------
# get summary
  acr <- ddply(survey1, .(coder_id), summarize,
              # total count of entries     
              count=length(coder_id),
              accept=sum(status=='accept'),
              reject=sum(status=='reject'),
              # percent acceptance
              accept_rate=sum(status=='accept')/length(coder_id))
  acr


## ------------------------------------------------------------------------------------------------
# mean per-person acceptance rate 
  mean(acr$accept_rate)


## ------------------------------------------------------------------------------------------------
# fix columns
  assignments$ABSTRACT <- as.character(assignments$ABSTRACT)
  keep$abstract <- NA
  assignments$TITLE <- as.character(assignments$TITLE)
  keep$title <- NA

# get list of values to replace
  pID <- unique(keep$paper_id)

# add abstracts
  for (i in 1:length(pID)){
      keep$abstract[keep$paper_id==pID[[i]]] <-
      assignments$ABSTRACT[assignments$STUDY_ID.1==pID[[i]]]
  }
  
# add title
  for (i in 1:length(pID)){
      keep$title[keep$paper_id==pID[[i]]] <-
      assignments$TITLE[assignments$STUDY_ID.1==pID[[i]]]
  }
  
# make into table, save and re-read
  ab_table  <- subset(keep, select=c("title","abstract"))
  write.table(ab_table, paste0(tab.dir,"s1_accepted_abstracts.txt"), sep=" ")
  ab_table <- VCorpus(VectorSource(readLines(paste0(tab.dir,
                                                    "s1_accepted_abstracts.txt"))))
  
# transform special characters into a space
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern," ",x))
  ab_table <- tm_map(ab_table, toSpace, "/")
  ab_table  <- tm_map(ab_table, toSpace, "@")
  ab_table  <- tm_map(ab_table, toSpace, "\\|")
  ab_table  <- tm_map(ab_table, toSpace, "\t")
  ab_table  <- tm_map(ab_table, toSpace, ",")
  ab_table  <- tm_map(ab_table, toSpace, ";")
  ab_table  <- tm_map(ab_table, toSpace, "_")

# cleanup
  ab_table <- tm_map(ab_table, content_transformer(tolower))
  ab_table <- tm_map(ab_table, removeWords, stopwords("english"))
  ab_table <- tm_map(ab_table, removeWords,
                     c('however','per','also','may', 'elsevier', 'can'))
  ab_table  <- tm_map(ab_table, toSpace, "-")
  ab_table <- tm_map(ab_table, removeNumbers)
  ab_table <- tm_map(ab_table, removePunctuation)
  ab_table <- tm_map(ab_table, stripWhitespace)
  
# Create term matrix
  term.mtrx <- TermDocumentMatrix(ab_table)
  mtrx <- as.matrix(term.mtrx)
  rows.mtrx <- sort(rowSums(mtrx),decreasing=TRUE)
  abterms.df <- data.frame(word = names(rows.mtrx),freq=rows.mtrx)

# Show frequency of the top 20 terms
  head(abterms.df, 20)


## ----wordcloud, results='hide', message=FALSE, warning=FALSE-------------------------------------
# Word cloud
  png(paste0(fig.dir,"S1_word_cloud.png"),
      height=6,width=6,units='in',res=300)
  par(mar = rep(0, 4))
  
  set.seed(1234)
  wordcloud(words = abterms.df$word, freq = abterms.df$freq, min.freq = 10,
            max.words=100, random.order=FALSE, rot.per=0.0, 
            colors=brewer.pal(8, "Dark2"))
  dev.off()


## ------------------------------------------------------------------------------------------------
# get summary
  ddply(keep, .(C4_study), summarize,
        # total count of entries     
        count=length(C4_study),
        percent=length(C4_study)/nrow(keep))


## ------------------------------------------------------------------------------------------------
# get total papers for which the tc relationships were inferred
  paste('number inferred papers:',sum(keep$explicit_distant_impacts=='No'))
  paste('number explicit papers:',sum(keep$explicit_distant_impacts=='Yes'))
  
# get total inferring rate
  paste('inferred percent:',sum(keep$explicit_distant_impacts=='No')/nrow(keep))
  paste('explicit percent:',sum(keep$explicit_distant_impacts=='Yes')/nrow(keep))


## ------------------------------------------------------------------------------------------------
# get summary
  ddply(keep, .(coder_id), summarize,
        # total count of entries     
        count=length(coder_id),
        inferred=sum(explicit_distant_impacts=='No'),
        explicit=sum(explicit_distant_impacts=='Yes'),
        # percent inferred
        inferred_rate=sum(explicit_distant_impacts=='No')/length(coder_id))


## ------------------------------------------------------------------------------------------------
# join tables
  survey1 <- bind_rows(keep,reject)

# export
  write.csv(survey1,paste0(dat.dir,'survey1_cleaned.csv'),row.names=FALSE)


