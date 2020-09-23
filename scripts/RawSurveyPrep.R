## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = FALSE, cache.comments = FALSE,
                      warning = FALSE, message = FALSE, results='hold')


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Source file
  source('./scripts/Reference.R')


## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data directory
  dat.dir


## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
# Final tables
  tab.dir


## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
# Final figures
  fig.dir


## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
tab.check.dir


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load previous workspace (if needed)
  #load("RawSurveyPrep.RData")

# surveys
  survey1 <- read.csv(paste0(dat.dir,'rawData\\',
                '1. Criteria Coding (Responses) - Form Responses 1.csv'))
  survey2 <- read.csv(paste0(dat.dir,'rawData\\',
                '2. Metacoupling Biodiversity Coupling Coding (Responses) - Form Responses 1.csv'))
  survey3 <- read.csv(paste0(dat.dir,'rawData\\',
                '3. Metacoupling impact on biodiversity metric(s) (Responses) - Form Responses 1.csv'))
  
# surveys in Chinese (E_Xing)
  cnS1 <- read.csv(paste0(dat.dir,'rawData\\survey1_cn_processed.csv'))
  cnS2 <- read.csv(paste0(dat.dir,'rawData\\survey2_cn_processed.csv'))
  cnS3 <- read.csv(paste0(dat.dir,'rawData\\survey3_cn_processed.csv'))
  cn_edit <- read.csv(paste0(dat.dir,'rawData\\cn_survey_amendments.csv'))
  
# manual edits
  s1_manual <- read.csv(paste0(dat.dir,'rawData\\survey1_manual_edits.csv'))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# structure
  #str(survey1)

# number entries
  paste('number of entries:',nrow(survey1))
  
# number columns
  paste('number of columns:',ncol(survey1))
  
# column names
  #colnames(survey1)

# number of unique papers surveyed
  paste('number of papers:',length(unique(survey1$Paper.ID.Code)))
  
# number of observers
  paste('number of observers:',length(unique(survey1$Coder.ID)))
  
# list of observers
  paste('observer names:')
  paste(unique(survey1$Coder.ID),collapse="; ")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# select columns
  s1_manual <- subset(s1_manual,
                      select = c('Timestamp','Coder.ID','Paper_ID_correction', 
                                 'Paper.ID.Code', 'First.Author.s.Last.Name'))
# show edits
  s1_manual


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# count
  nrow(s1_manual)

# count unique
  length(unique(s1_manual$Paper.ID.Code))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# subset
  s1_sub <- survey1 %>%
              filter(Paper.ID.Code %in% s1_manual$Paper.ID.Code) %>% 
              filter(Coder.ID %in% s1_manual$Coder.ID) %>% 
              # manually remove some extras
              filter(First.Author.s.Last.Name!='Pour') %>% 
              filter(First.Author.s.Last.Name!='Gallant') %>% 
              filter(Timestamp!='8/31/2020 14:35:40')


# ensure list of s1_sub matches list of s1_manual
  a <- s1_sub %>% arrange(Paper.ID.Code) %>% subset(select=c('Paper.ID.Code'))
  b <- s1_manual %>% arrange(Paper.ID.Code) %>% subset(select=c('Paper.ID.Code'))
  cat('test:\n')
  paste(a==b)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# remove from survey 1
  survey1 <- anti_join(survey1,s1_sub)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# edit paper IDs (NOTE: this edit requires that positions of wrong and right code to be the same!!)
  wrong_code <- s1_manual %>% arrange(Paper.ID.Code) %>% subset(select=c('Paper.ID.Code'))
  right_code <- s1_manual %>% arrange(Paper.ID.Code) %>% subset(select=c('Paper_ID_correction'))
  # convert to vector
    wrong_code <- wrong_code$Paper.ID.Code          
    right_code <- right_code$Paper_ID_correction
  # replace values
    s1_sub$Paper.ID.Code <- mapvalues(s1_sub$Paper.ID.Code,
                                      from=wrong_code,to=right_code)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# join edited surveys to survey 1
  survey1 <- rbind(survey1,s1_sub)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# paper ID manual edits
  survey1$Paper.ID.Code[survey1$Paper.ID.Code==2001] <- 2201
  # survey1$Paper.ID.Code[survey1$Paper.ID.Code==264 & 
  #                         survey1$Coder.ID=='R_Chen'] <- 2800


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop entries completely (invalid papers)
  survey1 <- survey1[!survey1$Paper.ID.Code==6005,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop entries completely (invalid papers)
  survey1 <- survey1[!survey1$Coder.ID=='V_Frans',]
  survey1 <- survey1[!survey1$Coder.ID=='R_Chen',]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop blank columns
  survey1 <-  subset(survey1, select = -c(X, X.1) )


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get colnames
  #colnames(survey1)

# change colnames (order matters!!!!!!!!)
  colnames(survey1) <- c("timestamp",
                         "coder_id",
                         "paper_id",
                         "year_pub",
                         "author",
                         "C1_biodiv",
                         "C2_meta",
                         "C3_quant",
                         "C4_study",
                         "meet_all_4",
                         "sup_check",
                         "comments",
                         "further_discuss",
                         "explicit_distant_impacts",
                         "explain_distant_impacts") 


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check structure
  #str(survey1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# convert timestamp to POSIX format
  survey1$timestamp <- lubridate::mdy_hms(as.character(survey1$timestamp),
                                          truncated=3)

  cnS1$timestamp <- lubridate::ymd_hms(as.character(cnS1$timestamp),
                                          truncated=3)
  #str(survey1$timestamp)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check column name matching. If mismatch is listed, correct before binding
  cat('column mismatch test:\n')
  setdiff(colnames(survey1),colnames(cnS1))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# paper ID manual edits
  cnS1$paper_id[cnS1$paper_id==3879] <- 3897
  cnS1$paper_id[cnS1$paper_id==3881] <- 3888


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# select columns
  cns1_manual <- subset(cn_edit,
                        select = c('paper_id',
                                   'explicit_distant_impacts',
                                   'explain_distant_impacts'))
# show edits
  #cns1_manual

# subset from cn1 survey
  cns1_sub <- cnS1 %>%
                arrange(paper_id) %>%
                  filter(paper_id %in% cns1_manual$paper_id)

# ensure list of s1_sub matches list of s1_manual
  a <- cns1_sub %>% arrange(paper_id) %>% subset(select=c('paper_id'))
  b <- cns1_manual %>% arrange(paper_id) %>% subset(select=c('paper_id'))
  cat('test:\n')
  paste(a==b)
 
# remove from cn survey 1
  cnS1 <- anti_join(cnS1,cns1_sub)
 
# edit entries by simply appending
  cns1_sub$explicit_distant_impacts <- cns1_manual$explicit_distant_impacts
  cns1_sub$explain_distant_impacts <- cns1_manual$explain_distant_impacts
    
# join edited surveys to survey 1
  cnS1 <- rbind(cnS1,cns1_sub)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# combine
  survey1 <- bind_rows(survey1,cnS1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# change dashes to underscores
  survey1$coder_id <- gsub("-", "_", survey1$coder_id)

# change first to cap and last names to cap and lower case
  survey1$coder_id <- str_to_title(gsub("_"," ",survey1$coder_id))
  survey1$coder_id <- gsub(" ","_",survey1$coder_id)
  survey1$coder_id <- gsub("Mg","MG",survey1$coder_id)

# show updated list
  paste('observer names:')
  paste(unique(survey1$coder_id),collapse="; ")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get original length
  a <- length(unique(c(survey1$paper_id,survey1$coder_id)))

# select latest data entries according to paper ID, observer and timestamp
  survey1 <-  survey1 %>% 
                # Within each grouping of col 1 and col 2...
                group_by(paper_id, coder_id) %>% 
                # Sort rows by descending order of timestamp...
                # (extra step helpful in checking what happens here)
                arrange(paper_id, coder_id, desc(timestamp)) %>% 
                # Pick the latest time
                slice(which.max(timestamp)) %>% 
                # ungroup
                ungroup()

# check if length matches
  b <- length(unique(c(survey1$paper_id,survey1$coder_id)))
  paste('test:', a == b)
  
# how many?
  paste('count:', b)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# change paper id field to character
  survey1$paper_id <- as.character(survey1$paper_id)

# list of papers to manually reject
  manual <- c(# based on survey 1 discussion
                1377, 3738,332,4841,    # A_Herzberger papers
                411, 922,               # A_Torres papers
                216, 259, 1253, 5576,   # E_Dean papers
                5032,
                5497, 4528, 4733, 2723, # K_Kapsar papers
                6276, 5589,             # M_Lei papers
                6535, 3502,             # MG_Chung papers
                #4352, 2036, 7010,2800, # R_Chen papers
                92,                     # Y_Dou papers
                1,94,203,770,           # Y_Li papers
                64,225, 619, 718,
                1879, 7011, 2427,       # Y_Zhang papers
              # based on survey 2 discussion
                1717, 1676,              # E_Dean papers
              
                4132                     # Manual reject based on Animal Mig fixes (CLH 9/23/2020)
              )

# make list of ineligible paper ID numbers and remove from survey
  s1_keep <- survey1 %>%
             # meeting all 4 criteria
               subset(meet_all_4!='No') %>%
             # manual rejections (based on meeting)
               subset(!grepl(paste(manual,collapse="$|^"), 
                           paper_id)) %>% 
             # manual rejections (based on meeting)
               subset((paper_id!=1488 & coder_id!='Y_Li')|
                        (coder_id!='Y_Li'& paper_id!=3924))

# subset separately for some other errors
  s1_keep2 <- survey1 %>% 
              subset((coder_id=='Y_Li'& paper_id==813)|
                      (coder_id=='Y_Li'& paper_id==6655)|
                      (coder_id=='Y_Li'& paper_id==2222))
  
# combine
  s1_keep <- rbind(s1_keep,s1_keep2) 
  
# anti-join to make list of rejected papers
  s1_reject <- anti_join(survey1,s1_keep)
  
# show length of accepted and rejected
  paste('number accepted papers:', length(unique(s1_keep$paper_id)));
  paste('number rejected papers:', length(unique(s1_reject$paper_id)))
  
# check if length of objects match survey form
  paste('test:', nrow(s1_keep)+nrow(s1_reject) == nrow(survey1))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# add column
  s1_keep$status <- 'accept'
  s1_reject$status <- 'reject'
  
# change 'meet_all_4' question
  s1_reject$meet_all_4 <- 'No'

# rbind
  survey1 <- rbind(s1_keep,s1_reject)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# list of completed surveys
  s1_complete <- survey1 %>% 
                 arrange(coder_id, paper_id, desc(timestamp)) %>% 
                 subset(select=c(timestamp,coder_id,paper_id))

# summaries per person
  t1 <- ddply(survey1,
            .(coder_id),
            summarize,
            # total count of papers per person     
            s1_papers=length(unique(paper_id)),
            # number rejected papers
            s1_reject=sum(status=='reject'),
            # number accepted papers
            s1_accept=sum(status=='accept'),
            # calc acceptance rate
            acc_rate=round(sum(status=='accept')/length(coder_id), digits=4),
            # last date someone entered data
            s1_last_date=max(timestamp)
            )
  
# save as CSVs
  write.csv(s1_complete,
            paste0(tab.check.dir,'survey1_papers_observers.csv'),
            row.names = FALSE)
  write.csv(t1,
            paste0(tab.check.dir,'survey1_progress.csv'),
            row.names = FALSE)
  
# show summary table
  t1


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# structure
  #str(survey2)

# number entries
  paste('number of entries:',nrow(survey2))
  
# number columns
  paste('number of columns:',ncol(survey2))
  
# column names
  #colnames(survey2)

# number of unique papers surveyed
  paste('number of papers:',length(unique(survey2$Paper.ID.Code)))
  
# number of observers
  paste('number of observers:',length(unique(survey2$Coder.ID)))
  
# list of observers
  paste('observer names:')
  paste(unique(survey2$Coder.ID),collapse="; ")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop entries completely (invalid papers)
  survey2 <- survey2[!survey2$Paper.ID.Code==6005,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# R_Chen paper ID error
  #survey2$Paper.ID.Code[survey2$Paper.ID.Code==109] <- 3924


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop entries completely (invalid papers)
  survey2 <- survey2[!survey2$Coder.ID=='R_Chen',]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop blank columns
  survey2 <-  subset(survey2, select = -c(X,X.1,X.2,X.3,X.4,X.5,X.6))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get colnames
  #colnames(survey2)

# change colnames (order matters!!!!!!!!)
  colnames(survey2) <- c("timestamp",
                         "coder_id",
                         "paper_id",
                         "year_pub",
                         "author",
                         "taxa_list",
                         "habitat",
                         "num_countries",
                         "list_countries",
                         "list_continents",
                         "scale_entire",
                         "num_time_pd",
                         "year_start",
                         "year_end",
                         "temp_res",
                         "temp_res_unk",
                         "meta_var_type",
                         "b4_dur_after",
                         "peri_tele_flows",
                         "tele_cat",
                         "peri_tele_sep",
                         "peri_tele_sep_unk",
                         "data_source_biodiv",
                         "data_source_meta",
                         "data_type_biodiv",
                         "data_type_meta",
                         "scale_biodiv",
                         "comments",
                         "num_biodiv_metrics",
                         "list_biodiv_metrics",
                         "further_discuss",
                         "biodiv_countries")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check structure
  #str(survey2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# convert timestamp to POSIX format
  survey2$timestamp <- lubridate::mdy_hms(as.character(survey2$timestamp),
                                          truncated=3)

  cnS2$timestamp <- lubridate::mdy_hms(as.character(cnS2$timestamp),
                                          truncated=3)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check column name matching. If mismatch is listed, correct before binding
  cat('column mismatch test:\n')
  setdiff(colnames(survey2),colnames(cnS2))
  setdiff(colnames(cnS2),colnames(survey2))
  ncol(survey2)==ncol(cnS2)


## ---- echo=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
## # TEMPORARY EDIT HERE UNTIL FIXES ARE MADE
##   # cnS2$scale_entire <- as.factor(cnS2$scale_entire)
##   # answers <- c('Yes','No')
##   # cnS2$further_discuss <- mapvalues(cnS2$further_discuss,from = 1:2,to=answers)
##   # cnS2$peri_tele_sep <- as.factor(cnS2$peri_tele_sep)
##   # cnS2$meta_var_type <- as.factor(cnS2$meta_var_type)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# combine
  survey2 <- bind_rows(survey2,cnS2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# change dashes to underscores
  survey2$coder_id <- gsub("-", "_", survey2$coder_id)

# change first to cap and last names to cap and lower case
  survey2$coder_id <- str_to_title(gsub("_"," ",survey2$coder_id))
  survey2$coder_id <- gsub(" ","_",survey2$coder_id)
  survey2$coder_id <- gsub("Mg","MG",survey2$coder_id)
  survey2$coder_id <- gsub("Herberger","Herzberger",survey2$coder_id)

# show updated list
  paste('observer names:')
  paste(unique(survey2$coder_id),collapse="; ")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get original length
  a <- length(unique(c(survey2$paper_id,survey2$coder_id)))

# select latest data entries according to paper ID, observer and timestamp
  survey2 <-  survey2 %>% 
                # Within each grouping of col 1 and col 2...
                group_by(paper_id, coder_id) %>% 
                # Sort rows by descending order of timestamp...
                # (extra step helpful in checking what happens here)
                arrange(paper_id, coder_id, desc(timestamp)) %>% 
                # Pick the latest time
                slice(which.max(timestamp)) %>% 
                # ungroup
                ungroup()
  
# check if length matches
  b <- length(unique(c(survey2$paper_id,survey2$coder_id)))
  paste('test:', a == b)
  
# how many?
  paste('count:', b)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# change to numeric
  survey2$paper_id <- as.character(survey2$paper_id)

# subset
  s2_keep1 <- survey2 %>% 
             # manual rejections (from list above)
               subset(!grepl(paste(manual,collapse="$|^"), 
                            paper_id)) %>% 
               subset(((paper_id!=1488 & coder_id!='Y_Li')|
                        (coder_id!='Y_Li'& paper_id!=3924)))

# subset separately for some other errors
  s2_keep2 <- survey2 %>% 
              subset(((coder_id=='Y_Li'& paper_id==813) |
                      (coder_id=='Y_Li'& paper_id==2222) |
                      (coder_id=='Y_Li'& paper_id==6655)))
  
# combine
  survey2 <- rbind(s2_keep1,s2_keep2)  
    
# get updated summary
  paste('number of entries:',nrow(survey2))
  paste('number of papers:',length(unique(survey2$paper_id)))
  paste('number of observers:',length(unique(survey2$coder_id)))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fix wrong paper ID
  #survey2$paper_id[survey2$paper_id==77 & survey2$coder_id=='R_Chen'] <- 6655


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# test match of survey 1 with survey 2
  length(unique(s1_keep$paper_id)) == length(unique(survey2$paper_id))


## ---- fig.height=3,fig.width=3----------------------------------------------------------------------------------------------------------------------------------------------
# make list of paper IDs from survey 1 'keep' for matching
  keep_list <- unique(s1_keep$paper_id)

# make list to compare with survey 2
  s2_papers <- unique(survey2$paper_id)

# compare first and second lists
  first <- keep_list
  second <- s2_papers
  # in both, same as call: intersect(first, second)
  both <- first[first %in% second]
  # only in 'first', same as: setdiff(first, second)
  onlyfirst <- first[!first %in% second]
  # only in 'second', same as: setdiff(second, first)
  onlysecond <- second[!second %in% first]

# venn diagram with count of papers in each
  venn(list(survey1_keep = first, survey2_entries = second))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# show only the second list of paper IDs (survey 2 only)
  onlysecond


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 1, not just the keep list
  survey1[survey1$paper_id %in% onlysecond,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 2 for those not in survey 1 at all
  survey2[survey2$paper_id %in% onlysecond,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 1, not just the keep list
  #survey1[survey1$paper_id %in% onlyfirst,]

# save list as csv (if there are entries)
  s1_missing <- survey2[survey2$paper_id %in% onlysecond,]
  s1_missing <- s1_missing %>%
                   arrange(coder_id, paper_id, desc(paper_id)) %>%
                   subset(select=c(coder_id,paper_id,year_pub,author))
  # conditional save
    if (nrow(s1_missing)>=1){
        write.csv(s1_missing,
                  paste0(tab.check.dir,'survey1_missing.csv'),
                  row.names = FALSE)  
      }
  
# show list
  s1_missing


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# show only the first list of paper IDs (survey 1 only)
  onlyfirst


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 1, not just the keep list
  #survey1[survey1$paper_id %in% onlyfirst,]

# save list as csv (if there are entries)
  s2_missing <- survey1[survey1$paper_id %in% onlyfirst,]
  s2_missing <- s2_missing %>%
                   arrange(coder_id, paper_id, desc(paper_id)) %>%
                   subset(select=c(coder_id,paper_id,year_pub,author,status))
  # conditional save
    if (nrow(s2_missing)>=1){
        write.csv(s2_missing,
                  paste0(tab.check.dir,'survey2_missing.csv'),
                  row.names = FALSE)  
      }
  
# show list
  s2_missing


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# list of completed surveys
  s2_complete <- survey2 %>% 
                 arrange(coder_id, paper_id, desc(timestamp)) %>% 
                 subset(select=c(timestamp,coder_id,paper_id))

# summaries per person
  t2 <- ddply(survey2,
            .(coder_id),
            summarize,
            # total count of papers per person     
            s2_papers=length(unique(paper_id)),
            # last date someone worked
            s2_last_date=max(timestamp)
            )
  
# S1_S2 mismatches per observer
  only2 <- survey2[survey2$paper_id %in% onlysecond,]
  only1 <- survey1[survey1$paper_id %in% onlyfirst,]
  only2 <- ddply(only2,.(coder_id),summarize,
                 num_s2_no_s1=length(unique(paper_id)))
  only1 <- ddply(only1,.(coder_id),summarize,
                 num_s1_no_s2=length(unique(paper_id)))
  
# add to table (conditional on whether there are entries)
  if (nrow(only1)>=1){
    t2 <- left_join(t2,only1,by="coder_id")
    } else {
      t2$num_s1_no_s2 <- NA # no entries, so leave blank
    }
  if (nrow(only2)>=1){
    t2 <- left_join(t2,only2,by="coder_id")
    } else {
      t2$num_s2_no_s1 <- NA # no entries, so leave blank
    }
  
# save as CSVs
  write.csv(s2_complete,
            paste0(tab.check.dir,'survey2_papers_observers.csv'),
            row.names = FALSE)
  write.csv(t2,
            paste0(tab.check.dir,'survey2_progress.csv'),
            row.names = FALSE)
  
# display progress here
  t2


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# structure
  #str(survey3)

# number entries
  paste('number of entries:',nrow(survey3))
  
# number columns
  paste('number of columns:',ncol(survey3))
  
# column names
  #colnames(survey3)

# number of unique papers surveyed
  paste('number of papers:',length(unique(survey3$Study.ID)))
  
# number of observers
  paste('number of observers:',length(unique(survey3$Coder.ID)))
  
# list of observers
  paste('observer names:')
  paste(unique(survey3$Coder.ID),collapse="; ")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# drop entries completely (invalid papers)
  survey3 <- survey3[!survey3$Coder.ID=='R_Chen',]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get colnames
  #colnames(survey3)

# change colnames (order matters!!!!!!!!)
  colnames(survey3) <- c("timestamp",
                         "paper_id",
                         "taxa",
                         "entry_id",
                         "taxon_or_func",
                         "biodiv_cat_1sp",
                         "biodiv_cat_multsp",
                         "biodiv_cat_habitat", 
                         "scale",
                         "biodiv_temp_res",
                         "effect",
                         "significant",
                         "p_value",
                         "notes",
                         "coder_id")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check structure
  #str(survey3)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# convert timestamp to POSIX format
  survey3$timestamp <- lubridate::mdy_hms(as.character(survey3$timestamp),
                                          truncated=3)

  cnS3$timestamp <- lubridate::ymd_hms(as.character(cnS3$timestamp),
                                          truncated=3)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check column name matching. If mismatch is listed, correct before binding
  cat('column mismatch test:\n')
  setdiff(colnames(survey3),colnames(cnS3))
  setdiff(colnames(cnS3),colnames(survey3))
  paste('ncol Survey3:',ncol(survey3))
  paste('ncol cnSurvey3:',ncol(cnS3))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# TEMPORARY EDIT HERE UNTIL FIXES ARE MADE
  # cnS3$unique_id <- as.factor(cnS3$unique_id)
  # cnS3$significant <- as.factor(cnS3$significant)

# fix structures to ensure matching
  cnS3$entry_id <- as.factor(cnS3$entry_id)
  cnS3$p_value <- as.character(cnS3$p_value)
  survey3$p_value <- as.character(survey3$p_value)

# combine
  survey3 <- bind_rows(survey3,cnS3)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# change dashes/periods to underscores
  survey3$coder_id <- gsub("-", "_", survey3$coder_id)
  survey3$coder_id <- gsub("\\.", "_", survey3$coder_id)

# change first to cap and last names to cap and lower case
  survey3$coder_id <- str_to_title(gsub("_"," ",survey3$coder_id))
  survey3$coder_id <- gsub(" ","_",survey3$coder_id)
  survey3$coder_id <- gsub("Mg","MG",survey3$coder_id)

# show updated list
  paste('observer names:')
  paste(unique(survey3$coder_id),collapse="; ")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# test all entries have unique timestamps
  a <- nrow(survey3)
  b <- length(unique(survey3$timestamp))
  paste('test:', a == b)


## ---- eval=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
## ### NOT SURE IF THIS MATTERS. NEED TO CHECK. ####
## 
## # There are errors in this part, where we don't have observer ID numbers or paper/entry ID numbers... This would also be an issue when filtering.
## 
## # # get original length
## #   a <- length(unique(c(survey3$paper_id,
## #                        survey3$coder_id,
## #                        survey3$entry_id,
## #                        survey3$taxa)))
## #
## # # select latest data entries according to paper ID, observer and timestamp
## #   survey3 <-  survey3 %>%
## #                 # Within each grouping of col 1 and col 2...
## #                 group_by(paper_id, coder_id, survey3$entry_id) %>%
## #                 # Sort rows by descending order of timestamp...
## #                 # (extra step helpful in checking what happens here)
## #                 arrange(paper_id, coder_id, entry_id, desc(timestamp)) %>%
## #                 # Pick the latest time
## #                 slice(which.max(timestamp)) %>%
## #                 # ungroup
## #                 ungroup()
## #
## # # check if length matches
## #   b <- length(unique(c(survey3$paper_id,survey3$coder_id)))
## #   paste('test:', a == b)
## #
## # # how many?
## #   paste('count:', a)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# correct ID number of a rejected paper
  # paperID 22, entry ID 22
    survey3$paper_id[survey3$coder_id=='A_Torres' &
              survey3$paper_id==22 & survey3$entry_id==22] <- 411

# subset out manual rejections
  s3_keep1 <- survey3 %>% 
             # manual rejections (from list above)
               subset(!grepl(paste(manual,collapse="$|^"), 
                           paper_id)) %>% 
               subset(((paper_id!=1488 & coder_id!='Y_Li')|
                      (coder_id!='Y_Li'& paper_id!=3924)))

# subset separately for some other errors
  s3_keep2 <- survey3 %>% 
              subset(((coder_id=='Y_Li'& paper_id==813) |
                      (coder_id=='Y_Li'& paper_id==6655) |
                      (coder_id=='Y_Li'& paper_id==2222) |
                      (coder_id=='X_Wu'& paper_id==1299)))
  
# combine
  survey3 <- rbind(s3_keep1,s3_keep2) 

# get updated summary
  paste('number of entries:', nrow(survey3))
  paste('number of papers:', length(unique(survey3$paper_id)))
  paste('number of observers:', length(unique(survey3$coder_id)))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# A_Torres errors
  # paper ID 1488, entry ID 4
    survey3$paper_id[survey3$coder_id=='A_Torres' &
              survey3$paper_id==4 & survey3$entry_id==1488] <- 1488
    survey3$entry_id[survey3$coder_id=='A_Torres' &
              survey3$paper_id==1488 & survey3$entry_id==1488] <- 4
  # paperID 3924, entry ID 5
    survey3$entry_id[survey3$coder_id=='A_Torres' &
              survey3$paper_id==5 & survey3$entry_id==''] <- 5
    survey3$paper_id[survey3$coder_id=='A_Torres' &
              survey3$paper_id==5 & survey3$entry_id==5] <- 3924
  # paperID 3924, entry ID 6
    survey3$entry_id[survey3$coder_id=='A_Torres' &
              survey3$paper_id==3924 & survey3$entry_id==''] <- 6
    
# E_Dean errors
  # paperID 22, entry ID 22
    survey3$paper_id[survey3$coder_id=='E_Dean' &
              survey3$paper_id==4 & survey3$entry_id==4] <- 2998
    survey3$paper_id[survey3$coder_id=='E_Dean' &
              survey3$paper_id==5 & survey3$entry_id==5] <- 5844
    survey3$entry_id[survey3$coder_id=='' &
                    survey3$entry_id=='Tayra (Eira barbara)'] <- 1
# C_Hovis errors
  # paper ID 6655
    survey3$paper_id[survey3$coder_id=='C_Hovis' & survey3$paper_id==6666] <- 6655
    
# MC_Chung errors
  survey3$coder_id[survey3$paper_id==45] <- "MG_Chung"
  survey3$coder_id[survey3$paper_id==654] <- "MG_Chung"

# R_Chen errors
  # paper ID 6655 (from survey 1 correction)
    # survey3$paper_id[survey3$coder_id=='R_Chen' & survey3$paper_id==77] <- 6655
    # survey3$paper_id[survey3$coder_id=='R_Chen' & survey3$paper_id==109] <- 3924

# X_Wu errors
  # fill blank paper ID
    survey3$paper_id[survey3$coder_id=='X_Wu' & survey3$paper_id==3] <- 1299  
    
# rejections from email confirmation
  # survey3 <- survey3[!(survey3$paper_id==0000 & survey3$coder_id=='____'),]



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Y_Zhang
  int <- interval(ymd_hms("2020-03-17 13:08:00"), ymd_hms("2020-03-17 13:10:59"))
  survey3$coder_id[survey3$timestamp %within% int] <- 'Y_Zhang'
  
# K_Kapsar
  int <- interval(ymd_hms("2020-03-17 14:38:00"), ymd_hms("2020-03-17 14:38:59"))
  int2 <- interval(ymd_hms("2020-03-17 11:53:00"), ymd_hms("2020-03-17 11:53:59"))
  int3 <- interval(ymd_hms("2020-03-17 12:31:40"), ymd_hms("2020-03-17 12:32:59"))
  survey3$coder_id[survey3$timestamp %within% int |
                   survey3$timestamp %within% int2 |
                   survey3$timestamp %within% int3] <- 'K_Kapsar'
  
# A_Herzberger
  int <- interval(ymd_hms("2020-03-17 10:58:00"), ymd_hms("2020-03-17 10:59:59"))
  int2 <- interval(ymd_hms("2020-03-17 12:53:30"), ymd_hms("2020-03-17 12:55:59"))
  survey3$coder_id[survey3$timestamp %within% int|
                   survey3$timestamp %within% int2] <- 'A_Herzberger'
  
# E_Dean
  int <- interval(ymd_hms("2020-03-15 13:32:00"), ymd_hms("2020-03-15 13:33:59"))
  int2 <- interval(ymd_hms("2020-03-15 12:33:00"), ymd_hms("2020-03-15 12:33:59"))
  int3 <- interval(ymd_hms("2020-03-15 14:41:00"), ymd_hms("2020-03-15 14:48:59"))
  survey3$coder_id[survey3$timestamp %within% int |
                   survey3$timestamp %within% int2|
                   survey3$timestamp %within% int3] <- 'E_Dean'


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# test match of survey 1 with survey 3
  length(unique(s1_keep$paper_id)) == length(unique(survey3$paper_id))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# test match of survey 2 with survey 3
  length(unique(survey2$paper_id)) == length(unique(survey3$paper_id))


## ---- fig.height=3,fig.width=3----------------------------------------------------------------------------------------------------------------------------------------------
# make list of paper IDs from survey 1 'keep' for matching
  keep_list <- unique(s1_keep$paper_id)

# make list to compare with survey 2
  s3_papers <- unique(survey3$paper_id)

# compare first and third lists
  first <- keep_list
  third <- s3_papers
  # in both, same as call: intersect(first, third)
  both <- first[first %in% third]
  # only in 'first', same as: setdiff(first, third)
  onlyfirst <- first[!first %in% third]
  # only in 'third', same as: setdiff(third, first)
  onlythird <- third[!third %in% first]

# venn diagram with count of papers in each
  venn(list(survey1_keep = first, survey3_entries = third))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# show only the third list of paper IDs (survey 3 only)
  onlythird


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 1, not just the keep list
  a <- survey1[survey1$paper_id %in% onlythird,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 3 for those not in survey 1 at all
  survey3[survey3$paper_id %in% onlythird,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# save list as csv (if there are entries)
  s31_missing <- survey3[survey3$paper_id %in% onlythird,]
  s31_missing <- s31_missing %>%
                   arrange(coder_id, paper_id) %>%
                   subset(select=c(coder_id,paper_id,entry_id))
  # conditional save
    if (nrow(s31_missing)>=1){
        write.csv(s31_missing,
                  paste0(tab.check.dir,'survey3_missing_survey1.csv'),
                  row.names = FALSE)  
    }
  
# show list
  s31_missing


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# show only the first list of paper IDs (survey 1 only)
  onlyfirst


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 1, not just the keep list
  #survey1[survey1$paper_id %in% onlyfirst,]

# save list as csv (if there are entries)
  s3_missing <- survey1[survey1$paper_id %in% onlyfirst,]
  s3_missing <- s3_missing %>%
                   arrange(coder_id, paper_id, desc(paper_id)) %>%
                   subset(select=c(coder_id,paper_id,year_pub,author,status))
  
  # conditional save
    if (nrow(s3_missing)>=1){
        write.csv(s3_missing,
                  paste0(tab.check.dir,'survey3_missing_from_1.csv'),
                  row.names = FALSE)  
    }
  
# show list
  s3_missing


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# S1_s3 mismatches per observer
  only3 <- survey3[survey3$paper_id %in% onlythird,]
  only1 <- survey1[survey1$paper_id %in% onlyfirst,]
  only3 <- ddply(only3,.(coder_id),summarize,
                 num_s3_no_s1=length(unique(paper_id)))
  only1 <- ddply(only1,.(coder_id),summarize,
                 num_s1_no_s3=length(unique(paper_id)))


## ---- fig.height=3,fig.width=3----------------------------------------------------------------------------------------------------------------------------------------------
# make list of paper IDs from survey 1 'keep' for matching
  keep_list <- unique(survey2$paper_id)

# make list to compare with survey 2
  s3_papers <- unique(survey3$paper_id)

# compare second and third lists
  second <- keep_list
  third <- s3_papers
  # in both, same as call: intersect(second, third)
  both <- second[second %in% third]
  # only in 'second', same as: setdiff(second, third)
  onlysecond <- second[!second %in% third]
  # only in 'third', same as: setdiff(third, second)
  onlythird <- third[!third %in% second]

# venn diagram with count of papers in each
  venn(list(survey2_entries = second, survey3_entries = third))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# show only the third list of paper IDs (survey 3 only)
  onlythird


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 2, not just the keep list
  survey2[survey2$paper_id %in% onlythird,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 3 for those not in survey 2 at all
  survey3[survey3$paper_id %in% onlythird,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# save list as csv (only if there are entries)
  s32_missing <- survey3[survey3$paper_id %in% onlythird,]
  s32_missing <- s32_missing %>%
                   arrange(coder_id, paper_id) %>%
                   subset(select=c(coder_id,paper_id,entry_id))
  # conditional save
    if (nrow(s32_missing)>=1){
    write.csv(s32_missing,
              paste0(tab.check.dir,'survey3_missing_survey2.csv'),
              row.names = FALSE)
    }
  
# show list
  s32_missing


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# show only the second list of paper IDs (survey 2 only)
  onlysecond


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we compare with the entire survey 2, not just the keep list
  survey2[survey2$paper_id %in% onlysecond,]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# save list as csv (if there are entries)
  s3_missing <- survey1[survey1$paper_id %in% onlysecond,]
  s3_missing <- s3_missing %>%
                   arrange(coder_id, paper_id, desc(paper_id)) %>%
                   subset(select=c(coder_id,paper_id,year_pub,author,status))
    # conditional save
    if (nrow(s3_missing)>=1){
        write.csv(s3_missing,
                  paste0(tab.check.dir,'survey3_missing_from_2.csv'),
                  row.names = FALSE)
    }

# show list
  s3_missing


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# show empty entry IDs
  survey3[survey3$entry_id=='',]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get unique entry IDs from list and add value
  pID_list <- survey3[survey3$entry_id=='',]$paper_id
  cID_list <- survey3[survey3$entry_id=='',]$coder_id

  for (i in 1:length(pID_list)){
    # activate to preview
    # print(survey3[survey3$paper_id==pID_list[[i]] &
    #                 survey3$coder_id==cID_list[[i]],])
  }


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# edit entry IDs manually
  survey3$entry_id[survey3$entry_id==''& survey3$coder_id==cID_list[[1]]] <- 1
  survey3$entry_id[survey3$entry_id==''& survey3$coder_id==cID_list[[2]]] <- 2
  survey3$entry_id[survey3$entry_id==''& survey3$coder_id==cID_list[[3]]] <- 6
  survey3$entry_id[survey3$entry_id==''& survey3$coder_id==cID_list[[4]]] <- 4
  survey3$entry_id[survey3$entry_id==''& survey3$coder_id==cID_list[[5]]] <- 2
  survey3$entry_id[survey3$entry_id==''& survey3$coder_id==cID_list[[6]]] <- 1
  survey3$entry[survey3$entry_id==''& survey3$paper_id==pID_list[[8]] & 
                     survey3$coder_id==cID_list[[8]]][1] <- 7
  survey3$entry[survey3$entry_id==''& survey3$paper_id==pID_list[[8]] & 
                     survey3$coder_id==cID_list[[8]]][2] <- 8
  survey3$entry_id[survey3$entry_id==''& survey3$paper_id==pID_list[[9]] & 
                     survey3$coder_id==cID_list[[9]]] <- 1


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# show empty coder IDs
  survey3[survey3$coder_id=='',]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# get paper IDs from list
  pID_list <- unique(survey3$paper_id[survey3$coder_id==''])

# show list
  pID_list


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# list of completed surveys
  s3_complete <- survey3 %>% 
                 arrange(coder_id, paper_id,desc(entry_id)) %>% 
                 subset(select=c(timestamp,coder_id,paper_id,entry_id))

# summaries per person
  t3 <- ddply(survey3,
            .(coder_id),
            summarize,
            # total count of papers per person     
            s3_papers=length(unique(paper_id)),
            # total count of entries per person     
            s3_entries=length(entry_id),
            # last date someone worked
            s3_last_date=max(timestamp)
            )
  
# S2_s3 mismatches per observer
  s3no2 <- survey3[survey3$paper_id %in% onlythird,]
  s3no2 <- ddply(s3no2,.(coder_id),summarize,
                 num_s3_no_s2=length(unique(paper_id)))
  s2no3 <- survey2[survey2$paper_id %in% onlysecond,]
  s2no3 <- ddply(s2no3,.(coder_id),summarize,
                 num_s2_no_s3=length(unique(paper_id)))
   
# add to table (conditional on whether there are entries)
  if (nrow(only1)>=1){
    t3 <- left_join(t3,only1,by="coder_id")
    } else {
      t3$num_s1_no_s3 <- NA # no entries, so leave blank
    }
  if (nrow(only3)>=1){
    t3 <- left_join(t3,only3,by="coder_id")
    } else {
      t3$num_s3_no_s1 <- NA # no entries, so leave blank
    }
  if (nrow(s3no2)>=1){
    t3 <- left_join(t3,s3no2,by="coder_id")
    } else {
      t3$num_s3_no_s2 <- NA # no entries, so leave blank
    }
  if (nrow(s2no3)>=1){
    t3 <- left_join(t3,s2no3,by="coder_id")
    } else {
      t3$num_s2_no_s3 <- NA # no entries, so leave blank
    }
                   
# save as CSVs
  write.csv(s3_complete,
            paste0(tab.check.dir,'survey3_papers_observers.csv'),
            row.names = FALSE)
  write.csv(t3,
            paste0(tab.check.dir,'survey3_progress.csv'),
            row.names = FALSE)
  
# show progress table
  t3


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# exports for manual check of study IDs
  write.csv(survey1,paste0(tab.check.dir,
                           'survey1_for_manual_check.csv'),row.names=FALSE)
  write.csv(survey2,paste0(tab.check.dir,
                           'survey2_for_manual_check.csv'),row.names=FALSE)
  write.csv(survey3,paste0(tab.check.dir,
                           'survey3_for_manual_check.csv'),row.names=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# list of common papers to extract
  commons <- c(6655,2577,3924,3456,1488)

# remove from full survey
  s1_full <- survey1 %>%
               # remove by list
                 subset(!grepl(paste(commons,collapse="|"),paper_id))
  s2_full <- survey2 %>%
               # remove by list
                 subset(!grepl(paste(commons,collapse="|"),paper_id))
  s3_full <- survey3 %>%
               # remove by list
                 subset(!grepl(paste(commons,collapse="|"),paper_id))

# anti-join to make list of rejected papers
  survey1c <- anti_join(survey1,s1_full)
  survey2c <- anti_join(survey2,s2_full)
  survey3c <- anti_join(survey3,s3_full)
  
# show length of accepted and rejected
  # paste('number accepted papers:', length(unique(s1_keep$paper_id)));
  # paste('number rejected papers:', length(unique(s1_reject$paper_id)))
  
# check if length of objects match survey form
  paste('test1:', nrow(survey1c)+nrow(s1_full) == nrow(survey1))
  paste('test2:', nrow(survey2c)+nrow(s2_full) == nrow(survey2))
  paste('test3:', nrow(survey3c)+nrow(s3_full) == nrow(survey3))


## ---- eval=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------
# make subfolder
  dir.create(paste0(dat.dir,'\\common_papers'),recursive=TRUE)

# exports for common paper surveys
  write.csv(survey1c,paste0(dat.dir,'\\common_papers\\',
                            'survey1_common_papers.csv'),row.names=FALSE)
  write.csv(survey2c,paste0(dat.dir,'\\common_papers\\',
                            'survey2_common_papers.csv'),row.names=FALSE)
  write.csv(survey3c,paste0(dat.dir,'\\common_papers\\',
                            'survey3_common_papers.csv'),row.names=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# make subfolder
  dir.create(paste0(dat.dir,'\\survey_cleanup'),recursive=TRUE)

# exports for survey cleanup
  write.csv(s1_full,paste0(dat.dir,'\\survey_cleanup\\',
                          'survey1_for_cleanup.csv'),row.names=FALSE)
  write.csv(s2_full,paste0(dat.dir,'\\survey_cleanup\\',
                          'survey2_for_cleanup.csv'),row.names=FALSE)
  write.csv(s3_full,paste0(dat.dir,'\\survey_cleanup\\',
                          'survey3_for_cleanup.csv'),row.names=FALSE)


## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
## # extract column names
##   s1_coln <- data.frame(column_name=colnames(survey1), question='NA')
##   s2_coln <- data.frame(column_name=colnames(survey2), question='NA')
##   s3_coln <- data.frame(column_name=colnames(survey3), question='NA')
## 
## # make subfolder
##   dir.create(paste0(dat.dir,'\\survey_colnames'),recursive=TRUE)
## 
## # exports for survey cleanup
##   write.csv(s1_coln,paste0(dat.dir,'\\survey_colnames\\',
##                           'survey1_colnames.csv'),row.names=FALSE)
##   write.csv(s2_coln,paste0(dat.dir,'\\survey_colnames\\',
##                           'survey2_colnames.csv'),row.names=FALSE)
##   write.csv(s3_coln,paste0(dat.dir,'\\survey_colnames\\',
##                           'survey3_colnames.csv'),row.names=FALSE)


## ----cache=TRUE,cache.comments=FALSE----------------------------------------------------------------------------------------------------------------------------------------
# save workspace to load later if needed
  save.image("RawSurveyPrep.RData")

