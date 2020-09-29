## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = FALSE, cache.comments = FALSE,
                      warning = FALSE, message = FALSE, results='hold')


## ----------------------------------------------------------------------------------------------------------------------------------------
# Source file
  source('./scripts/Reference.R')


## ---- echo=FALSE-------------------------------------------------------------------------------------------------------------------------
# Data directory
  dat.dir


## ---- echo=FALSE-------------------------------------------------------------------------------------------------------------------------
# Final tables
  tab.dir


## ---- echo=FALSE-------------------------------------------------------------------------------------------------------------------------
# Final figures
  fig.dir


## ----------------------------------------------------------------------------------------------------------------------------------------
# load previous workspace (if needed)
  #load("synthesisResults.RData")

# surveys
  survey2 <- read.csv(paste0(dat.dir,'survey2_cleaned.csv'))
  survey3 <- read.csv(paste0(dat.dir,'survey3_cleaned.csv'))


## ----------------------------------------------------------------------------------------------------------------------------------------
# left join survey 2 to survey 3
survey3$hab <- NA
hab.table <- as.data.frame(cbind(survey2$paper_id, survey2$habitat))
survey3$hab <- hab.table$V2[match(survey3$paper_id,hab.table$V1)]
survey3$tc <-NA
tc.table <- as.data.frame(cbind(survey2$paper_id, survey2$tele_cat))
survey3$tc <- tc.table$V2[match(survey3$paper_id,tc.table$V1)]

s23 <- left_join(survey3,survey2,by=c("paper_id","coder_id"))


## ----------------------------------------------------------------------------------------------------------------------------------------
# get col names
  colnames(s23)


## ----------------------------------------------------------------------------------------------------------------------------------------
unwanted <- c(# survey 3 fields
              'timestamp.x','taxa.x',
              # survey 2 fields
              'coder_id','entry','final_decision','X',
              'taxa.y', 'timestamp.y', 'year_pub', 'author',
              'taxa_list', 'num_time_pd', 'year_start',
              'year_end','temp_res','temp_res_unk',
              'b4_dur_after','comments','further_discuss'
              )

s23$taxa <- s23$taxa.x
s23$sig_effect <- '' #adding column now because there are errors below

s23 <- s23 %>%
        select(-one_of(unwanted))


## ----------------------------------------------------------------------------------------------------------------------------------------
levels(as.factor(s23$tele_cat))


## ----------------------------------------------------------------------------------------------------------------------------------------
# Check how many entries were "not evaluated" n=259
table(s23$significant)

# subset and drop last row (all NAs)
not.eval <- s23[s23$significant == "Not evaluated",]
not.eval <- not.eval[-nrow(not.eval),]
  
# How many paper IDs? n=57
length(unique(not.eval$paper_id))
unique(not.eval$paper_id)

# Look at impacts and metric type
table(not.eval$biodiv_cat_1sp)
table(not.eval$biodiv_cat_multsp)
table(not.eval$biodiv_cat_habitat)
table(not.eval$effect)



## ----------------------------------------------------------------------------------------------------------------------------------------



  for(i in 1:nrow(s23)) {
  
    # positive impact AND True/NotEval Sig == POSITIVE
    if(s23$effect[i] == 'Positive (beneficial)' &&
       (s23$significant[i] == 'TRUE' || s23$significant[i] == 'Not evaluated') )
    {
    s23$sig_effect[i] = 'Beneficial';
    }

    # negative impact AND True/NotEval Sig == NEGATIVE
    else if(s23$effect[i] == 'Negative (detrimental)' &&
       (s23$significant[i] == 'TRUE' || s23$significant[i] == 'Not evaluated') )
    {
    s23$sig_effect[i] = 'Detrimental';
    }

    # changed impact AND True/NotEval Sig == CHANGED
    else if(s23$effect[i] == 'Changed (e.g. species composition)' &&
    (s23$significant[i] == 'TRUE' || s23$significant[i] == 'Not evaluated') )
    {
    s23$sig_effect[i] = 'Changed';
    }

    else
    {
    s23$sig_effect[i] = 'No relation or unclear';
    }

  }

# Check if loop worked

check_sig_effect <- as.data.frame(cbind(s23$effect, s23$significant, s23$sig_effect))

# Look at distribution

table(s23$sig_effect)



## ----------------------------------------------------------------------------------------------------------------------------------------

Mode <- function(x) {
    tabulatedOutcomes <- table(x)
    sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
    mostCommonLabel <- names(sortedOutcomes)[1]
    mostCommonLabel
}

table(s23$paper_id,s23$sig_effect)

maj_effect <- s23 %>%
    dplyr::select(paper_id, sig_effect) %>%
    ungroup() %>%
    group_by(paper_id) %>%
    add_tally(name = 'total_sigeff') %>%
    dplyr::ungroup() %>%
    dplyr::group_by(paper_id, total_sigeff) %>%
    dplyr::mutate(maj_effect = Mode(sig_effect))
    arrange(paper_id) 

maj_effect2 <- s23 %>%
    dplyr::select(paper_id, sig_effect) %>%
    ungroup() %>%
    group_by(paper_id) %>%
    add_tally(name = 'total_sigeff') %>%
    dplyr::ungroup() %>%
    dplyr::group_by(paper_id, total_sigeff) %>%
    dplyr::count(sig_effect, name = 'n_sigeff') %>%
    arrange(paper_id) %>%
    dplyr::mutate(pct_sigeff = n_sigeff/total_sigeff*100) %>% 
    ungroup()

s23$maj_eff <- maj_effect$maj_effect

maj_effect2$maj_eff <- NA
maj.table <- as.data.frame(cbind(maj_effect$paper_id, maj_effect$maj_effect))
maj.table<-maj.table[!duplicated(maj.table),]


maj_effect2$maj_eff <- maj.table$V2[match(maj_effect2$paper_id,maj.table$V1)]

write.csv(maj_effect2, paste0(tab.dir, 'maj_effect.csv'))



## ----------------------------------------------------------------------------------------------------------------------------------------
# save as csv
  write.csv(s23, paste0(dat.dir,'s23.csv'), row.names = FALSE)

