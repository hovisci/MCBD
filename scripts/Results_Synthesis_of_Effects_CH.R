# Code for Cleaning Survey 2
# Adapted by CH from Code by VF
# 22 July 2020


source(file="scripts/Reference.R")    # include the reference.r file

#### Load Data ####
survey2 <- read.csv(paste0(dat.dir,'/Clean_July15/survey2_cleaned.csv'))
survey3 <- read.csv(paste0(dat.dir,'/Clean_July15/survey3_cleaned.csv'))


#### Format Data ####

# left join survey 2 to survey 3
s23 <- left_join(survey3,survey2,by=c("paper_id","coder_id"))

# get col names
colnames(s23)

unwanted <- c(# survey 3 fields
  'timestamp.x','notes','taxa.x',
  # survey 2 fields
  'coder_id','entry','final_decision','X',
  'taxa.y', 'timestamp.y', 'year_pub', 'author',
  'taxa_list', 'num_time_pd', 'year_start',
  'year_end','temp_res','temp_res_unk',
  'b4_dur_after','comments','further_discuss'
)

s23$taxa <- s23$taxa.x
s23$sig_effect <- '' # adding column now because there are errors below

s23 <- s23 %>%
  select(-one_of(unwanted))

levels(as.factor(s23$tele_cat))

# IMPORTANT STEP HERE:
  
#  Duplicate rows with multiple flow types. This means that effects and other results will be duplicated as well!!!!!!!!!!!!!!!!!!!!!!!!!
  
# set as character
s23$tele_cat <- as.character(s23$tele_cat)

# show original length of rows
print(paste('Original number of rows:',nrow(s23)))

# split multiple flows listed in a row into other new rows
s23 <- separate_rows(s23, tele_cat, sep=";", convert = TRUE)
s23$tele_cat <- as.factor(s23$tele_cat)

# show new length of rows
print(paste('Number of rows after separating multiple flows per paper:',nrow(s23)))

# show levels
summary(s23$tele_cat)

s23[is.na(s23$tele_cat),]

# CODE FOR A PERCENT IMPACT BY TELECOUPLING CATEGORY (unit of analysis is METRIC - need to do by study if possible)

a <- ddply(s23, .(tele_cat, effect), summarize,
           # total count of entries     
           count=length(tele_cat)) %>%
  # percents of effects within totals per cat
  group_by(tele_cat) %>%
  nest() %>% 
  mutate(perc_per_tele_cat=map(data, function(x) x$count/sum(x$count))) %>% 
  unnest(cols = c(data, perc_per_tele_cat))

# view

a

quick_plot <- ggplot(a, aes(fill=effect, y=perc_per_tele_cat, x=tele_cat)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("")

quick_plot
