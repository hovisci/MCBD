## Survey 2 cleanup 
## 2020-06-04 updated on 06-23
## Updated on 2020-09-07, 09-17
## Yuqian Zhang

# This cleanup is based on survey_cleanup_3 > survey2_for_cleanup.csv [new] from Veronica,
# and survey_cleanup > survey2_for_cleanup_completed2.csv [old] from Yuqian

library(dplyr)

getwd()


# Load datasets
# Suvey 2 for cleanup & Survey 2 for 5 common papers
survey2_old <- read.csv("./data/survey_cleanup/survey2_for_cleanup.csv", stringsAsFactors = F)
survey2_5 <-   read.csv("./data/common_papers/survey2_5_common_paper_selection_090120.csv", stringsAsFactors = F)

# str(survey2_old)
# str(survey2_5)

# Combine the two datasets
# Find the common column names
# names(survey2_5)[names(survey2_5)=="biodiv_countries"] <- "biodive_countries"
commoncol <- colnames(survey2_5)
  # commoncol
  # colnames(survey2_old)
survey2_process <- merge(survey2_old, survey2_5, by = commoncol, all=TRUE)

# Change based on Email response from coders
survey2_process$taxa_list[survey2_process$paper_id==292] <- "Fish" 
survey2_process$list_countries[survey2_process$paper_id==6502] <- "Canada, USA"
survey2_process$list_countries[survey2_process$paper_id==6948] <- "Brazil"
survey2_process$biodiv_countries[survey2_process$paper_id==5477] <- "Indonesia"
survey2_process$biodiv_countries[survey2_process$paper_id==5875] <- "Brazil"

write.csv(survey2_process, "./data/survey_cleanup/survey2_for_cleanup_updateBYyuqian.csv")

# Items to update in Xing, Ying's input
  # survey2_process$data_source_meta[survey2_process$paper_id==1040] <- "Fish" 
  # survey2_process$data_source_meta[survey2_process$paper_id==3668] <- "Fish" 





###### Previous code for record only, no function ------------------------------


# 
# 
# # old
# survey2_old <- read.csv("./data/survey_cleanup_v3/survey2_for_cleanup_v3.csv", 
#                         stringsAsFactors = F) ## no factor 
# str(survey2_old) # n=144
# 
# # number of unique entry for old
# paste ("Number of papers coded:", length(unique(survey2_old$paper_id)))
# 
# # new
# survey2_new <- read.csv("./data/survey_cleanup_v4/survey2_for_cleanup.csv",
#                         stringsAsFactors = F)
# # View(survey2_new)
# str(survey2_new) # n=146
# 
# # number of unique entry for new
# paste ("Number of papers coded:", length(unique(survey2_new$paper_id)))
# 
# # dataset modification: drop "num_ountries" col
# survey2_new2 <- subset(survey2_new, select = -c(num_countries))
# # View(survey2_new2)
# #str(survey2_new2)
# 
# 
# # Rows in Old only: 3
# rows4oldonly <- anti_join(survey2_old, survey2_new2,
#                           by = c("paper_id"))
# str(rows4oldonly)
#   #View(rows4oldonly)
# 
# # Rows in New only: 5
# rows4newonly <- anti_join(survey2_new2, survey2_old,
#                           by = c("paper_id"))
# str(rows4newonly)
# # View(rows4newonly)
# 
# # Rows to keep from Old: 141 = 144 - 3 
# rows2keep <- anti_join(survey2_old, rows4oldonly,
#                         by = c("paper_id"))
# # str(rows2keep)
# # View(rows2keep)
# 
# # Rows to keep in total: 146 = 141 + 5
# # Append the rows to keep from Old (69) to the New only (75) to create the Updated list
# 
# survey2_updated <- rbind(rows4newonly, rows2keep)
# 
# # str(survey2_updated)
# # View(survey2_updated)
# 
# write.csv(survey2_updated, "./data/survey_cleanup_v4/survey2_for_cleanup_v4.csv")
# 
