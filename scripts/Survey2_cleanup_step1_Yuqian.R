## Survey 2 cleanup 
## 2020-06-04 updated on 06-23
## Yuqian Zhang

# This cleanup is based on survey_cleanup_3 > survey2_for_cleanup.csv [new] from Veronica,
# and survey_cleanup > survey2_for_cleanup_completed2.csv [old] from Yuqian

library(dplyr)


path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
getwd()

# Load datasets
# old
survey2_old <- read.csv("./data/survey_cleanup_v3/survey2_for_cleanup_v3.csv", 
                        stringsAsFactors = F) ## no factor 
str(survey2_old) # n=144

# number of unique entry for old
paste ("Number of papers coded:", length(unique(survey2_old$paper_id)))

# new
survey2_new <- read.csv("./data/survey_cleanup_v4/survey2_for_cleanup.csv",
                        stringsAsFactors = F)
# View(survey2_new)
str(survey2_new) # n=146

# number of unique entry for new
paste ("Number of papers coded:", length(unique(survey2_new$paper_id)))

# dataset modification: drop "num_ountries" col
survey2_new2 <- subset(survey2_new, select = -c(num_countries))
# View(survey2_new2)
#str(survey2_new2)


# Rows in Old only: 3
rows4oldonly <- anti_join(survey2_old, survey2_new2,
                          by = c("paper_id"))
str(rows4oldonly)
  #View(rows4oldonly)

# Rows in New only: 5
rows4newonly <- anti_join(survey2_new2, survey2_old,
                          by = c("paper_id"))
str(rows4newonly)
# View(rows4newonly)

# Rows to keep from Old: 141 = 144 - 3 
rows2keep <- anti_join(survey2_old, rows4oldonly,
                        by = c("paper_id"))
# str(rows2keep)
# View(rows2keep)

# Rows to keep in total: 146 = 141 + 5
# Append the rows to keep from Old (69) to the New only (75) to create the Updated list

survey2_updated <- rbind(rows4newonly, rows2keep)

# str(survey2_updated)
# View(survey2_updated)

write.csv(survey2_updated, "./data/survey_cleanup_v4/survey2_for_cleanup_v4.csv")

