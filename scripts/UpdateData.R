# Code to update all data files
rm(list=ls())               

# Raw data 
knitr::purl(input = "./scripts/RawSurveyPrep.Rmd", output = "./scripts/RawSurveyPrep.R" )
source('./scripts/RawSurveyPrep.R')

# Survey 1 data clean
knitr::purl(input = "./scripts/Survey1_cleanup.Rmd", output = "./scripts/Survey1_cleanup.R" )
source('./scripts/Survey1_cleanup.R')

# Survey 2 data clean
knitr::purl(input = "./scripts/Survey2_cleanup.Rmd", output = "./scripts/Survey2_cleanup.R" )
source('./scripts/Survey2_cleanup.R')

# Survey 3 data clean
knitr::purl(input = "./scripts/Survey3_cleanup.Rmd", output = "./scripts/Survey3_cleanup.R" )

source('./scripts/Survey3_cleanup.R')

# Survey 2 & 3 merge
knitr::purl(input = "./scripts/S23_merge.Rmd", output = "./scripts/S23_merge.R" )
source('./scripts/S23_merge.R')

