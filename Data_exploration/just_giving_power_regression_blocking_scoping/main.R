# Justgiving sponsorship influence project
# additional code notes in: codenotes.md

##############################
# Setup 

library(here)
#library(checkpoint) #to avoid differential processing from different package versions
library(pacman)

here <- here::here

p_load(dplyr,magrittr,purrr,tidyverse,tidyr,broom,janitor,here,glue,dataMaid,glue,readr, lubridate,summarytools,gtools,knitr,pastecs,data.table)   #citr, reporttools, experiment, estimatr,  kableExtra, ggsignif, glmnet, glmnetUtils, rsample,snakecase,zoo
library(codebook)

source(here("baseopt_jg.R")) #basic options, definitions, abbreviations for functions, folder path names

dir.create(here("input_data","donations"), showWarnings = FALSE)
dir.create(here("input_data","fundraisers"), showWarnings = FALSE)

################################
# Copy in and put together Justgiving data files pulled through API (pulled in github: fundraising_data_pull, folder-file /data/R/just_giving_data_pull.R)

file.copy(from=getfund_folder, to=data_folder, recursive = TRUE) 
file.copy(from=getdon_folder, to=data_folder, recursive = TRUE) 

source(here("combine_available_data.R")) #check - many parsing failures

#source(paste(rpath,"/clean_data.R",sep="")) #(bulk) clean and impute data to allow for modeling/fitting (leave out 'recipes')

###################
#CODEBOOKS... to understand and communicate the generated datasets

rmarkdown::render(here("codebook_fdd_fd.Rmd")) #codebook for data aggregating donations at fundraiser level  

rmarkdown::render(here("codebook_Don_all.Rmd")) #codebook for donation data (1 per don)

#####################################
# Scoping potential for experiment, including statistical power 

#unlink('analysis/analysis_subst_cache', recursive = TRUE)
rmarkdown::render(here("scopingwork.Rmd"))

###############
# Codebooks for 'completed pages'

rmarkdown::render(here("codebook_ucfdd.Rmd")) #codebook for donation data (1 per don)

#source('R/predictDon.Rmd')
