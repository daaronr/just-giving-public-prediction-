# Justgiving sponsorship influence project
# additional code notes in: codenotes.md

##############################
# Setup 

library(here)
#library(checkpoint) #to avoid differential processing from different package versions
library(pacman)

here <- here::here

p_load(dplyr,magrittr,purrr,tidyverse,tidyr,broom,janitor,here,glue,dataMaid,glue,readr, lubridate,summarytools,gtools,knitr,pastecs,data.table, kableExtra)  #citr, reporttools, experiment, estimatr,  kableExtra, ggsignif, glmnet, glmnetUtils, rsample,snakecase,zoo

library(codebook)


source(here("R","baseopt_jg.R")) #basic options, definitions, abbreviations for functions, folder path names

#dir.create(here("input_data","donations"), showWarnings = FALSE)
#dir.create(here("input_data","fundraisers"), showWarnings = FALSE)

################################
# Copy in and put together Justgiving data files pulled through API (pulled in github: fundraising_data_pull, folder-file /data/R/just_giving_data_pull.R)

#file.copy(from=getfund_folder, to=data_folder, recursive = TRUE) 
#file.copy(from=getdon_folder, to=data_folder, recursive = TRUE) 

source(here("R","combine_available_data.R")) #check - many parsing failures


###################
#CODEBOOKS... to understand and communicate the generated datasets

rdr_cbk <- function(cbfile) { #Convenience function to make codebooks with options
rmarkdown::render(here("codebooks", cbfile), output_dir=here("codebooks"), intermediates_dir = here("codebooks"), knit_root_dir=here("codebooks"))
  }

rdr_cbk("codebook_don_all.Rmd") #codebook for donation data (1 per don)
rdr_cbk("codebook_fdd.Rmd") #codebook for donation data (1 per don) with fundraiser info
rdr_cbk("codebook_fdd_fd.Rmd") #codebook for data aggregating donations at fundraiser level  

# Codebooks for 'completed pages with positive donations, UK only'

rdr_cbk("codebook_uc_fdd.Rmd") #codebook for donation data (1 per don) with fundraiser info - UK fundraisers with 1+ donations, plausibly completed (25 weeks rule)

rdr_cbk("codebook_uc_fdd_fd.Rmd") #codebook for data aggregating donations at fundraiser level   - UK fundraisers with 1+ donation, plausibly completed ()25 weeks rule

