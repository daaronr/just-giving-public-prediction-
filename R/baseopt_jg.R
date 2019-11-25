# Folder path names

#getdon_folder <- here('..','..','fundraising_data_pull','data','just_giving_data_snapshots','donations')
data_folder <- here('input_data')

#getfund_folder <-"../../fundraising_data_pull/data/just_giving_data_snapshots/fundraisers"
#this one may cause problems later but I don't know otherwise how to refer to files further up in the system

outdir <- here("output_summaries")
donations_folder <- file.path(data_folder, 'donations')
fundraising_folder <- file.path(data_folder, 'fundraisers')

# Base options and sourcing
knitr::opts_chunk$set(echo = TRUE,include=TRUE, warning=FALSE)
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

library(pacman)
#p_load(knitr, dplyr, tidyverse, here, janitor, citr, reporttools, magrittr, glue, experiment, estimatr, broom, kableExtra, purrr, ggsignif, recipes, pwr,lubridate,huxtable,sandwich,randomizr)

#p_load(knitr, dplyr, tidyverse, here, janitor, citr, reporttools, magrittr, glue, experiment, estimatr, broom, kableExtra, purrr, ggsignif, recipes, rsample, snakecase, pwr,lubridate,glmnet,DescTools,statmod,paramtest,cobalt,huxtable,lmtest,sandwich,glmnetcr,randomizr,blockTools,skimr,coefplot)

options(kableExtra.latex.load_packages = FALSE)

options(warning.length = 100)
options(nwarnings = 1) # trying to limit display of  warnings; I don't think  it is working!
options(max.print = 1000)

options(scipen = 1, digits = 2)

# set important functions to correct package
select <- dplyr::select
fill <- tidyr::fill
as_factor <- forcats::as_factor
rename <- dplyr::rename
count <- dplyr::count
filter <- dplyr::filter
group_by <- dplyr::group_by
coalesce <- dplyr::coalesce
here <- here::here

# Lazy man's typing shortcuts and composite functions
pp <- base::print
sel <- dplyr::select
ft <- dplyr::filter
gb <- dplyr::group_by
summarise <- dplyr::summarise
summ <- base::summary

#handy negation

'%ni%' <- Negate('%in%')

# Summary and other options

#setSummaries(numeric= defaultNumericSummaries(remove = "variableType","uniqueValues"))


