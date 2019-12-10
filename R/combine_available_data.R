#library(checkpoint) #to avoid differential processing from different package versions
library(pacman)

p_load(dplyr,magrittr,purrr,tidyverse,tidyr,broom,janitor,here,glue,dataMaid,glue,readr, lubridate,summarytools,gtools,knitr,pastecs,data.table)   #citr, reporttools, experiment, estimatr,  kableExtra, ggsignif, glmnet, glmnetUtils, rsample,snakecase,zoo

here <- here::here 


source(here("R","baseopt_jg.R")) #basic options, definitions, abbreviations for functions
#try(source("baseopt_jg.R"))

#This script...

#Varlists (key lists of variables used below)
  #outcome variables 
  #categories of fundraisers
  #predictors/controls 
  #'eligibility' variables
  #key descriptives of donations
  #date and timing variables 
  #linking variables 
  #unknown variables 
  #useless and unchanging variables

#notes moved to codenotes.md

don_all <- donations_folder %>% #read in and combine 'all donations' from file paths defined in main.R
  list.files %>%
  map(~file.path(donations_folder,.x)) %>%
  map(read_csv) %>%
  reduce(bind_rows) %>%
  group_by(id) %>%
  filter(date_downloaded == max(date_downloaded)) %>%
  select(-ends_with("nil"),-image,-hasImage,-ends_with("Key"),-starts_with("show"),-starts_with("ownerProfileImage")) #removing most redundant variables

#comment(don_all) <- "All donations"

fundr_all <- fundraising_folder %>% #read in and combine 'all fundraisers' from file paths defined in main.R
  list.files(pattern="*.rds") %>%
  map(~file.path(fundraising_folder,.x)) %>%
  map(read_rds) %>% #reads in each rds in the list of files?
  data.table::rbindlist(use.names = TRUE, fill = TRUE, idcol = "origin") %>%
  as_tibble() %>% 
  group_by(pageId) %>%
  filter(date_downloaded == max(date_downloaded)) %>% #Ensure deleted/expired pages are in final dataset. Take  most recent version of that page. The last obs of an expired page won't be in the most recent download.
  select(-ends_with("nil"),-domain,-ends_with("Key"),-starts_with("show"),-starts_with("ownerProfileImage"),-matches("logo|currencySymbol|Highlight|Logo|ShortName.2")) #removing most redundant variables


#Code date variables
fundr_all <- fundr_all %>%
  mutate(CreatedDate=lubridate::ymd_hms(CreatedDate),date_downloaded=ymd_hms(date_downloaded), expiryDate=ymd(expiryDate)) %>%
  mutate(date_created=date(CreatedDate),
         wk_created=week(CreatedDate),
         mo_created=month(CreatedDate),
         wday_created = lubridate::wday(CreatedDate, label = TRUE),
         hr_created = lubridate::hour(CreatedDate),
         EventDate=lubridate::ymd_hms(EventDate)) 

# Cleaning and labelling work: (bulk) clean and impute data to allow for modeling/fitting (leave out 'recipes' for now)
source(here("R","clean_data.R"))

comment(fundr_all) <- "All fundraisers"

# Limit to UK-based fundraisers that raised a positive amount online

u_fundr_all <- fundr_all %>%
  dplyr::filter(totalRaisedOnline>0,   CountryCode=="United Kingdom")  


#Joining donation and fundraiser data
fdd <- don_all %>%
  left_join(fundr_all, by = c("pageShortName" = "pageShortName"))%>% 
  filter(!is.na(pageShortName))  

# Create summary variables for fundraiser-donations (fdd) on donations by time-on-page etc
suppressWarnings(
  fdd <- fdd %>%
    group_by(pageShortName) %>% arrange(pageShortName,donationDate) %>%
    mutate(donorLocalAmount=if_else(is.na(donorLocalAmount),0,donorLocalAmount),
           sum_donorLocalAmount = sum(donorLocalAmount), #summing and cumulating donations
           cumsum = cumsum(donorLocalAmount),
           cumshare = cumsum/sum_donorLocalAmount,
           unit=1, donnum = cumsum(unit), n_don = n(), cumshare_n_don = donnum/n_don, #sum & cum donation counts 
           dur_cdate = as.numeric(as.duration(interval(CreatedDate,donationDate)),"days"),
           dur_edate = as.numeric(as.duration(interval(EventDate,donationDate)),"days"),
           dur_cd_95 = min(dur_cdate[cumshare > 0.95]), #'duration' variables from earliest date with 95\% of donations
           dur_ed_95 = min(dur_edate[cumshare > 0.95]),
           don1_date = min(donationDate), #date/time of first don
           dur_dd1 = as.numeric(as.duration(interval(don1_date,donationDate)),"days"),
           dur_dd1_11am = as.numeric(as.duration(interval(floor_date(don1_date,unit="day")+hours(11),donationDate)),"days"),
           dur_dd1_10pm = as.numeric(as.duration(interval(floor_date(don1_date,unit="day")+hours(22),donationDate)),"days"),
           ###The below should be 'functionized' somehow, to apply to any
           n_don_11am_d1 = max(donnum[date(donationDate)==date(don1_date) & hour(donationDate)<11]), #num don's by 11am UK on same day as don1
           n_don_11am_d2 = max(donnum[dur_dd1_11am<1]), #... on next day
           n_don_11am = case_when(
             hour(don1_date) < 11  ~ n_don_11am_d1, 
             hour(don1_date) >=11 ~ n_don_11am_d2
           ), #assign to *subsequent* 11am only
           n_don_10pm_d1 = max(donnum[date(donationDate)==date(don1_date) & hour(donationDate)<22]), #as above but for 10pm
           n_don_10pm_d2 = max(donnum[dur_dd1_10pm<1]), #... on next day
           n_don_10pm = case_when(hour(don1_date)<=22 ~ n_don_10pm_d1, hour(don1_date)>22~ n_don_10pm_d2), #assign to *subsequent* 10pm only
           n_don_check = case_when( #donations at time of checking if check 2x per day
             hour(don1_date) < 11  ~ n_don_11am_d1, 
             hour(don1_date) >=11 & hour(don1_date) <22 ~ n_don_10pm_d1,
             hour(don1_date) >= 22 ~ n_don_11am_d2
           ), #assign to *subsequent* check time  
           ###same for donation amounts
           cumsum_11am_d1 = max(cumsum[date(donationDate)==date(don1_date) & hour(donationDate)<11]), 
           cumsum_11am_d2 = max(cumsum[dur_dd1_11am<1]), 
           cumsum_11am = case_when( hour(don1_date) < 11  ~ cumsum_11am_d1,  hour(don1_date) >=11 ~ cumsum_11am_d2), #assign to *subsequent* 11am only
           cumsum_10pm_d1 = max(cumsum[date(donationDate)==date(don1_date) & hour(donationDate)<22]), 
           cumsum_10pm_d2 = max(cumsum[dur_dd1_10pm<1]), 
           cumsum_10pm = case_when(hour(don1_date)<=22 ~ cumsum_10pm_d1, hour(don1_date)>22~ cumsum_10pm_d2), #assign to *subsequent* 10pm only
           cumsum_check = case_when(
             hour(don1_date) < 11  ~ cumsum_11am_d1,  hour(don1_date) >=11 & hour(don1_date) <22 ~ cumsum_10pm_d1, hour(don1_date) >= 22 ~ cumsum_11am_d2 ) #assign to *subsequent* check time  
    ) %>%
    ungroup()
)

comment(fdd) <- "All donations, 1 row per donation, with information on fundraisers"

pp("Collapse to 1 row per fundraiser, get key statistics for fundraiser (can merge back to fundr_all)")

fdd_f <- 
  list(.vars = lst("donorLocalAmount", c("dur_cdate", "dur_edate")),
       .funs = lst(funs(count_don=n(), sum_don=sum, med_don=median,mn_don=mean),first))  %>%
  pmap(~ fdd %>% group_by(pageShortName) %>% summarise_at(.x, .y)) %>% 
  reduce(inner_join, by = "pageShortName")

pp("Merge back key variables from Fundr_all")
fdd_fd <- fundr_all %>% 
  select(charity,fundraisingTarget,CountryCode,totalEstimatedGiftAid,pageShortName,activityId,activityType,activityType,eventId,eventName,EventDate,expiryDate,owner,status,CreatedDate,wday_created,hr_created) %>% 
right_join(fdd_f,by = "pageShortName")

rm(fdd_f)
  
comment(fdd_fd) <- "All fundraisers, with aggregated statistics on donations by fundraiser"

### "u_df" = Filtered UK-based pages that raised a positive amount online

u_fundr <- fundr_all %>%
  dplyr::filter(totalRaisedOnline>0, CountryCode=="United Kingdom")  

u_fdd <- fdd %>%  dplyr::filter(totalRaisedOnline>0, CountryCode=="United Kingdom")  

u_fdd_fd <- fdd_fd %>% dplyr::filter(sum_don>0, CountryCode=="United Kingdom") 
#totalRaisedOnline

### "uc_df": filtering the above to include only those that are plausibly completed at the last data collection (which should have been done just before the script is run for the '25 weeks' below to be plausible)

# Todo: either adjust the 'today' variable to represent the date of last API download - or user should redo the API download

uc_fundr <- u_fundr %>% filter(today()>ymd_hms(EventDate)+weeks(25)|today()>expiryDate)
uc_fdd <- u_fdd %>% filter(today()>ymd_hms(EventDate)+weeks(25)|today()>expiryDate)
uc_fdd_fd <- u_fdd_fd %>% filter(today()>ymd_hms(EventDate)+weeks(25)|today()>expiryDate)

