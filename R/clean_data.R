#Clean up and manipulate
#- Columns in correct formats (numerical, factor etc)
#- Remove boring columns
#- Improve labels
#- Useful constructions/summary variables

library(pacman)
p_load(dplyr,magrittr,purrr,tidyverse,tidyr,broom,janitor,glue,dataMaid,glue,readr, lubridate,summarytools,gtools,knitr,pastecs,sjlabelled) 

#Variables into proper formats
fundr_all <- fundr_all %>%
  ungroup(.) %>% 
mutate_at(.funs = funs(as.numeric(.)), .vars = vars(matches("Raised|Estimated|Amount"))) %>%
mutate_at(.funs = funs(as.factor(.)), .vars = vars(matches("CharityCreated|Type|eventName"))) %>%
mutate_at(.funs = funs(ymd_hms(.)), .vars = vars(matches("Date|date"))) 
 
#todo: Wish I could do this with a T-pipe but I can't get that to work for me!
fundr_all %>% 
  dplyr::select(matches("Raised|Estimated|Amount|CharityCreated|Type|eventName")) %>%
  str()
  
#drop variables that do not vary (by charity) and don't need preserving, or that we've already filtered on
#try(fundr_all <- fundr_all %>%
# select(-charity.description,-charity.profilePageUrl, -currencyCode))
    
#Labeling
  #exploring this variable to understand it
fundr_all %>%  group_by(eventName,activityCharityCreated) %>%  summarise (n = n()) %>% arrange(desc(n))    

fundr_all <- fundr_all %>%
var_labels(
  activityCharityCreated="?? page set up by the charity??"
)

don_all <- don_all %>%
  var_labels(
  source="Source of donation"
      )
  
#dplyr::mutate(ecount=dplyr::count(eventName)) 
#filter(ecount>100) 
#tabyl(eventName, sort=TRUE)

