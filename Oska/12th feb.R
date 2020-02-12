library(magrittr)
library(sjlabelled)
uc_fdd_fd %>%
  var_labels(
    charity = "The name of the charity which the fundraiser was set up for.",
    fundraising_target = "The target set, as a goal, for the fundraiser to earn.",
    country_code = "An indicator for the country in which the fundraiser was started. For the data selected for this prediction project, the only country is the United Kingdom.",
    total_estimated_gift_aid = "The total amount of funding from gift aid that the fundraiser received.Gift aid is a tax based scheme enabling registered charities to reclaim tax on a donation made by a UK taxpayer, effectively increasing the amount of the donation. https://en.wikipedia.org/wiki/Gift_Aid",
    page_short_name = "The shortened name of the fundraising page.",
    activity_id = "An identifier number for the fundraising activity.",
    event_id = "An identifier number for the fundraising event.",
    activity_type = "The type of fundraising event, for example a Charity Cycle.",
    ) %>%
  get_label()

tx <- readLines("C:/Users/oskas/OneDrive/Desktop/work.txt")
tx2 <- gsub(pattern = "=", replace = "= ", x = tx)
writeLines(tx2, con="C:/Users/oskas/OneDrive/Desktop/work.txt")

uc_fdd_fd <- uc_fdd_fd %>% 
  rename(uc_fdd_fd, colnames(uc_fdd_fd) = (to_snake_case(colnames(uc_fdd_fd))))


                  