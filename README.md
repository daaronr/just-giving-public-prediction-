# just-giving-public-prediction 
 

# Folders and files

`main.R`: Calls the other files to put the data together and produce the codebooks describing hte data

## codebooks 

Produces html summaries of the relevant data sets, 

See especially

- [codebook_uc_fdd: donations, with page info](codebooks/codebook_u_fdd.html)

- [codebook_uc_fdd_fd: pages, with aggregated donation info](codebooks/codebook_u_fdd.html)


## prediction 

The 'assignment' described with links and tips.
Also, some code you may want to examine that may help your work.

As html (web page) [HERE](prediction/prediction.html)

[prediction.Rmd](prediction/prediction.Rmd) As Rmd file (which produces the above web page)



## input_data

Contains the source data to input; this was downloaded from JustGiving's api 

## R 

Key R scripts for cleaning data, as well as  background functions. 

`combine_available_data.R`: Reads in fundraiser and donation data as pulled into folder `donations_folder` and `fundraising_folder`. Coding, merging, aggregating etc.

`scopingfunctions.R`, `functions.R`: helpful functions to use later perhaps

`baseopt_jg.R`: Base background options
