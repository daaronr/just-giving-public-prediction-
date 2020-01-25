#Adding attributes to the data

names(uc_fdd_fd["charity"])
attributes_list <- 
set_label(names(uc_fdd_fd["charity"]), label = "The name of the charity")
attributes(uc_fdd_fd$charity)
get_label(uc_fdd_fd$charity)
attr(names(uc_fdd_fd["charity"]), 'comment') <- "Name of the charity"
comment(names(uc_fdd_fd["charity"])) <- "Name of charity"
attributes(names(uc_fdd_fd["charity"]))
comment(uc_fdd_fd["cha"])
list_of_vars <- names(uc_fdd_fd)
