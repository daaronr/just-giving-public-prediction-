#Adding attributes to the data

names(uc_fdd_fd["charity"])

comment(uc_fdd_fd["cha"])
list_of_vars <- names(uc_fdd_fd)
list_of_vars
attribute_list <- c("name of charity", "target of the fundraiser")

list1 <- (rep("name of charity",22))
attribute_list
attributes_df <- data.frame(t(list1))
colnames(attributes_df) <- list_of_vars

for (i in 1:22) {
  comment(select(uc_fdd_fd, i)) <- attributes_df[i]
}
comment(uc_fdd_fd$charity) <- "ds"
comment_on_vars <- function(uc_fdd_fd, attributes_df) {
  comment(uc_fdd_fd)
}

comment(uc_fdd_fd$charity)
comment(uc_fdd_fd$charity) <- as.character((attributes_df[2]))
comment(uc_fdd_fd$charity)
