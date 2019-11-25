# Functions used in scopingwork.Rmd; adapting from functions.R in CharitySubstitutionExperiment

#formatting default options for tabyl
tabylstuff <- function(df,cap=""){
  adorn_totals(df,c("row","col")) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns() %>% 
    kable(caption=cap) %>%
    kable_styling(latex_options = "scale_down")
}

#Plotting functions:
dotplot_func <- function(df = ADSX, yvar = donation, xvar = Stage, treatvar = Treatment) {
  yvar <- enquo(yvar)
  xvar <- enquo(xvar)
  treatvar <- enquo(treatvar)
  df %>% 
    ungroup() %>%
    #mutate(xvar = as.factor(!!xvar)) %>%
    dplyr::group_by(!!xvar, !!treatvar) %>%
   # drop_na(!!yvar, !!treatvar) %>%
    dplyr::select(!!yvar, !!treatvar, !!xvar) %>%
    dplyr::summarise(meanyvar = mean(!!yvar, na.rm = TRUE)) %>%
    ggplot(aes(y = meanyvar, x = !!xvar, color = !!treatvar, group = !!treatvar)) +
    geom_point(size=6) +
    geom_line() +
    expand_limits(y = 0)+
    scale_x_continuous(breaks = c(1, 2))+
    scale_y_continuous(yvar)+
    theme(panel.grid.major.y  = element_line(color = "white", size = 0.3))+
    theme(panel.grid.minor.y  = element_line(color = "white", size = 0.1))+
    theme(panel.grid.major.x  = element_blank()) +
    theme(panel.grid.minor.x  = element_blank())+
    theme(axis.ticks.x =element_blank())+ 
    theme(axis.title = element_text(size=14), 
          axis.text = element_text(size=12))+
    theme(legend.text = element_text(size = 12),
    legend.title = element_text(size = 15, face = "bold"))
}

geom_mean <- function() {
  list(
    stat_summary(fun.y = "mean", geom = "point", fill = "red"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4)
  )
}

boxplot_func <- function(df=ADSX,yvar=donation,treatvar=Treatment,comparisons=list(c("No ask-Domestic", "Domestic-Domestic"))) {
  yvar <- enquo(yvar)
  treatvar <- enquo(treatvar)
  ungroup(df) %>%
    group_by( !!treatvar) %>%
    drop_na(!!yvar, !!treatvar) %>%
    dplyr::select(!!yvar, !!treatvar) %>%
    ggplot(aes(!!treatvar, !!yvar) ) +
    geom_boxplot() + 
    theme(axis.title = element_text(size=14), 
          axis.text = element_text(size=14))+
    theme(axis.text.x = element_text(size=12))+
    labs(title = treatvar,  y = yvar, caption = "p-values of Wilcox-(below) and  t-test (above brackets)")  + 
    geom_signif(comparisons = comparisons,
                step_increase = c(.4), vjust = 1.7, margin_top = .5, textsize = 5) + 
   geom_signif(comparisons = comparisons,
              step_increase = c(.4), vjust = 0, margin_top = .5, textsize = 5, test = "t.test") 
}



# Options and formatting code elements

sidebyside <- function(..., width = 60) {
  l <- list(...)
  p <- lapply(l, function(x) {
    xx <- capture.output(print(x, width = width))
    xx <- gsub("\"", "", xx)
    format(xx, justify = "left", width = width)
  })
  p <- do.call(cbind, p)
  sapply(seq_len(nrow(p)), function(x) paste(p[x, ], collapse = ""))
}

huxoptions <- function(df) {
  as_hux(df) %>%
    set_bold(everywhere, 1, TRUE) %>%
    # set_background_color(where(. < 0.1), 'grey') %>%
    set_all_borders(1) %>%
    huxtable::add_colnames() %>%
    set_number_format(3)
}


# Todo: make function to create our preferred types of summary statistics table
##Editing data functions
#FixNA and debug from:  #https://stackoverflow.com/questions/44200195/how-to-debug-contrasts-can-be-applied-only-to-factors-with-2-or-more-levels-er

NA_preproc <- function (dat) {
  for (j in 1:ncol(dat)) {
    x <- dat[[j]]
    if (is.factor(x) && anyNA(x)) dat[[j]] <- base::addNA(x)
    if (is.character(x)) dat[[j]] <- factor(x, exclude = NULL)
  }
  dat
}

# Parallel gather for multiple values
# syntax: parallel_gather(sample_data, key = "param", value = ends_with("mean"), sd = ends_with("sd"))
parallel_gather <- function(x, key, ..., convert = FALSE, factor_key = FALSE) {
  # enquos arguments
  lst <- rlang::quos(...)
  
  # check arguments
  if(length(lst) == 0) stop("Must pass at least one value = columns in parallel_gather()")
  if(is.null(names(lst)) || any(names(lst) == "")) {
    stop("All arguments to parallel_gather() must be named")
    
  }
  
  # use a hack to get column names as character using tidyeval and dplyr
  col_names <- tibble::as_tibble(stats::setNames(as.list(colnames(x)), colnames(x)))
  lst_as_colnames <- lapply(lst, function(name_quo) {
    dplyr::select(col_names, !!name_quo) %>% colnames()
  })
  
  # check length (each argument should refer to the same number of columns)
  arg_col_count <- vapply(lst_as_colnames, length, integer(1))
  if(!length(unique(arg_col_count)) == 1) {
    stop("All named arguments must refer to the same number of columns")
  }
  
  # id variables are those not mentioned in ...
  id_vars <- setdiff(colnames(x), unlist(lst_as_colnames))
  
  # do gather for each item in ..., using id_vars and cols mentioned in 
  # each argument
  gathered <- lapply(seq_along(lst_as_colnames), function(i) {
    tidyr::gather_(x[c(id_vars, lst_as_colnames[[i]])], 
                   key = key, value = names(lst_as_colnames)[i],
                   gather_cols = lst_as_colnames[[i]],
                   na.rm = FALSE, convert = convert, factor_key = factor_key)
  })
  
  # get id data
  id_data <- gathered[[1]][c(id_vars, key)]
  
  # select non-id vars for each melt operation
  gathered <- lapply(gathered, function(df) df[setdiff(colnames(df), c(id_vars, key))])
  
  # return cbind operation
  dplyr::bind_cols(id_data, gathered)
}

# TABLE HELPER FUNCTIONS

treat_recode <- function(df){
  mutate(TreatRow = fct_recode(as.factor(TreatRow), "Do-Do" = "1", "Do-Int" = "2", "Int-Do" = "3", "Int-Int" = "4", "No-Do" = "5" )) 
}

TreatCombinations <- function(df = ADSX, unique_treatments  = 2,combos=2){
  1:unique_treatments%>%
    combn(combos) %>%
    as.data.frame() %>%
    as.list()
}

unite_spread <- function(df){
  unite(df,Results, estimate, p.value, sep = " ") %>%
    spread(TreatCol, Results) 
}

#################
# Test functions (less important here)
#################

# Generic test function: ?a helper function
doTest <- function(pair, df = ADSX, stage = 2, depvar = donation, treatvar = Treatment, testname = "t.test2") {
  require("dplyr")
  depvar <- enquo(depvar)
  treatvar <- enquo(treatvar)
  thetest <- match.fun(testname)
  ADSplit <- df %>%
    ungroup() %>%
    dplyr::filter(!is.na(!!depvar) & Stage == stage) %>%
    split(pull(., !!treatvar)) %>%
    map(~dplyr::select(., !!depvar)) %>%
    extract(pair)
  TR <- thetest(pull(ADSplit[[1]], !!depvar), pull(ADSplit[[2]], !!depvar)) %>%
    broom::tidy() %>%
    mutate(TreatRow = pair[1], TreatCol = pair[2])
}

#doTest_noatr
#adds  `dplyr::filter(is.na(Attrited))` to the above

# Fisher's exact test (skip)
#fisher <- function(A, B) {
#...

#doFisher <- function(pair,df=ADSX,stage=2,depvar=d_donation,treatvar=Treatment){
#...

#fisher.bintest <-

# Lift tests
# t-test
t.test2 <- function(x, y) t.test(x, y)
liftedTT <- purrr::lift(t.test2, .unnamed = TRUE)

# Wilcoxon rank-sum test for continuous outcome variables.
# wilcoxon(subd[subd$Shares=="High", ]$EscThreshold, subd[subd$Shares=="Equal",]$EscThreshold)
# Also "randomization statistical inference"? (Mosaic package or "ri" package?)
wilcox.test2 <- function(x, y) wilcox.test(x, y, exact = FALSE)
liftedWilcox <- purrr::lift(wilcox.test2, .unnamed = TRUE)

#compare column types across two df (e.g., in advance of a merge); from https://stackoverflow.com/questions/45743991/r-compare-column-types-between-two-dataframes
#compareColumns <- function(df1, df2) {
#...

# Adapting function suggested by JohnH on Slack, to merge tables with overlapping columns and reconcile them
#merge_cols <- function(x, y, by) {
#...

# summary tables function(s)
##summary tables: n, mean, sd of depvar by treatvar, kable format 
sumtab2_func <- function(df = ADSX, depvar = donation, treatvar = TreatFirstAsk, treatvar2 = Stage) {
  treatvar <- enquo(treatvar)
  treatvar2 <- enquo(treatvar2)
  depvar <- enquo(depvar)
  df %>%
    ungroup() %>%
    filter(!is.na(!!depvar)) %>%
    group_by(!!treatvar, !!treatvar2) %>%
    dplyr::summarize(
      N = n(),
      Mean = round(mean(!!depvar, na.rm = T), 2),
      "Std.dev." = glue("(", {
        round(sd(!!depvar, na.rm = T), 2)
      }, ")")
    ) %>%
    unite(Results, Mean, "Std.dev.", N, sep = " ") %>%
    spread(!!treatvar2, Results) %>% 
    kable(escape = F) %>%
    kable_styling("striped", full_width = F)
}

#tabsum
tabsum <- function(df = ADSX, yvar = donation, xvar = Stage, treatvar = Treatment) {
  yvar <- enquo(yvar)
  xvar <- enquo(xvar)
  treatvar <- enquo(treatvar)
  df %>% 
    ungroup() %>%
    dplyr::group_by(!!xvar, !!treatvar) %>%
    dplyr::select(!!yvar, !!treatvar, !!xvar) %>%
    dplyr::summarise(meanyvar = mean(!!yvar, na.rm = TRUE)) 
}

#WIP function -- doesn't work yet:
tabylme <-  function(df = ADSX, rowvar = TreatFirstAsk, colvar = treat_second_ask, adorn = "row") {
  rowvar <- enquo(rowvar)
  colvar <- enquo(colvar)
tabyl(!!rowvar, !!colvar) %>%
  adorn_percentages(adorn) %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>% 
  kable() %>%
  kable_styling()
}

