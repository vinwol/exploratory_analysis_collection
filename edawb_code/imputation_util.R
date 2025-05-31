# Functions for column-wise imputation of NA values.

# Links:
# https://sebastiansauer.github.io/sum-isna/
# https://stackoverflow.com/questions/25022511/how-to-subset-a-list-based-on-the-length-of-its-elements-in-r
# https://stackoverflow.com/questions/61469201/pheatmap-won-t-cluster-rows-na-nan-inf-in-foreign-function-call-arg-10
# https://stackoverflow.com/questions/32545256/define-specific-value-colouring-with-pheatmap-in-r
# https://stackoverflow.com/questions/25022511/how-to-subset-a-list-based-on-the-length-of-its-elements-in-r
# http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html
# https://www.r-bloggers.com/2017/08/multiple-imputation-for-continuous-and-categorical-data/

# Replace NAs with zero.
#dependent_vars_ihc_pct_2[is.na(dependent_vars_ihc_pct_2)] <- 0
#dependent_vars_ihc_mm2[is.na(dependent_vars_ihc_mm2)] <- 0

get_mode <- function(x) {
  x <- x[!is.na(x)] # Making sure we are removing any NAs!
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

get_all_modes <- function(x) {
  x <- x[!is.na(x)] # Making sure we are removing any NAs!
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  return(ux[tab == max(tab)])
}

replace_na_by_mean <- function(vec) {
  vec[is.na(vec)] <- base::mean(vec, na.rm = TRUE)
  return(vec)
}

replace_na_by_mode <- function(vec) {
  vec[is.na(vec)] <- get_mode(vec)
  return(vec)
}

replace_na_by_median <- function(vec) {
  vec[is.na(vec)] <- stats::median(vec, na.rm = TRUE)
  return(vec)
}

impute_by_column_mean <- function(df) {
  return(as.data.frame(apply(df,2,replace_na_by_mean)))
}

impute_by_column_median <- function(df) {
  return(as.data.frame(apply(df,2,replace_na_by_median)))
}

impute_by_column_mode <- function(df) {
  return(as.data.frame(apply(df,2,replace_na_by_mode)))
}




