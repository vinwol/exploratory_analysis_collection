`%notin%` <- Negate(`%in%`)

# Acknowledgment: Source: R Package PCAmixdata.
split_up_data <- function(data) {
  data <- data.frame(data,check.names=T)
  class.col <- unlist(lapply(data,class))
  col.quant <- which(class.col %in% c("numeric","integer"))
  col.qual <- which(class.col %in% c("factor","character"))
  if ("integer" %in% class.col) {
    warning("Columns of class integer are considered as quantitative")
  }
  continuous_data <- NULL
  categorical_data <- NULL
  if (length(col.quant)!=0) {
    continuous_data <- data[,col.quant,drop=FALSE]
  }
  if (length(col.qual)!=0) {
    categorical_data <- data[,col.qual,drop=FALSE]
  }
  return(list(continuous_data=continuous_data,
              categorical_data=categorical_data,
              col_quant=col.quant,
              col_qual=col.qual))
}

adjust_dimension_names <- function(df) {
  new_col_names <- stringr::str_replace(tolower(colnames(df)), 'dim', 'PC')
  new_col_names <- stringr::str_replace_all(new_col_names, " ", "")
  colnames(df) <- new_col_names
  return(df)
}

adjust_ic_names <- function(df) {
  new_col_names <- stringr::str_replace(colnames(df), 'V', 'IC')
  colnames(df) <- tolower(new_col_names)
  return(df)
}

compare_columnwise_dfs_for_na_modification <- function(df1, df2) {
  col_names_df1 <- colnames(df1)
  col_names_df2 <- colnames(df2)
  modified_vars <- c()
  for (col_name in col_names_df1) {
    vals_df1 <- df1[[col_name]]
    if (any(is.na(vals_df1))) {
      if (col_name %in% col_names_df2) {
        vals_df2 <- df2[[col_name]]
        if (!any(is.na(vals_df2))) {
          # We found a column that contained NAs and got modified.
          modified_vars <- append(modified_vars, col_name)
        }
      }
    }
  }
  return(modified_vars)
}

