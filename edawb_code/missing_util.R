# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# Assuming data is MCAR (missing completely at random) and not MNAR (missing not at random), too much missing data can be a problem too.
# Usually a safe maximum threshold is 5% of the total for large datasets.
# If missing data for a certain feature or sample is more than 5% then you probably should leave that feature or sample out.
# We therefore check for features (columns) and samples (rows) where more than 5% of the data is missing using a simple function

# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# Assuming data is MCAR, too much missing data can be a problem too. Usually a safe maximum threshold is 5% of the total for large datasets. 
# If missing data for a certain feature or sample is more than 5% then you probably should leave that feature or sample out. 

percent_missing <- function(x) { sum(is.na(x))/length(x)*100 }

# Call:
# remove_nas_for_columns(df, "y")
# remove_nas_for_columns(df, c("y", "z"))
remove_nas_for_columns <- function(df, cols) {
  complete_cols <- complete.cases(df[, cols])
  return(df[complete_cols, ])
}

get_nas_from_vector <- function(vec) {
  na_count <- as.integer(sum(is.na(vec)))
  na_pct <- round(na_count / length(vec) * 100,2)
  return(paste0('na count=',na_count,', na pct=',na_pct))
}

get_missing_values_for_columns_from_continuous_data <- function(data) {
  
  d1 <- as.data.frame(t(summarytools::descr(data)))
  d1 <- tibble::rownames_to_column(d1, "variables")
  d2 <- dlookr::diagnose(data)
  
  joint_df <- Reduce(function(x, y) merge(x,
                                          y,
                                          all.x = FALSE,
                                          all.y = FALSE,
                                          by = c("variables")),
                     list(d1,d2))
  
  cols <- c("variable",
            "n_count",
            "n_pct",
            "missing_count",
            "missing_pct",
            "unique_count",
            "unique_rate")
  result_df <- data.frame(matrix(vector(),
                                 nrow(joint_df),
                                 length(cols),
                                 dimnames=list(c(),cols)),
                          stringsAsFactors=F)
  
  result_df$variable <- joint_df$variables
  result_df$n_count <- joint_df$N.Valid
  result_df$n_pct <- joint_df$Pct.Valid
  result_df$missing_count <- joint_df$missing_count
  result_df$missing_pct <- joint_df$missing_percent
  result_df$unique_count <- joint_df$unique_count
  result_df$unique_rate <- joint_df$unique_rate
  
  return(result_df)
}

get_pct_missing_column_wise <- function(data, digits) {
  df <- as.data.frame(apply(data,2,percent_missing))
  colnames(df) <- c("pct_missing")
  df <- tibble::rownames_to_column(df, "var")
  df_sorted <- df %>% dplyr::arrange(desc(pct_missing))
  df_sorted$pct_missing <- round(df_sorted$pct_missing, digits)
  return(df_sorted)
}

get_pct_missing_row_wise <- function(data, digits) {
  df <- as.data.frame(apply(data,1,percent_missing))
  colnames(df) <- c("pct_missing")
  df <- tibble::rownames_to_column(df, "row_index")
  df_sorted <- df %>% dplyr::arrange(desc(pct_missing))
  df_sorted$pct_missing <- round(df_sorted$pct_missing, digits)
  return(df_sorted)
}

get_number_of_nas <- function(df) { return(sum(is.na(df))) }

get_freq_missing_column_wise <- function(data) {
  sum_vec <- apply(data,2,get_number_of_nas)
  df <- data.frame(variable=names(data), freq_missing=sum_vec)
  row.names(df) <- NULL
  df_sorted <- df %>% dplyr::arrange(desc(freq_missing))
  names(df_sorted)[2] <- paste0('freq_missing (#rows:',nrow(data),')')
  return(df_sorted)
}

get_freq_missing_row_wise <- function(data) {
  sum_vec <- apply(data,1,get_number_of_nas)
  row_seq <- seq(from = 1, to = nrow(data), by = 1)
  df <- data.frame(row=row_seq, freq_missing=sum_vec)
  df_sorted <- df %>% dplyr::arrange(desc(freq_missing))
  names(df_sorted)[2] <- paste0('freq_missing (#cols:',ncol(data),')')
  return(df_sorted)
}

# Source:https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
get_pct_missing_plots_for_columns_and_rows <- function(df) {
  
  pct_missing_values <- df %>%
    tidyr::gather(key = "key", value = "val") %>%
    dplyr::mutate(isna = is.na(val)) %>%
    dplyr::group_by(key) %>%
    dplyr::mutate(total = n()) %>%
    dplyr::group_by(key, total, isna) %>%
    dplyr::summarise(num.isna = n()) %>%
    dplyr::mutate(pct = num.isna / total * 100)
  
  levels <- (pct_missing_values %>% 
               dplyr::filter(isna == T) %>% 
               dplyr::arrange(dplyr::desc(pct)))$key
  
  col_pct_plot <- pct_missing_values %>%
    ggplot() +
    geom_bar(aes(x = reorder(key, dplyr::desc(pct)),
                 y = pct, fill=isna),
             stat = 'identity', alpha=0.8) +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(name = "",
                      values = c('steelblue', 'tomato3'),
                      labels = c("Present", "Missing")) +
    coord_flip() +
    labs(title = "Percentage of missing values", x =
           'Variable', y = "% of missing values")
  
  count_missing_values <- df %>%
    dplyr::mutate(id = row_number()) %>%
    tidyr::gather(-id, key = "key", value = "val") %>%
    dplyr::mutate(isna = is.na(val))
  
  row_count_plot <- ggplot(count_missing_values, aes(key, id, fill = isna)) +
    geom_raster(alpha=0.8) +
    scale_fill_manual(name = "",
                      values = c('steelblue', 'tomato3'),
                      labels = c("Present", "Missing")) +
    scale_x_discrete(limits = levels) +
    labs(x = "Variable",
         y = "Row Number", title = "Missing values in rows") +
    coord_flip()
  
  return(list(col_pct_plot=col_pct_plot,row_count_plot=row_count_plot))
}

remove_columns_above_na_threshold <- function(df,threshold) {
  if (dim(df)[1] == 0 || dim(df)[2] == 0) {
    stop(paste0('df has no dimensions: ',dim(df)[1],',',dim(df)[2]))
  }
  total <- nrow(df)
  cols <- names(df)
  skipped <- c()
  for (var in cols) {
    vals <- df[[var]]
    na_pct <- (sum(is.na(vals)) / total) * 100
    if (na_pct >= threshold) {
      skipped <- c(skipped,var)
    }
  }
  if (length(skipped) > 0) {
    df_sub <- df %>% dplyr::select(-tidyselect::all_of(skipped))
    return(list(cleaned_df=df_sub, removed_cols=skipped))
  } else {
    return(list(cleaned_df=df, removed_cols=skipped))
  }  
}

remove_rows_above_na_threshold <- function(df,threshold) {
  if (dim(df)[1] == 0 || dim(df)[2] == 0) {
    stop(paste0('df has no dimensions: ',dim(df)[1],',',dim(df)[2]))
  }
  total <- ncol(df)
  keep <- c()
  skipped <- c()
  for (index in 1:nrow(df)) {
    r <- df[index,]
    na_pct <- (sum(is.na(r)) / total) * 100
    if (na_pct < threshold) {
      keep <- c(keep,index)
    } else {
      skipped <- c(skipped,index)
    }
  }
  if (length(skipped) > 0) {
    df_sub <- df[keep,]
    return(list(cleaned_df=df_sub, removed_rows=skipped))
  } else {
    return(list(cleaned_df=df, removed_rows=skipped))
  }  
}

get_bin_index <- function(val, bins) {
  val <- as.numeric(val)
  for (index in 1:length(bins)) {
    na_bin <- bins[index]
    na_bin_num <- stringr::str_remove_all(na_bin, '\\]|\\[|%|\\(|\\)')
    if (stringr::str_detect(na_bin_num, '-')) {
      na_bin_num <- unlist(str_split(na_bin_num, '-'))
      na_bin_num_lower <- as.numeric(na_bin_num[1])
      na_bin_num_upper <- as.numeric(na_bin_num[2])
    } else {
      na_bin_num_lower <- as.numeric(na_bin_num)
      na_bin_num_upper <- as.numeric(na_bin_num)
    }
    if (val >= na_bin_num_lower && val <= na_bin_num_upper) {
      return(index)
    } 
  } 
  stop('Could not find bin index!')
}

# Result is data frame with 2 columns:
# pct_missing and num_columns
# Rows are as follows:
# 0
# (0-10]
# (10-20]
# (20-30]
# (30-40]
# (40-50]
# (50-60]
# (60-70]
# (70-80]
# (80-90]
# (90-100)
# 100
# type can be either rowwise or columnwise.
get_pct_bins_of_nas <- function(df,pct_missing_bins,type='columnwise') {
  
  total <- 0
  res <- 0
  if (type == 'columnwise') {
    total <- nrow(df)
    res <- apply(df, MARGIN = 2, function(x) sum(is.na(x)))  
  } else if (type == 'rowwise') {
    total <- ncol(df)
    res <- apply(df, MARGIN = 1, function(x) sum(is.na(x)))  
  } else {
    stop(paste0('Could not make use of type ',type))
  }
  
  pct <- (res / total) * 100
  pct_freq_df <- as.data.frame(table(pct), stringsAsFactors=FALSE)
  
  freq_vec <- vector(mode="numeric", length=length(pct_missing_bins))
  
  for (index in 1:nrow(pct_freq_df)) {
    pct_missing <- as.numeric(pct_freq_df[[index,1]])
    freq <- as.numeric(pct_freq_df[[index,2]])
    freq_index <- get_bin_index(pct_missing,pct_missing_bins)
    freq_vec[freq_index] <- (freq_vec[freq_index] + freq)
  }  
  
  na_df <- data.frame(pct_missing=pct_missing_bins,
                      freq=freq_vec,
                      stringsAsFactors = FALSE)
  
  total_num_columns <- ncol(df)
  total_num_rows <- nrow(df)
  
  if (type == 'columnwise') {
    if (sum(na_df[,2]) != total_num_columns) {
      stop(paste0('Numbers do not add up: ',sum(na_df[,2]),',',total_num_columns))
    }
  } else {
    if (sum(na_df[,2]) != total_num_rows) {
      stop(paste0('Numbers do not add up: ',sum(na_df[,2]),',',total_num_rows))
    }
  }  
  
  if (type == 'columnwise') {
    names(na_df)[2] <- paste0('num_columns (#',total_num_columns,')')
  } else {
    names(na_df)[2] <- paste0('num_rows (#',total_num_rows,')')
  }
  
  # Create third column for showing the percentage of the number of columns/rows 
  # which are affected by NAs.
  if (type == 'columnwise') {
    pct <- (na_df[,2] / total_num_columns) * 100
    na_df$pct_columns <- round(pct,2)
  } else {
    pct <- (na_df[,2] / total_num_rows) * 100
    na_df$pct_rows <- round(pct,2)
  }
  return(na_df)
}

get_pct_bins_of_nas_for_rows_and_columns <- function(df,pct_missing_bins) {
  df_cols <- get_pct_bins_of_nas(df,pct_missing_bins,'columnwise')
  df_rows <- get_pct_bins_of_nas(df,pct_missing_bins,'rowwise')
  return(list(df_rows=df_rows,df_cols=df_cols))
}  

get_standard_pct_missing_bins <- function() {
  pct_missing_bins <- c('0%',        # bin 1
                        '(0-10]%',   # bin 2
                        '(10-20]%',  # bin 3
                        '(20-30]%',  # bin 4
                        '(30-40]%',  # bin 5
                        '(40-50]%',  # bin 6
                        '(50-60]%',  # bin 7
                        '(60-70]%',  # bin 8
                        '(70-80]%',  # bin 9
                        '(80-90]%',  # bin 10
                        '(90-100)%', # bin 11
                        '100%')      # bin 12
  return(pct_missing_bins)
}

generate_pct_missing_bins <- function(threshold) {
  if (!is.numeric(threshold)) {
    stop('threshold is not a number!')
  }
  if (threshold < 0 || threshold > 100) {
    stop('threshold not between 0 and 100!')
  }
  pct_missing_bins <- get_standard_pct_missing_bins()
  # Create custom bins depending on threshold.
  new_pct_missing_bins <- c()
  for (index in 1:length(pct_missing_bins)) {
    na_bin <- pct_missing_bins[index]
    na_bin_num <- stringr::str_remove_all(na_bin, '\\]|\\[|%|\\(|\\)')
    if (stringr::str_detect(na_bin_num, '-')) {
      na_bin_num <- unlist(str_split(na_bin_num, '-'))
      na_bin_num_lower <- as.numeric(na_bin_num[1])
      na_bin_num_upper <- as.numeric(na_bin_num[2])
    } else {
      na_bin_num_lower <- as.numeric(na_bin_num)
      na_bin_num_upper <- as.numeric(na_bin_num)
    }
    if (threshold > na_bin_num_lower && threshold < na_bin_num_upper) {
      custom_bin_1 <- paste0('(',na_bin_num_lower,'-',threshold,']')
      custom_bin_2 <- paste0('(',threshold,'-',na_bin_num_upper,']')
      new_pct_missing_bins <- c(new_pct_missing_bins, custom_bin_1, custom_bin_2)
    } else {
      new_pct_missing_bins <- c(new_pct_missing_bins, na_bin)
    }
  }
  return(new_pct_missing_bins)
}  

# Algorithm:
# Call example:
# removed_cols <- c()
# removed_rows <- c()
# removal_step <- c()
# removal_threshold <- c()
# threshold <- 35
# res <- remove_nas_iteratively_using_only_columns(adfc_subset_pct_wide,
#                               generate_pct_missing_bins(threshold),
#                               threshold,
#                               removal_step,
#                               removal_threshold,
#                               removed_cols, 
#                               removed_rows,
#                               FALSE)
# print('Sanity checks:')
# print(paste0('length(res$removal_step): ',length(res$removal_step)))
# print(paste0('length(res$removal_threshold): ',length(res$removal_threshold)))
# print(paste0('length(res$removed_cols)+length(res$removed_rows): ',(length(res$removed_cols)+length(res$removed_rows))))
# Getting all the removed columns by name:
# length(unique(unlist(res$removed_cols)))
# Getting the number of removed columns: dim(df_original)[2] - dim(df_cleaned)[2] 
# Getting the number of removed rows: dim(df_original)[1] - dim(df_cleaned)[1] 
remove_nas_by_using_only_columns <- function(df, threshold_remaining_nas_pct) {
  if ((threshold_remaining_nas_pct < 0) || (threshold_remaining_nas_pct > 100)) {
    stop(paste0('Threshold not between 0 and 100, ',threshold_remaining_nas_pct))
  }
  if (is.null(df)) {
    stop('Dataframe is NULL!')
  }
  if ((dim(df)[1] == 0) || (dim(df)[2] == 0)) {
    stop(paste0('Dataframe has zero dimensions (',dim(df)[1],',',dim(df)[2],')'))
  }
  keep_vars <- c()
  remove_vars <- c()
  total_rows <- nrow(df) 
  for (index in 1:ncol(df)) {
    var_name <- names(df)[index]
    var_vals <- df[,index]
    na_count <- sum(is.na(var_vals))
    na_pct <- (na_count / total_rows) * 100
    if (na_pct >= threshold_remaining_nas_pct) {
      # Remove the column.
      remove_vars <- c(remove_vars, var_name)
    } else {
      # Keep the column.
      keep_vars <- c(keep_vars, var_name)
    }
  } 
  if (length(remove_vars) > 0 && length(keep_vars) > 0) {
    df <- df %>% dplyr::select(tidyselect::all_of(keep_vars))  
    print('Removed the following columns:')
    print(remove_vars)
  } else {
    print('Did not remove any columns.')
  }
  print('dim(df):')
  print(dim(df))
  return(df)
}

remove_nas_by_using_only_rows <- function(df, threshold_remaining_nas_pct) {
  if ((threshold_remaining_nas_pct < 0) || (threshold_remaining_nas_pct > 100)) {
    stop(paste0('Threshold not between 0 and 100, ',threshold_remaining_nas_pct))
  }
  if (is.null(df)) { stop('Dataframe is NULL!') }
  if ((dim(df)[1] == 0) || (dim(df)[2] == 0)) {
    stop(paste0('Dataframe has zero dimensions (',dim(df)[1],',',dim(df)[2],')'))
  }
  keep_rows <- c()
  remove_rows <- c()
  total_cols <- ncol(df) 
  for (index in 1:nrow(df)) {
    row_vals <- df[index,]
    na_count <- sum(is.na(row_vals))
    na_pct <- (na_count / total_cols) * 100
    if (na_pct >= threshold_remaining_nas_pct) {
      # Remove the row.
      remove_rows <- c(remove_rows, index)
    } else {
      # Keep the row.
      keep_rows <- c(keep_rows, index)
    }
  } 
  if (length(remove_rows) > 0 && length(keep_rows) > 0) {
    df <- df %>% dplyr::slice(keep_rows)
    print(paste0('Removed total number of rows: ',length(remove_rows)))
  } else {
    print('Did not remove any rows.')
  }
  print('dim(df):')
  print(dim(df))
  return(df)
}

# Alternative algorithm for recursive removal without %bins but % between 0-100.
run_recursive_na_removal_using_columns_and_rows <- function(df, threshold) {
  print(paste0('dim(df): ',dim(df)))
  print(paste0('threshold: ',threshold))
  flag_na_col <- FALSE
  flag_na_row <- FALSE
  # Get maximum %NA column.
  total_rows <- nrow(df)
  max_na_col <- -25
  max_na_col_var <- ''
  for (index in 1:ncol(df)) {
    var_name <- names(df)[index]
    var_vals <- df[,index]
    na_count <- sum(is.na(var_vals))
    na_pct <- (na_count / total_rows) * 100
    if (na_pct > threshold) {
      flag_na_col <- TRUE
      if (na_pct >= max_na_col) {
        max_na_col <- na_pct
        max_na_col_var <- var_name
      }
    } 
  } # end for loop 
  # Get maximum %NA row.
  total_cols <- ncol(df) 
  max_na_row <- -25
  max_na_row_index <- -35
  for (index in 1:nrow(df)) {
    row_vals <- df[index,]
    na_count <- sum(is.na(row_vals))
    na_pct <- (na_count / total_cols) * 100
    if (na_pct > threshold) {
      flag_na_row <- TRUE
      if (na_pct >= max_na_col) {
        max_na_row <- na_pct
        max_na_row_index <- index
      }
    } 
  } # end for loop
  # Compare max NA column and row.
  if (max_na_col >= max_na_row && max_na_col >= threshold && max_na_col_var != '') {
    # Remove column.
    print(paste0('Removing column ',max_na_col_var))
    print(paste0('max_na_col=',max_na_col))
    print(paste0('max_na_row=',max_na_row))
    df <- df %>% dplyr::select(-tidyselect::all_of(max_na_col_var))  
    print(paste0('dim(df): ',dim(df)))
  } 
  if (max_na_row > max_na_col && max_na_row >= threshold && max_na_row_index >= 0) {
    # Remove row.
    print(paste0('Removing row with index ',max_na_row_index))
    print(paste0('max_na_col=',max_na_col))
    print(paste0('max_na_row=',max_na_row))
    df <- df[-max_na_row_index, ] 
    print(paste0('dim(df): ',dim(df)))
  }
  if (!flag_na_col && !flag_na_row) {
    # It is the base case.
    return(df)
  } else {
    # It is the recursive case.
    run_recursive_na_removal_using_columns_and_rows(df,threshold)
  }
}  

# Algorithm:
# Call example:
# removed_cols <- c()
# removed_rows <- c()
# removal_step <- c()
# removal_threshold <- c()
# threshold <- 35
# res <- remove_nas_recursively_using_columns_and_rows(adfc_subset_pct_wide,
#                               generate_pct_missing_bins(threshold),
#                               threshold,
#                               removal_step,
#                               removal_threshold,
#                               removed_cols, 
#                               removed_rows,
#                               FALSE)
# print('Sanity checks:')
# print(paste0('length(res$removal_step): ',length(res$removal_step)))
# print(paste0('length(res$removal_threshold): ',length(res$removal_threshold)))
# print(paste0('length(res$removed_cols)+length(res$removed_rows): ',(length(res$removed_cols)+length(res$removed_rows))))
# Getting all the removed columns by name:
# length(unique(unlist(res$removed_cols)))
# Getting the number of removed columns: dim(df_original)[2] - dim(df_cleaned)[2] 
# Getting the number of removed rows: dim(df_original)[1] - dim(df_cleaned)[1] 
# TODO: Think about adding an extra column for row IDs to track which rows were removed.
run_recursive_removal_of_nas_using_columns_and_rows_with_bins <- function(df, threshold) {
  removed_cols <- c()
  removed_rows <- c()
  removal_step <- c()
  removal_threshold <- c()
  terminate_recursion <- FALSE
  result <- remove_nas_recursively_using_columns_and_rows(df,
                                                          generate_pct_missing_bins(threshold),
                                                          threshold,
                                                          removal_step,
                                                          removal_threshold,
                                                          removed_cols, 
                                                          removed_rows,
                                                          terminate_recursion)
  return(result)
}



remove_nas_recursively_using_columns_and_rows <- function(df, 
                                                          pct_missing_bins,
                                                          threshold_remaining_nas_pct,
                                                          removal_step,
                                                          removal_threshold,
                                                          removed_cols,
                                                          removed_rows,
                                                          terminate_recursion) {
  
  if ((threshold_remaining_nas_pct < 0) || (threshold_remaining_nas_pct > 100)) {
    stop(paste0('Threshold not between 0 and 100, ',threshold_remaining_nas_pct))
  }
  
  if (is.null(df)) {
    stop('Dataframe is NULL!')
  }
  
  if ((dim(df)[1] == 0) || (dim(df)[2] == 0)) {
    stop(paste0('Dataframe has zero dimensions (',dim(df)[1],',',dim(df)[2],')'))
  }
  
  if (terminate_recursion) {
    print('------It is the end, termination of recursion!!!!!')
    return(list(cleaned_df=df,
                removal_step=removal_step,
                removal_threshold=removal_threshold,
                removed_cols=removed_cols,
                removed_rows=removed_rows))
  }
  
  na_bins <- rev(pct_missing_bins)
  
  # Get NA summary row-wise and column-wise.
  na_df_cols <- get_pct_bins_of_nas(df,pct_missing_bins,'columnwise')
  na_df_rows <- get_pct_bins_of_nas(df,pct_missing_bins,'rowwise')
  
  print(' Start: ')
  print('na_df_cols:')
  print(na_df_cols)
  print('')
  print('na_df_rows:')
  print(na_df_rows)
  print('')
  
  end_bin_reached <- FALSE
  
  for (index in 1:length(na_bins)) {
    print(paste0('index=',index))
    na_bin <- na_bins[index]
    print(paste0('na_bin=',na_bin))
    # Stop condition for recursion.
    na_bin_num <- stringr::str_remove_all(na_bin, '\\]|\\[|%|\\(|\\)')
    if (stringr::str_detect(na_bin_num, '-')) {
      na_bin_num <- unlist(str_split(na_bin_num, '-'))
      na_bin_num_lower <- as.numeric(na_bin_num[1])
      na_bin_num_upper <- as.numeric(na_bin_num[2])
    } else {
      na_bin_num_lower <- as.numeric(na_bin_num)
      na_bin_num_upper <- as.numeric(na_bin_num)
    }
   
    print(paste0('na_bin_num_lower=',na_bin_num_lower))
    print(paste0('na_bin_num_upper=',na_bin_num_upper))
    print(paste0('threshold_remaining_nas_pct=',threshold_remaining_nas_pct))
    
    if (threshold_remaining_nas_pct <= na_bin_num_upper && 
        threshold_remaining_nas_pct > na_bin_num_lower) {
      print('----------------- Reached the bin containing the threshold!!!!!')
      na_bin_num_lower <- threshold_remaining_nas_pct
      print(paste0('Now: na_bin_num_lower=',na_bin_num_lower))
      end_bin_reached <- TRUE
    }
    
    na_df_cols_subset <- na_df_cols %>% dplyr::filter(pct_missing == na_bin)
    na_df_rows_subset <- na_df_rows %>% dplyr::filter(pct_missing == na_bin)
    
    print(paste0('na_bin=',na_bin))
    print(paste0('na_bin_num_lower=',na_bin_num_lower))
    print(paste0('na_df_cols_subset$pct_columns=',na_df_cols_subset$pct_columns))
    print(paste0('na_df_rows_subset$pct_rows=',na_df_rows_subset$pct_rows))
    
    if (na_df_cols_subset$pct_columns == 0 && 
        na_df_rows_subset$pct_rows == 0 && 
        !end_bin_reached) {
      # No NA removal to do for this NA bin.
      print(paste0('For bin ',na_bin,' no NAs need to be removed since no rows and columns contain this percentage of NAs!'))
      next
    }  
    
    if (end_bin_reached) {
      print('************************* if (end_bin_reached) {...')
      res <- remove_columns_above_na_threshold(df,na_bin_num_lower) 
      df <- res$cleaned_df
      removed_cols <- c(removed_cols, list(list(res$removed_cols)))
      res <- remove_rows_above_na_threshold(df,na_bin_num_lower) 
      df <- res$cleaned_df
      removed_rows <- c(removed_rows, list(list(res$removed_rows)))
      removal_threshold <- c(removal_threshold, na_bin_num_lower, na_bin_num_lower)
      removal_step <- c(removal_step, 'rm_cols', 'rm_rows')
      
      
      if (index == 1) {
        end <- index
      } else {
        end <- index - 1  
      }
      
      # Check if we can terminate, meaning all the bins above the threshold do not contain NAs anymore.
      above_bins <- na_bins[1:end]
      
      print('xxxxxx above_bins:')
      print(above_bins)
      
      na_df_cols_subset <- na_df_cols %>% dplyr::filter(pct_missing %in% above_bins)
      na_df_rows_subset <- na_df_rows %>% dplyr::filter(pct_missing %in% above_bins)
      
      print('xxxxxxxxxxxx')
      print('na_df_cols_subset:')
      print(na_df_cols_subset)
      print('-----sum(na_df_cols_subset$num_columns)')
      print(sum(na_df_cols_subset$num_columns))
      print('')
      print('na_df_rows_subset:')
      print(na_df_rows_subset)
      print('-----sum(na_df_rows_subset$num_rows)')
      print(sum(na_df_rows_subset$num_rows))
      print('xxxxxxxxxxxx')
      
      if (sum(na_df_cols_subset$num_columns) == 0 && 
          sum(na_df_rows_subset$num_rows) == 0) {
        print('Termination reached')
        terminate_recursion <- TRUE
      } else {
        print('Termination not reached')
        terminate_recursion <- FALSE
      }
    } else {
      if (na_df_cols_subset$pct_columns >= na_df_rows_subset$pct_rows) {
        # Remove column-wise.
        print(paste0('************************* removing NA columns above ',na_bin_num_lower,'!!!!!!!!!!'))
        res <- remove_columns_above_na_threshold(df,na_bin_num_lower) 
        df <- res$cleaned_df
        removed_cols <- c(removed_cols, list(list(res$removed_cols)))
        removal_threshold <- c(removal_threshold, na_bin_num_lower)
        removal_step <- c(removal_step, 'rm_cols')
      } else {
        # Remove row-wise.
        print(paste0('************************* removing NA rows above ',na_bin_num_lower,'!!!!!!!!!!'))
        res <- remove_rows_above_na_threshold(df,na_bin_num_lower) 
        df <- res$cleaned_df
        removed_rows <- c(removed_rows, list(list(res$removed_rows)))
        removal_threshold <- c(removal_threshold, na_bin_num_lower)
        removal_step <- c(removal_step, 'rm_rows')
      }
    }  
    # Recursion:
    res <- remove_nas_recursively_using_columns_and_rows(df,
                                                         pct_missing_bins,
                                                         threshold_remaining_nas_pct,
                                                         removal_step,
                                                         removal_threshold,
                                                         removed_cols,
                                                         removed_rows,
                                                         terminate_recursion)

    df <- res$cleaned_df
    removal_step <- c(removal_step, res$removal_step)
    removal_threshold <- c(removal_threshold, res$removal_threshold)
    removed_cols <- c(removed_cols, res$removed_cols)
    removed_rows <- c(removed_rows, res$removed_rows)
    return(list(cleaned_df=df,
                removal_step=removal_step,
                removal_threshold=removal_threshold,
                removed_cols=removed_cols,
                removed_rows=removed_rows))
  }
}

remove_all_columns_with_only_nas <- function(df) {
  keep_vars <- c()
  remove_vars <- c()
  for (index in 1:ncol(df)) {
    var_name <- names(df)[index]
    var_vals <- df[,index]
    if (sum(is.na(var_vals)) == nrow(df)) {
      remove_vars <- c(remove_vars, var_name)
    } else {
      keep_vars <- c(keep_vars, var_name)
    }
  }
  if (length(remove_vars) > 0) {
    df <- df %>% dplyr::select(tidyselect::all_of(keep_vars))  
    print('Removed the following variables:')
    print(remove_vars)
  } 
  return(df)
}

remove_all_rows_with_only_nas <- function(df) {
  keep_indices <- c()
  remove_indices <- c()
  for (index in 1:nrow(df)) {
    row_vals <- df[index,]
    if (sum(is.na(row_vals)) == ncol(df)) {
      remove_indices <- c(remove_indices, index)
    } else {
      keep_indices <- c(keep_indices, index)
    }
  }
  if (length(remove_indices) > 0) {
    df <- df %>% dplyr::slice(keep_indices)
    print(paste0('Removed total number of rows: ',length(remove_indices)))
  } 
  return(df)
}



check_range_0_to_25 <- function(x) {
  if (x >= 0 && x <= 25) { return(TRUE) }
  return(FALSE)
}

check_range_25_to_50 <- function(x) {
  if (x > 25 && x <= 50) { return(TRUE) }
  return(FALSE)
}

check_range_50_to_75 <- function(x) {
  if (x > 50 && x <= 75) { return(TRUE) }
  return(FALSE)
}

check_range_75_to_100 <- function(x) {
  if (x > 75 && x <= 100) { return(TRUE) }
  return(FALSE)
}

count_total_number_of_nas <- function(df) {
  # sum(is.na(df))
  print(paste0('total number of elements: ',dim(df)[1] * dim(df)[2]))
  result <- table(is.na(df))
  print(paste0('Non NA values: ',result[1]))
  print(paste0('NA values: ',result[2]))
}

# provide summary of:
# total number of rows
# number of rows with NA between 0-25%
# number of rows with NA between 25-50%
# number of rows with NA between 50-75%
# number of rows with NA greater than 75%
get_row_wise_na_summary <- function(df) {
  res_df <- get_percent_nas_row_wise_as_df(df)
  res_list <- get_column_wise_na_ranges(res_df) # Can also be used for row summary.
  print(paste0('Total number of rows: ',nrow(df)))
  print(paste0('Number of rows with 0-25% NAs: ',length(res_list[[1]])))
  print(paste0('Number of rows with 25-50% NAs: ',length(res_list[[2]])))
  print(paste0('Number of rows with 50-75% NAs: ',length(res_list[[3]])))
  print(paste0('Number of rows with 75-100% NAs: ',length(res_list[[4]])))
}

# provide summary of:
# total number of columns
# number of columns with NA between 0-25%
# number of columns with NA between 25-50%
# number of columns with NA between 50-75%
# number of columns with NA greater than 75%
get_column_wise_na_summary <- function(df) {
  res_df <- get_percent_nas_column_wise_as_df(df)
  res_list <- get_column_wise_na_ranges(res_df)
  print(paste0('Total number of columns: ',ncol(df)))
  print(paste0('Number of columns with 0-25% NAs: ',length(res_list[[1]])))
  print(paste0('Number of columns with 25-50% NAs: ',length(res_list[[2]])))
  print(paste0('Number of columns with 50-75% NAs: ',length(res_list[[3]])))
  print(paste0('Number of columns with 75-100% NAs: ',length(res_list[[4]])))
}

get_column_wise_na_ranges <- function(df) {
  r1 <- names(which(apply(df, 2, check_range_0_to_25)))
  r2 <- names(which(apply(df, 2, check_range_25_to_50)))
  r3 <- names(which(apply(df, 2, check_range_50_to_75)))
  r4 <- names(which(apply(df, 2, check_range_75_to_100)))
  return(list(r1,r2,r3,r4))
}

get_percent_nas_column_wise_as_df <- function(df) {
  a <- apply(is.na(df), 2, which)
  if (length(a) == 0) {
    return(NULL)
  }
  a <- a[lengths(a) > 0 & a != ""]
  res <- sapply(a, function(i) { length(i)/nrow(df)*100 })
  return(as.data.frame(t(res)))
}

get_percent_nas_row_wise_as_df <- function(df) {
  a <- apply(is.na(df), 1, which)
  if (length(a) == 0) {
    return(NULL)
  }
  a <- a[lengths(a) > 0 & a != ""]
  res <- sapply(a, function(i) { length(i)/ncol(df)*100 })
  return(as.data.frame(t(res)))
}

# Count NAs over multiple columns
count_nas_per_column <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}

# Counting NAs rowwise.
count_nas_per_row <- function(df) {
  apply(df, MARGIN = 1, function(x) sum(is.na(x)))
}

remove_all_nas <- function(df) {
  na.omit(df)
}

remove_rows_with_nas <- function(df) {
  df[complete.cases(df), ]
}

get_number_of_rows_containing_some_na <- function(df) {
  a <- apply(is.na(df), 1, which)
  b <- a[lengths(a) > 0 & a != ""]
  return(length(b))
}

get_number_of_rows_containing_only_na <- function(df) {
  # find non-complete elements
  #ids.to.remove <- sapply(ips.info, function(i) length(i) < 11)
  # remove found elements
  #ips.info <- ips.info[!ids.to.remove]
  # create data.frame
  #df <- do.call(rbind, ips.info)
  num_cols <- ncol(df)
  a <- apply(is.na(df), 1, which)
  if (length(a) == 0) {
    return(0)
  }
  ids.to.keep <- sapply(a, function(i) length(i) == num_cols)
  a <- a[ids.to.keep]
  df <- do.call(rbind, a)
  if (is.null(df)) {
    return(0)
  }
  return(nrow(df))
}

get_number_of_columns_containing_some_na <- function(df) {
  a <- apply(is.na(df), 2, which)
  b <- a[lengths(a) > 0 & a != ""]
  return(length(b))
}

get_number_of_columns_containing_some_na2 <- function(df) {
  length(names(which(colSums(is.na(df)) > 0)))
}

get_number_of_columns_containing_only_na <- function(df) {
  num_rows <- nrow(df)
  a <- apply(is.na(df), 2, which)
  if (length(a) == 0) {
    return(0)
  }
  ids.to.keep <- sapply(a, function(i) length(i) == num_rows)
  a <- a[ids.to.keep]
  df <- do.call(rbind, a)
  if (is.null(df)) {
    return(0)
  }
  return(nrow(df))
}

remove_all_rows_consisting_only_of_na <- function(df) {
  ind <- apply(df, 1, function(x) all(is.na(x)))
  df <- df[ !ind, ]
  return(df)
}

remove_all_rows_consisting_only_of_na2 <- function(df) {
  df1 <- df[rowSums(is.na(df)) != ncol(df), ]
  return(df1)
}

remove_all_columns_consisting_only_of_na <- function(df) {
  ind <- apply(df, 2, function(x) all(is.na(x)))
  df <- df[ ,!ind ]
  return(df)
}

remove_all_columns_consisting_only_of_na2 <- function(df) {
  df1 <- df[,colSums(is.na(df)) != nrow(df)]
  return(df1)
}

find_all_rows_with_only_nas <- function(df) {
  res <- df[rowSums(is.na(df)) == ncol(df),]
  if (nrow(res) == 0) {
    return(NULL)
  }
  return(res)
}

find_all_rows_with_some_nas <- function(df) {
  res <- df[rowSums(is.na(df)) > 0,]
  if (nrow(res) == 0) {
    return(NULL)
  }
  return(res)
}

find_all_columns_with_only_nas <- function(df) {
  # res <- df[,colSums(is.na(df)) == nrow(df)]
  # if (nrow(res) == 0) {
  #   return(NULL)
  # }
  # return(res)
  a <- apply(is.na(df), 2, which)
  if (length(a) == 0) {
    return(NULL)
  }
  a <- a[lengths(a) > 0 & a != ""]
  cols.to.keep <- names(which(sapply(a, function(i) length(i) == nrow(df))))
  if (length(cols.to.keep) == 0) {
    return(NULL)
  }
  df[cols.to.keep] # not df[,cols.to.keep] because we want to keep the column name.
}

find_all_columns_with_some_nas <- function(df) {
  #df[,colSums(is.na(df)) > 0]
  #df[,colSums(is.na(df)) <= nrow(df)]
  a <- apply(is.na(df), 2, which)
  if (length(a) == 0) {
    return(NULL)
  }
  a <- a[lengths(a) > 0 & a != ""]
  cols.to.keep <- names(which(sapply(a, function(i) length(i) <= nrow(df))))
  if (length(cols.to.keep) == 0) {
    return(NULL)
  }
  df[cols.to.keep] # not df[,cols.to.keep] because we want to keep the column name.
  #df %>% dplyr::select(tidyselect::all_of(cols.to.keep))
}
