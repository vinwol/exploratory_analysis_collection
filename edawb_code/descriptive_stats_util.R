# Summary statistics for categorical variables.

get_categorical_variable_statistics <- function(data, threshold) {
  out <- split_up_data(data)
  cat_data <- out$categorical_data
  df_list <- list()
  index <- 0
  for (var in names(cat_data)) {
    index <- index + 1
    d <- table(cat_data[[var]]) %>% 
      as.data.frame() %>% 
      arrange(desc(Freq))
    if (nrow(d) > threshold) {
      print(paste0('Skipping variable ',var,', too many levels ',nrow(d)))
      next
    }
    d$pct <- round(d$Freq / sum(d$Freq) * 100, 2)
    d$cat <- var
    df_list[[index]] <- d
  }
  df <- do.call("rbind", df_list)
  result_df <- df[, c(4, 1, 2, 3)]
  colnames(result_df) <- c("var","levels","freq","pct")
  return(result_df)
}

get_variance_per_column <- function(df) {
  col_vars <- colnames(df)
  var_vals <- vector(mode = "list", length = length(col_vars))
  for (index in 1:length(col_vars)) {
    var_vals[[index]] <- var(df[[col_vars[index]]], na.rm=TRUE)
  }
  df <- data.frame(variables=col_vars,
                   variance=unlist(var_vals))
  return(df)
}

get_range_per_column <- function(df) {
  col_vars <- colnames(df)
  range_vals <- vector(mode = "list", length = length(col_vars))
  for (index in 1:length(col_vars)) {
    var <- col_vars[index]
    min <- min(df[[var]], na.rm=TRUE)
    max <- max(df[[var]], na.rm=TRUE)
    range <- abs(max - min)
    range_vals[[index]] <- range
  }
  df <- data.frame(variables=col_vars,
                   range=unlist(range_vals))
  return(df)
}

# Source: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
# v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
# getmode(v) => 2
# charv <- c("o","it","the","it","it")
# getmode(charv) => "it"
get_mode <- function(v) {
  # Alternative: as.numeric(names(which.max(table(v))))
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

get_mode_for_variables <- function(data) {
  apply(data, 2, get_mode)
}

# Source: https://stats.stackexchange.com/questions/221332/variance-of-a-distribution-of-multi-level-categorical-data
# range: 0 to infinity
# Shannon entropy.
get_entropy <- function(x) {
  px  <- table(x)/length(x)
  lpx <- log(px, base=2)
  ent <- -sum(px*lpx)
  return(ent)
}

get_entropy_for_variables <- function(data) {
  apply(data, 2, get_entropy)
}

# https://rdrr.io/github/raredd/ragree/man/unalike.html
# Currently, two methods for calculating the coefficient are implemented. 
# If method = 1, then the formula described above is used. If method = 2, then the formula described in Perry (2005).
# The default is method 1.
get_uac_for_variables <- function(data) {
  apply(data, 2, ragree::unalike)
}

get_uac_perry<- function(x) {
  ragree::unalike(x, method=2)
}

get_uac_perry_for_variables <- function(data) {
  apply(data, 2, get_uac_perry)
}

# Source: https://stat.ethz.ch/pipermail/r-help/2008-August/172323.html
show_histogram_for_mode <- function(x) {
  lim.inf <- min(x) - 1
  lim.sup <- max(x) + 1
  h <- hist(x, freq=FALSE, breaks=seq(lim.inf,lim.sup,0.2))
  s <- density(x, from=lim.inf, to=lim.sup, bw=0.2)
  n <- length(s$y)
  v1 <- s$y[1:(n-2)]
  v2 <- s$y[2:(n-1)]
  v3 <- s$y[3:n]
  ix <- 1 + which((v1<v2) & (v2>v3))
  lines(s$x,s$y,col="red")
  points(s$x[ix],s$y[ix],col="blue")
  return(h)
}

# Source: https://stat.ethz.ch/pipermail/r-help/2008-August/172323.html
show_density_for_mode <- function(x) {
  #dd <- density(x)
  #which.max(dd$y)
  #dd$x[which.max(dd$y)]
  dd <- density(x)
  p <- plot(dd)
  rug(x)
  abline(v=dd$x[which.max(dd$y)])
}

do_shapiro_test_on_continuous_df <- function(df) {
  var_name_vals <- c()
  stats_vals <- c()
  p_vals <- c()
  sample_vals <- c()
  for (index in 1:ncol(df)) {
    var_name <- names(df)[index]
    var <- df[[index]]
    var <- na.omit(var) # Need to remove NAs!
    if (length(var) < 3) {
      statistic <- NA
      p_value <- NA
      sample <- length(var)
      var_name_vals <- c(var_name_vals,var_name)
      stats_vals <- c(stats_vals,statistic)
      p_vals <- c(p_vals,p_value)
      sample_vals <- c(sample_vals,sample)
      next
    }  
    if (length(var) > 5000) {
      var <- var[1:5000]
    }
    # print(paste0('var_name=',var_name))
    # print('length(var):')
    # print(length(var))
    y <- shapiro.test(var)
    statistic <- round(y$statistic, 5)
    p_value <- round(y$p.value, 7)
    sample <- length(var)
    var_name_vals <- c(var_name_vals,var_name)
    stats_vals <- c(stats_vals,statistic)
    p_vals <- c(p_vals,p_value)
    sample_vals <- c(sample_vals,sample)
  }
  out_df <- data.frame(
    vars = var_name_vals, 
    statistic = stats_vals,
    p_value = p_vals,
    sample = sample_vals,
    row.names = NULL)
  return(out_df)
}

get_summary_stats <- function(data) {
  split_data <- split_up_data(data)
  con_data <- split_data$continuous_data
  cat_data <- split_data$categorical_data 
  res_cont <- NULL
  res_cat <- NULL
  if (!is.null(con_data)) {
    res_cont <- get_condensed_summary_stats_for_columns_from_continuous_data(con_data)  
  }
  if (!is.null(cat_data)) {
    res_cat <- get_summary_stats_for_columns_from_categorical_data(cat_data)
  }
  return(list(summary_cont_data=res_cont,
              summary_cat_data=res_cat))
}  

get_summary_stats_for_columns_from_continuous_data <- function(data) {

  result_df <- data.frame(matrix(vector(),
                                 ncol(data),
                                 25,
                                 dimnames=list(c(),
                                               c("variable",
                                                 "n_count",
                                                 "n_pct",
                                                 "missing_count",
                                                 "missing_pct",
                                                 "unique_count",
                                                 "unique_rate",
                                                 "var",
                                                 "sd",
                                                 "cv",
                                                 "mean",
                                                 "median",
                                                 "mad",
                                                 "min",
                                                 "max",
                                                 "range",
                                                 "iqr",
                                                 "outlier_count",
                                                 "outlier_ratio",
                                                 "outlier_mean",
                                                 "skewness",
                                                 "kurtosis",
                                                 "sw_norm_stats",
                                                 "sw_norm_pval",
                                                 "sw_norm_sample"))),
                          stringsAsFactors=F)

  dat1 <- as.data.frame(t(summarytools::descr(data)))
  dat1 <- tibble::rownames_to_column(dat1, "variables")
  dat2 <- dlookr::diagnose(data)
  var_df <- get_variance_per_column(data)
  range_df <- get_range_per_column(data)
  outlier_df <- dlookr::diagnose_outlier(data)
  #norm_df <- dlookr::normality(data)
  norm_df <- do_shapiro_test_on_continuous_df(data)
  norm_df <- norm_df %>% dplyr::rename(variables = vars)

  joint_df <- Reduce(function(x, y) merge(x,
                                          y,
                                          all.x = FALSE,
                                          all.y = FALSE,
                                          by = c("variables")),
                     list(dat1,var_df,range_df,dat2,outlier_df,norm_df))

  result_df$variable <- joint_df$variables
  result_df$n_count <- joint_df$N.Valid
  result_df$n_pct <- joint_df$Pct.Valid
  result_df$missing_count <- joint_df$missing_count
  result_df$missing_pct <- joint_df$missing_percent
  result_df$unique_count <- joint_df$unique_count
  result_df$unique_rate <- joint_df$unique_rate
  result_df$var <- joint_df$variance
  result_df$sd <- joint_df$Std.Dev
  result_df$cv <- joint_df$CV
  result_df$mean <- joint_df$Mean
  result_df$median <- joint_df$Median
  result_df$mad <- joint_df$MAD
  result_df$min <- joint_df$Min
  result_df$max <- joint_df$Max
  result_df$range <- joint_df$range
  result_df$iqr <- joint_df$IQR
  result_df$outlier_count <- joint_df$outliers_cnt
  result_df$outlier_ratio <- joint_df$outliers_ratio
  result_df$outlier_mean <- joint_df$outliers_mean
  result_df$skewness <- joint_df$Skewness
  result_df$kurtosis <- joint_df$Kurtosis
  result_df$sw_norm_stats <- joint_df$statistic
  result_df$sw_norm_pval <- joint_df$p_value
  result_df$sw_norm_sample <- joint_df$sample

  return(result_df)
}

get_condensed_summary_stats_for_columns_from_continuous_data <- function(data) {
  
  result_df <- get_summary_stats_for_columns_from_continuous_data(data)
  
  # Drop the following columns:
  drop_cols <- c( "outlier_count",
                  "outlier_ratio",
                  "outlier_mean",
                  "sw_norm_stats",
                  "sw_norm_pval",
                  "sw_norm_sample")
  result_df <- result_df %>% dplyr::select(-tidyselect::one_of(drop_cols)) %>% distinct()
  
  return(result_df)
}

get_summary_stats_for_columns_from_categorical_data <- function(data) {

  d1 <- dlookr::diagnose_category(data)
  d2 <- dlookr::diagnose(data)

  joint_df <- Reduce(function(x, y) merge(x,
                                          y,
                                          all.x = FALSE,
                                          all.y = FALSE,
                                          by = c("variables")),
                     list(d1,d2))

  result_df <- data.frame(matrix(vector(),
                                 nrow(joint_df),
                                 12,
                                 dimnames=list(c(),
                                               c("variable",
                                                 "n_count",
                                                 "n_pct",
                                                 "missing_count",
                                                 "missing_pct",
                                                 "unique_count",
                                                 "unique_rate",
                                                 "total_num_levels",
                                                 "levels",
                                                 "counts_per_levels",
                                                 "ratio_per_levels",
                                                 "levels_rank"))),
                          stringsAsFactors=F)
  
  result_df$variable <- joint_df$variables
  result_df$missing_count <- joint_df$missing_count
  result_df$missing_pct <- joint_df$missing_percent
  result_df$unique_count <- joint_df$unique_count
  result_df$unique_rate <- joint_df$unique_rate
  #result_df$mode <- unname(get_mode_for_variables(data))
  #result_df$entropy <- unname(get_entropy_for_variables(data))
  #result_df$uac <- unname(get_uac_for_variables(data))
  #result_df$uac_perry <- unname(get_uac_perry_for_variables(data))
  result_df$levels <- joint_df$levels
  result_df$counts_per_levels <- joint_df$freq
  result_df$ratio_per_levels <- joint_df$ratio
  result_df$levels_rank <- joint_df$rank

  result_df <- result_df %>%
    group_by(variable) %>%
    filter(!is.na(levels)) %>%
    mutate(n_count = sum(counts_per_levels)) %>%
    ungroup()

  result_df <- result_df %>%
    mutate(n_pct = (n_count/nrow(data)*100))

  result_df <- result_df %>%
    group_by(variable) %>%
    filter(!is.na(levels)) %>%
    mutate(total_num_levels = n()) %>%
    ungroup()

  return(result_df)
}

get_condensed_summary_stats_for_columns_from_categorical_data <- function(data) {
  
  result_df <- get_summary_stats_for_columns_from_categorical_data(data)
  
  # Drop the following columns:
  drop_cols <- c("levels","counts_per_levels","ratio_per_levels","levels_rank")
  result_df <- result_df %>% dplyr::select(-tidyselect::one_of(drop_cols)) %>% distinct()
  
  return(result_df)
}


get_summary_stats_for_categorical_data <- function(data) {
  df <- data.frame(matrix(NA, nrow = ncol(data), ncol = 5))
  names(df) <- c('variable','mode','entropy','uac','uac_perry')
  df$variable <- names(data) 
  df$mode <- unname(get_mode_for_variables(data))
  df$entropy <- unname(get_entropy_for_variables(data))
  df$uac <- unname(get_uac_for_variables(data))
  df$uac_perry <- unname(get_uac_perry_for_variables(data))
  return(df)
}





