# Diagnostics.

# TODO: Should we treat factors separate and use nlevels()?
examine_non_numeric_columns_for_number_of_values <- function(df) {
    non_numeric_vars <- c()
    for (index in 1:ncol(df)) {
        var_name <- names(df)[index]
        var_vals <- df[,index]
        if (class(var_vals) %in% c("character","factor")) {
            vals <- length(unique(var_vals))
            non_numeric_vars <- c(non_numeric_vars, paste0('\n',var_name,': ',vals))
        }     
    } 
    print(paste0('The following vars were detected: #',length(non_numeric_vars)))
    cat(non_numeric_vars)
} 

remove_non_numeric_columns_above_threshold_for_number_of_values <- function(df, threshold) {
    remove_vars <- c()
    logged_vars <- c()
    for (index in 1:ncol(df)) {
        var_name <- names(df)[index]
        var_vals <- df[,index]
        if (class(var_vals) %in% c("character","factor")) {
            vals <- length(unique(var_vals))
            if (vals > threshold) {
                remove_vars <- c(remove_vars, var_name) 
                logged_vars <- c(logged_vars, paste0('\n',var_name,': ',vals))
            }
        }     
    } 
    print(paste0('The following vars are removed: #',length(remove_vars)))
    cat(logged_vars)
    df <- df %>% dplyr::select(-tidyselect::all_of(remove_vars))
    return(df)
}    

detect_columns_with_identical_values <- function(df) {
    detected_cols <- c()
    for (index in 1:ncol(df)) {
        var_name <- names(df)[index]
        var_vals <- df[,index]
        if (length(unique(var_vals)) == 1) {
            detected_cols <- c(detected_cols, var_name)    
        }
    }
    if (length(detected_cols) == 0) {
        print('Did not find columns with identical values!')
    } else {
        print('Found the following columns with identical values:')
        print(detected_cols)
    }
}

remove_columns_with_identical_values <-function(df) {
    detected_cols <- c()
    for (index in 1:ncol(df)) {
        var_name <- names(df)[index]
        var_vals <- df[,index]
        if (length(unique(var_vals)) == 1) {
            detected_cols <- c(detected_cols, var_name)    
        }
    }
    if (length(detected_cols) > 0) {
        print('Will remove the following columns with identical values:')
        print(detected_cols)
        df <- df %>% dplyr::select(-tidyselect::all_of(detected_cols))
    } else {
        print('Found no columns with identical values!')
    }
    return(df)
}

remove_zero_variance_columns <- function(df) {
    df <- df[ , which(apply(df, 2, var) != 0)]
    return(df)
}

detect_zero_variance_columns <- function(df) {
    detected_cols <- c()
    for (index in 1:ncol(df)) {
        var_name <- names(df)[index]
        var_vals <- df[,index]
        if (class(var_vals) %in% c("numeric","integer")) {
            if (var(var_vals) == 0) {
                detected_cols <- c(detected_cols, var_name)    
            }
        }
    }    
    if (length(detected_cols) == 0) {
        print('Did not find columns with zero variance!')
    } else {
        print('Found the following columns with zero variance:')
        print(detected_cols)
    }
}