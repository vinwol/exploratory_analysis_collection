# TODO: see 
# https://rdrr.io/cran/mlr/man/removeConstantFeatures.html
remove_quasi_constant_features <- function(df, threshold) {
    
}

remove_duplicate_columns <- function(df) {
    return(df[!duplicated(as.list(df))])
}

remove_duplicate_rows <- function(df) {
    return(df[!duplicated(df),])
}

# Source: https://stats.stackexchange.com/questions/221332/variance-of-a-distribution-of-multi-level-categorical-data
# range: 0 to infinity
# Shannon entropy.
get_entropy <- function(x) {
    x <- na.omit(x)
    px  <- table(x)/length(x)
    lpx <- log(px, base=2)
    ent <- -sum(px*lpx)
    return(ent)
}

mad_with_center <- function(x, center_var) {
    mad(x,center_var)
}

# The name of the entropy estimator. 
# The package implements four estimators: 
# "emp", "mm", "shrink", "sg" (default:"emp")
get_mutual_information <- function(data, method) {
    infotheo::mutinformation(data, method=method)
}

order_columns_by_variance <- function(df) {
    col_names <- names(sort(apply(df, 2, var, na.rm=TRUE), decreasing = T))
    df <- df[,col_names]
    return(df)
}

#order_columns_by_mad <- function(df, center) {
order_columns_by_mad <- function(df) {
    #col_names <- names(sort(apply(df, 2, mad_with_center, center_var=center),decreasing=T))
    col_names <- names(sort(apply(df, 2, mad, na.rm=TRUE), decreasing = T))
    df <- df[,col_names]
    return(df)
}

order_columns_by_entropy <- function(df) {
    #col_names <- names(sort(apply(df, 2, mad_with_center, center_var=center),decreasing=T))
    col_names <- names(sort(apply(df, 2, get_entropy), decreasing = T))
    df <- df[,col_names]
    return(df)
}

order_columns_by_laplacian_score <- function(df, method, bandwidth) {
    df <- na.omit(df)
    res <- Rdimtools::do.lscore(df,
                                type=method,
                                t=bandwidth)
    sdf <- data.frame(matrix(vector(), 0, length(names(df))),stringsAsFactors=F)
    sdf[1,] <- res$lscore               
    names(sdf) <- names(df)
    col_names <- names(sort(sdf, decreasing = T))
    df <- df[col_names]
    return(df)
}

# Methods:
# "emp": This estimator computes the entropy of the empirical probability distribution.
# "mm": This is the Miller-Madow asymptotic bias corrected empirical estimator.
# "shrink": This is a shrinkage estimate of the entropy of a Dirichlet probability distribution.
# "sg": This is the Schurmann-Grassberger estimate of the entropy of a Dirichlet probability distribution.
# "ksmi": kernel smoothing approach to calculate Mutual Information. Uses a
# nonparametric bias correction giving Bias Corrected Mutual Information
order_columns_by_mutual_information <- function(df, method) {
    final_df <- NULL
    methods <- c("emp","mm","shrink","sg","ksmi")
    if (method %notin% methods) {
        stop('Could not find method for calculating mutual information!')
    }
    if (method %in% c("emp","mm","shrink","sg")) {
        # df needs to be discretized!
        final_df <- infotheo::mutinformation(df, method=method)  
    } else if (method == "ksmi") {
        split_data <- split_up_data(df)
        con_data <- split_data$continuous_data
        cat_data <- split_data$categorical_data
        con_df <- NULL
        if (!is.null(con_data) && dim(con_data)[1] > 0 && dim(con_data)[2] > 0) {
            con_df <- mpmi::cmi(con_data)$bcmi 
        }
        cat_df <- NULL
        if (!is.null(cat_data) && dim(cat_data)[1] > 0 && dim(cat_data)[2] > 0) {
            cat_df <- mpmi::dmi(cat_data)$bcmi 
        }
        if (!is.null(con_df) && !is.null(cat_df)) {
            final_df <- cbind(con_df, cat_df)    
        } 
        if (!is.null(con_df) && is.null(cat_df)) {
            final_df <- con_df
        } 
        if (is.null(con_df) && !is.null(cat_df)) {
            final_df <- cat_df
        } 
    } else {
        stop('Could not find a suitable method to calculate mutual information!')
    }
    col_names <- names(sort(final_df, decreasing = T))
    df <- df[col_names]
    return(df)
}    

select_top_features_by_variance <- function(df, topn) {
    df_sorted <- order_columns_by_variance(df)
    return(df_sorted[,1:topn])
}

select_top_features_by_mad <- function(df, topn) {
    df_sorted <- order_columns_by_mad(df)
    return(df_sorted[,1:topn])
}

select_top_features_by_entropy <- function(df, topn) {
    df_sorted <- order_columns_by_entropy(df)
    return(df_sorted[,1:topn])
}

select_top_features_by_mutual_information <- function(df, topn, method) {
    df_sorted <- order_columns_by_mutual_information(df, method)
    return(df_sorted[,1:topn])
}    

# use: "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".
# method: stats::cor(x, y, method = c("pearson", "kendall", "spearman"))
select_top_features_by_correlation <- function(df, threshold, use, method) {
    remove_vars <- c()
    for (c1 in names(df)) {
        for (c2 in names(df)) {
            if (c1 == c2) {
                next
            }
            x1 <- df[[c1]]
            x2 <- df[[c2]]
            res <- stats::cor(x1, x2, use=use, method=method)
            if (res < threshold) {
                # We flag column c2.
                remove_vars <- c(remove_vars, c2)
            }
        }    
    }
    df_subset <- df[ , !(names(df) %in% remove_vars)]
    return(df_subset)
} 

# method: a vector of neighborhood graph construction. 
# The following method are supported: 
# k nearest points : c("knn",k), 
# epsilon nearest neighbor: connects all the data poinst within a certain radius, c("enn",radius), and 
# connect proportion-amount of data points sequentially from the nearest to farthest: c("proportion",ratio).
# Default is c("proportion",0.1), connecting about 1/10 of nearest data points among all data points. 
# bandwith: bandwidth parameter for heat kernel in (0,∞).
select_top_features_by_laplacian_score <- function(data=data, topn=topn, method=metho, bandwidth=bandwidth) {
    df_sorted <- order_columns_by_laplacian_score(data, method, bandwidth)
    return(df_sorted[,1:topn]) 
} 

# Can handle continuous and categorical variables.
# Format of dataframe df: the features are the columns!
select_top_features_ranked_by_variance <- function(data=data, topn=500) {
    o <- split_up_data(data)
    cont <- o$continuous_data
    cat <- o$categorical_data
    if (!is.null(cat) && dim(cat)[1] != 0 && dim(cat)[2] != 0) {
        cat[] <- lapply(cat, factor)
        col_names <- names(cat)
        cat[col_names] <- lapply(cat[col_names], factor)
        cat <- sapply(cat, unclass)   
    }
    if (!is.null(cont) && !is.null(cat)) {
        data <- cbind(cont,cat)  
    } 
    if (is.null(cont) && !is.null(cat)) {
        data <- cat  
    }
    if (!is.null(cont) && is.null(cat)) {
        data <- cont
    }
    col_variance <- DelayedMatrixStats::colVars(DelayedArray(data))
    top_features_indices <- order(col_variance, decreasing = TRUE)[seq_len(min(topn,length(col_variance)))]
    top_features_vars <- names(data)[top_features_indices]
    return(top_features_vars)
} 




