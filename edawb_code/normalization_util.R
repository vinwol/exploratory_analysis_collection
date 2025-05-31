normalize_rnaseq <- function(df, gene_index, logger) {
    non_gene_exp_df <- df[,1:(gene_index-1)]
    gene_exp_df     <- df[,gene_index:length(names(df))]
    min_exp <- min(gene_exp_df)
    # Log2 transformation.
    norm_gene_exp_df <- log2(gene_exp_df + abs(min_exp) + 1)
    # TODO: Check if centering and scaling is needed.
    # Apply centering and scaling column-wise for each gene.
    # URL: https://stat.ethz.ch/R-manual/R-devel/library/base/html/scale.html
    #norm_gene_exp_df <- scale(norm_gene_exp_df, center = TRUE, scale = TRUE)
    df <- cbind(non_gene_exp_df,norm_gene_exp_df)
    return(df)
}

get_geometric_mean <- function(x) {
    exp(mean(log(x)))
}

# TODO: make function!
## Adjust Expression values so that they are all positive
# datasetObjectmin <- min(datasetObject$expr, na.rm=TRUE)
# if (datasetObjectmin <0) {datasetObject$expr <- datasetObject$expr + abs(datasetObjectmin) + 1}
# Example:
# gene_expressions <- c(-3,-12,1,2,5,10,100)
# normalized_gene_expressions <- log2(gene_expressions + 1 - min(gene_expressions))
adjust_columnwise_gene_expressions_to_be_positive_with_plus_one <- function(df, genes) {
    for (i in 1:length(genes)) {
        gene <- genes[i]
        gene_exp <- as.numeric(df[[gene]])
        gene_exp_min <- min(gene_exp, na.rm=TRUE)
        if (gene_exp_min < 0) {
            df[[gene]] <- (gene_exp + abs(gene_exp_min) + 1)
        }
    }
    return(df)
}

adjust_rowwise_gene_expressions_to_be_positive_with_plus_one <- function(df) {
    for (i in 1:nrow(df)) {
        gene_exp <- as.numeric(df[i,])
        gene_exp_min <- min(gene_exp, na.rm=TRUE)
        if (gene_exp_min < 0) {
            df[i,] <- (gene_exp + abs(gene_exp_min) + 1)
        }
    }
    return(df)
}

adjust_vector_to_be_positive_with_plus_one <- function(vals) {
    gene_exp <- as.numeric(vals)
    gene_exp_min <- min(gene_exp, na.rm=TRUE)
    if (gene_exp_min < 0) {
        vals <- (gene_exp + abs(gene_exp_min) + 1)
    }
    return(vals)
}

calculate_columnwise_min_max_normalization_for_df <- function(df) {
    norm_df <-
        as.data.frame(apply(df, 2, function(x) {
            (x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T))
        }
        ))
    return(norm_df)
}

calculate_rowwise_min_max_normalization_for_df <- function(df) {
    norm_df <-
        as.data.frame(t(apply(df, 1, function(x) {
            (x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T))
        }
        )))
    return(norm_df)
}

# https://neuropsychology.github.io/psycho.R/2018/03/29/standardize.html
# https://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/
# df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)
# is the same as
# df[[var]] <- (df[[var]] - mean(df[[var]])) / sd(df[[var]])
# Test:
# a <- c(5, 3, 7, 5, 5, 3, 4)
# az1 <- (a - mean(a)) / sd(a)
# az2 <- scale(a, center = TRUE, scale = TRUE)
calculate_columnwise_z_score_normalization_for_df <- function(df) {
    for (var in names(df)) {
        if (class(df[[var]]) == "numeric" | class(df[[var]]) == "integer") {
            #df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)
            df[[var]] <- (df[[var]] - mean(df[[var]])) / sd(df[[var]])
        }
    }
    return(df)
}

apply_rescale_to_df <- function(df) {
    dat <-
        data.frame(lapply(df, function(x)
            scale(
                x, center = FALSE, scale = max(x, na.rm = TRUE) / 100
            )))
}

do_range_scaling <- function(x) { (x-min(x))/(max(x)-min(x)) }

apply_log2_transformation_to_df <- function(df) {
    dat <- log2(df)
    return(df)
}

apply_log2_transformation_to_selected_columns <- function(df, selected_columns) {
    dat <- df[, selected_columns]
    dat <- log2(dat)
    df[, selected_columns] <- dat
    return(df)
}

order_columns_in_df_by_variance <- function(df) {
    col_names <- names(sort(apply(df, 2, stats::var), decreasing = T))
    df <- df[,col_names]
    return(df)
}

mad_with_center <- function(x, center_var) {
    stats::mad(x,center_var)
}

# mad(x, center = median(x), constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE)
# center: Optionally, the centre: defaults to the median.
# constant: scale factor.
order_columns_in_df_by_mad <- function(df) {
    col_names <- names(sort(apply(df, 2, stats::mad), decreasing = T))
    df <- df[,col_names]
    return(df)
}

#' Returns the median absolute error between vectors x and y.
#'
#' @param x a numeric vector
#' @param y a numeric vector of same length as x.
#' @return The median absolute error between x and y.
#'
#' @examples
#' MdAE(1:3,jitter(1:3))
mdae <- function(x,y) {
    return(median(abs(y-x)))
}

#' z-score
#'
#' Wrapper to calculate the z-score
#'
#' @param x a numeric matrix or data.frame.
#' @return The matrix of corresponding row z-scores.
#'
#' @examples
#' zscore(matrix(1:12,nrow=3))
zscore <- function(x) {
    t(scale(t(x)))
}

