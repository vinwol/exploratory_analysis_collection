# TODO: Add convenience methods for dataframes.

# Source: Package DMwR2
# Function for normalizing the range of values of a continuous variable.
# Taken from the book "Data preparation for data mining" by Dorian Pyle
# (pp. 271-274)
#
# This function ensures all values will be between 0 and 1.
#
# 13/05/2002, Luis Torgo.
# ----------------------------------------------------------------------
# Example :
# SoftMax(algae[,'NO3'])
# the following obtains the transformation just for one value
# SoftMax(45.23,avg=mean(algae[,'NO3'],na.rm=T),std=sd(algae[,'NO3'],na.rm=T))
#
# Note:
# The lambda parameter controls the range of values that gets a linear
# mapping. It represents the number of standard deviations that should be
# included in the linear mapping region (e.g. 1-> 68% of the distribution gets
# linear mapping, while 2-> 95.5%, 3 -> 99.7%, etc.)
do_softmax_transformation <- function(x,
                                      lambda=2,
                                      avg=mean(x,na.rm=T),
                                      std=sd(x,na.rm=T)) {
    if (is.data.frame(x) | is.array(x)) return(apply(x,2,do_softmax_transformation,lambda))
    vt <- (x-avg)/(lambda*(std/(2*pi)))
    1/(1+exp(-vt))
}

# Source: https://rpubs.com/FJRubio/softmax
do_softmax_transformation2 <- function(x) {
    n.x <- length(x)
    x1 <- sort(x, decreasing = TRUE)
    Lk <- x1[1]
    for (k in 1:(n.x-1)) {
        Lk <- max(x1[k+1], Lk) + log1p(exp(-abs(x1[k+1] - Lk))) 
    }
    val <- exp(x - Lk)
    return(val)
}

# https://www.r-bloggers.com/2012/03/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/
do_zscore_transformation <- function(x) {
    return(scale(x, center = TRUE, scale = TRUE))    
}

do_zscore_transformation2 <- function(x) {
    return((x - mean(x)) / sd(x))
}

do_minmax_transformation <- function(x) {
    return((x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T)))
}    

do_columnwise_minmax_transformation <- function(df) {
    norm_df <- as.data.frame(apply(df, 2, function(x) {
            (x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T))
        }))
    return(norm_df)
}

do_rowwise_minmax_transformation <- function(df) {
    norm_df <- as.data.frame(t(apply(df, 1, function(x) {
            (x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T))
        })))
    return(norm_df)
}

do_log2_transformation <- function(x) {
    min_val <- min(x, na.rm=TRUE)
    y <- log2(x + abs(min_val) + 1)
    return(y)
}

# Discretization methods.
# type parameter of function quantile(): nine quantile algorithms.
create_discretization <- function(x, partition) {
    if (partition %notin% c('median','quartiles','deciles')) {
        stop(paste0('Error in function create_discretization(): partition ',partition,' not found!'))
    }    
    result <- NULL
    #min <- min(x, na.rm = TRUE)
    #max <- max(x, na.rm = TRUE) 
    if (partition == 'median') {
        result <- cut(x, 
                      #breaks=seq(min, max, by = median(x,na.rm=TRUE)), 
                      breaks = unique(quantile(x,probs=seq(0,1,by=0.5),na.rm=TRUE,type=7)), 
                      include.lowest = TRUE,
                      right = FALSE, 
                      dig = 4)
    } 
    if (partition == 'quartiles') {
        result <- cut(x, 
                      breaks = unique(quantile(x,probs=seq(0,1,by=0.25),na.rm=TRUE,type=7)), 
                      include.lowest = TRUE,
                      right = FALSE, 
                      dig = 4)
    } 
    if (partition == 'deciles') {
        result <- cut(x, 
                      breaks = unique(quantile(x,probs=seq(0,1,by=0.1),na.rm=TRUE,type=5)), 
                      include.lowest = TRUE,
                      right = FALSE, 
                      dig = 4)
    }
    return(result)
}       









