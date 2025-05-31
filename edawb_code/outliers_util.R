# Outliers:
# https://statisticsbyjim.com/basics/outliers/
# https://www.sqlservercentral.com/articles/scoring-outliers-with-r
# https://rpubs.com/frasermyers/627592
# https://stackoverflow.com/questions/28866902/find-outlier-using-z-score
# https://cran.r-project.org/web/packages/outliers/index.html
# https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
# https://statsandr.com/blog/outliers-detection-in-r/
# Hampel filter
# Grubbs’s test
# Dixon’s test
# Rosner’s test
# Other functions and libs:
# {outliers} packages,
# via the lofactor() function from the {DMwR} package: Local Outlier Factor (LOF) 
# is an algorithm used to identify outliers by comparing the local density of a point with that of its neighbors,
# the outlierTest() from the {car} package gives the most extreme observation based 
# on the given model and allows to test whether it is an outlier,
# {OutlierDetection} package, 
# with the aq.plot() function from the {mvoutlier} package.
#
# outliers:
# scores(): Calculate scores of the sample.
# scores(x, type = c("z", "t", "chisq", "iqr", "mad"), prob = NA, lim = NA)
# This function calculates normal, t, chi-squared, IQR and MAD scores of given data.
# set.seed(1234)
# x = rnorm(10)
# outliers::scores(x)
# outliers::scores(x,prob=0.1)
#
# outliers: chisq.out.test() Chi-squared test for outlier
# outliers: cochran.test() Test for outlying or inlying variance
# outliers: outlier() Find value with largest difference from the mean
# outlier(x, opposite = FALSE, logical = FALSE)
# x a data sample, vector in most cases. If argument is a dataframe, then outlier is
# calculated for each column by sapply. The same behavior is applied by apply
# when the matrix is given

get_max_number_from_nested_list <- function(nl) {
    max <- 0
    for (i in 1:length(nl)) {
        num_elem <- length(nl[[i]])
        if (num_elem >= max) {
            max <- num_elem
        }
    } 
    return(max)
}

fill_up_vector <- function(vec, max) {
    if (length(vec) > max) {
        stop('Error: vector is larger than specified max!')
    }    
    if (length(vec) == max) {
        return(vec)
    }
    diff <- max - length(vec)
    vec <- c(vec,rep('',diff))
    return(vec)
}

create_dataframe_from_nested_list_for_outliers <- function(nl) {
    max <- get_max_number_from_nested_list(nl)
    cols <- names(nl)
    df <- data.frame(matrix('', nrow = max, ncol = length(cols)))
    names(df) <- cols
    for (col in cols) {
        df[[col]] <- fill_up_vector(nl[[col]], max)
    }
    return(df)
}

# ======================================================================
# From R package: DMwR
# OUTLIER RANKING
# (c) Luis Torgo, 2004-2008
# ======================================================================
# Using hierarchical clustering to obtain a ranking of outlierness for a
# set of "test" cases, using also a set of "tranining" cases.
# This separation in test and train is artificial and only necessary if the
# test cases are few...
# If the test.data is NULL the ranking respects all data given in the first
# parameter (the data), otherwise the obtained ranking respects the test.data
# The user can also supply a distance matrix instead in the data parameter.
# In that case (only usefull for optimization issues when trying different
# clustering algorithms for the same distance function) the ranking is the
# same as if test.data was NULL.
#
# The ranking can be obtained by different methods:
# 'linear'
# 'sigmoid'
# 'sizeDiff'
rank_outliers_by_clustering <- function(data,
                                        test.data=NULL,
                                        method='sizeDiff',
                                        method.pars=NULL,
                                        clus=list(dist='euclidean',alg='hclust',meth='ward'),
                                        power=1,
                                        verb=F) {
    
    # T0: the rankings are going to be on the "data" but the distance matrix is given
    if (inherits(data,'dist')) {
        test.pos <- 1
        N <- (1+sqrt(1+4*length(data)*2))/2
        # T1: the rankings are going to be on the "training data"
    } else if (is.null(test.data) ) {
        N <- NROW(data)
        test.pos <- 1 
        # T2: the rankings are for the "test data"
    } else {
        train.sz <- NROW(data)
        test.sz <- NROW(test.data)
        data <- rbind(data,test.data)
        N <- train.sz+test.sz
        test.pos <- train.sz+1
    }
    
    if (verb) cat('OR:: Distance calculation...')
    # Distance Calculation
    if (!inherits(data,'dist')) { # we were given a data set
        if (power > 1) 
            dist.mtrx <- dist(data,method=clus$dist)^power
        else
            dist.mtrx <- dist(data,method=clus$dist)
    } else dist.mtrx <- data    # we were already given a distance matrix
    
    if (verb) cat('\nOR:: Clustering...')
    # Hierarchical Clustering
    if (clus$alg != 'diana') {
        h <- do.call(clus$alg,list(dist.mtrx,method=clus$meth))
    } else {
        h <- do.call(clus$alg,list(dist.mtrx))
    }
    
    if (verb) {
        cat('\nOR:: Ranking...')
    }
    
    # Ranking
    # This is the major step, obtain rankings based on clustering results
    
    # This vector will hold the ranking score of each data point
    rk <- rep(0,N)
    
    # This is the linear method
    if (method=='linear') {
        #out.lim <- method.pars$sz.perc*N
        out.lim <- max(round(method.pars$sz.perc*N,0),2)
        szs <- rep(0,NROW(h$merge))
        mb <- list()
        for(ln in 1:length(szs)) {
            x <- sapply(1:2,function(p) ifelse(h$merge[ln,p] < 0,1,szs[h$merge[ln,p]]))
            szs[ln] <- sum(x)
            mb[[ln]] <- c(g1 <- if (h$merge[ln,1] < 0) -h$merge[ln,1] else mb[[h$merge[ln,1]]],
                          g2 <- if (h$merge[ln,2] < 0) -h$merge[ln,2] else mb[[h$merge[ln,2]]])
            
            h.fac <- ln/(N-1)
            
            if (x[1] < x[2]) {
                sc <- if (x[1] > out.lim) 0 else (1-(x[1]-1)/(N-2))*h.fac
                rk[g1] <- ifelse(rk[g1] > sc,rk[g1],sc)
            } else {
                sc <- if (x[2] > out.lim) 0 else (1-(x[2]-1)/(N-2))*h.fac
                rk[g2] <- ifelse(rk[g2] > sc,rk[g2],sc)
            }
        }
        rk.outliers <- order(rk[test.pos:N],decreasing=T)
        pb.outliers <- rk[test.pos:N]
        # This is the sigmoidal
    } else  if (method=='sigmoid') {
        #out.lim <- method.pars$sz.perc*N
        out.lim <- 2*max(round(method.pars$sz.perc*N,0),2)
        #cTemp <- (7*out.lim^2)/(1-out.lim)^2 
        szs <- rep(0,NROW(h$merge))
        mb <- list()
        for(ln in 1:length(szs)) {
            x <- sapply(1:2,function(p) ifelse(h$merge[ln,p] < 0,1,szs[h$merge[ln,p]]))
            szs[ln] <- sum(x)
            mb[[ln]] <- c(g1 <- if (h$merge[ln,1] < 0) -h$merge[ln,1] else mb[[h$merge[ln,1]]],
                          g2 <- if (h$merge[ln,2] < 0) -h$merge[ln,2] else mb[[h$merge[ln,2]]])
            
            h.fac <- exp( (-2)*( ((ln-(N-1))^2) / ((N-1)^2) ) )
            if (x[1] < x[2]) {
                sc <- (x[1]< out.lim)* (1 - exp( (-4) * ((x[1]-out.lim)^2 / (out.lim)^2 ))) * h.fac
                rk[g1] <- ifelse(rk[g1] > sc,rk[g1],sc)
            } else {
                sc <- (x[2]< out.lim)* (1 - exp( (-4) * ((x[2]-out.lim)^2 / (out.lim)^2 ))) * h.fac
                rk[g2] <- ifelse(rk[g2] > sc,rk[g2],sc)
            }
        }
        rk.outliers <- order(rk[test.pos:N],decreasing=T)
        pb.outliers <- rk[test.pos:N]
        # This is the group size differences method
    } else if (method=='sizeDiff') {
        szs <- rep(0,NROW(h$merge))
        mb <- list()
        for(ln in 1:length(szs)) {
            x <- sapply(1:2,function(p) ifelse(h$merge[ln,p] < 0,1,szs[h$merge[ln,p]]))
            szs[ln] <- sum(x)
            mb[[ln]] <- c(g1 <- if (h$merge[ln,1] < 0) -h$merge[ln,1] else mb[[h$merge[ln,1]]],
                          g2 <- if (h$merge[ln,2] < 0) -h$merge[ln,2] else mb[[h$merge[ln,2]]])
            
            if (x[1] < x[2]) {
                sc <- (x[2]-x[1])/(x[1]+x[2])
                rk[g1] <- ifelse(rk[g1] > sc,rk[g1],sc)
            } else {
                sc <- (x[1]-x[2])/(x[1]+x[2])
                rk[g2] <- ifelse(rk[g2] > sc,rk[g2],sc)
            }
        }
        rk.outliers <- order(rk[test.pos:N],decreasing=T)
        pb.outliers <- rk[test.pos:N]
    }
    names(rk.outliers) <- names(pb.outliers) <- row.names(data)[test.pos:N]
    
    # ---- Now build up the list that will be returned
    if (verb) cat('\n')
    list(rank.outliers=rk.outliers,
         prob.outliers=pb.outliers, # this is in the natural order and not
         hie=h,                     # outlierness order. To get the latter do
         dist=dist.mtrx)            # res$prob.outliers[res$rank.outliers]
}

# The further away an observation’s Z-score is from zero, the more unusual it is. 
# A standard cut-off value for finding outliers are Z-scores of +/-3 or further from zero. 
# find_outliers_rowwise_by_z_scores(matrix(rnorm(100), ncol=10), 2)
# TODO: Needs further work.
find_outliers_rowwise_by_z_scores <- function(df, th) {
    z <- abs(apply(df, 1, scale))
    return(df[z > th])
}

find_outliers_columnwise_by_z_scores <- function(df, th) {
    z <- as.data.frame(abs(apply(df, 2, scale)))
    cols <- names(z)
    ll <- list()
    for (col in cols) {
        ind <- which(z[[col]] > th)
        vals <- df[[col]][ind]
        ll <- c(ll,list(vals))
    }
    ll <- setNames(ll,cols)
    df <- create_dataframe_from_nested_list_for_outliers(ll)
    return(df)
}

find_outliers_by_zscore <- function(vec, th) {
    zscores <- abs(vec - mean(vec))/sd(vec)
    outlier_vals <- c()
    for (elm in zscores) {
        if (elm > th) {
            outlier_vals <- c(outlier_vals, elm)    
        }
    }
    return(outlier_vals)
}    


# Source: https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
# The interquartile range is the central 50% or the area between the 75th and the 25th percentile of a distribution. 
# A point is an outlier if it is above the 75th or below the 25th percentile by a factor of 1.5 times the IQR.
# For example, if
# Q1= 25th percentile
# Q3= 75th percentile
# Then, IQR= Q3 – Q1
# And an outlier would be a point below [Q1- (1.5)IQR] or above [Q3+(1.5)IQR].
# Using the quantile() function to find the 25th and the 75th percentile of the dataset, 
# and the IQR() function which elegantly gives the difference of the 75th and 25th percentiles.
# Q <- quantile(warpbreaks$breaks, probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(warpbreaks$breaks)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# eliminated<- subset(warpbreaks, warpbreaks$breaks > (Q[1] - 1.5*iqr) & warpbreaks$breaks < (Q[2]+1.5*iqr))
# ggbetweenstats(eliminated, wool, breaks, outlier.tagging = TRUE) 
find_outliers_by_iqr <- function(vec) {
    q <- quantile(vec, probs=c(.25, .75), na.rm = TRUE)
    iqr <- IQR(vec, na.rm = TRUE)
    up <- q[2] + 1.5 * iqr
    low <- q[1] - 1.5 * iqr
    outlier_vals <- c()
    for (elm in vec) {
        if (is.na(elm)) {
            next
        }
        if (elm < low || elm > up) {
            outlier_vals <- c(outlier_vals, elm)    
        }
    }
    return(outlier_vals)
}  

find_outliers_columnwise_by_iqr <- function(df) {
    res <- apply(df, 2, find_outliers_by_iqr)
    df <- create_dataframe_from_nested_list_for_outliers(res)
    return(df)
}    

# Source: https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
# boxplot(warpbreaks$breaks, plot=FALSE)$out
# outliers <- boxplot(warpbreaks$breaks, plot=FALSE)$out
# x<-warpbreaks ?
# x<- x[-which(x$breaks %in% outliers),]

# Source: https://statsandr.com/blog/outliers-detection-in-r
find_outliers_by_boxplot <- function(vec) {
    out <- boxplot.stats(vec)$out
    out_ind <- which(vec %in% c(out))
    return(vec[out_ind])
}  

find_outliers_columnwise_by_boxplot <- function(df) {
    res <- apply(df, 2, find_outliers_by_boxplot)
    df <- create_dataframe_from_nested_list_for_outliers(res)
    return(df)
}

# Source: https://statsandr.com/blog/outliers-detection-in-r
find_outliers_by_quantile <- function(vec, lower_quantile=0.025, upper_quantile=0.975) {
    lower_bound <- quantile(vec, lower_quantile, na.rm = TRUE)
    upper_bound <- quantile(vec, upper_quantile, na.rm = TRUE)
    outlier_ind <- which(vec < lower_bound | vec > upper_bound)
    return(vec[outlier_ind])
}  

find_outliers_columnwise_by_quantile <- function(df) {
    res <- apply(df, 2, find_outliers_by_quantile)
    df <- create_dataframe_from_nested_list_for_outliers(res)
    return(df)
}

# Source: https://statsandr.com/blog/outliers-detection-in-r
# Hampel filter:
# Considering as outliers the values outside the interval (I) formed by the median, 
# plus or minus 3 median absolute deviations (MAD):
# I = [median - 3 * MAD, median + 3 * MAD]    
find_outliers_by_hampel_filter <- function(vec) {
    lower_bound <- median(vec) - 3 * mad(vec, constant = 1)
    upper_bound <- median(vec) + 3 * mad(vec, constant = 1)
    outlier_ind <- which(vec < lower_bound | vec > upper_bound)
    return(vec[outlier_ind])
}

find_outliers_columnwise_by_hampel_filter <- function(df) {
    res <- apply(df, 2, find_outliers_by_hampel_filter)
    df <- create_dataframe_from_nested_list_for_outliers(res)
    return(df)
}

# Source: https://statsandr.com/blog/outliers-detection-in-r
# Grubbs test is used to test whether a single low or high value is an outlier.
# So if more than one outliers is suspected, the test has to be performed on these suspected outliers individually.
find_outliers_by_grubbs_test_on_lowest_value <- function(vec) {
    return(outliers::grubbs.test(vec, opposite = FALSE))
}    

# Source: https://statsandr.com/blog/outliers-detection-in-r
find_outliers_by_grubbs_test_on_highest_value <- function(vec) {
    return(outliers::grubbs.test(vec, opposite = TRUE))
}  

# Source: https://statsandr.com/blog/outliers-detection-in-r
# Dixon test is used to test whether a single low or high value is an outlier.
# Note that Dixon test is most useful for small sample size (usually n ≤ 25).
find_outliers_by_dixon_test_on_highest_value <- function(vec) {
    return(outliers::dixon.test(vec, opposite = TRUE))
}

# Source: https://statsandr.com/blog/outliers-detection-in-r
find_outliers_by_dixon_test_on_lowest_value <- function(vec) {
    return(outliers::dixon.test(vec, opposite = FALSE))
}

# Source: https://statsandr.com/blog/outliers-detection-in-r
# Rosner’s test for outliers has the advantages that:
# 1) it is used to detect several outliers at once (unlike Grubbs and Dixon test 
# which must be performed iteratively to screen for multiple outliers), and
# 2) it is designed to avoid the problem of masking, where an outlier that is 
# close in value to another outlier can go undetected.
# Unlike Dixon test, note that Rosner test is most appropriate when the sample 
# size is large (n ≥ 20).
# For this example, we set the number of suspected outliers to be equal to 3, 
# as suggested by the number of potential outliers outlined in a boxplot
find_outliers_by_rosner_test <- function(vec, num_outliers) {
    return(EnvStats::rosnerTest(vec, k = num_outliers))
}

get_outliers_by_performance_check_outliers <- function(data) {
    methods <- c("zscore", "zscore_robust", "iqr", "eti", "hdi", "bci", 
                 "mahalanobis", "mahalanobis_robust", "mcd", "ics", "optics", "lof")
    result_list <- list()
    data <- na.omit(data)
    for (index in 1:length(methods)) {
        method <- methods[index]
        res <- performance::check_outliers(data,method = method)
        df_outliers <- insight::get_data(data)[res, ]
        result_list[[index]] <- df_outliers
    }
    return(list(outliers_zscore=result_list[[1]],
                outliers_zscore_robust=result_list[[2]],
                outliers_iqr=result_list[[3]],
                outliers_eti=result_list[[4]],
                outliers_hdi=result_list[[5]],
                outliers_bci=result_list[[6]],
                outliers_mahalanobis=result_list[[7]],
                outliers_mahalanobis_robust=result_list[[8]],
                outliers_mcd=result_list[[9]],
                outliers_ics=result_list[[10]],
                outliers_optics=result_list[[11]],
                outliers_lof=result_list[[12]]))
}

# find_outliers_by_iqr()
# find_outliers_by_quantile()
# find_outliers_by_zscore()
# outliers::outlier()
# outliers::chisq.out.test()
# outliers::cochran.test()
# find_outliers_by_hampel_filter()
# outliers::grubbs.test()
# outliers::dixon.test()
# EnvStats::rosnerTest()
# performance::check_outliers()
# robustbase::adjOutlyingness()
get_outlier_summary <- function(data) {
    outliers_iqr <- find_outliers_columnwise_by_iqr(data)
    outliers_zscore <- find_outliers_columnwise_by_z_scores(data,3)
    outliers_quantile <- find_outliers_columnwise_by_quantile(data)
    outliers_hampel <- find_outliers_columnwise_by_hampel_filter(data)
    return(list(outliers_iqr=outliers_iqr,
                outliers_zscore=outliers_zscore,
                outliers_quantile=outliers_quantile,
                outliers_hampel=outliers_hampel))
}



