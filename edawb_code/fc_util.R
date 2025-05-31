# Source: ecfc package.
# https://github.roche.com/staedlen/ecfc
# Computes the logit transformation logit = log[p/(1 - p)] for the proportion p.
logit_mod <- function(x,eps=0.01) {
    x[x==1] <- 1-eps
    x[x==0] <- 0+eps
    log(x/(1-x))
}

transform_fc_pct_data <- function(df) {
    # Check if AVAL exists.
    if ('AVAL' %in% names(df)) {
        df$AVAL <- df$AVAL / 100
        df$AVAL <- logit_mod(df$AVAL)
    } else {
        stop('Could not find column AVAL!')
    }
    return(df)
}

transform_fc_cell_count_data <- function(df) {
    # Check if AVAL exists.
    if ('AVAL' %in% names(df)) {
        min_val <- min(df$AVAL, na.rm = TRUE)
        df$AVAL <- log2(df$AVAL + abs(min_val) + 1)
    } else {
        stop('Could not find column AVAL!')
    }
    return(df)
}

