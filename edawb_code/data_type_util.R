# old name: getTypeOfVariable2
get_var_type <- function(df = df, variable = variable) {
    x <- df[[variable]]
    if (is.character(x) || is.factor(x) || is.logical(x)) {
        typeof_x <- "discrete"
    } else {
        # TODO: Vincent: this logic is arbitrary.
        if (is.integer(x)) {
            if (length(unique(x)) > 10) {
                typeof_x <- "continuous"
            }  else {
                typeof_x <- "discrete"
            }
        } else {
            if (is.numeric(x)) {
                typeof_x <- "continuous"
            } else {
                x <- as.character(x)
                typeof_x <- "discrete"
                warning(paste0("Unrecognised variable type for ", variable,
                               ". Variable being coerced to discrete.
                                Please make sure the data provided is discrete 
                               or continuous."))
            }
        }
    }
    return(typeof_x)
}

is_mixed_data <- function(data) {
    data <- data.frame(data,check.names=T)
    class.col <- unlist(lapply(data,class))
    col.quant <- which(class.col %in% c("numeric","integer"))
    col.qual <- which(class.col %in% c("factor","character"))
    if (length(col.quant) > 0 && length(col.qual) > 0) {
        return(TRUE)
    }
    return(FALSE)
}    

is_data_continuous <- function(data) {
    data <- data.frame(data,check.names=T)
    class.col <- unlist(lapply(data,class))
    col.quant <- which(class.col %in% c("numeric","integer"))
    col.qual <- which(class.col %in% c("factor","character"))
    if (length(col.qual) == 0) {
        if (length(col.quant) > 0) {
            return(TRUE)
        }
        return(FALSE)
    } 
    if (length(col.qual) > 0) {
        return(FALSE)
    } 
}

is_data_categorical <- function(data) {
    data <- data.frame(data,check.names=T)
    class.col <- unlist(lapply(data,class))
    col.quant <- which(class.col %in% c("numeric","integer"))
    col.qual <- which(class.col %in% c("factor","character"))
    if (length(col.quant) == 0) {
        if (length(col.qual) > 0) {
            return(TRUE)
        }
        return(FALSE)
    } 
    if (length(col.quant) > 0) {
        return(FALSE)
    } 
}