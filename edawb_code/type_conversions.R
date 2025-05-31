# Type conversions.

convert_time_info_to_character <- function(data) {
    inx <- sapply(data, function(x) inherits(x, "Date") || inherits(x, "POSIXt") || inherits(x, "POSIXct"))
    data[inx] <- lapply(data[inx], as.character)
    return(data)
}