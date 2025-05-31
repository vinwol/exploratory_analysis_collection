remove_rows_with_na <- function(df) {
  return(df[complete.cases(df), ])
}

remove_columns_with_na <- function(df) {
  return(df[ ,complete.cases(df)])
}

remove_all_nas <- function(df) {
  return(na.omit(df))
}

apply_log_transformation <- function(independent_vars = independent_vars,
                                     white_list = NULL,
                                     black_list = NULL,
                                     log_func = 'log10') {
  var_list <- NULL
  if (is.null(white_list)) {
    var_list <- names(independent_vars)
  } else {
    var_list <- white_list
  }

  processed_vars <- vector(mode="character",length=length(var_list))
  process_operations <- vector(mode="character",length=length(var_list))
  var_counter <- 0

  for (var in var_list) {
    if (var %in% black_list) {
      next
    }
    if ((is.numeric(independent_vars[[var]]) ||
         is.integer(independent_vars[[var]])) &&
        !is.na(var(independent_vars[[var]]))) {

      # account for zeros before applying log
      idx.zeros <- independent_vars[[var]] == 0
      independent_vars[[var]][idx.zeros] <- 1

      var_counter <- var_counter + 1

      if (log_func == 'log') {
        processed_vars[var_counter] <- var
        process_operations[var_counter] <- 'ln transformation'
        independent_vars[[var]] <- log(independent_vars[[var]])
      } else if (log_func == 'log2') {
        processed_vars[var_counter] <- var
        process_operations[var_counter] <- 'log2 transformation'
        independent_vars[[var]] <- log2(independent_vars[[var]])
      } else if (log_func == 'log10') {
        processed_vars[var_counter] <- var
        process_operations[var_counter] <- 'log10 transformation'
        independent_vars[[var]] <- log10(independent_vars[[var]])
      } else {
        stop('Did not find suitable log transformation function!')
      }
    }
  }
  return(list(independent_vars=independent_vars,
              processed_vars=processed_vars[processed_vars != ""],
              process_operations=process_operations[process_operations != ""]))
}

apply_char_to_fact <- function(independent_vars = independent_vars,
                               white_list = NULL,
                               black_list = NULL) {
  var_list <- NULL
  if (is.null(white_list)) {
    var_list <- names(independent_vars)
  } else {
    var_list <- white_list
  }
  processed_vars <- vector(mode="character",length=length(var_list))
  process_operations <- vector(mode="character",length=length(var_list))
  var_counter <- 0
  for (var in var_list) {
    if (var %in% black_list) {
      next
    }
    if (get_var_type(independent_vars, var) == "discrete") {
      if (is.factor(independent_vars[[var]])) {
        next
      }
      processed_vars[var_counter] <- var
      process_operations[var_counter] <- 'char to factor transformation'
      independent_vars[[var]] <- as.character(independent_vars[[var]])
      independent_vars[[var]][which(is.na(independent_vars[[var]]))] <- "UNKOWN"
      independent_vars[[var]] <- as.factor(independent_vars[[var]])
    }
  }
  return(list(independent_vars=independent_vars,
              processed_vars=processed_vars[processed_vars != ""],
              process_operations=process_operations[process_operations != ""]))
}

# dependent_vars has the features column wise!
transform_independent_vars <- function(independent_vars = independent_vars,
                                       dependent_vars = dependent_vars,
                                       white_list = NULL,
                                       black_list = NULL) {

  var_list <- NULL
  if (is.null(white_list)) {
    var_list <- names(independent_vars)
  } else {
    var_list <- white_list
  }

  for (var in var_list) {

    # Skip all features in black_list.
    if (var %in% black_list) {
      next
    }

    # CHECK if variation is 0.
    if ((is.numeric(independent_vars[[var]]) ||
         is.integer(independent_vars[[var]])) &&
        !is.na(var(independent_vars[[var]]))) {
      if (var(independent_vars[[var]]) == 0) {
        # TODO: Vincent: report back on variables which have zero variance.
        next
      }
    }

    # check how many NAs.
    # TODO: Vincent: check line of code below.
    if (sum(is.na(independent_vars[[var]])) > nrow(dependent_vars)) {
      next
    }

    # CHECK for correct data type and convert if necessary to factor.
    if (get_var_type(independent_vars, var) == "discrete") {
      # ATTENTION also int values with less than 10 different values will be
      # converted --> no problem, makes sense
      independent_vars[[var]] <- as.character(independent_vars[[var]])
      independent_vars[[var]][which(is.na(independent_vars[[var]]))] <- "UNKOWN"
      independent_vars[[var]] <- as.factor(independent_vars[[var]])

      # skip if number of levels are more (number of cells)/2
      if (nlevels(independent_vars[[var]]) > (nrow(dependent_vars)/10)) {
        next
      }
    }

    # CHECK for integers
    if (is.numeric(independent_vars[[var]]) ||
        is.integer(independent_vars[[var]])) {

      # print(var)
      # log transformation to numeric
      #var.rsq <- paste0("log10_", var)

      # account for zeros before applying log
      idx.zeros <- independent_vars[[var]] == 0
      independent_vars[[var]][idx.zeros] <- 1

      # TODO: Vincent: To log transform should be an option.
      #independent_vars[[var.rsq]] <- log10(independent_vars[[var]])
      independent_vars[[var]] <- log10(independent_vars[[var]])
    }
  }
  return(independent_vars)
}

