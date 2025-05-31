# dependent_vars has the features as columns here!
do_variance_analysis <- function(dependent_vars = dependent_vars,     
                                 independent_vars = independent_vars, 
                                 pc_scores = pc_scores,
                                 rsq_threshold_pct = 25,
                                 whitelist = NULL,
                                 blacklist = NULL) {
  
  top_explanatory_vars_df <- data.frame()
  rsq_df <- data.frame()

  if (is.null(whitelist)) {
    potential_cfs <- names(independent_vars)
  } else {
    potential_cfs <- whitelist
  }
  
  skipped_vars <- vector(mode="character", length=length(potential_cfs))
  skipped_reasons <- vector(mode="character", length=length(potential_cfs))
  processed_vars <- vector(mode="character", length=length(potential_cfs))
  var_counter <- 0
  
  for (cf_var in potential_cfs) {
    # Skip all features in blacklist.
    if (cf_var %in% blacklist) {
      skipped_vars <- c(skipped_vars,cf_var)
      skipped_reasons <- c(skipped_reasons,'part of black list')
      next
    }
    var_vals <- independent_vars[[cf_var]]
    # check how many NAs.
    # TODO: Vincent: check line of code below.
    if (sum(is.na(var_vals)) > nrow(dependent_vars)) {
      skipped_vars <- c(skipped_vars,cf_var)
      skipped_reasons <- c(skipped_reasons,paste0('too many NAs: ',sum(is.na(var_vals))))
      next
    }
    # CHECK for correct data type and convert if necessary to factor.
    if (get_var_type(independent_vars, cf_var) == "discrete") {
      # skip if number of levels are more (number of cells)/2
      if (nlevels(var_vals) > (nrow(dependent_vars)/10)) {
        skipped_vars <- c(skipped_vars,cf_var)
        skipped_reasons <- c(skipped_reasons,paste0('too many levels: ',nlevels(var_vals)))
        next
      }
      tmp_rsq_df <- get_r_squared(independent_vars = independent_vars,
                                  pc_scores = pc_scores,
                                  variable = cf_var)
    }
    # CHECK for integers.
    if (is.numeric(var_vals) || is.integer(var_vals)) {
      if (!is.na(var(var_vals)) && var(var_vals) == 0) {
        skipped_vars <- c(skipped_vars,cf_var)
        skipped_reasons <- c(skipped_reasons,'variance is zero')
        next
      }
      tmp_rsq_df <- get_r_squared(independent_vars = independent_vars,
                                  pc_scores = pc_scores,
                                  variable = cf_var)
    }
    # FILL RSQ MATRIX & APPLY FILTER.
    if (!is.null(tmp_rsq_df) && !is.na(tmp_rsq_df)) {
      var_counter <- var_counter + 1
      processed_vars[var_counter] <- cf_var
      rsq_df <- rbind(rsq_df, data.frame(t(tmp_rsq_df), row.names = cf_var))
      tmp_rsq_df <- tmp_rsq_df[order(-tmp_rsq_df)]
      if (!is.na(tmp_rsq_df[1]) && ((tmp_rsq_df[1] * 100) > rsq_threshold_pct)) {
        top_explanatory_vars_df <- rbind(top_explanatory_vars_df,
                                         data.frame(PC=names(tmp_rsq_df[1]),
                                                    RSQ=tmp_rsq_df[1],
                                                    row.names=cf_var))
      }
    } else {
      skipped_vars <- c(skipped_vars,cf_var)
      skipped_reasons <- c(skipped_reasons,'could not calculate R squared')
    }
  }
  process_operations <- rep('R squared calculated',length(processed_vars))
  return(list(rsq_df=rsq_df,
              top_explanatory_vars_df=top_explanatory_vars_df,
              processed_vars=processed_vars[processed_vars != ""],
              process_operations=process_operations,
              skipped_vars=skipped_vars[skipped_vars != ""],
              skipped_reasons=skipped_reasons[skipped_reasons != ""]))
}
