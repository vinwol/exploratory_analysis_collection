# Links:
# How to calculate a varinace measure for categorical varibales:
# Shannon's entropy
# Total sum of squares
# https://stats.stackexchange.com/questions/221332/variance-of-a-distribution-of-multi-level-categorical-data
# https://math.stackexchange.com/questions/1135507/variance-measure-for-categorical-data
# https://en.wikipedia.org/wiki/Total_sum_of_squares
# http://www.dwoll.de/r/ssTypes.php
# https://stats.stackexchange.com/questions/174861/calculating-regression-sum-of-square-in-r
# https://stats.stackexchange.com/questions/166744/how-sum-of-squares-is-calculated-by-r-anova-function-fo-non-factor-variables-in
# Factorial ANOVA:
# https://learningstatisticswithr.com/book/anova2.html
# https://gaopinghuang0.github.io/2017/11/08/factorial-ANOVA-notes-and-R-code
# Entropy:
# https://journals.ametsoc.org/view/journals/atot/35/5/jtech-d-17-0056.1.xml
# https://www.hindawi.com/journals/complexity/2017/8715605/

get_custom_mcfadden_pseudo_rsq <- function(data) {
  # MCFadden pseudo RSQ.
  # nnet::multinom(): Fits multinomial log-linear models via neural networks.
  fit <- nnet::multinom(data$x ~ data$y, MaxNWts=100000, trace=FALSE)
  nullfit <- nnet::multinom(data$x ~ 1, MaxNWts=100000, trace=FALSE)
  mcfadden_val <- (1 - (stats::logLik(fit)/stats::logLik(nullfit)))
  return(mcfadden_val)
}

get_custom_adjusted_mcfadden_pseudo_rsq <- function(data) {
  # Adjusted MCFadden pseudo RSQ.
  # nnet::multinom(): Fits multinomial log-linear models via neural networks.
  fit <- nnet::multinom(data$x ~ data$y, MaxNWts=100000, trace=FALSE)
  nullfit <- nnet::multinom(data$x ~ 1, MaxNWts=100000, trace=FALSE)
  # adjusted McFadden pseuo rsq.
  mcfadden_val <- (1 - ((stats::logLik(fit)-fit$edf)/stats::logLik(nullfit)))
  return(mcfadden_val)
}

# Source: https://stats.stackexchange.com/questions/221332/variance-of-a-distribution-of-multi-level-categorical-data
# range: 0 to infinity
get_shannon_entropy_for_categorical_var <- function(cat.vect) {
  px  = table(cat.vect)/length(cat.vect)
  lpx = log(px, base=2)
  ent = -sum(px*lpx)
  return(ent)
}

# Source: https://stats.stackexchange.com/questions/221332/variance-of-a-distribution-of-multi-level-categorical-data
# range: [0-1]
get_sum_of_squared_probabilities <- function(cat.vect) {
  px   = table(cat.vect)/length(cat.vect)
  spx2 = sum(px^2)
  return(spx2)
}

#' Calculate RSQ value wrapper function
#'
#' This function allows you to calculate the RSQ values per principle component (pc).
#' @param object DGEList object
#' @param pca data.table of the principle components you would like to investigate
#' @param variable the variable from the object$samples data.frame for which the RSQ value will be calculated
#' @return RSQ value of variable and selected PCs.
#' @keywords RSQ
#' @export
#' @examples
#' calculates_RSQ()
#' old name: calculates_RSQ2
# calculate_r_squared <- function(independent_vars = independent_vars,
#                                 pca = pca,
#                                 variable = variable) {
# 
#   if (!(variable %in% colnames(independent_vars))) {
#     print(paste0("Variable ",variable," not found in independent_vars. Please make sure it exists."))
#     return(NULL)
#   }
# 
#   x <- independent_vars[, variable]
#   x_na <- is.na(x)
#   x <- x[!x_na]
# 
#   if (length(unique(x)) <= 1) {
#     print(paste0("Variable ",variable," has only one unique value, so cannot determine important principal components."))
#     return(NULL)
#   }
#   typeof_x <- get_var_type(independent_vars, variable)
#   if (typeof_x == "discrete") {
#     x_int <- as.factor(x)
#     design <- model.matrix( ~ x_int)
#   } else {
#     design <- model.matrix( ~ x)
#   }
#   pca_r_squared <- calculate_r_squared_value(y = pca[!x_na,], design = design) # TRANSPOSITION!!!!!!!!!!
#   names(pca_r_squared) <- colnames(pca)
#   return(pca_r_squared)
# }

#' Calculate RSQ values
#'
#' This function allows you to calculate the RSQ.
#' @param y vector of principale component
#' @param design model.matrix e.g.  ~ variable
#' @return ssr value
#' @keywords RSquared
#' @export
#' @examples
#' old name: getRSquared()
#' Parameter y: untranslated: columns: PCs, rows: dep. variables.
calculate_r_squared_value <- function(y = y, design = design) {
  ## Mean-centre rows to get correct R-squared values with the limma formula below.
  # Applying scale() is column wise where the PCs are in the columns!
  y0 <- t(scale(y, center = TRUE, scale = FALSE)) 
  ## Get linear model fit
  fit <- limma::lmFit(y0, design = design)
  ## Compute total sum of squares
  sst <- rowSums(y0 ^ 2)
  ## Compute residual sum of squares
  ssr <- sst - fit$df.residual * fit$sigma ^ 2
  ssr <- (ssr/sst)
  return(ssr)
}

get_r_squared <- function(independent_vars = independent_vars, 
                          pc_scores = pc_scores, # pca scores from dependent vars.
                          variable = NULL) {
  if (is.null(independent_vars)) {
    print("The supplied 'independent_vars' argument is NULL!")
    return(NULL)
  }
  if (dim(independent_vars)[1] == 0 || dim(independent_vars)[2] == 0) {
    print("The supplied 'independent_vars' does not have full dimensions!")
    return(NULL)
  }
  if (!(variable %in% colnames(independent_vars))) {
    print(paste0("Variable ",variable," not found in colnames(independent_vars)."))
    return(NULL)
  }
  x <- independent_vars[[variable]]
  x_na <- is.na(x)
  x <- x[!x_na]
  if (length(unique(x)) <= 1) {
    print(paste0("Variable ",variable," has only one unique value, so cannot determine important principal components."))
    return(NULL)
  }
  typeof_x <- get_var_type(independent_vars, variable)
  if (typeof_x == "discrete") {
    if (is.factor(x)) {
      design <- model.matrix(~x)
    } else {
      design <- model.matrix(~as.factor(x))
    }
  } else {
    design <- model.matrix(~x)
  }
  r_squared <- calculate_r_squared_value(pc_scores[!x_na, ], design) 
  names(r_squared) <- colnames(pc_scores)
  return(r_squared)
}

#' Run correlation analysis between confounding variables
#'
#' This functions identifies correlated variables in object$samples and PCA.
#' @param object DGEList object
#' @param cor_vars list features to correlate
#' @param whitelist list must-have features to correlate
#' @param blacklist list features will be excluded
#' @keywords RSQ
#' @return cf_rsq_df matrix Matrix of r2 correlation values between feartures
#' @export
#' @examples
#' cf.corr.analysis()
#
# MCFadden pseudo RSQ.
# nnet::multinom(): Fits multinomial log-linear models via neural networks.
# Important parameters for nnet::multinom(), with some of them 
# relating to nneet(): 
# weights	
# (case) weights for each example – if missing defaults to 1.
# size	
# number of units in the hidden layer. Can be zero if there are skip-layer units.
# entropy	
# switch for entropy (= maximum conditional likelihood) fitting. 
# Default by least-squares.
# softmax	
# switch for softmax (log-linear model) and maximum conditional likelihood fitting. 
# linout, entropy, softmax and censored are mutually exclusive.
# MaxNWts	
# The maximum allowable number of weights. There is no intrinsic limit in the code, 
# but increasing MaxNWts will probably allow fits that are very slow and time-consuming.
# Reference:
# https://www.rdocumentation.org/packages/DescTools/versions/0.99.42/topics/PseudoR2
# model object: glm, polr, multinom
# method:
# "McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "AldrichNelson", 
# "VeallZimmermann", "Efron", "McKelveyZavoina", "Tjur", 
# "AIC", "BIC", "logLik", "logLik0", "G2".
# Or "all" which runs all methods, omitted here!
do_corr_analysis <- function(independent_vars = independent_vars, 
                             cor_vars = cor_vars,
                             model = model,
                             method = method,
                             whitelist = NULL,
                             blacklist = NULL) {
  
  all_methods <- c("McFadden","McFaddenAdj","CoxSnell","Nagelkerke","AldrichNelson",
                   "VeallZimmermann","Efron","McKelveyZavoina","Tjur","AIC",
                   "BIC","logLik","logLik0","G2","Custom_McFadden","Custom_Adj_McFadden")
  
  all_models <- c('glm','multinom','polr')
  
  if (method %notin% all_methods) {
    stop(paste0('Could not find method ',method))
  } 
  
  if (model %notin% all_models) {
    stop(paste0('Could not find model ',model))
  } 

  cor_vars <- unique(union(cor_vars, whitelist))
  cor_vars <- setdiff(cor_vars, blacklist)
  
  if (is.null(cor_vars)) {
    cor_data <- independent_vars
  } else{
    cor_data <- independent_vars %>% dplyr::select(tidyselect::all_of(cor_vars))
  }
  
  #### fill CF RSQ  matrix ####
  # result means: how much is the variable in the row influenced by the
  # variables of the columns (in case of cont vs cat it is symmetric,
  # otherwise because of pseudo rsq in cat vs cat it is not symmetric)
  cf_rsq_df <- data.frame()

  for (var1 in colnames(cor_data)) {
    x <- cor_data[[var1]]
    tmp.pvals <- numeric()
    for (var2 in colnames(cor_data)) {
      #print(paste0(var1,"<<>>",var2))
      if(var1 == var2) {
        tmp.pvals <- c(tmp.pvals, 1)
      } else {
        y <- cor_data[[var2]]
        dat <- data.frame(x=x,y=y)
        dat <- na.omit(dat)
        if (is.factor(dat$x) && is.factor(dat$y)) {
          if (model == 'glm') {
            # stats::glm()
            fit <- glm(dat$x ~ dat$y, 
                       data = dat, 
                       family = binomial, 
                       control = list(maxit = 100)) # TODO Vincent: Make this a parameter.
            pseudo_r2_val <- DescTools::PseudoR2(fit, which=method)[[1]]
          } 
          if (model == 'multinom') {
            if (method == 'Custom_McFadden') {
              pseudo_r2_val <- get_custom_mcfadden_pseudo_rsq(dat)
            } else if (method == 'Custom_Adj_McFadden') {
              pseudo_r2_val <- get_custom_adjusted_mcfadden_pseudo_rsq(dat)
            } else {
              # nnet::multinom()
              fit <- multinom(dat$x ~ dat$y, 
                              MaxNWts = 100000, 
                              model = TRUE, 
                              trace = FALSE)
              pseudo_r2_val <- DescTools::PseudoR2(fit, which=method)[[1]]
            }  
          }
          if (model == 'polr') {
            # MASS::polr()
            fit <- polr(dat$x ~ dat$y, 
                        data = dat, 
                        Hess = TRUE)
            pseudo_r2_val <- DescTools::PseudoR2(fit, which=method)[[1]]
          }
          tmp.pvals <- c(tmp.pvals, pseudo_r2_val)
        }
        if (get_var_type(independent_vars, var1) == "continuous" &&
            get_var_type(independent_vars, var2) == "continuous") {
          design <- model.matrix( ~ dat$y)
          tmp.pvals <- c(tmp.pvals, calculate_r_squared_value(dat$x, design)) 
        }
        if ((get_var_type(independent_vars, var1) == "discrete" &&
             get_var_type(independent_vars, var2) == "continuous")) {
          design <- model.matrix( ~ dat$x)
          tmp.pvals <- c(tmp.pvals, calculate_r_squared_value(dat$y, design)) 
        }
        if ((get_var_type(independent_vars, var2) == "discrete" &&
             get_var_type(independent_vars, var1) == "continuous")) {
          design <- model.matrix( ~ dat$y)
          tmp.pvals <- c(tmp.pvals, calculate_r_squared_value(dat$x, design)) 
        }
      }
    }
    cf_rsq_df <- rbind(cf_rsq_df, data.frame(t(tmp.pvals), row.names = var1)) 
  }
  colnames(cf_rsq_df) <- colnames(cor_data)
  rownames(cf_rsq_df) <- colnames(cor_data)
  #cf_rsq_mat <- as.matrix(cf_rsq_df)
  return(cf_rsq_df)
}

get_r_squared_for_features <- function(independent_vars=independent_vars, 
                                       pc_scores=pc_scores, 
                                       features=features) {
  r_squared_list <- vector("list", length = nrow(features))
  counter <- 0
  for (var in rownames(features)) {
    counter <- counter + 1
    r_squared_list[[counter]] <- get_r_squared(independent_vars = independent_vars,
                                               pc_scores = pc_scores,
                                               variable = var)
  }
  return(r_squared_list)
}

