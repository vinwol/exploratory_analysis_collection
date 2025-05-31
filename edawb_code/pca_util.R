# https://sebastianraschka.com/Articles/2015_pca_in_3_steps.html
# https://stats.stackexchange.com/questions/2691/making-sense-of-principal-component-analysis-eigenvectors-eigenvalues
#
# https://stackoverflow.com/questions/16132007/a-function-for-calculating-the-eigenvalues-of-a-matrix-in-r
# library(rootSolve)
# myeig <- function(mat){
#   myeig1 <- function(lambda) {
#     y = mat
#     diag(y) = diag(mat) - lambda
#     return(det(y))
#   }
#   myeig2 <- function(lambda){
#     sapply(lambda, myeig1)
#   }
#   uniroot.all(myeig2, c(-10, 10))
# }
# myeig(as.matrix(data))

# PCA and eigenvalues:
# https://cran.r-project.org/web/packages/matlib/vignettes/eigen-ex1.html
# http://math.mit.edu/~gs/linearalgebra/ila0601.pdf
# https://math.stackexchange.com/questions/1280050/how-do-we-find-eigenvalues-from-given-eigenvectors-of-a-given-matrix
# https://stat.ethz.ch/pipermail/r-help/2005-August/076610.html
# You have two separate problems: running PCA and getting eigenvalues. The
# first is easy to solve: use prcomp instead of princomp (which only
# exists for  historic reasons).  Function prcomp can handle cases with more columns than rows. 
# pc <- prcomp(x)
# Above I assumed that your data are called x, or you can first make x,
# say: x <- rcauchy(200); dim(x) <- c(20,10) -- which puts a funny twist
# to comments on variances and standard deviations below.
# This saves something that are called 'sdev' or standard deviations, and
# you can get values that are (proportional to) eigenvalues simply by
# taking their squares:
# ev <- pc$sdev^2
# These may be good enough for you (they would be good enough for me).
# However, if you want to exactly replicate the numbers in some other
# piece of software, you may need to multiply these by some constant. If
# you don't need this, you may stop reading here.
# The eigenvalues above are related to usual 'unbiased' variance so that
# the following results are approximately equal:
# sum(ev)
# sum(apply(x, 2, var))
# If you want to get eigenvalues related to biased estimate of variance,
# you can do
# eb <- (1-1/nrow(x))*ev
# Function princomp uses these, as do some other software, but prcomp
# works hard and carefully to get the eigenvalues it uses instead of
# biased values (that would come naturally and directly in the algorithm
# it uses). 
# Some programs relate their eigenvalues to the sum of squares, and you
# can get these by
# es <- (nrow(x) - 1) * ev
# Finally, some popular programs in ecology (your affiliation) use
# proportional eigenvalues which you can get with:
# ev/sum(ev)


# Have common interface for return value of PCA methods like in R package
# pcaMethods.
#
# References:
# https://aaronschlegel.me/principal-component-analysis-r-example.html
# https://towardsdatascience.com/pca-eigenvectors-and-eigenvalues-1f968bc6777a
# https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff
# https://sebastianraschka.com/Articles/2015_pca_in_3_steps.html
# https://www.datacamp.com/community/tutorials/pca-analysis-r
# PCA object contains the following information:
# The center point ($center), 
# scaling ($scale), 
# standard deviation(sdev) of each principal component
# The relationship (correlation or anticorrelation, etc) between the initial variables and the principal components ($rotation)
# The values of each sample in terms of the principal components ($x)
#
# sdev:
# the standard deviations of the principal components.
# loadings:
# the matrix of variable loadings, i.e. a matrix whose columns contain the eigenvectors.
# scores:
# the scores of the supplied data on the principal components.
# scores <- data %*% L, where L is the loadings
#
# PCAmixdata:
# res.pcamix$ind$coord provides the scores:
# factor coordinates (=scores) of the individuals
#
# factoMineR:
# Where do I find scores [and loadings] in res.pca?
# Scores (i.e. principal coordinates) are in res.pca$ind$coord
# The variance of the individuals' coordinates for a dimension corresponds to the eigenvalue of this dimension.
#
# old name: pcaTopn2
# data is a dataframe or matrix where the features are expected to be the columns
# Reference: https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/Principal-Component-Analysis/Principal-component-analysis-in-R/index.html
get_pca_continuous_data <- function(data=data, npc=10, scale_features=TRUE) {
  # Check if data is continuous.
  if (!is_data_continuous(data)) {
    stop('Data is not fully continuous!')
  }
  # retx:	
  # a logical value indicating whether the rotated variables should be returned.
  # center:	
  # a logical value indicating whether the variables should be shifted to be zero centered. 
  # Alternately, a vector of length equal the number of columns of x can be supplied. 
  # The value is passed to scale.
  # scale.	
  # a logical value indicating whether the variables should be scaled to have unit 
  # variance before the analysis takes place. The default is FALSE for consistency 
  # with S, but in general scaling is advisable. 
  # Alternatively, a vector of length equal the number of columns of x can be supplied. 
  # The value is passed to scale.
  # rank.	
  # optionally, a number specifying the maximal rank, i.e., maximal number of principal components to be used. 
  # Can be set as alternative or in addition to tol, useful notably when the desired 
  # rank is considerably smaller than the dimensions of the matrix.
  res <- stats::prcomp(data, retx = TRUE, center = TRUE, scale. = scale_features, rank. = npc)
  # Principal component scores: 
  pc_scores <- res$x
  # Eigenvectors which are stored as a matrix in the rotation attribute. 
  # Also called loadings: The matrix of variable loadings (columns are eigenvectors).
  eigenvectors <- res$rotation 
  # Same as: eigenvectors <- eigen(cov(as.matrix(data)))$vectors
  # Eigenvalues: 
  eigenvalues <- eigen(cov(as.matrix(data)))$values
  # standard deviation of each principal component as a measure of variance.
  pc_sdev <- res$sdev
  # In order to get the variance explained by each principal component we square the standard deviation.
  # This correlates with the eigenvalues.
  # Example data: cor(eigenvalues,pc_var): 0.9998938 if scale. = TRUE, 1 if scale = FALSE
  pc_var <- res$sdev^2
  # To compute the proportion of variance explained by each principal component, 
  # we simply divide the variance explained by each principal component by the 
  # total variance explained by all principal components.
  # Percent variance explained by each principal component.
  pc_percent_var <- pc_var/sum(pc_var)
  return(list(pc_scores=pc_scores,
              pc_var=pc_var,
              pc_sdev=pc_sdev,
              pc_percent_var=pc_percent_var,
              eigenvalues=eigenvalues,
              eigenvectors=eigenvectors))
}

# Methods:
# pcaMethods::listPcaMethods('all')
# "svd", "nipals", "rnipals", "bpca", "ppca", "robustPca", "nlpca"  
# Imputation:
# "svdImpute" 
# "llsImpute"   
# "llsImputeAll"
# pcaMethods::listPcaMethods('linear')
# "svd"   "nipals"    "rnipals"   "bpca"  "ppca"  "svdImpute" "robustPca"
# pcaMethods::listPcaMethods('nonlinear'): "nlpca"
# svd: 
# a fast method which is also the standard method in R but which is not applicable for data with missing values.
# nipals: 
# an iterative fast method which is applicable also to data with missing values.
# rnipals:
# Same as nipals but implemented in R
# ppca: 
# Probabilistic PCA which is applicable also on data with missing values.
# Missing value estimation is typically better than NIPALS but also slower to compute and uses more memory.
# A port to R of the implementation by Jakob Verbeek.
# bpca: 
# Bayesian PCA which performs very well in the presence of missing values but is slower than PPCA.
# A port of the matlab implementation by Shigeyuki Oba.
# robustPca:
# This is a PCA implementation robust to outliers in a data set. It can also handle missing values, it is
# however NOT intended to be used for missing value estimation. As it is based on robustSVD we will
# get an accurate estimation for the loadings also for incomplete data or for data with outliers.
# nlpca: 
# Non-linear PCA which can find curves in data and in presence of such can perform accurate missing value estimation.
# Matlab port of the implementation by Mathias Scholz.
# Imputing missing values using the pcaMethods package:
# https://www.bioconductor.org/packages/release/bioc/vignettes/pcaMethods/inst/doc/missingValues.pdf
# methods:svd,nipals,ppca,bpca,nlpca
# Reference: https://www.bioconductor.org/packages/release/bioc/manuals/pcaMethods/man/pcaMethods.pdf
# scale:
# scale = c("none", "pareto", "vector", "uv")
# "UV": unit variance a = a/σa 
# "vector": vector normalisation b = b/||b||
# "pareto": sqrt UV
# to indicate which scaling should be used
# to scale the matrix with a variables and b samples. Can also be a vector of scales
# which should be used to scale the matrix. NULL value is interpreted as "none".
get_non_svd_pca_continuous_data <- function(data=data,
                                            method=method,
                                            npc=10,
                                            center=TRUE, 
                                            scale='vector',
                                            maxSteps=100, 
                                            maxIterations=100, 
                                            seed=123) {
  # Check if data is continuous.
  if (!is_data_continuous(data)) {
    stop('Data is not fully continuous!')
  }
  # 8 PCA methods:
  all_methods <- c("svd","nipals","rnipals","bpca","ppca","svdImpute","robustPca","nlpca")
  if (method %notin% all_methods) {
    stop(paste0('Did not find the following method: ',method))
  }
  if (method == 'nlpca') {
    res <- pcaMethods::pca(data, method=method, nPcs=npc, center=center, scale=scale, maxSteps=maxSteps)
  } else if (method == 'ppca') {
    res <- pcaMethods::pca(data, method=method, nPcs=npc, center=center, scale=scale, maxIterations=maxIterations, seed=seed)
  } else {
    res <- pcaMethods::pca(data, method=method, nPcs=npc, center=center, scale=scale)
  }
  pc_scores <- adjust_dimension_names(res@scores)
  eigenvectors <- res@loadings
  pc_var <- res@sDev^2
  eigenvalues <- pc_var # We can do this since it is highly correlated.
  pc_percent_var <- pc_var/sum(pc_var)
  pc_sdev <- res@sDev
  return(list(pc_scores=pc_scores,
              pc_var=pc_var,
              pc_sdev=pc_sdev,
              pc_percent_var=pc_percent_var,
              eigenvalues=eigenvalues,
              eigenvectors=eigenvectors))
}

# https://core.ac.uk/download/pdf/6303121.pdf
get_pca_categorical_data <- function(data=data, npc=10) {
  # Check if data is categorical.
  if (!is_data_categorical(data)) {
    stop('Data is not fully categorical!')
  }
  # ncp	
  # number of dimensions kept in the results (by default 5)
  # graph	
  # boolean, if TRUE a graph is displayed
  res <- FactoMineR::FAMD(data, ncp=npc, graph=FALSE)
  pc_scores <- adjust_dimension_names(res$ind$coord)
  eigenvectors <- res3$ind$contrib # TODO: Check that these are the eignevectors!
  pc_var <- res$eig[,1]
  eigenvalues <- pc_var
  pc_percent_var <- res$eig[,2]
  pc_sdev <- sqrt(pc_var)
  # Below is the same as pc_var:
  # df <- factoextra::get_eig(res)
  # rownames(df) <- NULL
  # eigenvalues <- df[,"eigenvalue"]
  return(list(pc_scores=pc_scores,
              pc_var=pc_var,
              pc_sdev=pc_sdev,
              pc_percent_var=pc_percent_var,
              eigenvalues=eigenvalues,
              eigenvectors=eigenvectors))
}

get_pca_mixed_data <- function(data=data, npc=10) {
  split <- split_up_data(data)
  continuous_data <- split$continuous_data
  categorical_data <- split$categorical_data
  res <- PCAmixdata::PCAmix(X.quanti=continuous_data,
                            X.quali=categorical_data,
                            ndim=npc,
                            rename.level=TRUE,
                            graph=FALSE)
  pc_scores <- adjust_dimension_names(res$ind$coord)
  eigenvectors <- res$sqload
  pc_var <- res$eig[,1]
  eigenvalues <- pc_var
  pc_percent_var <- res$eig[,2]
  pc_sdev <- sqrt(pc_var)
  return(list(pc_scores=pc_scores,
              pc_var=pc_var,
              pc_sdev=pc_sdev,
              pc_percent_var=pc_percent_var,
              eigenvalues=eigenvalues,
              eigenvectors=eigenvectors))
}

get_principle_components <- function(pca_data = NULL, npc = NULL) {
  if (is.null(npc) || npc > dim(pca_data$pc_scores)[2]) {
    return(data.table(pca_data$pc_scores))
  } 
  return(data.table(pca_data$pc_scores[, 1:npc]))
}

get_ica_principle_components <- function(ica_data = NULL, npc = NULL) {
  if (is.null(npc) || npc > dim(ica_data$ic_scores)[2]) {
    return(data.table(ica_data$ic_scores))
  } 
  return(data.table(ica_data$ic_scores[, 1:npc]))
}

get_fac_principle_components <- function(fac_data = NULL, npc = NULL) {
  if (is.null(npc) || npc > dim(fac_data$fac_scores)[2]) {
    return(data.table(fac_data$fac_scores))
  } 
  return(data.table(fac_data$fac_scores[, 1:npc]))
}

# Using ICA: Independent Component Analysis from R Package fasICA.
# Other R packages: ica
# fastICA(X, 
#         n.comp, 
#         alg.typ = c("parallel","deflation"), # default: 'parallel'
#         fun = c("logcosh","exp"), # default is logcosh???
#         alpha = 1.0, # constant in range [1, 2] used in approximation to neg-entropy when fun == "logcosh" 
#         method = c("R","C"), # if method == "R" then computations are done exclusively in R (default).
#         row.norm = FALSE, # a logical value indicating whether rows of the data matrix X should be standardized beforehand.
#         maxit = 200, 
#         tol = 1e-04, 
#         verbose = FALSE,
#         w.init = NULL)
get_ica_continuous_data <- function(data=data, npc=10, scale_features=TRUE) {
  res <- fastICA::fastICA(X=data, 
                          n.comp=npc,
                          row.norm=FALSE)
                          #row.norm=scale_features) # TODO: Vincent: check if this equivalent to scaling!
  pc_scores <- as.data.frame(res$X %*% res$K)
  ic_scores <- adjust_ic_names(as.data.frame(res$S))
  # TODO: Calculate variance.
  # See here: 
  # In the case of FastICA, the variance explained by the ICs collectively is 
  # exactly the same as the variance explained by the principal components (collectively) from which they are derived.
  return(list(pc_scores=pc_scores,
              ic_scores=ic_scores))
}

# R package mixOmics.
# Performs independent principal component analysis on the given data matrix, 
# a combination of Principal Component Analysis and Independent Component Analysis.
# Algorithm 
# 1. The original data matrix is centered.
# 2. PCA is used to reduce dimension and generate the loading vectors.
# 3. ICA (FastICA) is implemented on the loading vectors to generate independent loading vectors.
# 4. The centered data matrix is projected on the independent loading vectors to obtain the independent principal components.
get_ipca_continuous_data <- function(data=data, npc=10, scale_features=TRUE) {
  res <- mixOmics::ipca(X=data,
                        ncomp=npc,
                        scale=scale_features,
                        mode="parallel") # mode: "parallel", "deflation"
  ic_scores <- res$x
  loadings <- res$loadings$X # the independent loading vectors, similar to eigenvectors
  ic_var <- res$explained_variance
  ic_sdev <- sqrt(ic_var)
  ic_percent_var <- ic_var/sum(ic_var)
  return(list(ic_scores=ic_scores,
              ic_var=ic_var,
              ic_sdev=ic_sdev,
              ic_percent_var=ic_percent_var,
              loadings=loadings))
}  

# stats::factanal()
# scores = c("none", "regression", "Bartlett")
# rotation...
# psych::fa()
# fm: methods:
# "minres","uls","ols","wls","gls","pa","ml","minchi","minrank","alpha","old.min" 
# rotate	
# "none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" and 
# "bifactor" are orthogonal rotations. "Promax", "promax", "oblimin", "simplimax", 
# "bentlerQ, "geominQ" and "biquartimin" and "cluster" are possible oblique transformations of the solution. 
# The default is to do a oblimin transformation, although versions prior to 2009 defaulted to varimax.
get_factors_continuous_data <- function(data=data, npc=10, method="minres", rotate="oblimin") {
  # res <- stats::factanal(x=data,
  #                       factors=npc,
  #                       scores=scores)
  # loadings <- res$loadings
  # scores <- res$scores
  res <- psych::fa(r=data,
                   nfactors=npc,
                   fm=method,
                   rotate=rotate) 
  scores <- res$scores # factor scores 
  values <- res$values # Eigenvalues of the common factor solution
  loadings <- res$loadings  # loadings
  return(list(fac_scores=scores,
              fac_values=values,
              fac_loadings=loadings))
}  




