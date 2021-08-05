#' @title rollinglda: Construct Consistent Time Series from Textual Data
#'
#' @description RollingLDA is a rolling version of the Latent Dirichlet
#' Allocation (LDA). By an sequential approach, it enables the construction of
#' LDA-based time series of topics that are consistent with previous states of
#' LDA models. After an initial modeling, updates can be computed efficiently,
#' allowing for real-time monitoring and detection of events or structural breaks.\cr
#' For bug reports and feature requests please use the issue tracker:
#' \url{https://github.com/JonasRieger/rollinglda/issues}. Also have a look at
#' the (detailed) example at \url{https://github.com/JonasRieger/rollinglda}.
#'
#' @section Data:
#' \code{\link{reuters}} Example Dataset (91 articles from Reuters) for testing.
#'
#' @references
#' Rieger, Jonas (2020). "ldaPrototype: A method in R to get a Prototype of multiple Latent
#' Dirichlet Allocations". Journal of Open Source Software, \bold{5}(51), 2181,
#' DOI 10.21105/joss.02181, URL \url{https://doi.org/10.21105/joss.02181}.
#'
#' Rieger, Jonas, Jörg Rahnenführer and Carsten Jentsch (2020).
#' "Improving Latent Dirichlet Allocation: On Reliability of the Novel Method LDAPrototype".
#' In: \emph{Natural Language Processing and Information Systems, NLDB 2020.} LNCS 12089, pp. 118--125,
#' DOI 10.1007/978-3-030-51310-8_11, URL \url{https://doi.org/10.1007/978-3-030-51310-8_11}.
#'
#' Rieger, Jonas, Lars Koppers, Carsten Jentsch and Jörg Rahnenführer (2020).
#' "Improving Reliability of Latent Dirichlet Allocation by Assessing Its Stability using Clustering Techniques on Replicated Runs".
#' arXiv 2003.04980, URL \url{https://arxiv.org/abs/2003.04980}.
#'
#'
#' @import data.table
#' @import stats
#' @import checkmate
#' @importFrom utils combn hasName head
#' @importFrom progress progress_bar
#' @importFrom fs fs_path
#' @importFrom lda lda.collapsed.gibbs.sampler
#' @importFrom dendextend labels_colors labels_colors<- labels<-
#' @importFrom colorspace rainbow_hcl
#' @importFrom graphics plot abline
"_PACKAGE"

.getDefaultParameters = function(K){
  if (missing(K)){
    stop("Parameter K (number of modeled topics) must be set, no default!")
    #return(list(K = 100, alpha = 0.01, eta = 0.01, num.iterations = 200))
  }else{
    return(list(K = K, alpha = 1/K, eta = 1/K, num.iterations = 200))
  }
}

.defaultLimit.rel = function() 1/500
.defaultLimit.abs = function() 10
.defaultAtLeast = function() 0

# x is a LDARep or LDABatch (or a list of LDAs or single LDA object)
# returns the considered vocabulary of the first LDA object
# (useful for the case of simply having replications of the same parameters)
.defaultVocab = function(x){
  if (inherits(x, c("LDARep", "LDABatch"))){
    x = getLDA(x, job = getJob(x)$job.id[1], reduce = TRUE)
  }
  if (inherits(x, "LDA")){
    return(colnames(getTopics(x)))
  }else{
    return(colnames(getTopics(x[[1]])))
  }
}
