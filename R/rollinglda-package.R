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
#' \code{\link{economy}} Example Dataset (576 articles from Wikinews) for testing.
#'
#' @section Constructor:
#' \code{\link{as.RollingLDA}} RollingLDA objects used in this package.
#'
#' @section Getter:
#' \code{\link{getChunks}} Getter for \code{\link[=as.RollingLDA]{RollingLDA}} objects.
#'
#' @section Modeling:
#' \code{\link{RollingLDA}} Performing the method from scratch.\cr
#' \code{\link{updateRollingLDA}} Performing updates on \code{\link[=as.RollingLDA]{RollingLDA}} objects.
#'
#' @references
#' TBA
#'
#' @import data.table
#' @import checkmate
#' @import ldaPrototype
#' @importFrom utils tail
#' @importFrom tosca LDAprep
#' @importFrom stats na.omit
#' @importFrom lubridate is.Date
#' @importFrom lubridate NA_Date_
#' @useDynLib rollinglda, .registration=TRUE
"_PACKAGE"

.defaultParam = function() list(vocab.abs = 5L, vocab.rel = 0, vocab.fallback = 100L, doc.abs = 0L)
