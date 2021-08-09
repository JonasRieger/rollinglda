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
#' \code{\link{economy}} Example Dataset (576 articles) for testing.
#'
#' @section Constructor, Getter, Fitting: TBA ...
#'
#' @references
#' TBA
#'
#' @import data.table
#' @import stats
#' @import checkmate
#' @import ldaPrototype
"_PACKAGE"
