#' @title RollingLDA Object
#'
#' @description Constructor for RollingLDA objects used in this package.
#'
#' @details
#' If you call \code{as.RollingLDA} on an object \code{x} which already is of
#' the structure of an \code{RollingLDA} object (in particular a \code{RollingLDA}
#' object itself), the additional arguments \code{id, param, ...}
#' may be used to override the specific elements.
#'
#' @family RollingLDA functions
#'
#' @param x [\code{named list}]\cr
#' Output from \code{\link[lda]{lda.collapsed.gibbs.sampler}}. Alternatively each
#' element can be passed for individual results. Individually set elements
#' overwrite elements from \code{x}.
#' @param id TBA
#' @param lda TBA
#' @param docs TBA
#' @param dates TBA
#' @param vocab TBA
#' @param chunks TBA
#' @param param [\code{named list}]\cr
#' Parameters of the function call \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' List always should contain names "K", "alpha", "eta" and "num.iterations".
#' @param obj [\code{R} object]\cr
#' Object to test.
#' @param verbose [\code{logical(1)}]\cr
#' Should test information be given in the console?
#' @return [\code{named list}] \code{\link{RollingLDA}} object.
#'
#' @export as.RollingLDA
as.RollingLDA = function(x){
  if(FALSE){
    if (!missing(x)){
      if (missing(param) && hasName(x, "param")) param = x$param
      if (missing(assignments)) assignments = x$assignments
      if (missing(topics)) topics = x$topics
      if (missing(document_sums)) document_sums = x$document_sums
      if (missing(document_expects)) document_expects = x$document_expects
      if (missing(log.likelihoods)) log.likelihoods = x$log.likelihoods
    }
  }
  cat("not implemented")
}

#' @rdname as.RollingLDA
#' @export
is.RollingLDA = function(obj, verbose = FALSE){
  assert_flag(verbose)
  cat("not implemented")
  TRUE
}

#' @export
print.RollingLDA = function(x, ...){
  if(FALSE){
    val = .getValues.LDA(x)
    elements = paste0("\"", names(which(!sapply(x, is.null))), "\"")
    cat(
      "LDA Object with element(s)\n",
      paste0(elements, collapse = ", "), "\n ",
      val[1], " Texts with mean length of ", round(val[2], 2), " Tokens\n ",
      val[3], " different Words\n ",
      paste0(paste0(names(getParam(x)), ": ", round(unlist(getParam(x)), 4)), collapse = ", "),
      "\n\n", sep = ""
    )
  }
  cat("RollingLDA Object")
}
