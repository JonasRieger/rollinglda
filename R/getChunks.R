#' @title Getter for RollingLDA
#'
#' @description
#' Returns the corresponding element of a \code{\link{RollingLDA}} object.
#'
#' @param x [\code{named list}]\cr
#' \code{\link{RollingLDA}} object.
#' @param names [\code{character}]\cr
#' Names of the requested items (dates or docs). Default are all names.
#' @param inverse [\code{logical(1)}]\cr
#' Should all items except those with the given names be returned? Default is \code{FALSE}.
#' @param job not implemented for \code{\link{RollingLDA}} object.
#' See \code{\link[ldaPrototype:getSCLOP]{getLDA}}
#' @param reduce not implemented for \code{\link{RollingLDA}} object.
#' See \code{\link[ldaPrototype:getSCLOP]{getLDA}}
#' @param all not implemented for \code{\link{RollingLDA}} object.
#' See \code{\link[ldaPrototype:getSCLOP]{getLDA}}
#'
#' @family RollingLDA functions

#' @export getChunks
getChunks = function(x) UseMethod("getChunks")

#' @export
getChunks.RollingLDA = function(x){
  x$chunks
}

#' @rdname getChunks
#' @export getNames
getNames = function(x) UseMethod("getNames")

#' @export
getNames.RollingLDA = function(x){
  names(x$dates)
}

#' @rdname getChunks
#' @export getDates
getDates = function(x, names, inverse) UseMethod("getDates")

#' @export
getDates.RollingLDA = function(x, names, inverse = FALSE){
  assert_flag(inverse)
  if (missing(names)) names = getNames(x)
  assert_character(names, any.missing = FALSE, min.len = 1)
  if (inverse) names = setdiff(getNames(x), names)
  x$dates[na.omit(match(names, names(x$dates)))]
}

#' @rdname getChunks
#' @export getDocs
getDocs = function(x, names, inverse) UseMethod("getDocs")

#' @export
getDocs.RollingLDA = function(x, names, inverse = FALSE){
  assert_flag(inverse)
  if (missing(names)) names = getNames(x)
  assert_character(names, any.missing = FALSE, min.len = 1)
  if (inverse) names = setdiff(getNames(x), names)
  x$docs[na.omit(match(names, names(x$docs)))]
}

#' @rdname getChunks
#' @export getVocab
getVocab = function(x) UseMethod("getVocab")

#' @export
getVocab.RollingLDA = function(x){
  x$vocab
}

#' @rdname getChunks
#' @export
getLDA.RollingLDA = function(x, job, reduce, all){
  x$lda
}

#' @rdname getChunks
#' @export
getID.RollingLDA = function(x){
  x$id
}

#' @rdname getChunks
#' @export
getParam.RollingLDA = function(x){
  x$param
}
