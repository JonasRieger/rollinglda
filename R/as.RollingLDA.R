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
#' @param id [\code{character(1)}]\cr
#' Name for the computation/model.
#' @param lda [\code{named list}]\cr
#' \code{\link{LDA}} object.
#' @param docs [\code{named list}]\cr
#' Texts in a preprocessed format. See \code{\link[tosca]{LDAprep}}.
#' @param dates [\code{(un)named Date}]\cr
#' Dates of the texts. If unnamed, it must match the order of docs.
#' @param vocab [\code{character}]\cr
#' Vocabularies.
#' @param chunks [\code{data.table}]\cr
#' with specifications for each model chunk
#' \describe{
#'   \item{\code{chunk.id}}{[\code{integer}] bla}
#'   \item{\code{start.date}}{[\code{Date}] bla}
#'   \item{\code{end.date}}{[\code{Date}] bla}
#'   \item{\code{memory}}{[\code{Date}] bla}
#'   \item{\code{n}}{[\code{integer}] bla}
#'   \item{\code{n.dicsarded}}{[\code{integer}] bla}
#'   \item{\code{n.memory}}{[\code{integer}] bla}
#'   \item{\code{n.vocab}}{[\code{integer}] bla}
#' }
#' If not passed, \code{lda} is interpreted as initialization chunk.
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
as.RollingLDA = function(x, id, lda, docs, dates, vocab, chunks, param){
  if (!missing(x)){
    if (missing(id)) id = x$id
    if (missing(lda)) lda = x$lda
    if (missing(docs)) docs = x$docs
    if (missing(dates)) dates = x$dates
    if (missing(vocab)) vocab = x$vocab
    if (missing(chunks)) chunks = x$chunks
    if (missing(param)) param = x$param
  }
  if (missing(id)) id = "rolling - converted"
  if (!is.LDA(lda)){
    is.LDA(lda, verbose = TRUE)
    stop("\"lda\" not an LDA object")
  }
  if (is.null(names(dates))) names(dates) = names(docs)
  dates = as.Date(dates[match(names(dates), names(docs))])
  if (missing(vocab)) vocab = colnames(getTopics(lda))
  if (missing(chunks)){
    chunks = data.table(
      chunk.id = 0L,
      start.date = min(dates),
      end.date = max(dates),
      memory = NA_Date_,
      n = length(docs),
      n.discarded = NA_integer_,
      n.memory = NA_integer_,
      n.vocab = length(vocab)
    )
  }
  if (missing(param)) param = .defaultParam()
  res = list(
    id = id,
    lda = lda,
    docs = docs,
    dates = dates,
    vocab = vocab,
    chunks = chunks,
    param = param)
  class(res) = "RollingLDA"
  if (!is.RollingLDA(res)){
    is.RollingLDA(res, verbose = TRUE)
    stop("input arguments do not create a RollingLDA object")
  }
  res
}

#' @rdname as.RollingLDA
#' @export
is.RollingLDA = function(obj, verbose = FALSE){
  assert_flag(verbose)

  if (!inherits(obj, "RollingLDA")){
    if (verbose) message("object is not of class \"RollingLDA\"")
    return(FALSE)
  }

  if (!is.list(obj)){
    if (verbose) message("object is not a list")
    return(FALSE)
  }

  testNames = c("id", "lda", "docs", "dates", "vocab", "chunks", "param")

  if (!test_list(obj, types = c("character", "LDA", "list", "Date", "character", "data.table", "list"),
                 names = "named", any.missing = FALSE)){
    if (verbose) message(check_list(obj, types = c("character", "LDA", "list", "Date", "character", "data.table", "list"),
                                    names = "named", any.missing = FALSE))
    return(FALSE)
  }
  if (!test_set_equal(names(obj), testNames)){
    if (verbose) message(check_set_equal(names(obj), testNames))
    #message("object does not contain exactly the list elements of a \"RollingLDA\" object")
    return(FALSE)
  }

  #id
  if (verbose) message("id: ", appendLF = FALSE)
  id = getID(obj)
  if (!is.character(id) || !(length(id) == 1)){
    if (verbose) message("not a character of length 1")
    return(FALSE)
  }
  if (verbose) message("checked")

  #lda
  if (verbose) message("lda: ", appendLF = FALSE)
  lda = try(getLDA(obj, reduce = FALSE), silent = verbose)
  if(inherits(lda, "try-error")){
    return(FALSE)
  }
  if(!is.LDA(lda)){
    if (verbose) message("not an \"LDA\" object")
    return(FALSE)
  }
  if (verbose) message("checked")

  #docs
  if (verbose) message("docs: ", appendLF = FALSE)
  docs = getDocs(obj)
  if (!test_list(docs, min.len = 1, names = "unique", types = "matrix", any.missing = FALSE)){
    if (verbose) message(check_list(docs, min.len = 1, names = "unique", types = "matrix", any.missing = FALSE))
    return(FALSE)
  }
  if (!all(sapply(docs, nrow) == 2)){
    if (verbose) message("not all elements have two rows")
  }
  if (!all(sapply(docs, function(x) all(x[2,] == 1)))){
    if (verbose) message("not all values in the second row equal 1")
  }
  if (verbose) message("checked")

  #dates
  if (verbose) message("dates: ", appendLF = FALSE)
  dates = getDates(obj)
  if (!test_date(dates, any.missing = FALSE)){
    if (verbose) message(check_date(dates, any.missing = FALSE))
    return(FALSE)
  }
  if (!all(names(dates) %in% names(docs)) || !all(names(docs) %in% names(dates))){
    if (verbose) message("not same names as \"docs\"")
    return(FALSE)
  }
  if (length(dates) != length(docs)){
    if (verbose) message("not same length as \"docs\"")
  }
  if (verbose) message("checked")

  #vocab
  if (verbose) message("vocab: ", appendLF = FALSE)
  vocab = getVocab(obj)
  if (!test_character(vocab, any.missing = FALSE, unique = TRUE)){
    if (verbose) message(check_character(vocab, any.missing = FALSE, unique = TRUE))
    return(FALSE)
  }
  if (verbose) message("checked")

  #chunks
  if (verbose) message("chunks: ", appendLF = FALSE)
  chunks = getChunks(obj)
  if (!is.data.table(chunks) ||
      !all(c("chunk.id", "start.date", "end.date", "memory", "n", "n.discarded",
             "n.memory", "n.vocab") %in% colnames(chunks))){
    if (verbose) message("not a data.table with standard parameters")
    return(FALSE)
  }
  if (anyDuplicated(chunks$chunk.id)){
    if (verbose) message("duplicated \" chunk.id\"")
    return(FALSE)
  }
  if (!is.integer(chunks$chunk.id)){
    if (verbose) message("\"chunk.id\" is not an integer")
    return(FALSE)
  }
  if (!is.integer(chunks$n)){
    if (verbose) message("\"n\" is not an integer")
    return(FALSE)
  }
  if (!is.integer(chunks$n.discarded)){
    if (verbose) message("\"n.discarded\" is not an integer")
    return(FALSE)
  }
  if (!is.integer(chunks$n.memory)){
    if (verbose) message("\"n.memory\" is not an integer")
    return(FALSE)
  }
  if (!is.integer(chunks$n.vocab)){
    if (verbose) message("\"n.vocab\" is not an integer")
    return(FALSE)
  }
  if (!is.Date(chunks$start.date)){
    if (verbose) message("\"start.date\" is not an integer")
    return(FALSE)
  }
  if (!is.Date(chunks$end.date)){
    if (verbose) message("\"end.date\" is not an integer")
    return(FALSE)
  }
  if (!is.Date(chunks$memory)){
    if (verbose) message("\"memory\" is not an integer")
    return(FALSE)
  }
  if (length(dates) != sum(chunks$n)){
    if (verbose) message("sum of \"n\" does not match number of texts")
    return(FALSE)
  }
  if (length(vocab) != max(chunks$n.vocab)){
    if (verbose) message("sum of \"n.vocab\" does not match number of vocabularies")
    return(FALSE)
  }
  if (is.unsorted(chunks$n.vocab)){
    if (verbose) message("\"n.vocab\" is not monotonously increasing")
    return(FALSE)
  }
  if (any(is.na(chunks$chunk.id))){
    if (verbose) message("NA(s) in \"chunk.id\"")
    return(FALSE)
  }
  if (any(is.na(chunks$n))){
    if (verbose) message("NA(s) in \"n\"")
    return(FALSE)
  }
  if (any(is.na(chunks$n.vocab))){
    if (verbose) message("NA(s) in \"n.vocab\"")
    return(FALSE)
  }
  if (any(is.na(chunks$start.date))){
    if (verbose) message("NA(s) in \"start.date\"")
    return(FALSE)
  }
  if (any(is.na(chunks$end.date))){
    if (verbose) message("NA(s) in \"end.date\"")
    return(FALSE)
  }
  if (min(dates) < min(chunks$start.date)){
    if (verbose) message("minimum of \"start.date\" is larger than minimum of text's dates")
    return(FALSE)
  }
  if (max(dates) > max(chunks$end.date)){
    if (verbose) message("maximum of \"end.date\" is smaller than maximum of text's dates")
    return(FALSE)
  }
  if (verbose) message("checked")

  #param
  if (verbose) message("param: ", appendLF = FALSE)
  param = getParam(obj)
  testNames = c("vocab.abs", "vocab.rel", "vocab.fallback", "doc.abs")
  if (!test_list(param, types = c("numeric", "integer"), names = "named", any.missing = FALSE)){
    if (verbose) message(check_list(param, types = c("numeric", "integer"), names = "named", any.missing = FALSE))
    return(FALSE)
  }
  if (!test_set_equal(names(param), testNames)){
    if (verbose) message(check_set_equal(names(param), testNames))
    return(FALSE)
  }
  if (param$vocab.abs < 0){
    if (verbose) message("\"vocab.abs\" is smaller than 0")
    return(FALSE)
  }
  if (param$vocab.rel < 0){
    if (verbose) message("\"vocab.rel\" is smaller than 0")
    return(FALSE)
  }
  if (param$vocab.rel > 1){
    if (verbose) message("\"vocab.rel\" is greater than 0")
    return(FALSE)
  }
  if (param$vocab.fallback < 0){
    if (verbose) message("\"vocab.fallback\" is smaller than 0")
    return(FALSE)
  }
  if (param$doc.abs < 0){
    if (verbose) message("\"doc.abs\" is smaller than 0")
    return(FALSE)
  }
  if (verbose) message("checked")

  return(TRUE)
}

#' @export
print.RollingLDA = function(x, ...){
  elements = paste0("\"", names(which(!sapply(x, is.null))), "\"")
  cat(
    "RollingLDA Object named \"", getID(x),
    "\" with elements\n", paste0(elements, collapse = ", "), "\n ",
    nrow(getChunks(x)), " Chunks with Texts from ",
    as.character(min(getDates(x))), " to ", as.character(max(getDates(x))),
    "\n ", paste0(paste0(names(getParam(x)), ": ",
                         unlist(getParam(x))), collapse = ", "),
    "\n\n", sep = "")
  print(getLDA(x))
}
