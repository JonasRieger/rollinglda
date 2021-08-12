#' @title RollingLDA
#'
#' @description
#' Performs a rolling version of Latent Dirichlet Allocation.
#'
#' @details The function first computes a initial LDA model (using
#' \code{\link[ldaPrototype]{LDARep}} or \code{\link[ldaPrototype]{LDAPrototype}}).
#' Afterwards it models temporal chunks of texts with a specified memory for
#' initialization of each model chunk.
#'
#' The function returns a \code{RollingLDA} object. You can receive results and
#' all other elements of this object with getter functions (see \code{\link{getChunks}}).
#'
#' @family RollingLDA functions
#'
#' @param texts [\code{named list}]\cr
#' Tokenized texts.
#' @param dates [\code{(un)named Date}]\cr
#' Dates of the tokenized texts. If unnamed, it must match the order of texts.
#' @param chunks [\code{Date} or \code{character(1)}]\cr
#' Dates of the beginnings of each chunk to be modeled after the inital model.
#' If passed as \code{character}, dates are determined by passing \code{init}
#' plus one day as \code{from} argument, \code{max(dates)} as \code{to} argument
#' and \code{chunks} as \code{by} argument in \code{\link{seq.Date}}.
#' @param memory [\code{Date}, \code{character(1)} or \code{integer(1)}]\cr
#' Dates of the beginnings of each chunk's memory. If passed as \code{character},
#' dates are determined by using the dates of the beginnings of each chunk and
#' substracting the given time interval in \code{memory} passing it as
#' \code{by} argument in \code{\link{seq.Date}}. If passed as
#' \code{integer/numeric}, the dates are determined by going backwards the
#' modeled texts chronologically and taking the date of the text at position
#' \code{memory}.
#' @param vocab.abs [\code{integer(1)}]\cr
#' An absolute lower bound limit for which words are taken into account. All
#' words are considered in the vocabularies that have a count higher than
#' \code{vocab.abs} over all texts and at the same time a higher relative
#' frequency than \code{vocab.rel}.
#' @param vocab.rel [0,1]\cr
#' A relative lower bound limit for which words are taken into account. See also
#' \code{vocab.abs}.
#' @param vocab.fallback [\code{integer(1)}]\cr
#' An absolute lower bound limit for which words are taken into account. All
#' words are considered in the vocabularies that have a count higher than
#' \code{vocab.fallback} over all texts even if they might not have a higher
#' relative frequency than \code{vocab.rel}.
#' @param doc.abs [\code{integer(1)}]\cr
#' An absolute lower bound limit for which texts are taken into account. All
#' texts are considered for modeling that have more words (subsetted to words
#' occurring in the vocabularies) than \code{doc.abs}.
#' @param init [\code{Date(1)} or \code{integer(1)}]\cr
#' Date up to which the initial model should be computed. This parameter is
#' needed/used only if \code{chunks} is passed as \code{character}. Otherwise
#' the initial model is computed up to the first date in \code{chunks} minus one
#' day. If \code{init} is passed as \code{integer/numeric}, the \code{init}
#' lowest date from \code{dates} is selected.
#' @param type [\code{character(1)}]\cr
#' One of "ldaPrototype" or "lda" specifying whether a LDAProtoype or standard
#' LDA should be modeled as initial model. Default is "ldaprototype".
#' @param id [\code{character(1)}]\cr
#' Name for the computation/model.
#' @param ... additional arguments passed to \code{\link[ldaPrototype]{LDARep}}
#' or \code{\link[ldaPrototype]{LDAPrototype}}, respectively.
#' Default parameters are \code{alpha = eta = 1/K} and \code{num.iterations = 200}.
#' There is no default for \code{K}.
#' @return [\code{named list}] with entries
#'  \describe{
#'   \item{\code{id}}{[\code{character(1)}] See above.}
#'   \item{\code{lda}}{\code{\link{LDA}} object of the fitted RollingLDA.}
#'   \item{\code{docs}}{[\code{named list}] with modeled texts in a preprocessed format.
#'   See \code{\link[tosca]{LDAprep}}}
#'   \item{\code{dates}}{[\code{named Date}] with dates of the modeled texts.}
#'   \item{\code{vocab}}{[\code{character}] with the vocabularies considered
#'   for modeling.}
#'   \item{\code{chunks}}{[\code{data.table}] with specifications for each
#'   model chunk.}
#'   \item{\code{param}}{[\code{named list}] with parameter specifications for
#'   \code{vocab.abs} [\code{integer(1)}], \code{vocab.rel} [0,1],
#'   \code{vocab.fallback} [\code{integer(1)}] and
#'   \code{doc.abs} [\code{integer(1)}]. See above for explanation.}
#' }
#'
#' @export RollingLDA
RollingLDA = function(...) UseMethod("RollingLDA")

#' @rdname RollingLDA
#' @export
RollingLDA.default = function(texts, dates, chunks, memory,
  vocab.abs = 5, vocab.rel = 0, vocab.fallback = 100, doc.abs = 0,
  init, type = c("ldaprototype", "lda"), id, ...){

  type = match.arg(type)
  if (missing(id)) id = paste0("rolling-", type)

  if (is.null(names(dates))) names(dates) = names(texts)
  dates = dates[match(names(dates), names(texts))]
  assert_date(try(as.Date(dates)), any.missing = FALSE, len = length(texts))
  dates = as.Date(dates)

  if (!is.Date(chunks)){
    chunks.try = try(as.Date(chunks), silent = TRUE)
    if (inherits(chunks.try, "try-error")){
      if (is.numeric(init)) init = sort(dates)[init]
      assert_date(try(as.Date(init)), any.missing = FALSE, len = 1)
      init = as.Date(init)
      chunks = seq.Date(from = init+1, to = max(dates), by = tolower(chunks))
    }
    else chunks = chunks.try
  }
  assert_date(chunks, any.missing = FALSE)

  if (!is.Date(memory)){
    if (is.numeric(memory)){
      tmp = sort(dates, decreasing = TRUE)
      memory = as.Date(unname(
        sapply(chunks, function(x) (as.character(tmp[tmp < x])[memory]))))
    }
    memory.try = try(as.Date(memory), silent = TRUE)
    if (inherits(memory.try, "try-error")){
      if (!grepl("[0-9]", memory)) memory = paste0("1 ", memory)
      memory = as.Date(sapply(chunks, function(x)
        as.character(seq.Date(from = x, by = paste0("-", memory), length.out = 2)[2])))
    }
    else memory = memory.try
  }
  assert_date(memory, any.missing = FALSE, len = length(chunks))

  init = max(dates[dates < chunks[1]])
  wc = .computewordcounts(texts[dates < chunks[1]])
  vocab = wc$words[wc$wordcounts > vocab.abs &
      wc$wordcounts > min(vocab.rel * sum(wc$wordcounts), vocab.fallback)]
  docs = LDAprep(texts[dates < chunks[1]], vocab)
  docs = docs[sapply(docs, ncol) > doc.abs]

  if (type == "ldaprototype"){
    message("Fitting LDAPrototype as initial model.")
    lda = getLDA(LDAPrototype(docs = docs, vocabLDA = vocab, ...))
  }
  if (type == "lda"){
    message("Fitting LDA as initial model.")
    lda = getLDA(LDARep(docs = docs, vocab = vocab, n = 1, ...))
  }

  res = list(id = id, lda = lda,
    docs = docs, dates = dates[dates < chunks[1]], vocab = vocab,
    chunks = data.table(
      chunk.id = 0L,
      start.date = min(dates),
      end.date = init,
      memory = NA_Date_,
      n = length(docs),
      n.discarded = sum(dates < chunks[1]) - length(docs),
      n.memory = NA_integer_,
      n.vocab = length(vocab)
    ),
    param = list(vocab.abs = vocab.abs, vocab.rel = vocab.rel,
      vocab.fallback = vocab.fallback, doc.abs = doc.abs))
  class(res) = "RollingLDA"

  texts = texts[dates >= chunks[1]]
  dates = dates[dates >= chunks[1]]
  chunks = c(chunks[-1], max(dates))

  for (i in seq_along(chunks)){
    message("Fitting Chunk ", i, "/", length(chunks), ".")
    res = updateRollingLDA(
      x = res,
      texts = texts[dates < chunks[i]],
      dates = dates[dates < chunks[i]],
      memory = memory[i],
      compute.topics = FALSE
    )
    texts = texts[dates >= chunks[i]]
    dates = dates[dates >= chunks[i]]
  }
  message("Compute topic matrix.")
  res$lda$topics = compute_topics_matrix_from_assignments(
    assignments = getAssignments(getLDA(res)),
    docs = getDocs(res),
    K = getK(getLDA(res)),
    vocab = getVocab(res))
  invisible(res)
}
