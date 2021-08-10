#RollingLDA_update.RollingLDA = function(x, texts, dates, compute.topics = TRUE)

# texts muss named list of tokenized texts sein!!!
# dates muss gleiche Länge und Sortierung (wie texts) haben, oder benannt sein!

#getDocs muss noch geschrieben werden!
#getDates auch!

# texts = named list of tokenized texts
# dates = dates (benannt oder unbenannt - bei letzterem aber dann gleiche Sortierung)
# chunks = entweder date oder in der Form "ZahlUnit" = beginn jedes chunks
# memory = entweder date, numeric, oder in der Form "ZahlUnit" = beginn jedes Memorys
# init = date oder numeric = letztes date für initiale LDA - nur benötigt, falls chunks als character gegeben
# type = welche Form der initialen LDA
# param = LDAparam/LDAPrototypeparam
# id = character
rollinglda = function(texts, dates, chunks, memory,
  vocab.abs = 5, vocab.rel = 0, vocab.fallback = 100, doc.abs = 0,
  init, type = c("ldaprototype", "lda"), param, id){

  type = match.arg(type)
  if (missing(id)) id = paste0("rolling-", type)

  # cuts, memory, ...

  init = max(dates[dates < chunks[1]])
  wc = .computewordcounts(texts.init)
  vocab = wc$words[wc$wordcounts > vocab.abs &
      wc$wordcounts > min(vocab.rel * sum(wc$wordcounts), vocab.fallback)]
  docs = LDAprep(texts[dates < chunks[1]], vocab)
  docs = docs[sapply(docs, ncol) > doc.abs]

  # LDARep / LDAPrototype
  lda = getLDA(LDARep(docs = docs, vocab = vocab, n = 1, ...))
  lda = getLDA(LDAPrototype(docs = docs, vocab = vocab, ...))


  res = list(id = id, lda = lda, docs = docs, dates = dates[dates < chunks[1]],
    chunks = data.table(
      chunk.id = 0L,
      start.date = min(dates),
      end.date = init,
      memory = NA_Date_,
      n = length(docs),
      n.discarded = sum(dates < chunks[1]) - length(docs),
      n.memory = NA_integer_
    ),
    param = list(vocab.abs = vocab.abs, vocab.rel = vocab.rel,
      vocab.fallback = vocab.fallback, doc.abs = doc.abs))
  class(res) = "RollingLDA"

  texts = texts[dates >= chunks[1]]
  dates = dates[dates >= chunks[1]]
  chunks = c(chunks[-1], max(dates))

  # rollinglda_update
  for (i in seq_along(chunks)){
    res = rollinglda_update(
      x = res,
      texts = texts[dates < chunks[i]],
      dates = dates[dates < chunks[i]],
      memory = memory[i]
    )
    texts = texts[dates >= chunks[i]]
    dates = dates[dates >= chunks[i]]
  }
  invisible(res)
}

# texts = new texts: named list of tokenized texts
# dates = new dates (benannt oder unbenannt - bei letzterem aber dann gleiche Sortierung)
# memory = entweder date, numeric, oder in der Form "ZahlUnit"
# param = liste mit einträgen für vocab.abs etc.
rollinglda_update = function(x, texts, dates, memory, param = getParam(x)){

  # assert auf x!
  assert_list(texts, types = "character", names = "unique")
  if (!is.Date(dates)) dates = try(as.Date(dates), silent = TRUE)
  if (is.null(names(dates))) names(dates) = names(texts)
  dates = dates[match(names(dates), names(texts))]
  assert_date(dates, any.missing = FALSE, len = length(texts))
  assert_character(as.character(memory), any.missing = FALSE, len = 1)
  if (!is.Date(memory)){
    if (is.numeric(memory)){
      tmp = sort(getDates(x), decreasing = TRUE)[memory]
      message("memory = ", memory, ": using the date of the ", memory,
        "th last text as memory, i.e ", tmp)
      memory = tmp
    }
    memory = tolower(memory)
    memory.try = try(as.Date(memory), silent = TRUE)
    if (inherits(memory.try, "try-error")){
      unit.memory = gsub("([0-9]*)(.*)", "\\2", memory)
      cand = c("day", "week", "month", "quarter", "year")
      assert_choice(unit.memory, c(cand, paste0(cand, "s")))
      if (unit.memory %in% c("year", "years")){
        update.start = min(dates)
        floored = " (first new date)"
      }else{
        update.start = floor_date(min(dates), unit.memory)
        floored = paste0(" (first new date floored to ", unit.memory, ")")
      }
      if (unit.memory %in% c("quarter", "quarters")){
        number.quarter = gsub("([0-9]*)(.*)", "\\1", memory)
        memory = paste0(ifelse(number.quarter == "", 3,
          3 * as.integer(number.quarter)), "month")
      }
      message("memory = ", memory, ": using texts of the last ", memory, " from ",
        update.start, floored, " as memory, i.e. ", update.start - period(memory))
      memory = update.start - period(memory)
    }else memory = memory.try
  }
  assert_date(memory, any.missing = FALSE, len = 1)
  # assert auf param!
  vocab.abs = param$vocab.abs
  vocab.rel = param$vocab.rel
  vocab.fallback = param$vocab.fallback
  docs.abs = param$docs.abs

  dates.memory = getDates(x)
  id.memory = names(dates.memory[dates.memory >= memory])
  docs.memory = getDocs(x, names = id.memory)
  n.memory = length(docs.memory)

  # message("Verwende Texte ab ", memory, " als memory (", n.init, ")")

  step.new = rollinglda_one_step(
    lda = getLDA(x),
    docs = docs.memory,
    texts = texts,
    vocab.abs = vocab.abs,
    vocab.rel = vocab.rel,
    vocab.fallback = vocab.fallback,
    doc.abs = doc.abs
  )
  dates = dates[match(names(step.new$docs), names(dates))]
  dates = c(getDates(x, names = id.memory, inverse = TRUE), dates)
  if (!keep.docs) docs = step.new$docs
  else docs = append(getDocs(x, names = id.memory, inverse = TRUE), step.new$docs)
  chunks = merge.data.table(chunks, data.table(
    chunk.id = max(chunks$chunk.id) + 1L,
    start.date = min(dates),
    end.date = max(dates),
    memory = memory,
    n = step.new$n.docs,
    n.discarded = step.new$n.docs.deleted,
    n.memory = n.memory
  ), all = TRUE)
  res = list(id = getID(x), lda = step.new$lda,
    docs = docs, dates = dates, chunks = chunks,
    param = list(vocab.abs = vocab.abs, vocab.rel = vocab.rel,
      vocab.fallback = vocab.fallback, doc.abs = doc.abs))
  class(res) = "RollingLDA"
  invisible(res)
}

# internal
rollinglda_one_step = function(lda, docs, texts,
  vocab.abs = 5, vocab.rel = 0, vocab.fallback = 100, doc.abs = 0){

  wc = .computewordcounts(texts)
  vocab.new = wc$words[wc$wordcounts > vocab.abs &
      wc$wordcounts > min(vocab.rel * sum(wc$wordcounts), vocab.fallback)]
  ind = !(vocab.new %in% vocab)
  if(any(ind)) vocab = c(vocab, vocab.new[ind])
  docs.new = LDAprep(texts, vocab)
  docs.new = docs.new[sapply(docs.new, ncol) > doc.abs]
  n.docs.new = length(docs.new)
  n.docs.deleted = length(texts) - n.docs.new
  n.memory = length(docs)

  docs = append(docs, docs.new)
  assignments.new.sampled = lapply(sapply(docs.new, ncol), function(n)
    as.integer(sample(getK(lda), n, replace = TRUE)-1))

  res = rollinglda_one_step_fitting(
    assignments = append(tail(getAssignments(lda), n.init), assignments.new.sampled),
    docs = docs,
    vocab = vocab,
    n.init = n.memory,
    K = getK(lda),
    alpha = getAlpha(lda),
    eta = getEta(lda),
    num.iterations = getNum.iterations(lda)
  )
  list(
    lda = LDA(
      assignments = append(getAssignments(lda), res$assignments),
      document_sums = cbind(getDocument_sums(lda), res$document_sums),
      param = list(K = K, alpha = alpha, eta = eta, num.iterations = num.iterations)
    ),
    docs = docs,
    n.docs.new = n.docs.new,
    n.docs.deleted = n.docs.deleted,
    n.memory = n.memory)
}

# internal
rollinglda_one_step_fitting = function(assignments, docs, vocab, n.init,
  K, alpha, eta, num.iterations){

  topics = compute_topics_matrix_from_assignments(assignments, docs, K, vocab)
  res = ldaGibbs(docs = docs, K = K, vocab,
    num.iterations = num.iterations, alpha = alpha, eta = eta,
    initial = list(
      assignments = assignments,
      topics = topics,
      topic_sums = matrix(as.integer(rowSums(topics)))),
    n.init = n.init
  )
  list(
    assignments = res$assignments[(n.init+1):length(docs)],
    document_sums = res$document_sums[,(n.init+1):length(docs)]
  )
}

# internal
compute_topics_matrix_from_assignments = function(assignments, docs, K, vocab){
  n.voc = length(vocab)
  assignments_flatten = unlist(assignments) + 1
  docs_flatten = unlist(lapply(docs, function(x) x[1,])) + 1
  topics = do.call(rbind,
    lapply(seq_len(K), function(k)
      tabulate(docs_flatten[assignments_flatten == k], n.voc)))
  colnames(topics) = vocab
  topics
}
