#RollingLDA_update.RollingLDA = function(x, texts, dates, compute.topics = TRUE)

# texts muss named list of tokenized texts sein!!!
# dates muss gleiche Länge und Sortierung (wie texts) haben, oder benannt sein!

rollinglda = function(){


  step.new = rollinglda_one_step(lda, docs, texts,
    vocab.abs = vocab.abs, vocab.rel = vocab.rel, vocab.fallback = vocab.fallback,
    doc.abs = doc.abs)

  # TODO!
  #if (!keep.docs){ ...
  #docs = docs[]
  #dates = dates[]
  #chunks = data.table() ...
  res = list(lda = lda, docs = docs, dates = dates, chunks = chunks,
    param = list(vocab.abs = vocab.abs, vocab.rel = vocab.rel,
      vocab.fallback = vocab.fallback, doc.abs = doc.abs))
  class(res) = "RollingLDA"
  invisible(res)
}


# internal (docs ist hier schon korrekt reduziert - docs = docs.memory!!)
rollinglda_one_step = function(lda, docs, texts,
  vocab.abs = 5, vocab.rel = 0, vocab.fallback = 100, doc.abs = 0,
  compute.topics = FALSE){

  # vocab bestimmen?
  # assignments bestimmen (memory-begin bis aktuelles-chunk-ende)
  # docs bestimmen (memory-begin bis aktuelles-chunk-ende)
  # n.init bestimmen (Anzahl memory-begin bis memory-ende)
  # stats als Zeile zu einer Tabelle hinzufügen und mit zurückgeben

  wc = .computewordcounts(texts)
  vocab.new = wc$words[wc$wordcounts > vocab.abs &
      wc$wordcounts > min(vocab.rel * sum(wc$wordcounts), vocab.fallback)]
  ind = !(vocab.new %in% vocab)
  if(any(ind)) vocab = c(vocab, vocab.new[ind])
  docs.new = LDAprep(texts, vocab)
  docs.new = docs.new[sapply(docs.new, ncol) > doc.abs]
  n.docs.new = length(docs.new)
  n.docs.deleted = length(texts) - n.docs.new
  n.init = length(docs)

  docs = append(docs, docs.new)
  assignments.new.sampled = lapply(sapply(docs.new, ncol), function(n)
    as.integer(sample(getK(lda), n, replace = TRUE)-1))

  res = rollinglda_one_step_fitting(
    assignments = append(tail(getAssignments(lda), n.init), assignments.new.sampled),
    docs = docs,
    vocab = vocab,
    n.init = length(docs),
    K = getK(lda),
    alpha = getAlpha(lda),
    eta = getEta(lda),
    num.iterations = getNum.iterations(lda)
  )
  if(compute.topics)
    topics = compute_topics_matrix_from_assignments(
      append(getAssignments(lda), res$assignments), ALL_docs, getK(lda), vocab)
  else topics = NULL
  list(
    lda = LDA(
      assignments = append(getAssignments(lda), res$assignments),
      topics = topics,
      document_sums = cbind(getDocument_sums(lda), res$document_sums),
      param = list(K = K, alpha = alpha, eta = eta, num.iterations = num.iterations)),
    docs = docs,
    n.docs.new = n.docs.new,
    n.docs.deleted = n.docs.deleted,
    n.init = n.init)
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
    n.init = n.init)
  list(
    assignments = res$assignments[(n.init+1):length(docs)],
    document_sums = res$document_sums[,(n.init+1):length(docs)])
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
