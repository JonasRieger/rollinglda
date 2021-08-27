rollinglda_one_step = function(lda, docs, texts, vocab,
  vocab.abs = 5L, vocab.rel = 0, vocab.fallback = 100L, doc.abs = 0L){
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
    assignments = append(tail(getAssignments(lda), n.memory), assignments.new.sampled),
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
      param = getParam(lda)
    ),
    docs = docs,
    vocab = vocab,
    n.docs.new = n.docs.new,
    n.docs.deleted = n.docs.deleted,
    n.memory = n.memory)
}

rollinglda_one_step_fitting = function(assignments, docs, vocab, n.init,
  K, alpha, eta, num.iterations){
  topics = compute_topics_matrix_from_assignments(assignments, docs, K, vocab)
  res = ldaGibbs(docs = docs, K = K, vocab = vocab,
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

ldaGibbs = function(docs, K, vocab, num.iterations, alpha, eta, initial = NULL, n.init = 0L){
  lengths = as.integer(length(vocab))
  retval = structure(
    .Call("ldagibbs", docs, as.integer(K), lengths, as.integer(num.iterations),
      as.double(alpha), as.double(eta), initial, as.integer(n.init)),
    names = c("assignments", "topics", "topic_sums", "document_sums"))
  colnames(retval$topics) = vocab
  retval
}
