.computewordcounts = function(texts){
  k = 100000
  n = length(texts)
  words = sort(unique(unlist(texts)))
  wordcounts = rep(0L, length(words))
  for (i in 0:floor(length(texts)/k)){
    tmp = table(unlist(texts[(i * k + 1):(min(n, i * k + k))]))
    mtch = match(names(tmp), words)
    wordcounts[mtch] = wordcounts[mtch] + unname(tmp)
  }
  list(words = words, wordcounts = wordcounts)
}

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
