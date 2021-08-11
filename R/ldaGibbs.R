ldaGibbs = function(docs, K, vocab, num.iterations, alpha, eta, initial = NULL,
                     burnin = 0L, compute.log.likelihood = FALSE, trace = 0L,
                     freeze.topics = FALSE, n.init = 0L){
  message("ldaGibbs")
  lengths = as.integer(length(vocab))
  retval = structure(
    .Call("ldagibbs", docs, as.integer(K), lengths, as.integer(num.iterations),
          as.double(alpha), as.double(eta), initial, as.integer(burnin),
          as.logical(compute.log.likelihood), trace, as.logical(freeze.topics),
          as.integer(n.init)),
    names = c("assignments", "topics", "topic_sums", "document_sums",
              "document_expects", "log.likelihoods"))
  colnames(retval$topics) = vocab
  retval
}
