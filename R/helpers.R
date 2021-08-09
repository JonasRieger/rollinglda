.makeProgressBar = function(progress, ...) {
  if (progress && getOption("width") >= 20L){
    progress_bar$new(...)
  }else{
    list(tick = function(len = 1, tokens = list()) NULL, update = function(ratio, tokens) NULL)
  }
}

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
