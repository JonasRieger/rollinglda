#' @title Sequential LDA
#'
#' @description
#' Performs a single sequential fitting of Latent Dirichlet Allocation.
#'
#' @details .................... The function generates multiple LDA runs with the possability of
#' using parallelization. The integration is done by the
#' \code{\link[parallelMap:parallelMap]{parallelMap-package}}.
#'
#' The function returns a \code{LDARep} object. You can receive results and
#' all other elements of this object with getter functions (see \code{\link{getJob}}).
#'
#' @family replication functions
#' @family LDA functions
#' @family workflow functions
#'
#' @param docs [\code{list}]\cr
#' Documents as received from \code{\link[tosca]{LDAprep}}.
#' @param vocab [\code{character}]\cr
#' Vocabularies passed to \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' For additional (and necessary) arguments passed, see ellipsis (three-dot argument).
#' @param n [\code{integer(1)}]\cr
#' Number of Replications.
#' @param seeds [\code{integer(n)}]\cr
#' Random Seeds for each Replication.
#' @param id [\code{character(1)}]\cr
#' Name for the computation.
#' @param pm.backend [\code{character(1)}]\cr
#' One of "multicore", "socket" or "mpi".
#' If \code{pm.backend} is set, \code{\link[parallelMap]{parallelStart}} is
#' called before computation is started and \code{\link[parallelMap]{parallelStop}}
#' is called after.
#' @param ncpus [\code{integer(1)}]\cr
#' Number of (physical) CPUs to use. If \code{pm.backend} is passed,
#' default is determined by \code{\link[future]{availableCores}}.
#' @param ... additional arguments passed to \code{\link[lda]{lda.collapsed.gibbs.sampler}}.
#' Arguments will be coerced to a vector of length \code{n}.
#' Default parameters are \code{alpha = eta = 1/K} and \code{num.iterations = 200}.
#' There is no default for \code{K}.
#' @return [\code{named list}] with entries \code{id} for computation's name,
#' \code{jobs} for the parameter settings and \code{lda} for the results itself.
#'
#' @examples
#' res = LDARep(docs = reuters_docs, vocab = reuters_vocab, n = 4, seeds = 1:4,
#'    id = "myComputation", K = 7:10, alpha = 1, eta = 0.01, num.iterations = 20)
#' res
#' getJob(res)
#' getID(res)
#' getLDA(res, 4)
#'
#' \donttest{
#' LDARep(docs = reuters_docs, vocab = reuters_vocab,
#'    K = 10, num.iterations = 100, pm.backend = "socket")
#' }
#'
#' @export LDARep

seqLDA.LDAPrototype
seqLDA.LDA
seqLDA.RollingLDA

seqLDA = function(init, docs, dates, vocab, vocab.limit, memory, dates.init, ..){

  assert_multi_class(init, c("LDA", "LDAPrototype", "RollingLDA"))

  if (inherits(init, "LDAPrototype")) init = getLDA(init)

  if (!inherits(init, "RollingLDA")){ # später in seqLDA.LDA(Prototype) auslagern, alternative dann seqLDA.RollingLDA
    if (missing(dates.init)){
      stop("dates.init is missing but needed for fitting if init does not inherit from class RollingLDA.")
    }
    ninit = length(getAssignments(init))
    dates.init = as.Date(dates.init)
    assert_date(dates.init, any.missing = FALSE, len = ninit)
    seqs = data.table(seq.id = 0,
                      start = min(dates.init),
                      end = max(dates.init),
                      index = list(seq_len(ninit)),
                      N = ninit,
                      V = ncol(getTopics(init)),
                      memory = NA_Date_,
                      memory.index = list())
  }

  # vocab muss Obermenge von colnames(getTopics(init)) sein -> sonst Warnung + aufblähen!
  vocabinit = colnames(getTopics(init))
  if (!missing(vocab) && !all(vocabinit %in% vocab)){
    warning("vocab must be a superset of the vocabulary of the initial model: expand vocab.")
    vocab = c(vocabinit, setdiff(vocab, vocabinit))
  }
  # schalter setzen: nehme festes vokabular und nicht über vocab.limit bestimmtes











  assert_string(id, min.chars = 1)
  assert_list(docs, min.len = 1, names = "unique", types = "matrix", any.missing = FALSE)
  stopifnot(all(sapply(docs, nrow) == 2),
            all(sapply(docs, function(x) all(x[2,] == 1))))
  assert_character(vocab, any.missing = FALSE, unique = TRUE, min.len = 2)
  assert_int(n, lower = 1)

  args = .paramList(n = n, ...)
  if (missing(seeds) || length(seeds) != n){
    message("No seeds given or length of given seeds differs from number of replications: sample seeds. Sampled seeds can be obtained via getJob().")
    if (!exists(".Random.seed", envir = globalenv())){
      runif(1)
    }
    oldseed = .Random.seed
    seeds = sample(9999999, n)
    .Random.seed <<- oldseed
  }
  if (anyDuplicated(seeds)){
    message(sum(duplicated(seeds)), " duplicated seeds.")
  }
  args$seed = seeds
  args$fun = function(seed, ...){
    set.seed(seed)
    LDA(lda.collapsed.gibbs.sampler(documents = docs, vocab = vocab, ...),
        param = list(...))
  }

  if (!missing(pm.backend) && !is.null(pm.backend)){
    if (missing(ncpus) || is.null(ncpus)) ncpus = future::availableCores()
    assert_choice(pm.backend, choices = c("multicore", "socket", "mpi"))
    assert_int(ncpus, lower = 1)
    parallelMap::parallelStart(mode = pm.backend, cpus = ncpus)
  }

  parallelMap::parallelExport("docs", "vocab")
  ldas = do.call(parallelMap::parallelMap, args = args)
  if (!missing(pm.backend) && !is.null(pm.backend)) parallelMap::parallelStop()
  job.id = seq_len(n)
  args = data.table(job.id = job.id,
                    do.call(cbind, args[names(args) != "fun"]))
  names(ldas) = job.id

  res = list(id = id, lda = ldas, jobs = args)
  class(res) = "LDARep"
  res
}

#' @export
print.LDARep = function(x, ...){
  jobs = getJob(x)
  parameters = unique(jobs[, !colnames(jobs) %in% c("job.id", "seed"), with = FALSE])
  if (nrow(parameters) == 1){
    parameters = paste0("parameters ",
                        paste0(paste0(colnames(parameters), ": ", as.character(round(parameters, 4))), collapse = ", "))
  }else{
    parameters = paste0(nrow(parameters), " different parameter sets.")
  }
  cat(
    "LDARep Object \"", getID(x), "\"\n ",
    nrow(jobs), " LDA Runs", "\n ",
    "with ", parameters, "\n\n",
    sep = ""
  )
}
