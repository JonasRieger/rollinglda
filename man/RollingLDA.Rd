% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RollingLDA.R
\name{RollingLDA}
\alias{RollingLDA}
\alias{RollingLDA.default}
\title{RollingLDA}
\usage{
RollingLDA(...)

\method{RollingLDA}{default}(
  texts,
  dates,
  chunks,
  memory,
  vocab.abs = 5L,
  vocab.rel = 0,
  vocab.fallback = 100L,
  doc.abs = 0L,
  memory.fallback = 0L,
  init,
  type = c("ldaprototype", "lda"),
  id,
  ...
)
}
\arguments{
\item{...}{additional arguments passed to \code{\link[ldaPrototype]{LDARep}}
or \code{\link[ldaPrototype]{LDAPrototype}}, respectively.
Default parameters are \code{alpha = eta = 1/K} and \code{num.iterations = 200}.
There is no default for \code{K}.}

\item{texts}{[\code{named list}]\cr
Tokenized texts.}

\item{dates}{[\code{(un)named Date}]\cr
Dates of the tokenized texts. If unnamed, it must match the order of texts.}

\item{chunks}{[\code{Date} or \code{character(1)}]\cr
Sorted dates of the beginnings of each chunk to be modeled after the initial model.
If passed as \code{character}, dates are determined by passing \code{init}
plus one day as \code{from} argument, \code{max(dates)} as \code{to} argument
and \code{chunks} as \code{by} argument in \code{\link{seq.Date}}.}

\item{memory}{[\code{Date}, \code{character(1)} or \code{integer(1)}]\cr
Sorted dates of the beginnings of each chunk's memory. If passed as \code{character},
dates are determined by using the dates of the beginnings of each chunk and
substracting the given time interval in \code{memory} passing it as
\code{by} argument in \code{\link{seq.Date}}. If passed as
\code{integer/numeric}, the dates are determined by going backwards the
modeled texts chronologically and taking the date of the text at position
\code{memory}.}

\item{vocab.abs}{[\code{integer(1)}]\cr
An absolute lower bound limit for which words are taken into account. All
words are considered in the vocabularies that have a count higher than
\code{vocab.abs} over all texts and at the same time a higher relative
frequency than \code{vocab.rel}. Default is 5.}

\item{vocab.rel}{[0,1]\cr
A relative lower bound limit for which words are taken into account. See also
\code{vocab.abs}. Default is 0.}

\item{vocab.fallback}{[\code{integer(1)}]\cr
An absolute lower bound limit for which words are taken into account. All
words are considered in the vocabularies that have a count higher than
\code{vocab.fallback} over all texts even if they might not have a higher
relative frequency than \code{vocab.rel}. Default is 100.}

\item{doc.abs}{[\code{integer(1)}]\cr
An absolute lower bound limit for which texts are taken into account. All
texts are considered for modeling that have more words (subsetted to words
occurring in the vocabularies) than \code{doc.abs}. Default is 0.}

\item{memory.fallback}{[\code{integer(1)}]\cr
If there are no texts as memory in a certain chunk, \code{memory} is
determined by going backwards the modeled texts chronologically and taking
the date of the text at position \code{memory.fallback}.
Default is 0, which means "end the fitting".}

\item{init}{[\code{Date(1)} or \code{integer(1)}]\cr
Date up to which the initial model should be computed. This parameter is
needed/used only if \code{chunks} is passed as \code{character}. Otherwise
the initial model is computed up to the first date in \code{chunks} minus one
day. If \code{init} is passed as \code{integer/numeric}, the \code{init}
lowest date from \code{dates} is selected.}

\item{type}{[\code{character(1)}]\cr
One of "ldaPrototype" or "lda" specifying whether a LDAProtoype or standard
LDA should be modeled as initial model. Default is "ldaprototype".}

\item{id}{[\code{character(1)}]\cr
Name for the computation/model.}
}
\value{
[\code{named list}] with entries
 \describe{
  \item{\code{id}}{[\code{character(1)}] See above.}
  \item{\code{lda}}{\code{\link{LDA}} object of the fitted RollingLDA.}
  \item{\code{docs}}{[\code{named list}] with modeled texts in a preprocessed format.
  See \code{\link[tosca]{LDAprep}}.}
  \item{\code{dates}}{[\code{named Date}] with dates of the modeled texts.}
  \item{\code{vocab}}{[\code{character}] with the vocabularies considered
  for modeling.}
  \item{\code{chunks}}{[\code{data.table}] with specifications for each
  model chunk.}
  \item{\code{param}}{[\code{named list}] with parameter specifications for
  \code{vocab.abs} [\code{integer(1)}], \code{vocab.rel} [0,1],
  \code{vocab.fallback} [\code{integer(1)}] and
  \code{doc.abs} [\code{integer(1)}]. See above for explanation.}
}
}
\description{
Performs a rolling version of Latent Dirichlet Allocation.
}
\details{
The function first computes a initial LDA model (using
\code{\link[ldaPrototype]{LDARep}} or \code{\link[ldaPrototype]{LDAPrototype}}).
Afterwards it models temporal chunks of texts with a specified memory for
initialization of each model chunk.

The function returns a \code{RollingLDA} object. You can receive results and
all other elements of this object with getter functions (see \code{\link{getChunks}}).
}
\examples{
roll_lda = RollingLDA(texts = economy_texts,
                      dates = economy_dates,
                      chunks = "quarter",
                      memory = "3 quarter",
                      init = "2008-07-03",
                      K = 10,
                      type = "lda")

roll_lda
getChunks(roll_lda)
getLDA(roll_lda)

\donttest{
roll_proto = RollingLDA(texts = economy_texts,
                        dates = economy_dates,
                        chunks = "quarter",
                        memory = "3 quarter",
                        init = "2007-07-03",
                        K = 10,
                        n = 12,
                        pm.backend = "socket",
                        ncpus = 2)

roll_proto
getChunks(roll_proto)
getLDA(roll_proto)
}

}
\seealso{
Other RollingLDA functions: 
\code{\link{as.RollingLDA}()},
\code{\link{getChunks}()},
\code{\link{updateRollingLDA}()}
}
\concept{RollingLDA functions}
