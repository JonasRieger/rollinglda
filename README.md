# rollinglda
[![CRAN](https://www.r-pkg.org/badges/version/rollinglda)](https://cran.r-project.org/package=rollinglda)
[![R build status](https://github.com/JonasRieger/rollinglda/workflows/R-CMD-check/badge.svg)](https://github.com/JonasRieger/rollinglda/actions)
[![Build status](https://ci.appveyor.com/api/projects/status/88s2flwd8o4b067k?svg=true)](https://ci.appveyor.com/project/JonasRieger/rollinglda)
[![codecov](https://codecov.io/gh/JonasRieger/rollinglda/branch/main/graph/badge.svg?token=6BM1Z3A2D8)](https://app.codecov.io/gh/JonasRieger/rollinglda)
[![DOI](https://zenodo.org/badge/392967136.svg)](https://zenodo.org/badge/latestdoi/392967136)

## Construct Consistent Time Series from Textual Data
RollingLDA is a rolling version of the Latent Dirichlet Allocation. By a sequential approach, it enables the construction of LDA-based time series of topics that are consistent with previous states of LDA models. After an initial modeling, updates can be computed efficiently, allowing for real-time monitoring and detection of events or structural breaks.

## Citation
Please cite the package using the BibTeX entry, which is obtained by the call ``citation("rollinglda")``.

## References (related to the methodology)
* Rieger, J., Jentsch, C. & Rahnenführer, J. (2021). RollingLDA: An Update Algorithm of Latent Dirichlet Allocation to Construct Consistent Time Series from Textual Data. [EMNLP Findings 2021](https://doi.org/10.18653/v1/2021.findings-emnlp.201), pp. 2337–2347.

Please also have a look at this short overview on topic modeling in R:
* Wiedemann, G. (2022). The World of Topic Modeling in R. [M&K Medien & Kommunikationswissenschaft](https://doi.org/10.5771/1615-634X-2022-3-286), 70(3), pp. 286-291.

## Related Software
* [tm](https://CRAN.R-project.org/package=tm) is useful for preprocessing text data.
* [lda](https://CRAN.R-project.org/package=lda) offers a fast implementation of the Latent Dirichlet Allocation and is used by ``ldaPrototype``.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) offers a implementation of a model selection algorithm to increase the reliability of interpretations taken from LDA results and is used by ``rollinglda``.
* [quanteda](https://quanteda.io/) is a framework for "Quantitative Analysis of Textual Data".
* [stm](https://www.structuraltopicmodel.com/) is a framework for Structural Topic Models.
* [tosca](https://github.com/Docma-TU/tosca) is a framework for statistical methods in content analysis including visualizations and validation techniques. It is also useful for managing and manipulating text data to a structure requested by ``ldaPrototype`` and ``rollinglda``.
* [topicmodels](https://CRAN.R-project.org/package=topicmodels) is another framework for various topic models based on the Latent Dirichlet Allocation and Correlated Topics Models.
* [(c)dtm](https://github.com/blei-lab/dtm) is an implementation of dynamic topic models.
* [Online LDA](https://github.com/blei-lab/onlineldavb) is an implementation of online learning for Latent Dirichlet Allocation.

## Related Methods
* [TM-LDA](https://doi.org/10.1145/2339530.2339552) is an online modeling approach for latent topics (especially in social media).
* [Streaming-LDA](https://doi.org/10.1145/2939672.2939781) is a Copula-based approach to model document streams.
* [Topics over Time](https://doi.org/10.1145/1150402.1150450) is a continuous time model for word co-occurences.
* [This paper](https://doi.org/10.1145/2020408.2020551) presents a time-dependent topic model for multiple text streams.

## Contribution
This R package is licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).
For bug reports (lack of documentation, misleading or wrong documentation, unexpected behaviour, ...) and feature requests please use the [issue tracker](https://github.com/JonasRieger/rollinglda/issues).
Pull requests are welcome and will be included at the discretion of the author.

## Installation
```{R}
install.packages("rollinglda")
```
For the development version use [devtools](https://cran.r-project.org/package=devtools):
```{R}
devtools::install_github("JonasRieger/rollinglda")
```

## (Quick Start) Example
Load the package and the example dataset rom Wikinews consisting of 576 articles - [tosca](https://github.com/Docma-TU/tosca) or [quanteda](https://quanteda.io/) can be used to manipulate text data to the format requested by ``rollinglda``: The texts should be provided as a uniquely named list of tokenized texts, and the associated dates should be provided either as a named vector of dates or (at least) in the same order as the passed texts.
```{R}
library(rollinglda)
data(economy_texts)
data(economy_dates)
```
Then, the modeling is similar to the modeling of a standard latent Dirichlet allocation (LDA) by specifying the data ``texts`` and ``dates``, the parameters ``K``, ``alpha`` (default: ``1/K``), ``eta`` (default: ``1/K``) and ``num.iterations`` (default: ``200``), as well as the parameters ``chunks``, ``memory``, ``init`` and ``type`` relevant for the RollingLDA. By means of ``chunks`` the user determines at which interval steps the texts are to be modeled, starting from one day after ``init``, the date specifying the end of the initialization period for which a standard LDA (``type = "lda"``) or LDAPrototype (``type = "ldaprototype"``) is modeled. In addition, ``memory`` specifies how much knowledge about the past model should be used for each interval (``chunk``).

In the case below, the 576 Wikinews texts are initially modeled up to July 3rd, 2008. Starting from that, the modeling is executed quarterly, namely with the start dates July 4th, 2008 and October 4th, 2008 (see ``getChunks``). The texts published in the corresponding periods are modeled together, each with the last three quarters as memory, thus corresponding to October 4th, 2007 and January 4th, 2008, respectively. Note that the modeling is stochastic for both scenarios, using ``type = "lda"`` and using the default ``type = "ldaprototype"`` (see [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) package) as initial modeling step, i.e. the results will be fully reproducible only when using the same ``seeds``.
```{R}
roll_lda = RollingLDA(texts = economy_texts,
                      dates = economy_dates,
                      chunks = "quarter",
                      memory = "3 quarter",
                      init = "2008-07-03",
                      K = 10,
                      type = "lda",
                      seeds = 42)
# Fitting LDA as initial model.
# Exporting objects to package env on master for mode: local
# Fitting Chunk 1/2.
# Fitting Chunk 2/2.
# Compute topic matrix.
```
Using the function ``getChunks`` a lot of information about the modeling can be displayed. For some of these values further parameters of the method (see ``?RollingLDA``) are also relevant.
```{R}
getChunks(roll_lda)
#    chunk.id start.date   end.date     memory   n n.discarded n.memory n.vocab
# 1:        0 2007-01-01 2008-07-03       <NA> 470           2       NA    2691
# 2:        1 2008-07-05 2008-09-30 2007-10-04  50           0      204    2720
# 3:        2 2008-10-04 2008-12-29 2008-01-04  54           0      186    2814
```
It is noticeable that the ``start.date`` of the first chunk is not 4th July, 2008. This is due to the fact that there are no texts for this day. The table shows the actual minimum and maximum dates per chunk. From ``n.vocab`` one can see how the vocabulary of the model increases due to the (frequent enough, see parameters ``vocab.abs``, ``vocab.rel`` and ``vocab.fallback``) use of new words within the observation intervals.

You can use ``getLDA`` to convert a `RollingLDA` object into a standard ``LDA`` object, which can be further processed using several functions from the [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) and [tosca](https://github.com/Docma-TU/tosca) packages. You can also use ``getVocab`` to get the entire vocabulary of the model.
```{R}
roll_lda
# RollingLDA Object named "rolling-lda" with elements
# "id", "lda", "docs", "dates", "vocab", "chunks", "param"
#  3 Chunks with Texts from 2007-01-01 to 2008-12-29
#  vocab.abs: 5, vocab.rel: 0, vocab.fallback: 100, doc.abs: 0
# 
# LDA Object with element(s)
# "param", "assignments", "topics", "document_sums"
#  574 Texts with mean length of 120.68 Tokens
#  2814 different Words
#  K: 10, alpha: 0.1, eta: 0.1, num.iterations: 200

getLDA(roll_lda)
# LDA Object with element(s)
# "param", "assignments", "topics", "document_sums"
#  574 Texts with mean length of 120.68 Tokens
#  2814 different Words
#  K: 10, alpha: 0.1, eta: 0.1, num.iterations: 200
```
Finally, such an existing model ``roll_lda`` can be updated using the ``updateRollingLDA`` function. Note that the ``RollingLDA`` function can also be used for updating if the first argument in the function call is the ``RollingLDA`` object to be updated. Have a look at the help page ``?updateRollingLDA`` for a minimal example of updating an existing model.
