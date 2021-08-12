# rollinglda
[![Build Status](https://travis-ci.org/JonasRieger/rollinglda.svg?branch=master)](https://travis-ci.org/JonasRieger/rollinglda)
[![CRAN](https://www.r-pkg.org/badges/version/rollinglda)](https://cran.r-project.org/package=rollinglda)
[![Coverage Status](https://coveralls.io/repos/github/JonasRieger/rollinglda/badge.svg?branch=master)](https://coveralls.io/github/JonasRieger/rollinglda?branch=master)

## Construct Consistent Time Series from Textual Data
RollingLDA is a rolling version of the Latent Dirichlet Allocation. By an sequential approach, it enables the construction of LDA-based time series of topics that are consistent with previous states of LDA models. After an initial modeling, updates can be computed efficiently, allowing for real-time monitoring and detection of events or structural breaks.

## Citation
Please cite the package using the BibTeX entry, which is obtained by the call ``citation("rollinglda")``.

## References
* TBA

## Related Software
* [tm](https://CRAN.R-project.org/package=tm) is useful for preprocessing text data.
* [lda](https://CRAN.R-project.org/package=lda) offers a fast implementation of the Latent Dirichlet Allocation and is used by ``ldaPrototype``.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) offers a implementation of a model selection algorithm to increase the reliability of interpretations taken from LDA results and is used by ``rollinglda``.
* [quanteda](https://quanteda.io/) is a framework for "Quantitative Analysis of Textual Data".
* [stm](https://www.structuraltopicmodel.com/) is a framework for Structural Topic Models.
* [tosca](https://github.com/Docma-TU/tosca) is a framework for statistical methods in content analysis including visualizations and validation techniques. It is also useful for managing and manipulating text data to a structure requested by ``ldaPrototype`` and ``rollinglda``.
* [topicmodels](https://CRAN.R-project.org/package=topicmodels) is another framework for various topic models based on the Latent Dirichlet Allocation and Correlated Topics Models.
* [(c)dtm](https://github.com/blei-lab/dtm) is an implementation of dynamic topic models.

## Related Methods
* [TM-LDA](https://doi.org/10.1145/2339530.2339552) is an online modeling approach for latent topics (especially in social media).
* [Streaming-LDA](https://doi.org/10.1145/2939672.2939781) is a Copula-based approach to model document streams.
* [Topics over Time](https://doi.org/10.1145/1150402.1150450) is a continuous time model for word co-occurences.
* [This paper](https://doi.org/10.1145/2020408.2020551) presents a time-dependent topic model for multiple text streams.
* [This paper](https://papers.nips.cc/paper/2010/hash/71f6278d140af599e06ad9bf1ba03cb0-Abstract.html) presents an approach of online learning for Latent Dirichlet Allocation.

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
TBA
