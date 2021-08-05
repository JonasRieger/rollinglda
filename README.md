# rollinglda
[![Build Status](https://travis-ci.org/JonasRieger/rollinglda.svg?branch=master)](https://travis-ci.org/JonasRieger/rollinglda)
[![CRAN](https://www.r-pkg.org/badges/version/rollinglda)](https://cran.r-project.org/package=rollinglda)
[![Coverage Status](https://coveralls.io/repos/github/JonasRieger/rollinglda/badge.svg?branch=master)](https://coveralls.io/github/JonasRieger/rollinglda?branch=master)

## Construct Consistent Time Series from Textual Data
RollingLDA is a rolling version of the Latent Dirichlet Allocation. By an sequential approach, it enables the construction of LDA-based time series of topics that are consistent with previous states of LDA models. After an initial modeling, updates can be computed efficiently, allowing for real-time monitoring and detection of events or structural breaks.

## Citation
Please cite package using the BibTeX entry, which is obtained by the call ``citation("rollinglda")``.

## References
* TBA

## Related Software
* [tm](https://CRAN.R-project.org/package=tm) is useful for preprocessing text data.
* [lda](https://CRAN.R-project.org/package=lda) offers a fast implementation of the Latent Dirichlet Allocation and is used by ``ldaPrototype``.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) offers a implementation of a model selection algorithm to increase the reliability of interpretations taken from LDA results.
* [quanteda](https://quanteda.io/) is a framework for "Quantitative Analysis of Textual Data".
* [stm](https://www.structuraltopicmodel.com/) is a framework for Structural Topic Models.
* [tosca](https://github.com/Docma-TU/tosca) is a framework for statistical methods in content analysis including visualizations and validation techniques. It is also useful for managing and manipulating text data to a structure requested by ``ldaPrototype``.
* [topicmodels](https://CRAN.R-project.org/package=topicmodels) is another framework for various topic models based on the Latent Dirichlet Allocation and Correlated Topics Models.

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
