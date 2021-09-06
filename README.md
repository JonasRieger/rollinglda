# rollinglda
[![CRAN](https://www.r-pkg.org/badges/version/rollinglda)](https://cran.r-project.org/package=rollinglda)
[![R build status](https://github.com/JonasRieger/rollinglda/workflows/R-CMD-check/badge.svg)](https://github.com/JonasRieger/rollinglda/actions)
[![Build status](https://ci.appveyor.com/api/projects/status/88s2flwd8o4b067k?svg=true)](https://ci.appveyor.com/project/JonasRieger/rollinglda)
[![codecov](https://codecov.io/gh/JonasRieger/rollinglda/branch/main/graph/badge.svg?token=6BM1Z3A2D8)](https://codecov.io/gh/JonasRieger/rollinglda)
[![DOI](https://zenodo.org/badge/392967136.svg)](https://zenodo.org/badge/latestdoi/392967136)

## Construct Consistent Time Series from Textual Data
RollingLDA is a rolling version of the Latent Dirichlet Allocation. By a sequential approach, it enables the construction of LDA-based time series of topics that are consistent with previous states of LDA models. After an initial modeling, updates can be computed efficiently, allowing for real-time monitoring and detection of events or structural breaks.

## Citation
Please cite the package using the BibTeX entry, which is obtained by the call ``citation("rollinglda")``.

## References
* Rieger, J., Jentsch, C. & Rahnenführer, J. (2021). RollingLDA: An Update Algorithm of Latent Dirichlet Allocation to Construct Consistent Time Series from Textual Data. Accepted for [Findings of EMNLP 2021](https://www.statistik.tu-dortmund.de/fileadmin/user_upload/Lehrstuehle/IWuS/Forschung/rollinglda.pdf).

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
#TBA install.packages("rollinglda")
```
For the development version use [devtools](https://cran.r-project.org/package=devtools):
```{R}
devtools::install_github("JonasRieger/rollinglda")
```

## (Quick Start) Example
TBA
