\name{modSearch}
\alias{modSearch}
\title{Internal function for speff()}
\description{\code{modSearch} is used internally by \code{speff} to construct an optimal model for 
prediction of the study endpoint or estimation of the missingness mechanism.}
\usage{
modSearch(formula, x, y, endpoint, method, optimal)
}
\arguments{
  \item{formula}{a formula object with the response on the left of the \code{~} operator, and the linear
  predictor on the right.}
  \item{x}{a matrix of at least two predictors}
  \item{y}{a response vector}
  \item{endpoint}{a character string specifying the type of the response variable; possible values are 
  "\code{quantitative}" or "\code{dichotomous}".}
  \item{method}{a character string specifying the type of search technique used in the model selection procedure; 
  possible values are "\code{exhaustive}", "\code{forward}", or \cr "\code{backward}".}
  \item{optimal}{specifies the optimization criterion for model selection; possible values are "\code{cp}" for 
  Mallow's Cp, "\code{bic}" for BIC, and "\code{rsq}" for R-squared.}
}
\seealso{\code{\link{speff}}}
