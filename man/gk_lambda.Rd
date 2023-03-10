% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gk_lambda.R
\encoding{UTF-8}
\name{gk_lambda}
\alias{gk_lambda}
\title{Computes the Goodman and Kruskal's lambda of a categorical time series}
\usage{
gk_lambda(series, lag = 1, categories, features = FALSE)
}
\arguments{
\item{series}{A CTS.}

\item{lag}{The considered lag (default is 1).}

\item{categories}{A vector of type factor containing the corresponding
categories.}

\item{features}{Logical. If \code{features = FALSE} (default), the value of Goodman and Kruskal's lambda is returned. Otherwise, the function
returns a matrix with the individual components of Goodman and Kruskal's lambda}
}
\value{
If \code{features = FALSE} (default), returns the value of the Goodman and Kruskal's lambda. Otherwise, the function
returns a matrix of features, i.e., the matrix contains the features employed to compute the
Goodman and Kruskal's lambda.
}
\description{
\code{gk_lambda} returns the value of the Goodman and Kruskal's lambda for
a categorical time series
}
\details{
Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
\eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
estimated Goodman and Kruskal's lambda, \eqn{\widehat{\lambda}(l)=\frac{\sum_{j=1}^{r}\max_i\widehat{p}_{ij}(l)-\max_i\widehat{p}_i}{1-\max_i\widehat{p}_i}},
where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
category, and \eqn{\widehat{p}_{ij}(l)} is the natural estimate of the joint probability
for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. If \code{features = TRUE}, the function
returns a vector whose components are the quantities \eqn{\max_i\widehat{p}_{ij}(l)},
\eqn{i=1,2, \ldots,r}.
}
\examples{
gkl <- gk_lambda(series = GeneticSequences$data[[1]],
categories = factor(c('a', 'c', 'g', 't'))) # Computing the Goodman and Kruskal's lambda
# for the first series in dataset GeneticSequences
feature_matrix <- gk_lambda(series = GeneticSequences$data[[1]],
categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
# matrix of features
}
\references{
{

  \insertRef{weiss2008measuring}{ctsfeatures}

}
}
\author{
??ngel L??pez-Oriona, Jos?? A. Vilar
}
