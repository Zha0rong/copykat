% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseline.GMM.R
\name{baseline.GMM}
\alias{baseline.GMM}
\title{pre-define a group of normal cells with GMM.}
\usage{
baseline.GMM(
  CNA.mat,
  max.normal = 5,
  mu.cut = 0.05,
  Nfraq.cut = 0.99,
  RE.before = basa,
  n.cores = 1,
  maxit = 10000
)
}
\arguments{
\item{CNA.mat}{smoothed data matrix; genes in rows; cell names in columns.}

\item{max.normal}{find the first number diploid cells to save efforts.}

\item{mu.cut}{diploid baseline cutoff.}

\item{Nfraq.cut}{minimal fraction of genomes with CNAs.}
}
\value{
1) predefined diploid cell names; 2) clustering results; 3) inferred baseline.
}
\description{
pre-define a group of normal cells with GMM.
}
\examples{
test.gmm <- baseline.GMM(CNA.mat=smooth.com, max.normal=30, mu.cut=0.05, Nfraq.cut=0.99)

test.gmm.cells <- test.bnc$preN
}
