\name{futile.any-package}
\alias{futile.any-package}
\alias{futile.any}
\docType{package}
\title{ Futile library to provide some polymorphism }
\description{
  This package contains a collection of utility functions that provide 
  polymorphism over certain data types. These any* functions 
  attempt to consolidate attribute access of lists, vectors, matrices, arrays,
  and other data structures.
}
\details{
\tabular{ll}{
Package: \tab futile.any\cr
Type: \tab Package\cr
Version: \tab 1.3.0\cr
Date: \tab 2013-02-02\cr
License: \tab LGPL-3\cr
LazyLoad: \tab yes\cr
}
  The anylength and anynames functions consolidate attribute access across many
  data structures providing a bit of convenience via polymorphism. The anytypes
  function provides the classes or types of a data.frame-like object. This is
  useful when parsing data and it is not always clear how values will be 
  parsed.
}
\author{
Brian Lee Yung Rowe <r@zatonovo.com>
}
\keyword{ package }
\keyword{ attribute }
\keyword{ logic }
\seealso{ \code{\link{anylength}}, \code{\link{anynames}}, \code{\link{anytypes}} }
\examples{
  m <- matrix(c(1,2,3,4,5,6), ncol=2)
  anylength(m)

  v <- c(1,2,3,4,5)
  anylength(v)

  m <- matrix(c(1,2,3,4,5,6), ncol=2)
  anynames(m) <- c('d','e')
  anynames(m)

  v <- c(a=1,b=2,c=3,d=4,e=5)
  anynames(v)

  l <- list(a=1,b=2,c=3,d=4,e=5)
  anynames(l)
}
