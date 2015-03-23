\name{rDiscrete}
\alias{rDiscrete}

\title{
Generate a Random Data from Discrete Probability Function
}
\description{
Generates random data from a given empirical probability function. It also returns cumulative distribution function corresponding to the entered probability function.
}
\usage{
rDiscrete(n = 1, pf)
}
\arguments{
  \item{n}{
number of observations.
}
  \item{pf}{
empirical probability function.
}
}
\details{
\code{pf} is an array of any dimensionality with all elements sum up to one. If its dimension is greater than one, it is transformed to a row vector column-by-column. 
}
\value{
A list including 
\item{rdiscrete}{an \eqn{nx1} vector that gives generated random data.}
\item{cdf}{a vector including cumulative distribution function.} 
}
\references{
Kroese D.P., Taimre T., Botev Z.I. (2011) \emph{Handbook of Monte Carlo Methods}, Wiley, New York. 
}
\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}

\examples{
p = c(0.23,0.11,0.05,0.03,0.31,0.03,0.22,0.02)
rDiscrete(n=2,pf=p)

# pf would be entered as a matrix:

p = matrix(c(0.23,0.11,0.05,0.03,0.31,0.03,0.22,0.02), nrow=2, ncol=4, byrow = TRUE) 
rDiscrete(n=2,pf=p)

p = matrix(c(0.23,0.11,0.05,0.03,0.31,0.03,0.22,0.02), nrow=4, ncol=2, byrow = TRUE) 
rDiscrete(n=2,pf=p)

# or pf would be entered as a three dimensional array:

p = array(c(0.23,0.11,0.05,0.03,0.31,0.03,0.22,0.02), dim=c(2,2,2))
rDiscrete(n=2,pf=p)
}
\keyword{datagen}
\keyword{discrete}
\keyword{distribution}
