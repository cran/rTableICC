\name{rTable.2x2xK}
\alias{rTable.2x2xK}

\title{
Randomly Generate 2x2xK Contingency Tables
}
\description{
A generic function that generates 2 x 2 x K contingency tables under product multinomial, multinomial or Poisson sampling plans.
}
\usage{
rTable.2x2xK(p,sampling="Multinomial",N,K=NULL,lambda=NULL,print.raw=FALSE)
}
\arguments{
  \item{p}{
  A finite  \eqn{2\times 2\times K} matrix of cell probabilities.
  }
  \item{sampling}{
  Sampling plan. It takes 'Product' for product multinomial sampling, 'Multinomial' for multinomial sampling, and 'Poisson' for Poisson sampling plans.
  } 
  \item{N}{
  Total number of individuals to be generated under product multinomial or multinomial sampling plans. It is a vector of positive integers containing total number of observations in each center under product multinomial sampling plan, a positive integer of total sample size under all centers under multinomial sampling plan, and not required under Poisson sampling plan. If \code{N} is not a positive integer, its converted to do so.
  }  
\item{K}{
  Number of centers. It must be supplied if a scalar is entered as the value of \code{lambda}.
  }  
  \item{lambda}{
  Mean number of individuals in each cell of table. It is either a \eqn{2\times 2\times K} positive matrix or a positive scalar under Poisson sampling plan and not required for both multinomial and product multinomial sampling plans. If a positive scalar is entered, mean number of individuals in each cell will be equal to each other.
  }    
  \item{print.raw}{
  If \code{TRUE}, generated raw data is printed on the screen.
  }   
}
\details{
To generate random tables under multinomial sampling plan, multinomial distribution with entered cell probabilities and total number of observations is directly used.

To generate random tables under product multinomial sampling plan, center totals must be entered by \code{N}. It is not possible to fix any dimension of 2x2 table under each center. Suppose that center totals are denoted by \eqn{n_{ij+}}, where \eqn{i,j=1,2}. Then with the counts satisfying \eqn{\sum_{ij}n_{ijk}=n_{ij+}}, we have the following multinomial form that \code{rTable.RxC} uses (Agresti, 2002):

\deqn{
      \frac{n_{ij+}!}{\prod_{ij}n_{ijk}!}\prod_{ij}p_{ij|k}^{n_{ijk}},
      } 

where \eqn{k=1,\dots,K}, \eqn{n_{ijk}} is the count of cell \eqn{(i,j,k)}, and given that an individual is in \eqn{k}th center, \eqn{p_{ij|k}} is the conditional probability of being the cell \eqn{(i,j)} of 2x2 table. This multinomial form is used to generate data under each center.

To generate random tables under Poisson sampling plan, Poisson distribution with entered mean cell counts is directly used.
}
\value{
 A list with the following elements:
\item{rTable}{A \eqn{2\times 2\times K} dimensional array including generated 2 x 2 x K contingency table.}
\item{rTable.raw}{Generated table in a \eqn{N\times 3} dimensional matrix in raw data format. First columns represent 2x2 table and the third is for center.}
\item{N}{Total number of generated individuals.}
\item{sampling}{Used sampling plan in data generation.}
\item{K}{Number of centers.}
\item{ICC}{Returns \code{FALSE} to indicate the data is generated without intraclass-correlated clusters.}
\item{structure}{Returns "2 x 2 x K" to indicate structure of generated table is "2 x 2 x K."}
\item{print.raw}{\code{TRUE} if generated table will be printed in raw data format.}
\item{print.regular}{\code{TRUE} if generated table will be printed in the format determined by \code{structure}.}
}
\references{
Agresti A. (2002) \emph{Categorical Data Analysis}, Wiley, New York.

Demirhan, H. and Hamurkaroglu, C. (2008) Bayesian estimation of log odds ratios from RxC and 2 x 2 x K contingency tables, \emph{Statistica Neerlandica} \bold{62}, 405-424.

Kroese D.P., Taimre T., Botev Z.I. (2011) \emph{Handbook of Monte Carlo Methods}, Wiley, New York. 
}
\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}

\examples{
# --- Generate a 2x2x8 contingency table under multinomial sampling plan ---
num.centers=8                                   # Number of centers
sampl="Multinomial"                             # Generate table under multinomial 
                                                #  sampling plan
cell.prob=array(0.03125,dim=c(2,2,num.centers)) # Enter cell probabilities in 2x2xK format 
num.obs=124                                     # Number of observations

x=rTable.2x2xK(p=cell.prob,sampling=sampl,N=num.obs)
print(x)

# --- Generate a 2x2x8 contingency table under product multinomial sampling plan ---
sampl="Product"                                 # Generate table under product 
                                                #  multinomial sampling plan
center.margins=array(10,num.centers)            # Enter center margins 

y=rTable.2x2xK(p=cell.prob,sampling=sampl,N=center.margins)
print(y)

# --- Generate a 2x2x8 contingency table under Poisson sampling plan ---
num.centers=3
sampl="Poisson"                                 # Generate table under Poisson 
                                                #  sampling plan
cell.mean=array(3,dim=c(2,2,num.centers))       # Enter mean number of individuals 
                                                #  in each cell

z=rTable.2x2xK(sampling=sampl,lambda=cell.mean)
print(z)
}
\keyword{ datagen }
\keyword{contingency}
\keyword{Poisson}
\keyword{multinomial}
\keyword{product}
