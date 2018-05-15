\name{rTable.RxC}
\alias{rTable.RxC}

\title{
Randomly Generate R x C Contingency Tables
}
\description{
A generic function that generates an RxC contingency table under product multinomial, multinomial, or Poisson sampling plans.
}
\usage{
rTable.RxC(p,row.margins=NULL,col.margins=NULL,sampling="Multinomial",N,
            lambda=NULL,print.raw=FALSE)
}

\arguments{
  \item{p}{
  A finite \eqn{R\times C} matrix of cell probabilities. It is not required under Poisson sampling plan.
  }
  \item{row.margins}{
  Includes fixed row margins under product multinomial sampling plan and not required for both multinomial and Poisson sampling plans. 
  }
  \item{col.margins}{
  Includes fixed column margins under product multinomial sampling plan and not required for both multinomial and Poisson sampling plans.
  }  
  \item{sampling}{
  Sampling plan. It takes 'Product' for product multinomial sampling, 'Multinomial' for multinomial sampling, and 'Poisson' for Poisson sampling plans.
  } 
  \item{N}{
  Total number of individuals to be generated under product multinomial or multinomial sampling plans. It is a positive integer of total sample size under all centers for multinomial sampling plan and not required for both product multinomial and Poisson sampling plans. If \code{N} is not a positive integer, its converted to do so.
  }
  \item{lambda}{
  Mean number of individuals in each cell of table. It is either a \eqn{R\times C} positive matrix or a positive scalar under Poisson sampling plan and not required for both multinomial and product multinomial sampling plans. If a positive scalar is entered, mean number of individuals in each cell will be equal to each other.
  }   
  \item{print.raw}{
  If \code{TRUE}, generated raw data is printed on the screen.
  } 
}
\details{
To generate random tables under multinomial sampling plan, multinomial distribution with entered cell probabilities and total number of observations is directly used.

To generate random tables under product multinomial sampling plan, at least one of \code{row.margins} or \code{col.margins} must be entered. Because both cell probabilities and fixed row or column margins are entered at the same time, margin probabilities calculated over fixed margins and entered \eqn{R\times C} matrix of cell probabilities must be equal to each other. Suppose that row totals are fixed and \eqn{n_{i+}} denote fixed row margins. Then with the counts satisfying \eqn{\sum_{j}n_{ij}=n_{i+}}, we have the following multinomial form that \code{rTable.RxC} uses (Agresti, 2002):

\deqn{
      \frac{n_{i+}!}{\prod_{j}n_{ij}!}\prod_{j}p_{j|i}^{n_{ij}},
      } 

where \eqn{j=1,\dots,C}, \eqn{n_{ij}} is the count of cell \eqn{(i,j)}, and given that an individual is in \eqn{i}th row, \eqn{p_{j|i}} is the conditional probability of being in \eqn{j}th column of table. This multinomial form is used to generate data under each row margin. When column totals are fixed the same manner as the case of fixed row totals is followed.

To generate random tables under Poisson sampling plan, Poisson distribution with entered mean cell counts is directly used.
}
\value{
 A list with the following elements:
\item{rTable}{An \eqn{R\times C} dimensional matrix including generated R x C contingency table.}
\item{rTable.raw}{Generated table in a \eqn{N\times 2} dimensional matrix in raw data format. Columns represent row and column numbers where each individual falls.}
\item{N}{Total number of generated individuals.}
\item{sampling}{Used sampling plan in data generation.}
\item{R}{Number of rows.}
\item{C}{Number of columns.}
\item{ICC}{Returns \code{FALSE} to indicate the data is generated without intraclass-correlated clusters.}
\item{structure}{Returns "R x C" to indicate structure of generated table is "R x C."}
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
# --- Generate a 5x7 contingency table under multinomial sampling plan ---
num.row=5                                       # Number of rows
num.col=7                                       # Number of columns
sampl="Multinomial"                             # Generate table under 
                                                #  multinomial sampling plan
cell.prob=array(1/35,dim=c(num.row,num.col))    # Enter cell probabilities 
                                                #  in RxC format 
num.obs=124                                     # Number of observations

x=rTable.RxC(p=cell.prob,sampling=sampl,N=num.obs)
print(x)

# --- Generate a 3x3 contingency table under product multinomial sampling plan  ---
# --- with fixed row margins                                                    ---
num.row=3                                       # Number of rows
num.col=3                                       # Number of columns
row=c(32,12,11)                                 # Fixed row counts
sampl="Product"                                 # Generate table under product 
                                                #  multinomial sampling plan
cell.prob=array(0,dim=c(num.row,num.col))       # Enter cell probabilities in RxC format 
cell.prob[1,1]=0.12
cell.prob[1,2]=0.24
cell.prob[1,3]=32/55-0.36
cell.prob[2,1]=0.07
cell.prob[2,2]=0.1
cell.prob[2,3]=12/55-0.17
cell.prob[3,1]=0.05
cell.prob[3,2]=0.10
cell.prob[3,3]=11/55-0.15                       # Marginal and cell probabilities  
                                                #  should be equal to each other

y1=rTable.RxC(p=cell.prob,sampling=sampl,row.margins=row)
print(y1)

# --- Generate a 3x3 contingency table under product multinomial sampling plan ---
# --- with fixed row margins                                                   ---
num.row=3                                       # Number of rows
num.col=3                                       # Number of columns
col=c(5,5,10)                                   # Fixed row counts
sampl="Product"                                 # Generate table under product 
                                                #  multinomial sampling plan
cell.prob=array(0,dim=c(num.row,num.col))       # Enter cell probabilities in RxC format 
cell.prob[1,1]=0.1
cell.prob[1,2]=0.1
cell.prob[1,3]=0.05
cell.prob[2,1]=0.05
cell.prob[2,2]=0.1
cell.prob[2,3]=0.1
cell.prob[3,1]=0.3
cell.prob[3,2]=0.1
cell.prob[3,3]=0.1                             # Marginal and cell probabilities 
                                               #  should be equal to each other

y2=rTable.RxC(p=cell.prob,sampling=sampl,col.margins=col)
print(y2)

# --- Generate a 6x4 contingency table under Poisson sampling plan ---
num.row=6                                       # Number of rows
num.col=4                                       # Number of columns
sampl="Poisson"                                 # Generate table under Poisson 
                                                #  sampling plan
cell.mean=array(3,dim=c(6,4))                   # Enter mean number of individuals 
                                                #  in each cell

z=rTable.RxC(lambda=cell.mean,sampling=sampl)
print(z)

}
\keyword{ datagen }
\keyword{contingency}
\keyword{Poisson}
\keyword{multinomial}
\keyword{product}