\name{rmacBoot}
\alias{rmacBoot}
\alias{fmacBoot}
\alias{rmacBoot.default}
\alias{rmacBoot.table}
\alias{fmacBoot.default}
\alias{fmacBoot.table}

\title{A General Method for Calculating the RMAC and FMAC using BCa Bootstrapping}
\description{Calculates the RMAC and FMAC using the ideal bootstrap estimate and calculates the confidence interval using BCa bootstrapping, given a specific cost function.}

\usage{
rmacBoot(x,...)
fmacBoot(x,...)

\S3method{rmacBoot}{default}(x,cost, conf.level= 0.95, na.rm = FALSE, numr = 999,...)
\S3method{rmacBoot}{table}(x,cost, conf.level= 0.95, na.rm = FALSE, numr = 999,...)

\S3method{fmacBoot}{default}(x,cost, conf.level= 0.95, na.rm = FALSE, numr = 999, ...)
\S3method{fmacBoot}{table}(x,cost, conf.level= 0.95, na.rm = FALSE, numr = 999, ...)

}

\arguments{
  \item{x}{table, matrix or data frame of responses, where the responses are in columns 1 and 2 of the matrix or data frame}
  \item{cost}{a function that defines the cost function}
  \item{conf.level}{confidence level for interval}
  \item{na.rm}{logical, remove missing values for both if missing response for either}
  \item{numr}{integer indicating the number of bootstrapping samples to use (R)}
  \item{...}{any other arguments passed to the function; not currently used}
}

\details{
For categorical data, these functions accept data in table form, ie. a table of counts.  If the data is in a table, it is first converted to two vectors of scores before the agreement coefficient is calculated. The default methods assumes that the data are in the first two columns of \code{x}.

If the two input vectors are not the same length, an error is retuned.

The cost function should have two imputs, corresponding to individual values from the two input vectors.  See below for an example of a cost function.

The confindence intervals are calculated using the built-in R function \code{boot.out} with the BCa option.
}

\value{
  A list with class "htest" containing the following components is returned:
  \item{method}{a character string describing the statistical method used}
  \item{statistic}{the value of the test statistic with a name describing it}
  \item{conf.int}{a confidence internal for the agreement coefficient}
  \item{estimate}{an estimate of the agreement coefficient}
  \item{alternative}{a character string describing the altnative hypothesis}
  \item{p.value}{the p-value for the test}
  \item{data.name}{a character string giving the names of the data}

}
\references{
Fay, M.P. (2005). Random marginal agreement coefficients: Rethinking the adjustment for chance in agreement coefficients. Biostatistics, 6: 171-180. }

\author{Jennifer Kirk}

\seealso{See also: \code{\link{rmac-package}}, \code{\link{cac}}, \code{\link{wkappa}}}

\examples{
#A simple example of a categorical data set (scores are 1 or 2)
set.seed(41919)
measure1 <- c( rep(1,15), rep(2,5) )
measure2 <- c( rep(1,12), rep(2,8) )
measures<- cbind(measure1, measure2)

#the cost function
nominal <- function(p,q){ 	test <- abs(p-q)
	ifelse(test > 0, 1, 0) }

#the default method
fmacBoot(measures, cost = "nominal")
rmacBoot(measures, cost = "nominal")

#the table method
mtable<- table(measure1, measure2)
fmacBoot(mtable, cost = "nominal")
rmacBoot(mtable, cost = "nominal")

#A simple example of a continuous data set
set.seed(12321)
x<-rnorm(10,1+(1:10))
y<-rnorm(10,(1:10))

plot(x,y,xlim=c(0,12),ylim=c(0,12))
lines(c(0,12),c(0,12))

example<- cbind(x,y)
absolute<- function(p,q){ abs(p-q) }

#Now with the absolute cost function
fmacBoot(example,"absolute")

#this is an alternative method to the cac function
rmacBoot(example,"absolute")
cor.test(x,y,alternative="two.sided")
}

\keyword{htest}
