\name{cac}
\alias{cac}

\title{A General Method for Calculating the RMAC and FMAC for Continuous Data}
\description{Calculates the FMAC or RMAC for continuous data sets.  When the squared difference function is used with the FMAC, it is equivalent to Lin's concordance correlation coefficient.}

\usage{cac(x,type = c("absolute", "squared"), method = c("fmac", "rmac") , alternative = c("two.sided", "less", "greater"), conf.level= 0.95, na.rm = FALSE, numr = 999)}

\arguments{
  \item{x}{matrix or data frame of responses, where the responses are in columns 1 and 2}
  \item{type}{either "absolute" or "squared"; indicates the cost function}
  \item{method}{indicates which method of calculating the agreement coefficient to use}
  \item{alternative}{either "two.sided", "less", "greater"; indicates the t-test if using the squared difference function}
  \item{conf.level}{confidence level for interval}
  \item{na.rm}{logical, remove missing values for both if missing response for either}
  \item{numr}{integer indicating the number of bootstrapping samples to use (R)}
}
\details{
The function assumes that the input vectors are the first two columns of \code{x}. This function calculates the FMAC and RMAC for continuous data sets using one of two built in cost functions.  If the squared difference cost function is used with the RMAC, this function uses an optimized method. Otherwise, this function uses the general methods, \code{\link{fmacBoot}} and \code{\link{rmacBoot}}.

The confidence intervals are calculated using the BCa method (as per \code{\link{fmacBoot}} and \code{\link{rmacBoot}}), unless the squared difference cost function is used, in which case the delta method is used (See the supplement for Fay (2005) for a description.).
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
Fay, M.P. (2005). Random marginal agreement coefficients: Rethinking the adjustment for chance in agreement coefficients. Biostatistics, 6: 171-180.

Lin, L. I. (1989) A concordance correlation coefficient to evaluate reproducibility. Biometrics. 45: 255-268. (Correction: 2000, pp. 324-325)}

\author{Jennifer Kirk (using functions written by M.P. Fay)}

\seealso{\code{\link{rmac-package}}, \code{\link{rmacBoot}}, \code{\link{wkappa}}}

\examples{
# A simple example
set.seed(12321)
x<-rnorm(10,1+(1:10))
y<-rnorm(10,(1:10))

plot(x,y,xlim=c(0,12),ylim=c(0,12))
lines(c(0,12),c(0,12))

example<- cbind(x,y)

cac(example, type = "absolute", method = "fmac")
cac(example, type = "absolute", method = "rmac")
#cor.test(x,y,alternative="two.sided")

}

\keyword{htest}

