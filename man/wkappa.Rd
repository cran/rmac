\name{wkappa}
\alias{wkappa}
\alias{wkappa.default}
\alias{wkappa.table}

\title{Kappa Agreement Statistics}
\description{Calculates Cohen's kappa and Scott's pi (also called random marginal agreement coefficient).}

\usage{
wkappa(x,...)

\S3method{wkappa}{default}(x, Wcode = 0, method = c("fmac", "rmac"), conf.int = TRUE, conf.level = 0.95, s = 1:k, ...)
\S3method{wkappa}{table}(x, Wcode = 0, method = c("fmac", "rmac"), conf.int = TRUE, conf.level = 0.95, s = 1:k, ...)

}

\arguments{
  \item{x}{table, matrix or data frame of responses, where the responses are in columns 1 and 2 of the matrix or data frame}
  \item{Wcode}{0=categorical weights, 1=absolute value of difference, 2=squared difference}
  \item{method}{indicates which method of calculating the agreement coefficient to use}
  \item{conf.int}{logical, calculate confidence intervals}
  \item{conf.level}{the confidence level}
  \item{s}{vector of scores for values}
  \item{...}{any other arguments passed to the function; not currently used}
}

\details{
If the input data is individual scores, it is converted to a contingency table and a list of factors is created based on the combined set of unique levels from each input data set.  This method includes factors that are present in one data set and absent in the other.

The default method assumes that the data is in the first two columns of \code{x}.

The confidence intervals are calculated using the delta method (See the supplement to Fay (2005) for more details.). If the number of zeros in the contigency matrix is too great, then the confidence intervals cannot be calculated and are set to (-1,1).
}

\value{
 Returns a vector of doubles containing the weighted kappa and/or random marginal agreement coefficient statistic and the upper and lower bounds of the confidence interval for each statistic.
 
 Note that if \code{do.ci = FALSE}, then the confidence interval upper and lower bounds are not returned.
}

\references{
Cohen, J. (1960). A coefficient of agreement for nominal scales. Educ. Psychol. Meas., 20: 37-46.

Cohen, J. (1968). Weighted kappa: Nominal scale agreement with provision for scaled disagreement or partial credit. Psychol. Bull., 70: 213-220.

Fay, M.P. (2005). Random marginal agreement coefficients: Rethinking the adjustment for chance in agreement coefficients. Biostatistics, 6: 171-180.

Scott, W. A. (1955). Reliability of content analysis: The case of nominal scale coding. Public Opinion Quart., 19: 321-325.}

\author{Jennifer Kirk (using functions written by M.P. Fay)}

\seealso{See also: \code{\link{rmac-package}}, \code{\link{rmacBoot}}, \code{\link{cac}}}

\examples{
#a simple example with two vectors of measurements (scores are 1 or 2)
set.seed(41919)
measure1 <- c( rep(1,15), rep(2,5) )
measure2 <- c( rep(1,12), rep(2,8) )
measures<- cbind(measure1, measure2)

#the default method
wkappa(measures, method = "fmac")
wkappa(measures, method = "rmac")

#the table method
mtable<- table(measure1, measure2)
wkappa(mtable, method = "fmac")
wkappa(mtable, method = "rmac")
}
\keyword{htest}

