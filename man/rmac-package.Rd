\name{rmac-package}
\alias{rmac-package}
\alias{rmac}
\docType{package}

\title{Fixed and Random Marginal Agreement Coefficients}
\description{
This package consists of four functions for calculating fixed and random marginal agreement coefficients based on the paper Fay (2005).  The fixed marginal agreement coefficient (FMAC) is the traditional agreement coefficient, which assumes that each measurement's marginal distribution is the best estimate of its true distribution for the calculation of expected agreement due to chance. Examples of the fixed marginal agreement coefficient include the weighted kappa and the concordance coerrelation coefficient. In certain cases, fixed marginal agreement coefficients will indicated a high degree of agreement when the marginal distributions are extremely unbalanced (Feinstein and Cicchetti 1989).  As an alternative to this, the Fay (2005) paper proposes the random marginal agreement coefficient (RMAC), which assumes that a mixture distribution of the two measurement distributions is the best estimate of both measurements' true distributions.  For example, the paper uses the Westlund and Kurkland (1953) multiple sclerosis data.  Patients were rated for likelihood of multiple sclerosis on a scale from 1 to 4 by two different neurologists. As the code below (see example section) shows, when the original data (\code{msd1}) is modified so that the 10 patients neurologist 1 awareded a score of "3"  and neurologist 2 awarded a score of "1" receive a score of "1" from neurologist 1 and a score of "3" from neurologist 2, the FMAC changes, while the RMAC is the same.


This package provides a general method for calculating the random marginal agreement coefficient in \code{\link{rmacBoot}} and a general method for calculating the fixed marginal agreement coefficient in \code{\link{fmacBoot}}.  For categorical data, \code{\link{wkappa}} provides the FMAC, which is equivalent to Cohen's kappa, and the RMAC, which is equivalent to Scott's pi for the 2 x 2 case. For continuous data, \code{\link{cac}} provides the fixed marginal agreement coefficient, which with the squared difference cost function is equivalent to Lin's concordance correlation coefficient, and the random marginal agreement coefficient, presented in Fay (2005).}

\details{
\tabular{ll}{
Package: \tab rmac\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2008-05-31\cr                     

License: \tab GPL2 \cr}
}

\author{Jennifer Kirk
Maintainer: Jennifer Kirk <jenn.kirk@nih.gov>}

\references{
Cohen, J. (1960). A coefficient of agreement for nominal scales. Educ. Psychol. Meas., 20: 37-46.

Cohen, J. (1968). Weighted kappa: Nominal scale agreement with provision for scaled disagreement or partial credit. Psychol. Bull., 70: 213-220.

Fay, M.P. (2005). Random marginal agreement coefficients: Rethinking the adjustment for chance in agreement coefficients. Biostatistics, 6: 171-180.

Feinstein, A. R. and Cicchetti, D. V. (1989). High agreement but low kappa: I. the problems of two paradoxes. J. Clin. Epidemiol. 43: 543-549.

Lin, L. I. (1989) A concordance correlation coefficient to evaluate reproducibility. Biometrics. 45: 255-268. (Correction: 200, pp. 324-325)

Scott, W. A. (1955). Reliability of content analysis: The case of nominal scale coding. Public Opinion Quart., 19: 321-325.

Westlund, K. B. and Kurkland, L. T. (1953). Studies in multiple sclerosis in Winnipeg, Manitoba and New Orleans, Louisiana. American Journal of Hygiene. 57: 380-396.
}

\examples{
#The Multiple Sclerosis Diagnoses Example (from Fay 2005)
#the original data
msd1<- as.table(matrix(data = c(38,5,0,1,33,11,3,0,10,14,5,6,3,7,3,10), 4,4, byrow = TRUE))
msd1
#the data with cell counts (1,3) and (3,1) reversed
msd2<- as.table(matrix(data = c(38,5,10,1,33,11,3,0,0,14,5,6,3,7,3,10), 4,4, byrow = TRUE))
msd2

#calculate the FMAC of each data set
wkappa(msd1, method = "fmac")
wkappa(msd2, method = "fmac")

#calculate the FMAC of each data set
wkappa(msd1, method = "rmac")
wkappa(msd2, method = "rmac")
}


\seealso{ \code{\link{wkappa}}, \code{\link{cac}}, \code{\link{rmacBoot}}}

\keyword{ package }
