\name{Srates}
\alias{Srates}
\title{Interest rates of the Svensson's model.}
\description{
Returns the interest rates by Svensson's model.
}
\usage{
Srates(Coeff, maturity, whichRate = "Forward")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Coeff}{ vector or matrix of the beta's coefficients and of \eqn{\lambda_1} and \eqn{\lambda_2}.}
  \item{maturity}{maturity of the yield curve of which want to return the interest rates.}
  \item{whichRate}{which rate want to return: "Spot" or "Forward" rates. }
}
\details{
 \code{Coeff} is a vector or matrix of the four coefficients of the Svensson's model, while \code{lambdaValues} is a vector or matrix of two lambda values of Svensson's model.
}
\value{
Return interest rates in matrix object with number of rows equal to \code{nrow(Coeff)} and number of columns equal to \code{length(maturity)}.
}
\references{ 
Svensson, L.E. (1994), Estimating and Interpreting Forward Interest Rates: Sweden 1992-1994, \emph{IMF Working Paper}, \bold{WP/94/114}.

Nelson, C.R., and A.F. Siegel (1987), Parsimonious Modeling of Yield Curve, \emph{The Journal of Business}, \bold{60}, 473-489.
}
\author{Sergio Salvino Guirreri}

\examples{
data(ECBYieldCurve)
tauECB <- c(0.25,0.5,1:30)
Y <- Svensson(ECBYieldCurve[1:10,], tauECB, c(1,4), c(5,8) )
B <- Srates(Y, tauECB, whichRate="Spot")
plot(tauECB,ECBYieldCurve[10,],main="Fitting Svensson's yield curve", type="o")
lines(tauECB,B[10,], col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),col=c(1,2),lty=1)
grid()
}

\keyword{ htest }
