\name{Svensson}
\alias{Svensson}
\title{ Estimation of the Svensson parameters}
\description{
  Returns the estimated coefficients of the Svensson's model.
}
\usage{
Svensson(rate = FedYieldCurve, maturity = tau, Tau1 = c(3, 12), Tau2 = c(60, 120))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rate}{ vector or matrix which contains the interest rates.}
  \item{maturity}{ vector wich contains the maturity ( in months) of the \code{rate}. The vector's length must be the same 
		of the number of columns of the \code{rate}.}
  \item{Tau1}{ vector of short-term maturity }
  \item{Tau2}{ vector of long-term maturity }
}
\details{
The Svensson's model to describe the forward rate is:
\deqn{y_t(\tau) = \beta_{0} + \beta_{1} \exp\left( -\frac{\tau}{\lambda_1} \right) + \beta_2
\frac{\tau}{\lambda_1} \exp \left( -\frac{\tau}{\lambda_1} \right) + \beta_3
\frac{\tau}{\lambda_2} \exp \left( -\frac{\tau}{\lambda_2} \right) }

The spot rate can be derived from forward rate and it is given by:
\deqn{
y_t(\tau) = \beta_0 + \beta_1 \frac{ 1- \exp(
-\frac{\tau}{\lambda_1}) }{\frac{\tau}{\lambda_1} } + \beta_2 \left[\frac{ 1- \exp(
-\frac{\tau}{\lambda_1}) }{\frac{\tau}{\lambda_1} } - \exp( -\frac{\tau}{\lambda_1})
\right]
+ \beta_3  \left[\frac{ 1- \exp(-\frac{\tau}{\lambda_2}) }{\frac{\tau}{\lambda_2} } -
\exp( -\frac{\tau}{\lambda_2})
\right]}
}

\value{
Returns a data frame with the estimated coefficients: $\beta_{0}$, $\beta_{1}$, $\beta_{2}$,$\beta_{3}$, $\lambda_1$ and $\lambda_2$. 
}
\references{ 
Svensson, Lars E. O.,Estimating and Interpreting Forward Interest Rates: Sweden 1992 - 1994(September 1994). NBER Working Paper Series, Vol. w4871, pp. -, 1994. Available at SSRN: http://ssrn.com/abstract=226502

Nelson, C.R., and A.F. Siegel (1987), Parsimonious Modeling of Yield Curve, \emph{The Journal of Business}, \bold{60}, 473-489.
}
\author{ Sergio Salvino Guirreri}
\examples{
data(FedYieldCurve)
tau <- c(3,6,12,60,84,120)
A <- Svensson(FedYieldCurve[1:10,], tau, c(3,12), c(60,120) )
}
\keyword{models}
\keyword{htest}
