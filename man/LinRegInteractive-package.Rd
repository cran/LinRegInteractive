\name{LinRegInteractive-package}
\alias{LinRegInteractive-package}
\alias{LinRegInteractive}
\docType{package}
\title{
Interactive Interpretation of Linear Regression Models
}
\description{
The implementation is based on the package \href{http://CRAN.R-project.org/package=rpanel}{\pkg{rpanel}} and provides an interactive visualization of effects, response functions and marginal effects for different kinds of regression models. Major features are the interactive approach and the handling of the effects of categorical covariates: if two or more factors are used as covariates every combination of the levels of each factor (referred to as \emph{groups}) is treated separately. The automatic calculation of marginal effects and a number of possibilities to customize the graphical output are useful features as well. In the accompanying vignette the usage is described in detail. Please refer to the documentation of the generic function \code{\link{fxInteractive}} for available methods.
}
\details{
\tabular{ll}{
Package: \tab LinRegInteractive\cr
Type: \tab Package\cr
Version: \tab 0.3-0\cr
Date: \tab 2014-11-12\cr
License: \tab GPL-2\cr
}
}
\author{
Martin Meermeyer \email{meermeyer@statistik.uni-wuppertal.de}
}
\references{
Bowman, A., Crawford, E., Alexander, G., and Bowman, R. (2007). rpanel: Simple interactive
controls for R functions using the tcltk package. \emph{Journal of Statistical Software}, 17(9), 1-18.
}
\keyword{package}
\seealso{
The noninteractive visualization of the results for various types of regression models can be achieved with the package \href{http://CRAN.R-project.org/package=effects}{\pkg{effects}}.
}
\examples{
\dontrun{
### Metric dependent variable ###
data("munichrent03")
model.rent <- lm(rent ~ yearc + I(yearc^2) + rooms + area*location + upkitchen,
    data=munichrent03)
fxInteractive(model.rent)

### Binary dependent variable ###
data("creditdata")
model.cd <- glm(credit ~ amount + I(amount^2) + age + duration*teleph + housing,
    family=binomial, data=creditdata)
fxInteractive(model.cd)
}
}
