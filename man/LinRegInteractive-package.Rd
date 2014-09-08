\name{LinRegInteractive-package}
\alias{LinRegInteractive-package}
\alias{LinRegInteractive}
\docType{package}
\title{
Interactive Interpretation of Linear Regression Models
}
\description{
The two main functions \code{\link{lm.interactive}} and \code{\link{glm.interactive}} are based on the package \href{http://CRAN.R-project.org/package=rpanel}{\pkg{rpanel}} and provide an interactive visualization of effects and marginal effects for linear regression models and of link functions, response functions and marginal effects for generalized linear regression models for binary dependent variables. A major feature is the handling of the effects of categorical covariates: if two or more factors are used as covariates every combination of the levels of each factor (referred to as \emph{groups}) is treated separately. The interactive approach, the automatic calculation of marginal effects and far-reaching possibilities to customize the graphical output are characteristic features as well. In the accompanying vignette the usage is described in detail. 
}
\details{
\tabular{ll}{
Package: \tab LinRegInteractive\cr
Type: \tab Package\cr
Version: \tab 0.2-2\cr
Date: \tab 2014-09-08\cr
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

### Binary dependent variable ###
data("creditdata")
model.cd <- glm(credit ~ amount + I(amount^2) + age + duration*teleph + housing,
    family=binomial, data=creditdata)
glm.interactive(model.cd)

### Metric dependent variable ###
data("munichrent03")
model.rent <- lm(rent ~ yearc + I(yearc^2) + rooms + area*location + upkitchen,
    data=munichrent03)
lm.interactive(model.rent)
}
}
