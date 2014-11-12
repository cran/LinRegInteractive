\name{fxInteractive}
\alias{fxInteractive}
\title{
Interactive Interpretation of Linear Regression Models
}
\description{
\code{fxInteractive} is a generic function for the interactive visualization of the results of various types of regression models. Originally the function was developed to translate proposals for the interpretation of models for binary outcomes made by Hoetker (2007) into practice. The function invokes particular methods for different kinds of fitted-model objects, see the list of available methods in the details. The implementation is based on the package \href{http://CRAN.R-project.org/package=rpanel}{\pkg{rpanel}}. Major features are the interactive approach and the handling of the effects of categorical covariates: if two or more factors are used as covariates every combination of the levels of each factor (referred to as \emph{groups}) is treated separately. The automatic calculation of marginal effects and a number of possibilities to customize the graphical output are useful features as well. In the accompanying vignette the usage is described in detail. 
}
\usage{
fxInteractive(model, ...)
}
\arguments{
  \item{model}{An appropriate fitted-model object (mandatory), see details.}
\item{...}{Additional arguments for different aspects of usage, visualization and output. See the documentation of the corresponding methods for details.}
}
\details{
The only mandatory argument of the function is an appropriate fitted-model object.
In this package version methods for the following regression models are available:
\itemize{
\item{Generalized linear models fitted with \code{\link{glm}}. The \code{glm}-method also works for generalized additive models fitted with \code{\link{gam}} (package \href{http://CRAN.R-project.org/package=gam}{\pkg{gam}} and \href{http://CRAN.R-project.org/package=mgcv}{\pkg{mgcv}}).}
\item{Multiple linear regression models fitted with \code{\link{lm}}.}
\item{Linear mixed-effects models fitted with \code{\link{lme}} (package \href{http://CRAN.R-project.org/package=nlme}{\pkg{nlme}}).}
}
For the fitted-model objects the following prerequisites must be met:
\itemize{
\item{The model must contain at least one metric covariate.}
\item{The model must be specified with the formula interface and the data frame containing the variables must be passed with the \code{data} argument.}
\item{The categorical variables must be \code{\link{factor}}s (ordered or unordered).}
}

The basic idea of the function is the interactive usage. It is nevertheless
easy to reproduce the plots for publication purposes, refer to section 5 of the vignette for this.

Users of the IDE \bold{RStudio} may need to change the graphic device with \code{options(device = "x11")}
before calling the function because in current versions of RStudio multiple graphic devices occasionally do not work.}
\value{
The default method just returns a notice. For supported fitted-model objects no object is returned. By calling the function usually a menu appears from which a metric covariate employed in the model must be selected. After choosing the covariate a graphic device which contains a termplot of the selected metric covariate and a GUI-panel to manipulate the plot will be opened. The GUI-panel has the following elements:
\itemize{
\item{A slider for each metric covariate.}
\item{A radiobox to select the type of the current display (effect, response (if appropriate) and marginal effect).}
\item{A checkbox to select the factor combinations (groups) to be displayed.}
\item{A button to print tables of output to the console, see below.}
}
The appearance of the panel can be controlled by a number of arguments, see section 6.4 of the vignette for details.

When the button is clicked four tables are printed to the console:
\enumerate{
\item{Table of coefficients obtained by the \code{\link{summary}}-method.}
\item{Table of the chosen values of the metric covariates and their ECDF-values in the dataset.}
\item{Table of the link and response function at the chosen values of the metric covariates for each group.}
\item{Table of marginal effects for each metric covariate at the chosen values of the metric covariates for each group.}
}
By setting the argument \code{latex2console} to \code{TRUE} (see the documentation of the methods) the latter three tables are printed as LaTeX-code using functions provided by the package \href{http://CRAN.R-project.org/package=xtable}{\pkg{xtable}} (Dahl, 2012), see section 4 of the vignette for details. 

The format of the text output and the layout of the plots can be controlled to a large extent, see section 6.1, 6.2 and 6.3 of the vignette for details.

The code to save plots in a platform independent way is adopted from the program archive accompanying Kruschke (2014) (URL: \url{http://www.indiana.edu/~kruschke/DoingBayesianDataAnalysis/Programs/openGraphSaveGraph.R}).
}
\references{
Bowman, A., Crawford, E., Alexander, G., and Bowman, R. (2007). rpanel: Simple interactive
controls for R functions using the tcltk package. \emph{Journal of Statistical Software}, 17(9), 1-18.

Dahl, D. B. (2014). xtable: Export tables to LaTeX or HTML. R package version 1.7-4.

Hoetker, G. (2007). The use of logit and probit models in strategic management research: Critical
issues. \emph{Strategic Management Journal}, 28(4), 331-343.

Kruschke, J. K. (2014). \emph{Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan}, 2nd edition.
Waltham (MA): Elsevier (Academic Press). 

}
\author{ 
Martin Meermeyer \email{meermeyer@statistik.uni-wuppertal.de}
}
\seealso{
\code{\link{fxInteractive.glm}} is the corresponding method for generalized linear models fitted with \code{\link{glm}}.

\code{\link{fxInteractive.lm}} is the corresponding method for multiple linear regression models fitted with \code{\link{lm}}.

\code{\link{fxInteractive.lme}} is the corresponding method for linear mixed-effects models fitted with \code{\link{lme}} (package \href{http://CRAN.R-project.org/package=nlme}{\pkg{nlme}}).

The noninteractive visualization of the results for various types of regression models can be achieved with the package \href{http://CRAN.R-project.org/package=effects}{\pkg{effects}}.
}
