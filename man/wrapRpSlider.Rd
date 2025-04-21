\name{wrapRpSlider}
\alias{wrapRpSlider}
\title{
Wrapper Function for rp.slider
}
\description{
Wrapper function to provide a variable name of type \code{\link[base]{character}} to \code{\link[rpanel]{rp.slider}}.
}
\usage{
wrapRpSlider(panel, variable, from, to, action, title=NA, 
log = FALSE, showvalue = FALSE, resolution = 0, 
initval = NULL, pos = NULL, horizontal = TRUE)
}
\arguments{
See documentation of \code{\link[rpanel]{rp.slider}}. The argument \code{variable} must be of type \code{\link[base]{character}} here.
}
\value{
A function call of \code{\link[rpanel]{rp.slider}} which can be evaluated.
}
\note{
Function is only for internal use in \pkg{LinRegInteractive}.
}
\keyword{internal}