\name{wrap.rp.slider}
\alias{wrap.rp.slider}
\title{
Wrapper Function for rp.slider
}
\description{
Wrapper function to provide a variable name of type \code{\link{character}} to \code{\link{rp.slider}}.
}
\usage{
wrap.rp.slider(panel, variable, from, to, action, title=NA, 
log = FALSE, showvalue = FALSE, resolution = 0, 
initval = NULL, pos = NULL, horizontal = TRUE)
}
\arguments{
See documentation of \code{\link{rp.slider}}. The argument \code{variable} must be of type \code{\link{character}} here.
}
\value{
A function call of \code{\link{rp.slider}} which can be evaluated.
}
\note{
Function is only for internal use in \pkg{LinRegInteractive}.
}
\keyword{internal}