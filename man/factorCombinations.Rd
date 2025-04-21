\name{factorCombinations}
\alias{factorCombinations}
\title{
Combine Levels of Different Factors to Groups
}
\description{
All levels of the factors from a \code{\link[base]{data.frame}} provided are combined, each combination of factor levels is referred to as \emph{group}.
}
\usage{
factorCombinations(X, factor.sep = "|", level.sep = ".", count=TRUE)
}
\arguments{
  \item{X}{
A \code{\link[base]{data.frame}} containing at least one factor.
}
\item{factor.sep}{Character by which the factor-factor level combinations are separated in the group names.}
\item{level.sep}{Character by which the level names are separated from the corresponding factor names in the group names.}
\item{count}{Should the occurences of the different groups in the \code{data.frame} provided be counted? Default to \code{TRUE}.}
}
\value{
A list with the following components:
\item{combinations}{A data frame containing every combination of factor levels (groups) for the factors provided.}
\item{names}{A character vector with the names of the groups.  Factor-factor level combinations are separated by \code{factor.sep} and level names from the corresponding factor names by \code{level.sep}.}
\item{counts}{If \code{count} is \code{TRUE} a vector indicating the number of occurences of the different groups within the \code{data.frame} provided to the function. \code{NULL} if \code{count} is set to \code{FALSE}.} 
}
\note{
Function is mainly for internal use in \pkg{LinRegInteractive} but may be useful for other purposes as well.
}

\examples{
# multiple factors
data("CO2")
print(factorCombinations(CO2))

# single factor
data("chickwts")
print(factorCombinations(chickwts))
}

