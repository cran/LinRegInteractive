\name{factor.combinations}
\alias{factor.combinations}
\title{
Combine Levels of Different Factors to Groups
}
\description{
All levels of the factors from a \code{\link{data.frame}} provided are combined, each combination of factor levels is referred to as \emph{group}.
}
\usage{
factor.combinations(X, factor.sep = "-", level.sep = ".")
}
\arguments{
  \item{X}{
A \code{\link{data.frame}} containing at least one factor.
}
\item{factor.sep}{Character by which the factor-factor level combinations are separated in the group names. The character may neither appear within the factor nor the level names for the function to work.}
\item{level.sep}{Character by which the level names are separated from the corresponding factor names in the group names. The character may neither appear within the factor nor the level names for the function to work.}
}
\value{
A list with the following components:
\item{comb}{A data frame containing every combination of factor levels (groups) for the factors provided.}
\item{names}{A character vector with the names of the groups.  Factor-factor level combinations are separated by \code{factor.sep} and level names from the corresponding factor names by \code{level.sep}.}
\item{counts}{A vector indicating the number of occurences of the different groups within the data frame provided to the function.} 
}
\note{
Function is mainly for internal use in \pkg{LinRegInteractive} but may be useful for other purposes as well.
}

\examples{
# multiple factors
data("CO2")
print(factor.combinations(CO2))

# single factor
data("chickwts")
print(factor.combinations(chickwts))
}

