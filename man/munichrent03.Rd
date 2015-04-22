\name{munichrent03}
\alias{munichrent03}
\docType{data}
\title{Data for the Rent Index 2003 in Munich, Germany}
\description{
Sample of 2,053 appartments from the data collected for the preparation of the Munich rent index 2003.
}
\usage{data("munichrent03")}
\format{
  A data frame with 2,053 observations on the following 12 variables.
  \describe{
    \item{\code{rent}}{Net rent in EUR (numeric).}
	\item{\code{rentsqm}}{Net rent per square meter in EUR (numeric).}
	\item{\code{area}}{Floor area in square meters (numeric).}
	\item{\code{rooms}}{Number of rooms (numeric).}
	\item{\code{yearc}}{Year of construction (numeric).}
	\item{\code{bathextra}}{Factor: High quality equipment in the bathroom?}
	\item{\code{bathtile}}{Factor: Bathroom tiled?}
	\item{\code{cheating}}{Factor: Central heating available?}
	\item{\code{district}}{Urban district where the apartment is located. Factor with 25 levels: "\code{All-Umenz}" (Allach - Untermenzing), "\code{Alt-Le}" (Altstadt - Lehel), "\code{Au-Haid}" (Au - Haidhausen), "\code{Au-Lo-La}" (Aubing - Lochhausen - Langwied), "\code{BamLaim}" (Berg am Laim), "\code{Bogenh}" (Bogenhausen), "\code{Feld-Has}" (Feldmoching - Hasenbergl), "\code{Had}" (Hadern), "\code{Laim}" (Laim), "\code{Lud-Isar}"(Ludwigsvorstadt - Isarvorstadt), "\code{Maxvor}" (Maxvorstadt), "\code{Mil-AmH}" (Milbertshofen - Am Hart), "\code{Moos}" (Moosach), "\code{Neuh-Nymp}" (Neuhausen - Nymphenburg), "\code{Obgies}" (Obergiesing), "\code{Pas-Obmenz}" (Pasing - Obermenzing), "\code{Ram-Per}" (Ramersdorf - Perlach), "\code{SchwWest}" (Schwabing West), "\code{Schwab-Frei}" (Schwabing - Freimann), "\code{Schwanth}" (Schwanthalerhoehe), "\code{Send}" (Sendling), "\code{Send-West}" (Sendling - Westpark), "\code{Th-Ob-Fo-Fu-So}" (Thalkirchen - Obersendling - Forstenried - Fuerstenried - Solln), "\code{Trud-Riem}" (Trudering - Riem) and "\code{Ugies-Har}" (Untergiesing - Harlaching).}
	\item{\code{location}}{Quality of location. Ordered factor with levels "\code{normal}",  "\code{good}" and "\code{top}".}
	\item{\code{upkitchen}}{Factor: Upscale equipment in kitchen?}
	\item{\code{wwater}}{Factor: Hot water supply available?}
  }
}
\details{
A detailed description (German only) of the factor levels can be found in the rent index brochure of the city of Munich. The corresponding URL violates the CRAN-policies, therefore it must be copy-and-pasted into a browser: \samp{http://www.muenchen.de/rathaus/dms/Home/Stadtverwaltung/Sozialreferat/wohnungsamt/Mietspiegel/pdf_mietspiegel/mietspiegel2013/Brosch\%C3\%BCre\%20Mietspiegel\%202013.pdf}
}
\source{
\url{http://www.stat.uni-muenchen.de/service/datenarchiv/miete/miete03.html}
}
\references{
Fahrmeir, L., Kneib, T., Lang, S., Marx, B. (2013): \emph{Regression: Models, Methods and Applications}. Berlin: Springer.

Fahrmeir, L., Kuenstler, R., Pigeot, I., Tutz, G. (2004): \emph{Statistik: der Weg zur Datenanalyse}, 5th edition. Berlin: Springer.
}
\examples{
data("munichrent03")
print(summary(munichrent03))

par(ask=TRUE)
plot(munichrent03[,1:5])
plot(rentsqm ~ bathextra, data=munichrent03)
plot(rentsqm ~ bathtile, data=munichrent03)
plot(rentsqm ~ cheating, data=munichrent03)
plot(rentsqm ~ location, data=munichrent03)
plot(rentsqm ~ upkitchen, data=munichrent03)
plot(rentsqm ~ wwater, data=munichrent03)

oldpar <- par(no.readonly = TRUE)
par(las=3, mar=c(8,4,4,2)+0.1)
plot(rentsqm ~ district, xlab=NA, data=munichrent03)
par(oldpar)

par(ask=FALSE)
}
\keyword{datasets}
