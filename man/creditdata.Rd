\name{creditdata}
\alias{creditdata}
\docType{data}
\title{Credit Default Data}
\description{
Stratified sample of 1,000 credit receivers (in the following \emph{cr}) from a private bank in southern Germany. The data were sampled at the end of the 1970s, therefore the credit amount is given in DEM (1.95583 DEM = 1 EUR).
}
\usage{data("creditdata")}
\format{
  A data frame with 1,000 observations on the following 21 variables.
  \describe{
    \item{\code{credit}}{Factor with levels "\code{good}" (no problems occur in redemption) and "\code{bad}" (problems occur in redemption).}
    \item{\code{amount}}{Credit amount in DEM (numeric).}
    \item{\code{duration}}{Duration of lent term in months (numeric).}
    \item{\code{age}}{Age of the cr in years (numeric).}
    \item{\code{addcredit}}{Has the cr additional credits at other vendors? Ordered factor with levels "\code{none}" (no additional credits), "\code{bank}" (at other banks) and "\code{bargain}" (at other vendors than banks).}
    \item{\code{asset}}{Most valuable asset of the cr. Ordered factor with levels "\code{none}" (no valuable assets), "\code{car}" (car), "\code{savings}" (savings) and "\code{prop}" (freehold property).}
    \item{\code{bail}}{Bailsman available?  Ordered factor with levels "\code{none}" (no bailsmen), "\code{applicant}" (bailsman is additional applicant for the credit) and "\code{yes}" (external bailsman).}
    \item{\code{curacc}}{Current account of the cr. Ordered factor with levels "\code{none}" (no account), "\code{debit}" (account with dedit balance), "\code{credit}" (account with credit balance) and "\code{wage}" (account with periodical income).}
    \item{\code{empldur}}{Duration of employment of the cr at the current employer. Ordered factor with levels "\code{none}" (unemployed), "\code{max1y}" (less than 1 year), "\code{max4y}" (between 1 and 4 years), "\code{max7y}" (between 4 and 7 years) and "\code{min7y}" (more than 7 years).}
    \item{\code{housdur}}{For which period does the cr lives in current housing? Ordered factor with levels "\code{max1y}" (less than 1 year), "\code{max4y}" (between 1 and 4 years), "\code{max7y}" (between 4 and 7 years) and "\code{min7y}" (more than 7 years).}
    \item{\code{housing}}{Accommodation of the cr. Ordered factor with levels "\code{social}" (social accommodation), "\code{rent}" (rented accommodation) and "\code{freehold}" (freehold accommodation).}
    \item{\code{immigrant}}{Factor: Is the cr an immigrant?}
	\item{\code{installment}}{Monthly installment in \% of disposable income.  Ordered factor with levels "\code{max20}" (installment is less than 20\% of disposable income), "\code{max25}" (installment is more than 20\% and less than 25\% of disposable income), "\code{max35}" (installment is more than 25\% and less than 35\% of disposable income) and "\code{min35}" (installment is more than 35\% of disposable income).}
    \item{\code{intuse}}{Intended usage for the credit. Factor with levels "\code{car-new}" (buy a new car), "\code{car-old}" (buy an used car), "\code{fittings}" (buy fittings), "\code{hifi}" (buy radio or TV), "\code{holiday}" (pay a holiday), "\code{househ}" (buy new household items), "\code{misc}" (miscellaneous activities), "\code{qualif}" (pay a new qualification), "\code{repair}" (repair something) and "\code{retrain}" (pay a retrain).}
    \item{\code{job}}{Employment status of the cr. Ordered factor with levels "\code{none}" (unemployed), "\code{unskilled}" (unskilled worker), "\code{medium}" (intermediate position) and "\code{higher}" (leading position).}
    \item{\code{martsex}}{Martial status and sex of the cr. Factor with levels "\code{f-div-m-sing}" (female-divorced and male-single), "\code{f-sing}" (female-single), "\code{m-div}" (male-divorced) and "\code{m-mar}" (male-married).}
    \item{\code{moral}}{Payment moral of the cr. Factor with levels "\code{alright}" (no problems with credits in the past at the bank), "\code{critical}" (further credits at other banks or problems with account management), "\code{none}" (no credits previously or fully paid back), "\code{yetalright}" (no problems with current credits at the bank so far) and "\code{problems}" (problems with credits in the past).}
    \item{\code{pers}}{Number of persons who are entitled to maintenance from cr. Ordered factor with levels "\code{max2}" (up to 2) and "\code{min3}" (more than 2).}
    \item{\code{prevcred}}{Number of previous credits of the cr at the bank (including the actual). Ordered factor with levels "\code{max1}" (1 credit), "\code{max3}" (between 2 and 3), "\code{max5}" (between 4 and 5) and "\code{min6}" (6 or more).}
    \item{\code{savacc}}{Savings account of the cr. Ordered factor with levels "\code{none}" (no savings account), "\code{max1h}" (savings account with a balance up to 100 DEM), "\code{max5h}" (savings account with a balance between 100 DEM and 500 DEM), "\code{max1t}" (savings account with a balance between 500 DEM and 1.000 DEM) and "\code{min1t}" (savings account with a balance greater than 1,000 DEM).}
    \item{\code{teleph}}{Factor: Has the cr a landline?}
  }
}
\source{
\url{https://doi.org/10.5282/ubm/data.23}
}
\references{
Fahrmeir, L., Hamerle, A., Tutz, G. (1996): \emph{Multivariate statistische Verfahren}, 2nd edition. Berlin: de Gruyter. 390 ff.

Fahrmeir, L., Kneib, T., Lang, S., Marx, B. (2013): \emph{Regression: Models, Methods and Applications}. Berlin: Springer.

Open Data LMU (2010): \emph{Kreditscoring zur Klassifikation von Kreditnehmern}.
}
\examples{
data("creditdata")
print(summary(creditdata))

par(ask=TRUE)
plot(credit ~ amount, data=creditdata)
plot(credit ~ duration, data=creditdata)
plot(credit ~ age, data=creditdata)
plot(credit ~ housing, data=creditdata)
plot(credit ~ intuse, data=creditdata)
plot(credit ~ teleph, data=creditdata)
par(ask=FALSE)
}
\keyword{datasets}
