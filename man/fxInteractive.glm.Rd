\name{fxInteractive.glm}
\alias{fxInteractive.glm}
\title{
Interactive Interpretation of Generalized Linear Models
}
\description{
This method implements proposals for the interpretation of models for binary outcomes made by Hoetker (2007) but works for other types of generalized linear models as well. The method is also suitable for generalized additive models fitted with \code{\link{gam}} (package \href{http://CRAN.R-project.org/package=gam}{\pkg{gam}} and \href{http://CRAN.R-project.org/package=mgcv}{\pkg{mgcv}}). 
}
\usage{
\method{fxInteractive}{glm}(model, initial.values = as.list(NULL),
    preselect.var = NA, preselect.type = "link", preselect.groups = NULL,
    dev.height = 18, dev.width = 18, dev.width.legend = 8, dev.pointsize = 10,
    dev.defined = FALSE, ylim = NA, col = NA, lty = 1, lwd = 1, 
    main = NA, main.line = 1.5, xlab = NA, ylab = NA, 
    legend.add = TRUE, legend.space = legend.add, legend.only = FALSE,
    legend.pos = "center", legend.cex = 1, legend.width.factor = 1, 
    rug.ticksize = 0.02, rug.col = "black", vline.actual = TRUE, 
    pos.hlines = c(0, 0.5, 0), n.effects = 100, 
    autosave.plot = FALSE, snapshot.plot = FALSE, 
    graphics.filename = "LinRegIntPlot", graphics.numbering = !autosave.plot,
    graphics.type = "pdf", factor.sep = "|", level.sep = ".", 
    latex2console = FALSE, xtable.big.mark = ".", xtable.decimal.mark = ",", 
    xtable.digits = NULL, xtable.display = NULL, xtable.booktabs = FALSE,                 
    panel.title = "Generalized Linear Model", label.button = "Snapshot",
    label.slider.act = "Variable displayed: ", label.box.type = "Type",
    label.types = c("linear predictor", "response", "marginal effect"),
    label.box.groups = "Groups", 
    slider.width = 200, slider.height = 60, button.height = 30,
    box.type.height = 90, box.group.character.width = 7, 
    box.group.line.height = 28, dist.obj.width = 20,
    dist.obj.height = 10, ...)
}
\arguments{
\item{model}{Object of class \code{\link{glm}} (mandatory).}
\item{initial.values}{Initial values for the metric covariates in a named list, default to the means. See section 4 of the vignette and examples below.}
\item{preselect.var}{Name of continuous variable to be displayed as character or \code{NA} (default) for menu selection.}
\item{preselect.type}{The type of the initial plot to be displayed. Must be one of the values \code{"link"} (default), \code{"response"} or \code{"marginal"}.}
\item{preselect.groups}{Numeric vector with the index of the groups which are displayed in the initial plot. If \code{NULL} (the default) all groups are displayed.}
\item{dev.height}{Height of graphic device in cm, default to \code{18}.}
\item{dev.width}{Width of plot area in graphic device in cm, default to \code{18}.}
\item{dev.width.legend}{Width of legend area in graphic device in cm, default to \code{8}.}
\item{dev.pointsize}{Character pointsize of graphic device, default to \code{10}.}
\item{dev.defined}{Graphic device predefined? Default to \code{FALSE}, see section 6.3 of the vignette for usage.}
\item{ylim}{With a numeric vector of length 2 the plot limits in y-direction can be set. If NA (the default) these are determined automatically.}
\item{col}{Vector of color specifications to represent different groups. Passed to the line commands and the legend. Actual palette and consecutive sequence if \code{NA} (default).}
\item{lty}{Vector of line type specifications to represent different groups. Passed to the line commands and the legend, default to solid lines.}
\item{lwd}{Vector of line width specifications to represent different groups. Passed to the line commands and the legend, default to \code{1}.}
\item{main}{Title for the plot, default to \code{NA}.}
\item{main.line}{Height for plot title in lines which is passed to title(), default to 1.5.}
\item{xlab}{Label for the x-axis. Name of the selected covariate, if \code{NA} (the default).}
\item{ylab}{Label for the y-axis. Name of the selected plot type (see argument \code{label.types}), if \code{NA} (the default).}
\item{legend.add}{Should a legend be added to the plot? Default to \code{TRUE}.}
\item{legend.space}{Should the space for the legend be reserved? Default to the value of \code{legend.add}. Setting \code{legend.add} to \code{FALSE} and \code{legend.space} to \code{TRUE} plots white space instead of the legend. This can be useful when different plots are arranged in a document to ensure exact alignments and sizes, see section 6.2 of the vignette for details.}
\item{legend.only}{Should just the legend be plotted? Default to \code{FALSE}. A plot with the legend alone can be useful when different plots are arranged in a document, see section 6.2 of the vignette for details.}
\item{legend.pos}{Position of the legend as character, see \code{\link{legend}} for details. Default to \code{"center"}.}
\item{legend.cex}{Relative size of legend text. Can be reduced if the model contains many groups. Default to \code{1}.}
\item{legend.width.factor}{Factor by which the width of the legend is increased. Default to \code{1}. Increasing this can solve the problem that the legend annotations do not fit in the surrounding box when the plots are saved as PDF or EPS files, see section 5 of the vignette for details.}
\item{rug.ticksize}{Length of rugplot tickmarks, dafault to \code{0.02}. Set to \code{0} or \code{NA}, if no rugplot should be drawn. For many observations the rug considerably slows down the rebuild of the plot.}
\item{rug.col}{Color of rugplot tickmarks, default to \code{"black"}.}
\item{vline.actual}{Add vertical line at actual position of selected metric covariate? Default to \code{TRUE}.}
\item{pos.hlines}{Positions of the horizontal lines for [1] the plot of the link function, [2] the plot of the response and [3] the plot of marginal effects. \code{NA} for no lines, default to \code{c(0,0.5,0)}.}
\item{n.effects}{Number of equally spaced points over the span of the selected metric covariate to calculate the effects for plotting, default to \code{100}. Increase, if lines are not smooth.}
\item{autosave.plot}{Directly save the initial plot? Default to \code{FALSE}. If set to \code{TRUE} the GUI-panel is immediately closed after initialization.}
\item{snapshot.plot}{Save plot when snapshot button is pressed? Default to \code{FALSE}, see section 5 of the vignette for details.}
\item{graphics.filename}{Filename (optionally including a path) as character for graphic file.}
\item{graphics.numbering}{If \code{TRUE} (the default) a 3 digits number is automatically appended to the filenname to avoid that existing graphic files are overwritten.}
\item{graphics.type}{Graphics file type argument, default to \code{"pdf"}. On Windows systems all file types accepted by \code{\link{savePlot}} work. Under non Windows systems allowed values are \code{"pdf"}, \code{"eps"}, \code{"png"}, \code{"jpeg"}, \code{"jpg"}, \code{"tiff"} and \code{"bmp"}. }
\item{factor.sep}{Character separating the factor-factor level combinations in the group names (default to \code{"|"}).}
\item{level.sep}{Character separating the factor name and the corresponding factor levels in the group names (default to \code{"."}).}
\item{latex2console}{Should the textoutput triggered by the snapshot button be printed as LaTeX-code? Default to \code{FALSE}.}
\item{xtable.big.mark}{Bigmark character for LaTeX output passed to \code{\link{print.xtable}}, default to \code{","}.}
\item{xtable.decimal.mark}{Decimal character for LaTeX output passed to \code{\link{print.xtable}}, default to \code{"."}.}
\item{xtable.digits}{Number of digits for LaTeX output passed to \code{\link{xtable}}, default to \code{NULL}.}
\item{xtable.display}{Display style for LaTeX output passed to \code{\link{xtable}}, default to \code{NULL}.}
\item{xtable.booktabs}{Use the LaTeX package \code{booktabs} for horizontal lines in LaTeX tables, default to \code{FALSE}. Passed to \code{\link{print.xtable}}.}
\item{panel.title}{Title used in the title bar of the GUI-panel of type \code{character}.}
\item{label.button}{Label for the snapshot-button of type \code{character}.}
\item{label.slider.act}{Additional label for the slider of the selected metric covariate of type \code{character}.}
\item{label.box.type}{Title for the radiogroup box of type \code{character}.}
\item{label.types}{Labels for radiogroup buttons (\code{character} vector of length 3). By default these are also used as corresponding annotations for the y-axis.}
\item{label.box.groups}{Title for the checkbox of type \code{character}.}
\item{slider.width}{Width of each slider in points (default to \code{200}).}
\item{slider.height}{Height of each slider in points (default to \code{60}).}
\item{button.height}{Height of snapshot button in points (default to \code{30}).}
\item{box.type.height}{Height of radiobox for type selection in points (default to \code{90}).}
\item{box.group.character.width}{The width of the boxes is basically a this value times the number of characters in points (default to \code{7}).}
\item{box.group.line.height}{The height of the checkbox is this value times the number of groups in points (default to \code{28}).}
\item{dist.obj.width}{Vertical distance between sliders and boxes and vertical margins in points (default to \code{20}).}
\item{dist.obj.height}{Horizontal distance between panel objects in points (default to \code{10}).}
\item{...}{Other graphical parameters passed to \code{\link{par}}.}
}
\details{
The only mandatory argument of the function is a fitted-model object of class \code{\link{glm}}. For this object the following prerequisites must be met:
\itemize{
\item{The model must contain at least one metric covariate.}
\item{The model must be specified with the formula interface and the data frame containing the variables must be passed with the \code{data} argument.}
\item{The categorical variables must be \code{\link{factor}}s (ordered or unordered).}
}
Please refer to the documentation of \code{\link{fxInteractive}} or the vignette for more details.
}
\value{
No object is returned, please refer to the documentation of \code{\link{fxInteractive}} or the vignette for more details.
}
\references{
Hoetker, G. (2007). The use of logit and probit models in strategic management research: Critical
issues. \emph{Strategic Management Journal}, 28(4), 331-343.

Kleiber, C., Zeileis, A. (2008). \emph{Applied Econometrics with R}. New York: Springer.
}
\author{ 
Martin Meermeyer \email{meermeyer@statistik.uni-wuppertal.de}
}
\examples{
### Model specification ###
data("creditdata")

## Treat ordered factors as unordered factors.
options(contrasts=c("contr.treatment","contr.treatment"))

model.cd <- glm(credit ~ amount + I(amount^2) + age + duration*teleph + housing,
    family=binomial, data=creditdata)

\dontrun{

### Basic usage ###

## RStudio users may need to change the graphic device, see details.
options(device = "x11")

## Using defaults
fxInteractive(model.cd)

## Switch text output to LaTeX
fxInteractive(model.cd, latex2console = TRUE)

## Continental European number format in LaTeX output
fxInteractive(model.cd, latex2console = TRUE, decimal.mark = ",", big.mark = ".")

## Save plot as PDF to current working directory when 'Snapshot' is clicked
fxInteractive(model.cd, snapshot.plot = TRUE)

## Change color scheme and line types
fxInteractive(model.cd, col = rep(c(2,4),each = 3), lty = c(1,3,5))

## Change separation characters
fxInteractive(model.cd, factor.sep = "-", level.sep = ">")

## Suppress legend
fxInteractive(model.cd, legend.add = FALSE)

## Suppress rug plot
fxInteractive(model.cd, rug.ticksize = NA)

## Set initial values of metric covatiates 
fxInteractive(model.cd, initial.values = list(amount=5000, age=30, duration=24))

## Preselect covariate, plot type and groups
fxInteractive(model.cd, preselect.var = "duration", preselect.type = "response",
    preselect.groups = c(2,5))

## Preselect covariate and plot type and change axis annotations
fxInteractive(model.cd, preselect.var = "duration", preselect.type = "response", 
    xlab = "duration (months)", ylab = "probability of credit default")


### Visualization of statistical concepts ###

## Nonparametric effect 
require("splines")
model.cd.bs <- glm(credit ~ bs(amount) + age + duration*teleph + housing,
    family = binomial, data = creditdata)
fxInteractive(model.cd.bs, preselect.var = "amount")

## Generalized additive model
require("mgcv")
model.cd.mgcv <- gam(credit ~ s(amount) + age + duration*teleph + housing,
family = binomial, data = creditdata)
fxInteractive(model.cd.mgcv)

## Interaction effect (directly)
fxInteractive(model.cd, preselect.var = "duration")

## Interaction effect (indirectly)
fxInteractive(model.cd, preselect.var = "age")
# manipulate slider for 'duration'

## Quasi-complete separation
# example from Kleiber, Zeileis (2008), p. 130ff 
require("AER")
data("MurderRates")
model.mr <- glm(I(executions > 0) ~ time + income + noncauc + lfp + southern,
    family = binomial, data = MurderRates)
fxInteractive(model.mr, preselect.var = "income", preselect.type = "response")


### Additional examples ###

## Customize device for printing
fxInteractive(model.cd, 
    dev.width = 6,
    dev.width.legend = 4,
    dev.height = 6,
    dev.pointsize = 6,
    col = c("darkred","red","salmon","darkblue","blue","lightblue"),
    legend.width.factor = 1.1,
    vline.actual = FALSE,
    snapshot.plot = TRUE,
    graphics.filename = "creddefault-termplot",
    mar = c(2.5,2.5,1,1)+0.1,
    mgp = c(1.5,0.5,0),
    tcl = -0.3)

## Save predefined plot automatically
fxInteractive(model.cd,
    initial.values = list(amount=5000, duration=24, age=30),
    preselect.var = "duration",
    preselect.type = "link",
    autosave.plot = TRUE,
    graphics.filename = "fig-creddefault-duration-link",
    legend.width.factor = 1.05)

## Modifications for models with many groups
# Increase space for legend and squeeze panel controls
model.cd.moregroups <- glm(credit ~ amount + I(amount^2) + age 
    + duration*teleph + housing + job, family = binomial, data = creditdata)
fxInteractive(model.cd.moregroups, 
    dev.width.legend = 9,
    legend.cex = 1,
    box.type.height = 90,
    box.group.character.width = 6,
    box.group.line.height = 25,
    dist.obj.height = 2)

# Squeeze legend and  panel controls
model.cd.manygroups <- glm(credit ~ amount + I(amount^2) + age 
    + duration*teleph + housing + intuse, family = binomial, data = creditdata)
fxInteractive(model.cd.manygroups, 
    dev.width.legend = 6,
    legend.cex = 0.54,
    box.type.height = 90,
    box.group.character.width = 6,
    box.group.line.height = 25,
    dist.obj.height = 2)
# Note that checkbox for groups grows beyond screen

# Preselect groups and specifiy color and line types directly
model.cd.manygroups <- glm(credit ~ amount + I(amount^2) + age 
    + duration*teleph + housing + intuse, family = binomial, data = creditdata)
index.groups <- c(1,11,21,31,41,51)
vec.col <- NULL
vec.col[index.groups] <- c(1:6)
vec.lty <- NULL
vec.lty[index.groups] <- rep(c(1,2), each = 3)
fxInteractive(model.cd.manygroups,
preselect.var = "amount",
preselect.groups = index.groups,
col = vec.col,
lty = vec.lty)
}
}