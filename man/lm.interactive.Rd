\name{lm.interactive}
\alias{lm.interactive}
\title{
Interactive Interpretation of the Multiple Linear Regression Model
}
\description{
The function is based on the package \href{http://CRAN.R-project.org/package=rpanel}{\pkg{rpanel}} and a byproduct of the development of \code{\link{glm.interactive}}. A major feature is the handling of the effects of categorical covariates: if two or more factors are used as covariates every combination of the levels of each factor (referred to as \emph{groups}) is treated separately. The interactive approach, the automatic calculation of marginal effects and far-reaching possibilities to customize the graphical output are characteristic features as well. In the accompanying vignette the usage is described in detail. 
}
\usage{
lm.interactive(model, initial.values = as.list(NULL), preselect.var = NA,
    preselect.type = "effect", preselect.groups = NULL, 
    dev.height = 18, dev.width = 18, dev.width.legend = 8, dev.pointsize = 10,
    dev.defined = FALSE, col = NA, lty = 1, lwd = 1, main = NA, xlab = NA, ylab = NA,
    legend.add = TRUE, legend.space = legend.add, legend.only = FALSE, 
    legend.pos = "center", legend.cex = 1, legend.width.factor = 1, 
    rug.ticksize = 0.02, rug.col = "black", vline.actual = TRUE, pos.hline.effect = 0,
    pos.hline.marginal = 0, n.effects = 100, autosave.plot = FALSE,
    snapshot.plot = FALSE, graphics.filename = "LinRegIntPlot",
    graphics.extension = "pdf", latex2console = FALSE, decimal.mark = ".",
    big.mark = ",", factor.sep = "-", level.sep = ".", 
    panel.title = "Linear Model", label.button = "Snapshot",
    label.slider.act = "Variable displayed: ", label.box.type = "Type", 
    label.types = c("effect", "marginal effect"), label.box.groups = "Groups",
    slider.width = 200, slider.height = 60, button.height = 30,
    box.type.height = 70, box.group.character.width = 7,
    box.group.line.height = 30, dist.obj.width = 20, dist.obj.height = 10,
    ...)
}
\arguments{
\item{model}{Object of class \code{\link{lm}} (mandatory). Other fitted-model objects may work as well, see section 7 of the vignette and examples below.}
\item{initial.values}{Initial values for the metric covariates in a named list, default to the means. See section 5 of the vignette and examples below.}
\item{preselect.var}{Name of continous variable to be displayed as character or \code{NA} (default) for menu selection.}
\item{preselect.type}{The type of the initial plot to be displayed. Must be one of the values \code{"effect"} (default) or \code{"marginal"}.}
\item{preselect.groups}{Numeric vector with the index of the groups which are displayed in the initial plot. If \code{NULL} (the default) all groups are displayed.}
\item{dev.height}{Height of graphic device in cm, default to \code{18}.}
\item{dev.width}{Width of plot area in graphic device in cm, default to \code{18}.}
\item{dev.width.legend}{Width of legend area in graphic device in cm, default to \code{8}.}
\item{dev.pointsize}{Character pointsize of graphic device, default to \code{10}.}
\item{dev.defined}{Graphic device predefined? Default to \code{FALSE}, see section 6.3 of the vignette for usage.}
\item{col}{Vector of color specifications to represent different groups. Passed to the line commands and the legend. Actual palette and consecutive sequence if \code{NA} (default).}
\item{lty}{Vector of line type specifications to represent different groups. Passed to the line commands and the legend, default to solid lines.}
\item{lwd}{Vector of line width specifications to represent different groups. Passed to the line commands and the legend, default to \code{1}.}
\item{main}{Title for the plot, default to \code{NA}.}
\item{xlab}{Label for the x-axis. Name of the selected covariate, if \code{NA} (the default).}
\item{ylab}{Label for the y-axis. Name of the selected plot type (see argument \code{label.types}), if \code{NA} (the default).}
\item{legend.add}{Should a legend be added to the plot? Default to \code{TRUE}.}
\item{legend.space}{Should the space for the legend be reserved? Default to the value of \code{legend.add}. Setting \code{legend.add} to \code{FALSE} and \code{legend.space} to \code{TRUE} plots white space instead of the legend. This can be useful when different plots are arranged in a document to ensure exact alignments and sizes, see section 6.2 of the vignette for details.}
\item{legend.only}{Should just the legend be plotted? Default to \code{FALSE}. A plot with the legend alone can be useful when different plots are arranged in a document, see section 6.2 of the vignette for details.}
\item{legend.pos}{Position of the legend as character, see \code{\link{legend}} for details. Default to \code{"center"}.}
\item{legend.cex}{Relative size of legend text. Can be reduced if the model contains many groups. Default to \code{1}.}
\item{legend.width.factor}{Factor by which the width of the legend is increased. Default to \code{1}. Increasing this can solve the problem that the legend annotations do not fit in the surrounding box when the plots are saved as PDF or EPS files, see section 5 of the vignette for details.}
\item{rug.ticksize}{Length of rugplot tickmarks, dafault to \code{0.02}. Set to \code{0} or \code{NA}, if no rugplot should be drawn.  For many observations the rug considerably slows down the rebuild of the plot.}
\item{rug.col}{Color of rugplot tickmarks, default to \code{"black"}.}
\item{vline.actual}{Add vertical line at actual position of selected metric covariate? Default to \code{TRUE}.}
\item{pos.hline.effect}{Position of horizontal line for plot of the effects.\code{NA} for no line, default to \code{0}.}
\item{pos.hline.marginal}{Position of horizontal line for marginal effects plot. \code{NA} for no line, default to \code{0}.}
\item{n.effects}{Number of equally spaced points over the span of the selected metric covariate to calculate the effects for plotting, default to \code{100}. Increase, if lines are not smooth.}
\item{autosave.plot}{Directly save the initial plot? Default to \code{FALSE}. If set to \code{TRUE} the GUI-panel is immediately closed after initialization.}
\item{snapshot.plot}{Save plot via \code{\link{savePlot}} when snapshot button is pressed? Default to \code{FALSE}, see section 5 of the vignette for details.}
\item{graphics.filename}{Filename (optionally including a path) as character for graphic file.}
\item{graphics.extension}{Filetype argument passed to \code{\link{savePlot}}, default to \code{"pdf"}.}
\item{latex2console}{Should the textoutput triggered by the snapshot button be printed as LaTeX-code? Default to \code{FALSE}.}
\item{decimal.mark}{Decimal character for LaTeX output, default to \code{"."}.}
\item{big.mark}{Bigmark character for LaTeX output, default to \code{","}.}
\item{factor.sep}{Character separating the factor-factor level combinations in the group names (default to \code{"-"}).}
\item{level.sep}{Character separating the factor name and the corresponding factor levels in the group names (default to \code{"."}).}
\item{panel.title}{Title used in the title bar of the GUI-panel of type \code{character}.}
\item{label.button}{Label for the snapshot-button of type \code{character}.}
\item{label.slider.act}{Additional label for the slider of the selected metric covariate of type \code{character}.}
\item{label.box.type}{Title for the radiogroup box of type \code{character}.}
\item{label.types}{Labels for radiogroup buttons (\code{character} vector of length 3). By default these are also used as corresponding annotations for the y-axis.}
\item{label.box.groups}{Title for the checkbox of type \code{character}.}
\item{slider.width}{Width of each slider in points (default to \code{200}).}
\item{slider.height}{Height of each slider in points (default to \code{60}).}
\item{button.height}{Height of snapshot button in points (default to \code{30}).}
\item{box.type.height}{Height of radiobox for type selection in points (default to \code{70}).}
\item{box.group.character.width}{The width of the boxes is basically a this value times the number of characters in points (default to \code{7}).}
\item{box.group.line.height}{The height of the checkbox is this value times the number of groups in points (default to \code{29}).}
\item{dist.obj.width}{Vertical distance between sliders and boxes and vertical margins in points (default to \code{20}).}
\item{dist.obj.height}{Horizontal distance between panel objects in points (default to \code{10}).}
\item{...}{Other graphical parameters passed to \code{\link{par}}.}
}
\details{
The only mandatory argument of the function is a fitted-model object of class \code{\link{lm}}. For this object the following prerequisites must be met:
\itemize{
\item{The model must contain at least one metric covariate.}
\item{The model must be specified with the formula interface and the data frame containing the variables must be passed with the \code{data} argument.}
\item{The categorical variables must be \code{\link{factor}}s (ordered or unordered).}
}
The function may work for other fitted-model objects as well, see section 7 of the vignette for details and the examples below. The basic idea of the function lies in the interactive usage. It is nevertheless
easy to reproduce the plots for publication purposes, refer to section 5 of the vignette for this.

Objects of class \code{\link{lm}} lack a \code{data} component. For some formulas this can cause problems, refer to section 7 of the vignette for details and the example for a nonparametric effect below.

Users of the IDE \bold{RStudio} may need to change the graphic device with \code{options(device = "x11")}
before calling the function because in current versions of RStudio multiple graphic devices occasionally do not work.}
\value{
There is no object returned. By calling the function usually a menu appears from which a metric covariate employed in the model must be selected. After choosing the covariate a graphic device which contains a termplot of the selected metric covariate and a 
GUI-panel to manipulate the plot will be opened. The GUI-panel has the following elements:
\itemize{
\item{A slider for each metric covariate.}
\item{A radiobox to select the type of the current display (effect or marginal effect).}
\item{A checkbox to select the factor combinations (groups) to be displayed.}
\item{A button to print tables of output to the console, see below.}
}
The appearance of the panel can be controlled by a number of arguments, see section 6.4 of the vignette for details.

When the button is clicked four tables are printed to the console:
\enumerate{
\item{Table of coefficients obtained by the \code{\link{summary}}-method.}
\item{Table of the chosen values of the metric covariates and their ECDF-values in the dataset.}
\item{Table of the effects at the chosen values of the metric covariates for each group.}
\item{Table of marginal effects for each metric covariate at the chosen values of the metric covariates for each group.}
}
With the argument \code{latex2console} set to \code{TRUE} the tables are printed as LaTeX-code, see section 4 of the vignette for details. The format of the text output and the layout of the plots can be
controlled to a large extent, see section 6.1 and 6.2 of the vignette for details and the examples below.
}
\references{
Bowman, A., Crawford, E., Alexander, G., and Bowman, R. (2007). rpanel: Simple interactive
controls for R functions using the tcltk package. \emph{Journal of Statistical Software}, 17(9), 1-18.

Dahl, D. B. (2012). xtable: Export tables to LaTeX or HTML. R package version 1.7-0.
}
\author{ 
Martin Meermeyer \email{meermeyer@statistik.uni-wuppertal.de}
}
\note{  
Printing LaTeX-output to the console was inspired by the package \href{http://CRAN.R-project.org/package=xtable}{\pkg{xtable}} (Dahl, 2012), but an own implementation is used. 
}
\seealso{
\code{\link{glm.interactive}} for the interactive visualization of models for binary outcomes.

The noninteractive visualization of the results for various types of regression models can be achieved with the package \href{http://CRAN.R-project.org/package=effects}{\pkg{effects}}.
}
\examples{
### Model specification ###
data("munichrent03")
model.rent <- lm(rent ~ yearc + I(yearc^2) + rooms + area*location + upkitchen,
    data=munichrent03)

\dontrun{

### Basic usage ###

## RStudio users may need to change the graphic device, see details.
options(device = "x11")

## Using defaults
lm.interactive(model.rent)

## Switch text output to LaTeX
lm.interactive(model.rent, latex2console=TRUE)

## Continental European number format in LaTeX output
lm.interactive(model.rent, latex2console=TRUE, decimal.mark=",", big.mark=".")

## Save plot as PDF to current working directory when 'Snapshot' is clicked
lm.interactive(model.rent, snapshot.plot=TRUE)

## Change color scheme and line types
lm.interactive(model.rent, col=rep(c(1,2,4),each=2), lty=c(1,5))

## Change separation characters
lm.interactive(model.rent, factor.sep="|", level.sep=">")

## Suppress legend
lm.interactive(model.rent, legend.add=FALSE)

## Suppress rug plot
lm.interactive(model.rent, rug.ticksize=NA)

## Set initial values of metric covatiates 
lm.interactive(model.rent, initial.values=list(yearc=1990, rooms=3, area=80))

## Preselect covariate, plot type and groups
lm.interactive(model.rent, preselect.var="yearc", preselect.type="effect",
    preselect.groups=c(2,4,6))

## Preselect covariate and plot type and change axis annotations
lm.interactive(model.rent, preselect.var="yearc", preselect.type="effect", 
    xlab="year of construction", ylab="net rent (EUR)")


### Visualization of statistical concepts ###

## Nonlinear effect 
lm.interactive(model.rent, preselect.var="yearc")

## Nonparametric effect 
require("splines")
model.rent.bs <- lm(rent ~ bs(yearc) + rooms + area*location + upkitchen,
    data=munichrent03)
model.rent.bs$data <- munichrent03
lm.interactive(model.rent.bs, preselect.var="yearc")
# Note that the data must be attached to the fitted-model object
# afterwards, see section 7 of the vignette for details.

## Interaction effect (directly)
lm.interactive(model.rent, preselect.var="area")

## Interaction effect (indirectly)
lm.interactive(model.rent, preselect.var="yearc")
# manipulate slider for 'area'


### Other fitted-model objects ###

## Generalized additive model with gam() from package 'gam'
require("gam")
model.rent.gam <- gam(rent ~ s(yearc) + rooms + s(area)*location + upkitchen,
    data=munichrent03)
lm.interactive(model.rent.gam)

## Generalized additive model with gam() from package'mgcv'
# Incompatible to package 'gam', restart R Session if necessary
require("mgcv")
model.rent.mgcv <- gam(rent ~ s(yearc) + rooms + area*location + upkitchen,
    data=munichrent03)
lm.interactive(model.rent.mgcv)


### Additional examples ###

## Customize device for printing
lm.interactive(model.rent, 
    dev.width=6,
    dev.width.legend=4,
    dev.height=6,
    dev.pointsize=6,
    col=c("red","darkred","blue","darkblue","green","darkgreen"),
    legend.width.factor=1.1,
    vline.actual=FALSE,
    snapshot.plot=TRUE,
    graphics.filename="munichrent-termplot",
    mar=c(2.5,2.5,1,1)+0.1,
    mgp=c(1.5,0.5,0),
    tcl= -0.3)

## Save predefined plot automatically
lm.interactive(model.rent,
    initial.values=list(yearc=1990, rooms=3, area=80), 
    preselect.var="area",
    preselect.type="marginal",
    autosave.plot=TRUE,
    graphics.filename="fig-rent-area-marg",
    legend.width.factor=1.05)

## Modifications for models with many groups
# Increase space for legend and squeeze panel controls
model.rent.moregroups <- lm(rent ~ yearc + I(yearc^2) + rooms + area*location 
    + upkitchen + bathtile, data=munichrent03)
lm.interactive(model.rent.moregroups, 
    dev.width.legend=9,
    legend.cex=1,
    box.type.height=65,
    box.group.character.width=6,
    box.group.line.height=28,
    dist.obj.height=2)

# Squeeze legend and  panel controls
model.rent.manygroups <- lm(rent ~ yearc + I(yearc^2) + rooms + area 
    + district + upkitchen, data=munichrent03)
lm.interactive(model.rent.manygroups, 
    dev.width.legend=6,
    legend.cex=0.65,
    box.type.height=90,
    box.group.character.width=6,
    box.group.line.height=25,
    dist.obj.height=2)
# Note that checkbox for groups grows beyond screen
}
}