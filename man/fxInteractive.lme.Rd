\name{fxInteractive.lme}
\alias{fxInteractive.lme}
\title{
Interactive Interpretation of Linear Mixed-Effects Models
}
\description{
This method facilitates the interpretation of linear mixed-effects models and is a byproduct of the method \code{\link{fxInteractive.glm}} for  generalized linear models.
}
\usage{
\method{fxInteractive}{lme} (model, predict.lme.level = 0, initial.values = as.list(NULL),
    preselect.var = NA, preselect.type = "effect", preselect.groups = NULL,
    dev.height = 18, dev.width = 18, dev.width.legend = 8, dev.pointsize = 10,
    dev.defined = FALSE, ylim = NA, col = NA, lty = 1, lwd = 1, 
    main = NA, main.line = 1.5, xlab = NA, ylab = NA, 
    legend.add = TRUE, legend.space = legend.add, legend.only = FALSE,
    legend.pos = "center", legend.cex = 1, legend.width.factor = 1, 
    rug.ticksize = 0.02, rug.col = "black", vline.actual = TRUE, 
    pos.hlines = c(0, 0), n.effects = 100, 
    autosave.plot = FALSE, snapshot.plot = FALSE, 
    graphics.filename = "LinRegIntPlot", graphics.numbering = !autosave.plot,
    graphics.type = "pdf", factor.sep = "|", level.sep = ".", 
    latex2console = FALSE, xtable.big.mark = ".", xtable.decimal.mark = ",", 
    xtable.digits = NULL, xtable.display = NULL, xtable.booktabs = FALSE,                 
    panel.title = "Linear Mixed-Effects Model", label.button = "Snapshot",
    label.slider.act = "Variable displayed: ", label.box.type = "Type",
    label.types = c("effect", "marginal effect"),
    label.box.groups = "Groups", 
    slider.width = 200, slider.height = 60, button.height = 30,
    box.type.height = 70, box.group.character.width = 7, 
    box.group.line.height = 28, dist.obj.width = 20,
    dist.obj.height = 10, ...)
}
\arguments{
\item{model}{Object of class \code{\link{lme}} (package \href{http://CRAN.R-project.org/package=nlme}{\pkg{nlme}}) (mandatory).}
\item{predict.lme.level}{Level of grouping to be displayed, passed to \code{\link{predict.lme}}. Only one level can be displayed, default to \code{0}.}
\item{initial.values}{Initial values for the metric covariates in a named list, default to the means. See section 4 of the vignette and examples below.}
\item{preselect.var}{Name of continuous variable to be displayed as character or \code{NA} (default) for menu selection.}
\item{preselect.type}{The type of the initial plot to be displayed. Must be one of the values \code{"effect"} (default) or \code{"marginal"}.}
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
\item{pos.hlines}{Positions of the horizontal lines for [1] the plot of the effects and [2] the plot of marginal effects. \code{NA} for no lines, default to \code{c(0,0)}.}
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
\item{label.types}{Labels for radiogroup buttons (\code{character} vector of length 2). By default these are also used as corresponding annotations for the y-axis.}
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
The only mandatory argument of the function is a fitted-model object of class \code{\link{lme}} (package \href{http://CRAN.R-project.org/package=nlme}{\pkg{nlme}}). For this object the following prerequisites must be met:
\itemize{
\item{The model must contain at least one metric covariate.}
\item{The model must be specified with the formula interface and the data frame containing the variables must be passed with the \code{data} argument.}
\item{The categorical variables must be \code{\link{factor}}s (ordered or unordered).}
}
In this method the additional argument \code{predict.lme.level} is responsible for the level of grouping to be displayed, see the documentation of \code{\link{predict.lme}}.

Please refer to the documentation of \code{\link{fxInteractive}} or the vignette for more details.
}
\value{
No object is returned, please refer to the documentation of \code{\link{fxInteractive}} or the vignette for more details.
}
\seealso{
The examples in \code{\link{fxInteractive.glm}} and \code{\link{fxInteractive.lm}} are more elaborated and show different aspects of usage and customization which are valid for this method, too.
}
\author{ 
Martin Meermeyer \email{meermeyer@statistik.uni-wuppertal.de}
}
\examples{
require("nlme")
data("Orthodont")
LMEmod <- lme(distance ~ age + Sex, data = Orthodont, random = ~ age | Subject)

\dontrun{
## RStudio users may need to change the graphic device, see details.
options(device = "x11")

## Grouping level 0 is used by default.
fxInteractive(LMEmod, legend.cex=0.6)

## Display grouping level 1
fxInteractive(LMEmod, predict.lme.level=1, legend.cex=0.6)

## For individual specific factors only observable factor combinations are meaningful 
# the grouping variable must be the last column
combs.count    <- factorCombinations(Orthodont[,c(4,3)])$counts 
combs.observed <- combs.count > 0
fxInteractive(LMEmod, predict.lme.level=1, preselect.groups=combs.observed)
}
}