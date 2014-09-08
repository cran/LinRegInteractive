lm.interactive <- function(model, # object of class lm is mandatory
		
		## Control initial appearance ##
		initial.values           = as.list(NULL), # Initial values for the metric covariates in a named list, default to the means. See details.
		preselect.var            = NA, # Name of continous variable to be displayed as character or NA for menu selection, default to NA.
		preselect.type           = "effect",
		preselect.groups 		     = NULL, # Index of groups to be preselectes. If set to NULL (default) all groups are preselected.
		
		## Parameters for plot layout ##
		dev.height          = 18,	 # Height of graphic device in cm.
		dev.width           = 18,	 # Width of plot area in graphic device in cm.
		dev.width.legend    = 8,	 # Width of legend area in graphic device in cm.
		dev.pointsize		= 10,	 # Character pointsize of graphic device.
		dev.defined         = FALSE, # Graphic device predefined, e. g. customized for printing? Default to FALSE, see details.
		col					= NA,	 # Vector of color specifications for the groups. Passed to the line commands and to the legend. Actual palette and consecutive sequence if NA (default).
		lty					= 1,     # Vector of line type specifications for the groups. Passed to the line commands and to the legend, default to solid lines.
		lwd					= 1,     # Vector of line width specifications for the groups. Passed to the line commands and to the legend, default to 1.
		main				= NA,	 # Label for the plot title.
		xlab				= NA,	 # Label for the x-axis. Name of the selected covariate, if NA.
		ylab				= NA,	 # Label for the y-axis. Name of the selected plot type (see argument label.types), if NA.
		legend.add          = TRUE,  # Should a legend be added to the plot? Default to TRUE, but too many groups can cause problems.
		legend.space        = legend.add, # Should the space for the legend be reserved? Default to the value of legend.add. Setting legend.add to FALSE and legend.space to TRUE plots white space instead of the legend. This can be useful if different termplots will be arranged in a document.  
		legend.only         = FALSE,
		legend.pos			= "center", # Position of the legend, see legend() for details.
		legend.cex          = 1,	 # Relative size of legend text, reduce for many groups.
		legend.width.factor = 1,
		rug.ticksize		= 0.02,  # Length of rugplot tickmarks. Set to 0, if no rugplot should be drawn.
		rug.col				= "black",	 # Color of rugplot tickmarks, default to black.	
		vline.actual        = TRUE,  # Add vertical line at actual postion of selceted metric covariate? Default to TRUE.
		pos.hline.effect  	= 0,     # Positon of horizontal line for termplot, NA for no line.
		pos.hline.marginal  = 0,     # Positon of horizontal line for marginal effects plot, NA for no line.
		n.effects           = 100,   # Number of equally space points over the span of the metric covariate used for plotting.
		
		## Parameters for plot snapshot
		autosave.plot       = FALSE,    # Save the initial plot?
		snapshot.plot       = FALSE,    # Save plot via savePlot() when snapshot button is pressed? Default to FALSE.
		graphics.filename   = "LinRegIntPlot",  # Filename as character for graphic file.
		graphics.extension  = "pdf",    # Filetype argument passed to savePlot().
		
		## Parameters for text-output ##
		latex2console       = FALSE, # Should the textoutput triggered by the snapshot button be printed as LaTeX?
		decimal.mark        = ".",   # Decimal character for LaTeX output.
		big.mark            = ",",   # Bigmark character for LaTeX output.
		factor.sep          = "-", # character by which the factors are separated in the groupnames
		level.sep           = ".", # character by which the levels are separated in the groupnames
						
		## Annotations for panel 
		panel.title         = "Linear Model" ,					# Title used in the panel.
		label.button        = "Snapshot" ,						# Label for the snapshot-button.
		label.slider.act    = "Variable displayed: " ,			# Additional label for the slider of the selected metric covariate. 
		label.box.type      = "Type" ,							# Title for the radiogroup box.
		label.types         = c("effect", "marginal effect"),   # Lables for radiogroup buttons (character vector of length 2).
		label.box.groups    = "Groups" ,						# Title for the checkbox.
		
		## Parameters to control the size of the panel.
		slider.width                = 200 , # Width of each slider.    
		slider.height               = 60  , # Height of each slider.   
		button.height               = 30  , # Height of snapshot button.
		box.type.height             = 70  , # Height of radiobox for type selection.
		box.group.character.width   = 7   , # The width of the boxes is basically a this value times the number of characters.
		box.group.line.height       = 30  , # The height of the checkbox is this value times the number of groups.
		dist.obj.width              = 20  , # Vertical distance between sliders and boxes and vertical margins. 
		dist.obj.height             = 10  , # Horizontal distance between panel objects.
		
		... ) # other graphical parameters passed to par()	
	{
	#################################################################	
	# Pick covariates employed, check variables for factors, assign #
	# corresponding objects and initialize graphic window			#
	#################################################################	
	
	# pick covariates which are employed in the model
	if(!is.null(model$data))
		{# extract data from model$data if possible
		X <- get_all_vars(model$terms, model$data)[,-1,drop=FALSE]
		}else
		{# try to extract data from model$model
		X <- get_all_vars(model$terms, model$model)[,-1,drop=FALSE]	
		}
	
	# identify factors from model matrix
	logicalindex.factor <- as.vector(NULL)
	num.level           <- as.vector(NULL)
	for(i in 1:dim(X)[2])
	{
		is.factor.act       <- is.factor(X[,i])
		logicalindex.factor <- c(logicalindex.factor, is.factor.act)
		if(is.factor.act)
		{
			num.level.act <- length(levels(X[,i]))
			num.level     <- c(num.level, num.level.act)
		}
	}
	
	# Are factors used as covariates?
	factors.present <- !is.null(num.level)
	
	# separate factors and continous covariates 
	X.continous   <- X[,!logicalindex.factor, drop=FALSE]
	num.continous <- dim(X.continous)[2]
	
	# switch for special treatment of single metric covariates
	single.covariate <- num.continous==1 
	
	if(factors.present)
	{
		# separate factors 
		X.factor    <- X[,logicalindex.factor, drop=FALSE]
		num.groups  <- prod(num.level)
		
		# build all factor combinations
		factor.comb <- factor.combinations(X.factor, factor.sep=factor.sep, level.sep=level.sep, count=FALSE)
	}
	
	# If not specified in the function call: if there is only one continous covariate, choose it. 
	# Otherwise select the continous covariate to be displayed from popup list.
	if(is.na(preselect.var))
		{
		if(single.covariate)
			{
			var.selection <- colnames(X.continous)[1]
			}else
			{	
			var.selection <- select.list(colnames(X.continous))
			}
		}else
		{# in no variable is preselected
		if(single.covariate)
			{# if there is only one metric covariate, chose it
			var.selection <- colnames(X.continous)[1]
			}else
			{# Check if preselected covariate exists, if not, show the menu	
			if(any(colnames(X.continous)==preselect.var))
				{
				var.selection <- preselect.var		
				}else
				{
				var.selection <- select.list(colnames(X.continous))	
				}	
			}
		}
	
	# If no variable is selected from popup list leave function.
	if(var.selection=="") return()
	
	# pick continous covariate under invstigation
	x.actual           <- X.continous[,var.selection]
	x.actual.sequence  <- seq(min(x.actual),max(x.actual), length=n.effects)
	X.pred             <- data.frame(x=x.actual.sequence)
	colnames(X.pred)   <- var.selection
	
	# pick other continous covariates if present
	if(!single.covariate)
	{	
		X.continous.other     <- X.continous[,-match(var.selection, colnames(X.continous)), drop=FALSE]
		nam.X.continous.other <- colnames(X.continous.other)
		num.continous.other   <- length(nam.X.continous.other)
		val.X.continous.other <- colMeans(X.continous.other)
	}
	
	# If not predefined nor switched off via the argument: set up a new graphic device 
	if(!dev.defined)
	{	
		if(legend.only)
		{# Initialize device for legend only
			dev.new(width=dev.width.legend/2.54, height=dev.height/2.54, pointsize=dev.pointsize, buffered=TRUE, noRStudioGD = TRUE)
			par(cex=1, ...)
		}else
		{# If factors are used as covariates and legend should be printed, split plot region via layout()	
			if(factors.present & legend.space)
			{
				dev.new(width=(dev.width + dev.width.legend)/2.54, height=dev.height/2.54, pointsize=dev.pointsize, buffered=TRUE, noRStudioGD = TRUE)
				fraction.plot <- round((dev.width/(dev.width + dev.width.legend))*100, digits=0)
				layoutmatrix  <- matrix(c(rep(2, fraction.plot), rep(1, (100-fraction.plot))),1,100)
				layout(layoutmatrix)
				par(cex=1, ...)
			}else
			{
				dev.new(width=dev.width/2.54, height=dev.height/2.54, pointsize=dev.pointsize, buffered=TRUE, noRStudioGD = TRUE)
				par(cex=1, ...)
			}
		}
	}
	
	#############################################	
	#  Action when panel items are manipulated 	#
	#  (except action for snapshot-button)		#	
	#############################################
	func.panel.action <- function(panel) 
	{# Main panel function, must be defined within the namespace where panel is established
		# read plot type from panel
		if(length(panel$type)==0) # in the first call of the panel the slots are sometimes NULL   
			{
			type.actual <- "effect"
			}else
			{
			type.actual <- panel$type
			}
		
		# read group selection from panel if groups are present
		if(factors.present)
			{
			if(length(panel$groups)==0) 
				{# in the first call of the panel the slots are sometimes NULL 
				if(is.null(preselect.groups))
					{
					logical.index.groups  <- rep(TRUE, times=num.groups)
					}else
					{
					logical.index.groups  <- rep(FALSE, times=num.groups)
					logical.index.groups[preselect.groups]  <- TRUE
					}
				index.groups.selected <- c(1:num.groups)[logical.index.groups]
				# if all groups are deselected choose the first
				if(sum(logical.index.groups)==0)
					{
					index.groups.selected  <- 1  
					}
				}else
				{
				logical.index.groups  <- panel$groups
				index.groups.selected <- c(1:num.groups)[logical.index.groups]
			
				# if all groups are deselected choose the first
				if(sum(logical.index.groups)==0)
					{
					index.groups.selected  <- 1  
					}
				}
			}# end if factors.present	
			
		# read value of actual variable from panel
		if(length(panel$var.selected.act)==0)
		    {
			var.selected.act <- 1
			}else
			{
			var.selected.act <- panel$var.selected.act      
			}   
		
		# build designmatrix for predict-method from continous covariates
		if(single.covariate)
		{
			X.pred.metr <- X.pred			
		}else
		{# read actual values from panel and add to designmatrix for predict-method
			for(i in 1:num.continous.other)
			{
				if(length(panel[[nam.X.continous.other[i]]])==0) # initialization fails
				{
					val.X.continous.other[i] <- i
				}else
				{
					val.X.continous.other[i] <- panel[[nam.X.continous.other[i]]]
				}
			}
			X.pred.metr <- cbind(X.pred, t(val.X.continous.other))
		}
		
		# If factors are used as covariates, calculate predictions for each group.
		if(factors.present)
		{
			# calculate predictions for each selected group
			list.groups <- as.list(NULL)
			for(i in seq(along=index.groups.selected))
			{
				index.group.actual <- index.groups.selected[i]  
				
				# read actual factor combination
				factor.group.actual <- factor.comb$comb[index.group.actual,,drop=FALSE]
				rownames(factor.group.actual) <- ""
				
				# combine with continous covariates to new designmatrix
				X.pred <- cbind(X.pred.metr, factor.group.actual)
				
				# calculate effect or marginal effect
				switch(type.actual,
						effect = pred.x.actual <- predict(model, newdata=X.pred),
						marginal = pred.x.actual <- splinefun(x=x.actual.sequence, y=predict(model, newdata=X.pred))(x.actual.sequence, deriv=1))
						
				
				# store results for each and every group 
				sub.list <- list(name=factor.comb$names[index.group.actual],
						prog=pred.x.actual)
				
				list.groups[[i]] <- sub.list 
			}
		}else
		{
			# calculate effect or marginal effect
			switch(type.actual,
					effect = pred.x.actual <- predict(model, newdata=X.pred.metr),
					marginal = pred.x.actual <- splinefun(x=x.actual.sequence, y=predict(model, newdata=X.pred.metr))(x.actual.sequence, deriv=1))
			
			index.groups.selected <- 1
			
			# store results
			list.groups <- as.list(NULL)
			list.groups[[1]] <- list(name="default", prog=pred.x.actual)
		}
		
		# determine plot limits in y-direction
		switch(type.actual,
				effect = limits.y <- c(min(unlist(lapply(list.groups,"[",2))), max(unlist(lapply(list.groups,"[",2)))),
				marginal = limits.y <- c(min(unlist(lapply(list.groups,"[",2))), max(unlist(lapply(list.groups,"[",2))))
		)

		### Plotting commands ###
		# draw effects for each and every group
		# when no factors are present set number of groups to 1 for correct color handling
		if(!factors.present) num.groups <- 1
		# specify color scheme
		if(any(is.na(col))) 
		{
			col.types <- c(1:num.groups)
		}else
		{	
			col.types <- rep(col, times=num.groups)
		}
		
		lty.types <- rep(lty, times=num.groups)
		lwd.types <- rep(lwd, times=num.groups)
		
		# in conjunction with dev.flush() and buffered=TRUE animation is more fluent
		dev.hold()
		
		if(factors.present & legend.only)
		{# When legend.only is active plot legend only
			old.mar <- par("mar")
			par(mar=c(old.mar[1],0,old.mar[3],0))
			plot(1,1, type="n", axes=FALSE, xlab=NA, ylab=NA)
			legend.labels <- paste(factor.comb$names[index.groups.selected],"", sep="")
			legend(x=legend.pos, legend=legend.labels, text.width=max(strwidth(legend.labels, cex=legend.cex))*legend.width.factor, col=col.types[index.groups.selected], lty=lty.types[index.groups.selected], lwd=lwd.types[index.groups.selected], cex=legend.cex)
			par(mar=old.mar)    
			}else	
			{# when inactive incorporate other legend parameters
			# when factors are used as covariates and switch is on: add legend first to allow adding elements to the main plot	
			if(factors.present & legend.space)
			{
				old.mar <- par("mar")
				par(mar=c(old.mar[1],0,old.mar[3],0))
				plot(1,1, type="n", axes=FALSE, xlab=NA, ylab=NA)
				if(legend.add)
				{
					legend.labels <- paste(factor.comb$names[index.groups.selected],"", sep="")
					legend(x=legend.pos, legend=legend.labels, text.width=max(strwidth(legend.labels, cex=legend.cex))*legend.width.factor, col=col.types[index.groups.selected], lty=lty.types[index.groups.selected], lwd=lwd.types[index.groups.selected], cex=legend.cex)
				}
				par(mar=old.mar)    
			}		
			
			for(i in seq(along=index.groups.selected))
			{
				# initial plot in first loop	
				if(i==1)
				{
					if(is.na(xlab)) xlab <- var.selection
					if(is.na(ylab))
						switch(type.actual,
								effect   = ylab <- label.types[1],
								marginal = ylab <- label.types[2])
					
					
					plot(x.actual.sequence, rep(1, times=length(x.actual.sequence)), type="n", ylim=limits.y, xlab=xlab, ylab=ylab, main=main)
					if(!((rug.ticksize==0)||is.na(rug.ticksize))) rug(x.actual,  ticksize = rug.ticksize, col= rug.col) 
				}
				
				index.group.actual <- index.groups.selected[i]  
				
				# if there is only a litte bit variation in the effect draw a horizontal line
				if(sd(list.groups[[i]]$prog)<10^-10)
				{
					abline(h=mean(list.groups[[i]]$prog), col=col.types[index.group.actual], lty=lty.types[index.group.actual], lwd=lwd.types[index.group.actual])	
				}else	
				{ 
					lines(x.actual.sequence, list.groups[[i]]$prog, type="l", col=col.types[index.group.actual], lty=lty.types[index.group.actual], lwd=lwd.types[index.group.actual])
				}     
			}
			
			# vertical line at actual position
			if(vline.actual) abline(v=(var.selected.act))
			
			# draw horizontal line at specified positions
			switch(type.actual,
					effect 	 = abline(h=pos.hline.effect),
					marginal = abline(h=pos.hline.marginal))	
		}# end if legend.only
		
		# in conjunction with dev.hold() and buffered=TRUE animation is more fluent
		dev.flush()
		
		# When autosave is activated directly save the plot
		if(autosave.plot)
			{
			# Create unused filename for plot
			for(i in 1:1000)
				{
				graphics.filename.candidate <- paste(graphics.filename,"-", formatC(i, width=3, flag="0"), sep="")
				if(file.exists(paste(graphics.filename.candidate, ".", graphics.extension, sep=""))) next else break
				}
			savePlot(filename = graphics.filename.candidate, type = graphics.extension)	
			}
				
		panel
	} # end body action()
	
	#############################################	
	#    Action when snapshot-button is used  	#
	#############################################
	internal.snapshot <- function(panel)
	{# function must be defined within the namespace where panel is established
	# save plot if snapshot.plot is TRUE
	if(snapshot.plot)      
		{               
		# Create unused filename for plot
		for(i in 1:1000)
			{
			graphics.filename.candidate <- paste(graphics.filename,"-", formatC(i, width=3, flag="0"), sep="")
			if(file.exists(paste(graphics.filename.candidate, ".", graphics.extension, sep=""))) next else break
			}
		savePlot(filename = graphics.filename.candidate, type = graphics.extension)
		}	
		
		# read values from panel and build design matrix for predict-method
		var.selected.act <- panel$var.selected.act
		if(single.covariate)
		{
			X.pred.metr <- data.frame(var.selected.act)
			colnames(X.pred.metr)[1] <- var.selection
			vek.x.continous.actual <- var.selected.act
			names(vek.x.continous.actual) <- var.selection
		}else
		{	
			for(i in 1:num.continous.other)
			{
				val.X.continous.other[i] <- panel[[nam.X.continous.other[i]]]
			}
			
			# build designmatrix from continous covariates
			X.pred.metr <- cbind(var.selected.act, t(val.X.continous.other))
			colnames(X.pred.metr)[1] <- var.selection
			vek.x.continous.actual <- as.vector(X.pred.metr)
			names(vek.x.continous.actual) <- colnames(X.pred.metr)
		}
		
		if(factors.present)
		{
			X.pred <- cbind(X.pred.metr, factor.comb$comb)
		}else
		{
			X.pred <- as.data.frame(X.pred.metr)
		}
		
		output.effect.resp <- cbind(predict(model, newdata=X.pred),1)[,-2, drop=FALSE]		
		colnames(output.effect.resp)  <- c("effect")
		
		if(factors.present)
		{
			rownames(output.effect.resp)  <- factor.comb$names
		}else
		{
			rownames(output.effect.resp)  <- ""   
		}
		
		# calculate ECDF-value for each continous covariate and  marginal effects in each group
		F.x.continous 	<- NULL
		output.me 		<- NULL
		for(i in 1:num.continous)
		{
			nam.x.marginal.actual       <- colnames(X.continous)[i]
			
			x.marginal.actual           <- X.continous[,nam.x.marginal.actual, drop=TRUE]
			x.marginal.actual.sequence  <- seq(min(x.marginal.actual),max(x.marginal.actual), length=n.effects)
			X.marginal.pred             <- data.frame(x=x.marginal.actual.sequence)
			colnames(X.marginal.pred)   <- nam.x.marginal.actual
			
			if(!single.covariate)
			{	
				nam.X.marginal.other        <- colnames(X.continous)[-i]
				X.marginal.pred             <- cbind(X.marginal.pred, X.pred.metr[,nam.X.marginal.other, drop=FALSE])
				colnames(X.marginal.pred)   <- c(nam.x.marginal.actual, nam.X.marginal.other)   
			}
			
			x.marginal.actual.selected  <- X.pred.metr[,nam.x.marginal.actual, drop=TRUE]
			
			# ECDF-value	
			F.x.continous.actual        <- sum(x.marginal.actual <= x.marginal.actual.selected)/length(x.marginal.actual)
			F.x.continous               <- c(F.x.continous, F.x.continous.actual)
			
			# calculate marginals for actual continous covariate in each group
			marginal.actual.groups <- NULL
			if(factors.present)
			{
				for(i in 1:num.groups)
				{
					# read actual factor combination
					factor.group.actual <- factor.comb$comb[i,,drop=FALSE]
					rownames(factor.group.actual) <- ""
					
					# combine with continous covariates to new designmatrix
					X.marginal.pred.actual <- cbind(X.marginal.pred, factor.group.actual)
					
					marginal.actual.groups.actual   <- splinefun(x=x.marginal.actual.sequence, y=predict(model, newdata=X.marginal.pred.actual))(x.marginal.actual.selected, deriv=1)
					marginal.actual.groups          <- c(marginal.actual.groups, marginal.actual.groups.actual)
				}   
			}else
			{
				marginal.actual.groups  <-	splinefun(x=x.marginal.actual.sequence, y=predict(model, newdata=X.marginal.pred))(x.marginal.actual.selected, deriv=1) 
			}
			
			output.me <- cbind(output.me, marginal.actual.groups)
		}#end for-loop continous covariates
		
		names(F.x.continous) <- colnames(X.continous)
		
		colnames(output.me) <- paste("marg.eff.",colnames(X.continous),sep="")
		
		if(factors.present)
		{
			rownames(output.me) <- factor.comb$names
		}else
		{
			rownames(output.me) <- ""   
		}
		
		if(!latex2console)
		{
			print(summary(model))
			cat(fill=TRUE)
			cat("Actual values of metric covariates", fill=TRUE)
			print(vek.x.continous.actual, fill=TRUE)
			
			cat(fill=TRUE)
			cat("ECDF of actual values of metric covariates", fill=TRUE)
			print(F.x.continous)
			
			cat(fill=TRUE)
			print(output.effect.resp)
			
			cat(fill=TRUE)
			print(output.me)
		}else
		{
			cat(fill=TRUE)
			cat("% \\usepackage(booktabs) required in preamble", fill=TRUE)
			cat(fill=TRUE)
			
			# tables for coefficients, resembles table from print(summary(model))
			# for other classes than "lm": is there a slot $coef... 
			if(!is.null(summary(model)$coef))
				{# ... and does it have 4 columns?
				if(dim(summary(model)$coef)[2]==4)
					{
					tab.coef        <- summary(model)$coef
					rownames.plot   <- rownames(tab.coef)
					rownames.plot   <- gsub("^", "\\verb+^+", rownames.plot, fixed=TRUE) # make "^" stable for LaTeX
					
					coef.est.plot <- format(tab.coef[,1], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
					coef.std.plot <- format(tab.coef[,2], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
					t.value.plot  <- format(tab.coef[,3], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
					p.value.plot  <- format(tab.coef[,4], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
					
					cat("\\begin{table}[ht]", fill=TRUE)
					cat("\\centering" , fill=TRUE)
					cat("\\begin{tabular}{lrrrr} \\toprule", fill=TRUE)
					cat(" &  \\multicolumn{1}{c}{Estimate} & \\multicolumn{1}{c}{Std. Error} & \\multicolumn{1}{c}{z value} & \\multicolumn{1}{c}{Pr(>$|\\mbox{z}|$)} \\\\ \\midrule", fill=TRUE)
					for(i in 1:dim(tab.coef)[1])
					{
						cat(rownames.plot[i], " & ", coef.est.plot[i] , " & ", coef.std.plot[i] , " & ", t.value.plot[i] , " & ", p.value.plot[i] , " \\\\" , sep="", fill=TRUE)
					}
					cat(" \\bottomrule ", sep="", fill=TRUE)
					cat("\\end{tabular}", fill=TRUE)
					cat("\\caption{Model coefficients}", fill=TRUE)
					cat("\\end{table}", fill=TRUE)
					}
				}
			
			# table of current x- and ECDF-values
			vek.x.continous.actual.latex    <- format(vek.x.continous.actual    , trim = TRUE, drop0trailing = TRUE,  big.mark = big.mark, decimal.mark = decimal.mark)
			F.x.continous.latex             <- format(F.x.continous             , trim = TRUE, drop0trailing = TRUE,  big.mark = big.mark, decimal.mark = decimal.mark)
			output.effect.resp.latex        <- format(output.effect.resp        , trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
			output.me.latex                 <- format(output.me                 , trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
			
			cat(fill=TRUE)
			cat("\\begin{table}[ht]", fill=TRUE)
			cat("\\centering" , fill=TRUE)
			
			cat("\\begin{tabular}{l")
			for(j in 1:length(vek.x.continous.actual))
			{
				cat("c")
			}
			cat("} \\toprule",fill=TRUE)
			
			cat("\\multicolumn{1}{c}{variable} ")
			for(j in 1:length(vek.x.continous.actual))
			{
				cat(" &  \\multicolumn{1}{c}{",names(vek.x.continous.actual)[j],"} ", sep="")
			}
			cat("\\\\ \\midrule ", sep="", fill=TRUE)
			
			cat("\\multicolumn{1}{c}{value} ")
			
			for(j in 1:length(vek.x.continous.actual))
			{
				cat(" & ",vek.x.continous.actual.latex[j], sep="")
			}
			cat(" \\\\ ", sep="", fill=TRUE)
			
			cat("\\multicolumn{1}{c}{ECDF(value)} ")
			for(j in 1:length(F.x.continous))
			{
				cat(" & ",F.x.continous.latex[j], sep="")
			}
			cat(" \\\\ \\bottomrule ", sep="", fill=TRUE)
			cat("\\end{tabular}", fill=TRUE)
			cat("\\caption{Values of metric covariates where results are calculated}", fill=TRUE)
			cat("\\end{table}", fill=TRUE)
			
			# If factors are used as covariates build corresponding output.
			if(factors.present)
				{	
				# table of link and response
				cat(fill=TRUE)
				cat("\\begin{table}[ht]", fill=TRUE)
				cat("\\centering" , fill=TRUE)
				cat("\\begin{tabular}{lc} \\toprule", fill=TRUE)
				cat("\\multicolumn{1}{c}{groups} &  \\multicolumn{1}{c}{effect}  \\\\ \\midrule", fill=TRUE)
				
				for(i in 1:dim(output.effect.resp.latex)[1])
				{
					cat(rownames(output.effect.resp.latex)[i], " & ", output.effect.resp.latex[i,1], " \\\\" , sep="", fill=TRUE)
				}
				cat(" \\bottomrule ", sep="", fill=TRUE)
				cat("\\end{tabular}", fill=TRUE)
				cat("\\caption{Linear predictor and response in different groups for given values of metric covariates}", fill=TRUE)
				cat("\\end{table}", fill=TRUE)
				
				# table of marginal effects
				cat(fill=TRUE)
				cat("\\begin{table}[ht]", fill=TRUE)
				cat("\\centering" , fill=TRUE)
				
				cat("\\begin{tabular}{l")
				for(j in 1:(length(vek.x.continous.actual)-1))
				{
					cat("r")
				}
				cat("r} \\toprule",fill=TRUE)
				
				for(j in 1:length(vek.x.continous.actual))
				{
					cat(" &  \\multicolumn{1}{c}{",names(vek.x.continous.actual)[j], "}", sep="")
				}
				cat(" \\\\ \\midrule ", sep="", fill=TRUE)
				
				cat("\\multicolumn{1}{c}{value} ")
				for(j in 1:length(vek.x.continous.actual))
				{
					cat(" & ",vek.x.continous.actual.latex[j], sep="")
				}
				cat(" \\\\ ", sep="", fill=TRUE)
				
				cat("\\multicolumn{1}{c}{ECDF(value)} ")
				for(j in 1:length(F.x.continous))
				{
					cat(" & ",F.x.continous.latex[j], sep="")
				}
				cat(" \\\\ \\midrule ", sep="", fill=TRUE)
				
				
				for(i in 1:dim(output.me.latex)[1])
				{
					cat(rownames(output.me.latex)[i], "", sep="")
					for(j in 1:dim(output.me.latex)[2])
					{
						cat(" & ",output.me.latex[i,j], sep="")
					}
					cat(" \\\\ ", sep="", fill=TRUE)
				}
				cat(" \\bottomrule ", sep="", fill=TRUE)
				cat("\\end{tabular}", fill=TRUE)
				cat("\\caption{Marginal effects in different groups for given values of metric covariates}", fill=TRUE)
				cat("\\end{table}", fill=TRUE)
			}else
			{# latex output if no factors are present
				# table of link and response
				cat(fill=TRUE)
				cat("\\begin{table}[ht]", fill=TRUE)
				cat("\\centering" , fill=TRUE)
				cat("\\begin{tabular}{c} \\toprule", fill=TRUE)
				cat(" \\multicolumn{1}{c}{effect}  \\\\ \\midrule", fill=TRUE)
				for(i in 1:dim(output.effect.resp.latex)[1])
				{
					cat(output.effect.resp.latex[i,1], " \\\\" , sep="", fill=TRUE)
				}
				cat(" \\bottomrule ", sep="", fill=TRUE)
				cat("\\end{tabular}", fill=TRUE)
				cat("\\caption{Linear predictor for given values of metric covariates}", fill=TRUE)
				cat("\\end{table}", fill=TRUE)
				
				# table of marginal effects
				cat(fill=TRUE)  
				cat("\\begin{table}[ht]", fill=TRUE) 
				cat("\\centering" , fill=TRUE)  
				
				cat("\\begin{tabular}{l")   
				for(j in 1:length(vek.x.continous.actual))  
				{   
					cat("r")    
				}   
				cat("} \\toprule",fill=TRUE)   
				
				for(j in 1:length(vek.x.continous.actual))  
				{   
					cat(" & \\multicolumn{1}{c}{",names(vek.x.continous.actual)[j], "}", sep="")    
				}   
				cat(" \\\\ \\midrule ", sep="", fill=TRUE)  
				
				cat("\\multicolumn{1}{c}{value} ")  
				for(j in 1:length(vek.x.continous.actual))  
				{   
					cat(" & ",vek.x.continous.actual.latex[j], sep="")  
				}   
				cat(" \\\\ ", sep="", fill=TRUE)    
				
				cat("\\multicolumn{1}{c}{ECDF(value)} ")    
				for(j in 1:length(F.x.continous))   
				{   
					cat(" & ",F.x.continous.latex[j], sep="")   
				}   
				cat(" \\\\ \\midrule ", sep="", fill=TRUE)
				
				for(i in 1:dim(output.me.latex)[1])
				{
					cat("\\multicolumn{1}{c}{marginal effect} ")
					for(j in 1:dim(output.me.latex)[2])
					{
						cat(" & ",output.me.latex[i,j], sep="")
					}
					cat(" \\\\ ", sep="", fill=TRUE)
				}
				cat(" \\bottomrule ", sep="", fill=TRUE)
				cat("\\end{tabular}", fill=TRUE)
				cat("\\caption{Marginal effects for given values of metric covariates}", fill=TRUE)
				cat("\\end{table}", fill=TRUE)
			}# end if factors.present
		}# end if latex2console
		panel
	}# end body internal.snapshot
	
	#############################################	
	#  Determine panel dimensions and layout 	#
	#############################################	
	
	# Calculate slider positions and sizes
	y.pos.slider.other <- 2.5*dist.obj.height + slider.height
	
	if(factors.present)
	{
		# Determine maximum length of group name
		length.factorgroup.name <- NULL 
		for(i in 1:num.groups)
		{
			length.factorgroup.name <- c(length.factorgroup.name, length(strsplit(factor.comb$names[i], split=NULL)[[1]]))
		}
		max.length.factorgroup.name <- max(length.factorgroup.name)
	}else
	{#if no groups are present set to fix value
		max.length.factorgroup.name <- 20   
	}
	
	# Calculate button position and size
	button.width        <- max(box.group.character.width*max.length.factorgroup.name, box.group.character.width*18)
	button.pos.x        <- 2*dist.obj.width + slider.width
	button.pos.y        <- dist.obj.height
	
	# Calculate box positions and sizes
	boxes.width         <- button.width
	box.pos.x           <- button.pos.x
	box.type.pos.y      <- button.pos.y + button.height + dist.obj.height
	box.groups.pos.y    <- box.type.pos.y  + box.type.height + dist.obj.height
	
	if(factors.present)
	{
		box.group.height <- box.group.line.height*length(factor.comb$names)
	}else
	{
		box.group.height <- 0   
	}
	
	# Calculate overall panel size
	overall.width   <- box.pos.x + dist.obj.width + boxes.width
	overall.height  <- max((4*dist.obj.height + box.type.height + box.group.height + button.height), ((num.continous-1)*slider.height + y.pos.slider.other + dist.obj.height))
	
	#########################################################	
	#  Define main panel and populate it with GUI controls 	#
	#########################################################
	mainpanel <- rp.control(title=panel.title, size = c(overall.width,overall.height))
	
	# Slider for displayed continous variable
	initial.act <- initial.values[[var.selection]]
	if(is.null(initial.act)) initial.act <- mean(x.actual) 
	
	eval(wrap.rp.slider(panel="mainpanel", variable="var.selected.act", title=paste(label.slider.act, var.selection, sep=""), 
					from = min(x.actual), to=max(x.actual), initval=initial.act,
					pos= c(dist.obj.width, dist.obj.height, slider.width, slider.height) , showvalue =TRUE, action = "func.panel.action"))
	
	# Sliders for other continous variables (if present)
	if(!single.covariate)
	{
		for(i in 1:num.continous.other)
		{
			y.pos.slider <- y.pos.slider.other + (i-1)*slider.height
			
			initial.act <- initial.values[[nam.X.continous.other[i]]]
			if(is.null(initial.act)) initial.act <- mean(X.continous.other[,i])
			
			slidercall <- wrap.rp.slider(panel="mainpanel", variable=nam.X.continous.other[i],title=nam.X.continous.other[i], 
					from = min(X.continous.other[,i]), to=max(X.continous.other[,i]), initval=initial.act,
					pos= c(dist.obj.width, y.pos.slider, slider.width, slider.height) , showvalue =TRUE, action = "func.panel.action")
			eval(slidercall)
		}
	}
	
	# Button for snapshot
	rp.button(panel=mainpanel, action = internal.snapshot, title = label.button , pos = c(button.pos.x, button.pos.y, button.width, button.height))
	
	# Radiogroup for type
	# if type is preselected check for allowed value
	if(any(preselect.type==c("effect", "marginal")))
		{# if allowed use it
		type.initval <- preselect.type[1]
		}else
		{# if not allowed set as effect
		type.initval <- "effect"	
		}

	# Cheat R CMD check	
	radiogroup.call <- "rp.radiogroup(panel=mainpanel, variable=type, vals=c('effect', 'marginal'), initval=type.initval, labels=label.types, 
                        title = label.box.type, action = func.panel.action, pos = c(box.pos.x, box.type.pos.y, boxes.width, box.type.height))"
	eval(parse(text=radiogroup.call)) 
		
	# Checkbox for groups, if factors are employed in the model
	if(factors.present)
		{
		if(is.null(preselect.groups))
			{
			init.logical.index.groups  <- rep(TRUE, times=num.groups)
			}else
			{
			init.logical.index.groups  <- rep(FALSE, times=num.groups)
			init.logical.index.groups[preselect.groups]  <- TRUE
			}
			
		# Cheat R CMD check	
	    checkbox.call <- "rp.checkbox(panel=mainpanel, variable=groups, action = func.panel.action, initval=init.logical.index.groups,
                          labels = factor.comb$names, title = label.box.groups, pos = c(box.pos.x, box.groups.pos.y, boxes.width, box.group.height))"
		eval(parse(text=checkbox.call)) 
		}
	
	# Initalize panel
	rp.do(panel=mainpanel, func.panel.action)
	
	# when autosave is activated close panel after initialization
	if(autosave.plot) rp.control.dispose(panel=mainpanel)
}


glm.interactive <- function(model, # object of class glm is mandatory
		## Control initial appearance ##
		initial.values           = as.list(NULL), # Initial values for the metric covariates in a named list, default to the means. See details.
		preselect.var            = NA, # Name of continous variable to be displayed as character or NA for menu selection, default to NA.
		preselect.type           = "link",
		preselect.groups 		 = NULL, # Index of groups to be preselectes. If set to NULL (default) all groups are preselected.
		
		## Parameters for plot layout ##
		dev.height          = 18,	 # Height of graphic device in cm.
		dev.width           = 18,	 # Width of plot area in graphic device in cm.
		dev.width.legend    = 8,	 # Width of legend area in graphic device in cm.
		dev.pointsize		= 10,	 # Character pointsize of graphic device.
		dev.defined         = FALSE, # Graphic device predefined, e. g. customized for printing? Default to FALSE, see details.
		col					= NA,	 # Vector of color specifications for the groups. Passed to the line commands and to the legend. Actual palette and consecutive sequence if NA (default).
		lty					= 1,     # Vector of line type specifications for the groups. Passed to the line commands and to the legend, default to solid lines.
		lwd					= 1,     # Vector of line width specifications for the groups. Passed to the line commands and to the legend, default to 1.
		main				= NA,	 # Label for the plot title.
		xlab				= NA,	 # Label for the x-axis. Name of the selected covariate, if NA.
		ylab				= NA,	 # Label for the y-axis. Name of the selected plot type (see argument label.types), if NA.
		legend.add          = TRUE,  # Should a legend be added to the plot? Default to TRUE, but too many groups can cause problems.
		legend.space        = legend.add, # Should the space for the legend be reserved? Default to the value of legend.add. Setting legend.add to FALSE and legend.space to TRUE plots white space instead of the legend. This can be useful if different termplots will be arranged in a document.  
		legend.only         = FALSE,
		legend.pos			= "center", # Position of the legend, see legend for details.
		legend.cex          = 1,	 # Relative size of legend text, reduce for many groups.
		legend.width.factor = 1, 
		rug.ticksize		= 0.02,  # Length of rugplot tickmarks. Set to 0, if no rugplot should be drawn.
		rug.col				= "black",	 # Color of rugplot tickmarks, default to black.
		vline.actual        = TRUE,  # Add vertical line at actual postion of selceted metric covariate? Default to TRUE.
		pos.hline.link  	= 0,     # Positon of horizontal line for termplot, NA for no line.
		pos.hline.response  = 0.5,   # Positon of horizontal line for response plot, NA for no line.
		pos.hline.marginal  = 0,     # Positon of horizontal line for marginal effects plot, NA for no line.
		n.effects           = 100,   # Number of equally spaced points over the span of the selected metric covariate used for plotting.
		
		## Parameters for plot snapshot
		autosave.plot       = FALSE,    # Save the initial plot?
		snapshot.plot       = FALSE,    # Save plot via savePlot() when snapshot button is pressed? Default to FALSE.
		graphics.filename   = "LinRegIntPlot",  # Filename as character for graphic file.
		graphics.extension  = "pdf",    # Filetype argument passed to savePlot().
		
		## Parameters for tex output ##
		latex2console       = FALSE, # Should the textoutput triggered by the snapshot button be printed as LaTeX?
		decimal.mark        = ".",   # Decimal character for LaTeX output.
		big.mark            = ",",   # Bigmark character for LaTeX output.
		factor.sep          = "-", # character by which the factors are separated in the groupnames
		level.sep           = ".", # character by which the levels are separated in the groupnames
						
		## Annotations for panel 
		panel.title         = "Model for Binary Response" ,                           # Title used in the panel.
		label.button        = "Snapshot" ,                                            # Label for the snapshot-button.
		label.slider.act    = "Variable displayed: " ,                                # Additional label for the slider of the selected metric covariate. 
		label.box.type      = "Type" ,                                                # Title for the radiogroup box.
		label.types         = c("linear predictor", "probability", "marginal effect"),# Lables for radiogroup buttons (character vector of length 3).
		label.box.groups    = "Groups" ,                                              # Title for the checkbox.
		
		## Parameters to control the size of the panel.
		slider.width                = 200 , # Width of each slider.    
		slider.height               = 60  , # Height of each slider.   
		button.height               = 30  , # Height of snapshot button.
		box.type.height             = 90  , # Height of radiobox for type selection.
		box.group.character.width   = 7   , # The width of the boxes is basically a this value times the number of characters.
		box.group.line.height       = 28  , # The height of the checkbox is this value times the number of groups.
		dist.obj.width              = 20  , # Vertical distance between sliders and boxes and vertical margins. 
		dist.obj.height             = 10  , # Horizontal distance between panel objects.
		
		... ) # other graphical parameters passed to par()	
	{
	#################################################################	
	# Pick covariates employed, check variables for factors, assign #
	# corresponding objects and initialize graphic window			#
	#################################################################	
	# pick covariates which are employed in the model
	if(!is.null(model$data))
		{# extract data from model$data
		X <- get_all_vars(model$terms, model$data)[,-1,drop=FALSE]
		}else
		{# try to extract data from model$model
		X <- get_all_vars(model$terms, model$model)[,-1,drop=FALSE]	
		}
	
	# identify factors from model matrix
	logicalindex.factor <- as.vector(NULL)
	num.level           <- as.vector(NULL)
	for(i in 1:dim(X)[2])
	{
		is.factor.act       <- is.factor(X[,i])
		logicalindex.factor <- c(logicalindex.factor, is.factor.act)
		if(is.factor.act)
		{
			num.level.act <- length(levels(X[,i]))
			num.level     <- c(num.level, num.level.act)
		}
	}
	
	# Are factors used as covariates?
	factors.present <- !is.null(num.level)
	
	# separate factors and continous covariates 
	X.continous   <- X[,!logicalindex.factor, drop=FALSE]
	num.continous <- dim(X.continous)[2]
	
	# switch for special treatment of single metric covariates
	single.covariate <- num.continous==1 
		
	if(factors.present)
		{
		# separate factors 
		X.factor    <- X[,logicalindex.factor, drop=FALSE]
		num.groups  <- prod(num.level)
		
		# build all factor combinations
		factor.comb <- factor.combinations(X.factor, factor.sep=factor.sep, level.sep=level.sep, count=FALSE)
		}
	
	# If not specified in the function call: if there is only one continous covariate, choose it. 
	# Otherwise select the continous covariate to be displayed from popup list.
	if(is.na(preselect.var))
		{
		if(single.covariate)
			{
			var.selection <- colnames(X.continous)[1]
			}else
			{	
			var.selection <- select.list(colnames(X.continous))
			}
		}else
		{# in no variable is preselected
		if(single.covariate)
			{# if there is only one metric covariate, chose it
			var.selection <- colnames(X.continous)[1]
			}else
			{# Check if preselected covariate exists, if not, show the menu	
			if(any(colnames(X.continous)==preselect.var))
				{
				var.selection <- preselect.var		
				}else
				{
				var.selection <- select.list(colnames(X.continous))	
				}	
			}
		}
	
	# If no variable is selected from popup list leave function.
	if(var.selection=="") return()
	
	# pick continous covariate under invstigation
	x.actual           <- X.continous[,var.selection]
	x.actual.sequence  <- seq(min(x.actual),max(x.actual), length=n.effects)
	X.pred             <- data.frame(x=x.actual.sequence)
	colnames(X.pred)   <- var.selection
	
	# pick other continous covariates if present
	if(!single.covariate)
	{	
		X.continous.other     <- X.continous[,-match(var.selection, colnames(X.continous)), drop=FALSE]
		nam.X.continous.other <- colnames(X.continous.other)
		num.continous.other   <- length(nam.X.continous.other)
		val.X.continous.other <- colMeans(X.continous.other)
	}
	
	# If not predefined nor switched off via the argument: set up a new graphic device 
	if(!dev.defined)
		{	
		if(legend.only)
			{# Initialize device for legend only
			dev.new(width=dev.width.legend/2.54, height=dev.height/2.54, pointsize=dev.pointsize, buffered=TRUE, noRStudioGD = TRUE)
			par(cex=1, ...)
			}else
			{# If factors are used as covariates and legend should be printed, split plot region via layout()	
			if(factors.present & legend.space)
				{
				dev.new(width=(dev.width + dev.width.legend)/2.54, height=dev.height/2.54, pointsize=dev.pointsize, buffered=TRUE, noRStudioGD = TRUE)
				fraction.plot <- round((dev.width/(dev.width + dev.width.legend))*100, digits=0)
				layoutmatrix  <- matrix(c(rep(2, fraction.plot), rep(1, (100-fraction.plot))),1,100)
				layout(layoutmatrix)
				par(cex=1, ...)
				}else
				{
				dev.new(width=dev.width/2.54, height=dev.height/2.54, pointsize=dev.pointsize, buffered=TRUE, noRStudioGD = TRUE)
				par(cex=1, ...)
			 	}
			}
		}
	
	#############################################	
	#  Action when panel items are manipulated 	#
	#  (except action for snapshot-button)		#	
	#############################################
	func.panel.action <- function(panel) 
		{# Main panel function, must be defined within the namespace where panel is established
		# read plot type from panel
		if(length(panel$type)==0) # in the first call of the panel the slots are sometimes NULL   
			{
			type.actual <- "link"
			}else
			{
			type.actual <- panel$type
			}
			
		# read group selection from panel if groups are present
		if(factors.present)
			{
			if(length(panel$groups)==0) 
				{# in the first call of the panel the slots are sometimes NULL 
				if(is.null(preselect.groups))
					{
					logical.index.groups  <- rep(TRUE, times=num.groups)
					}else
					{
					logical.index.groups  <- rep(FALSE, times=num.groups)
					logical.index.groups[preselect.groups]  <- TRUE
					}
				index.groups.selected <- c(1:num.groups)[logical.index.groups]
				# if all groups are deselected choose the first
				if(sum(logical.index.groups)==0)
					{
					index.groups.selected  <- 1  
					}
				}else
				{
				logical.index.groups  <- panel$groups
				index.groups.selected <- c(1:num.groups)[logical.index.groups]
					
				# if all groups are deselected choose the first
				if(sum(logical.index.groups)==0)
					{
					index.groups.selected  <- 1  
					}
				}
			}# end if factors.present
		
		# read value of actual variable from panel
		if(length(panel$var.selected.act)==0)
		{
			var.selected.act <- 1
		}else
		{
			var.selected.act <- panel$var.selected.act      
		}   
		
		# build designmatrix for predict-method from continous covariates
		if(single.covariate)
		{
			X.pred.metr <- X.pred			
		}else
		{# read actual values from panel and add to designmatrix for predict-method
			for(i in 1:num.continous.other)
			{
				if(length(panel[[nam.X.continous.other[i]]])==0) # initialization fails
				{
					val.X.continous.other[i] <- i
				}else
				{
					val.X.continous.other[i] <- panel[[nam.X.continous.other[i]]]
				}
			}
			X.pred.metr <- cbind(X.pred, t(val.X.continous.other))
		}
				
		# If factors are used as covariates, calculate predictions for each group.
		if(factors.present)
		{
			# calculate predictions for each selected group
			list.groups <- as.list(NULL)
			for(i in seq(along=index.groups.selected))
			{
				index.group.actual <- index.groups.selected[i]  
				
				# read actual factor combination
				factor.group.actual <- factor.comb$comb[index.group.actual,,drop=FALSE]
				rownames(factor.group.actual) <- ""
				
				# combine with continous covariates to new designmatrix
				X.pred <- cbind(X.pred.metr, factor.group.actual)
				
				# calculate linear predictor, probability or marginal effect
				switch(type.actual,
						link = pred.x.actual <- predict(model, newdata=X.pred, type="link"),
						response = pred.x.actual <- predict(model, newdata=X.pred, type="response"),
						marginal = pred.x.actual <- splinefun(x=x.actual.sequence, y=predict(model, newdata=X.pred, type="response"))(x.actual.sequence, deriv=1))
						
				# store results for each and every group 
				sub.list <- list(name=factor.comb$names[index.group.actual],
						prog=pred.x.actual)
				
				list.groups[[i]] <- sub.list 
			}
		}else
		{
			# calculate probability, linear predictor or marginal effect
			switch(type.actual,
					link     = pred.x.actual <- predict(model, newdata=X.pred.metr, type="link"),
					response = pred.x.actual <- predict(model, newdata=X.pred.metr, type="response"),
					marginal = pred.x.actual <- splinefun(x=x.actual.sequence, y=predict(model, newdata=X.pred.metr, type="response"))(x.actual.sequence, deriv=1))
			
			index.groups.selected <- 1
			
			# store results
			list.groups <- as.list(NULL)
			list.groups[[1]] <- list(name="default", prog=pred.x.actual)
		}
		
		# determine plot limits in y-direction
		switch(type.actual,
				link     = limits.y <- c(min(unlist(lapply(list.groups,"[",2))), max(unlist(lapply(list.groups,"[",2)))),
				response = limits.y <- c(0,1) ,
				marginal = limits.y <- c(min(unlist(lapply(list.groups,"[",2))), max(unlist(lapply(list.groups,"[",2))))
		)
		
		### Plotting commands ###
		# draw effects for each and every group
		# when no factors are present set number of groups to 1 for correct color handling
		if(!factors.present) num.groups <- 1
		# specify color scheme
		if(any(is.na(col))) 
		{
			col.types <- c(1:num.groups)
		}else
		{	
			col.types <- rep(col, times=num.groups)
		}
		
		lty.types <- rep(lty, times=num.groups)
		lwd.types <- rep(lwd, times=num.groups)
		
		# in conjunction with dev.flush() and buffered=TRUE animation is more fluent
		dev.hold()
	
		if(factors.present & legend.only)
			{# When legend.only is active plot legend only
			old.mar <- par("mar")
			par(mar=c(old.mar[1],0,old.mar[3],0))
			plot(1,1, type="n", axes=FALSE, xlab=NA, ylab=NA)
			legend.labels <- paste(factor.comb$names[index.groups.selected],"", sep="")
			legend(x=legend.pos, legend=legend.labels, text.width=max(strwidth(legend.labels, cex=legend.cex))*legend.width.factor, col=col.types[index.groups.selected], lty=lty.types[index.groups.selected], lwd=lwd.types[index.groups.selected], cex=legend.cex)
			par(mar=old.mar)    
			}else	
			{# when inactive incorporate other legend parameters
			# when factors are used as covariates and switch is on: add legend first to allow adding elements to the main plot	
			if(factors.present & legend.space)
			{
				old.mar <- par("mar")
				par(mar=c(old.mar[1],0,old.mar[3],0))
				plot(1,1, type="n", axes=FALSE, xlab=NA, ylab=NA)
				if(legend.add)
					{
					legend.labels <- paste(factor.comb$names[index.groups.selected],"", sep="")
					legend(x=legend.pos, legend=legend.labels, text.width=max(strwidth(legend.labels, cex=legend.cex))*legend.width.factor, col=col.types[index.groups.selected], lty=lty.types[index.groups.selected], lwd=lwd.types[index.groups.selected], cex=legend.cex)
					}
				par(mar=old.mar)    
			}
			
			for(i in seq(along=index.groups.selected))
			{
				# initial plot in first loop	
				if(i==1)
				{
					if(is.na(xlab)) xlab <- var.selection
					if(is.na(ylab)) switch(type.actual,
								link     = ylab <- label.types[1],
								response = ylab <- label.types[2],
								marginal = ylab <- label.types[3])
					
					plot(x.actual.sequence, rep(1, times=length(x.actual.sequence)), type="n", ylim=limits.y, xlab=xlab, ylab=ylab, main=main)
					if(!((rug.ticksize==0)||is.na(rug.ticksize))) rug(x.actual,  ticksize = rug.ticksize, col= rug.col) 
				}
				
				index.group.actual <- index.groups.selected[i]  
				
				# if there is only a litte bit variation in the effect draw a horizontal line
				if(sd(list.groups[[i]]$prog)<10^-10)
				{
					abline(h=mean(list.groups[[i]]$prog), col=col.types[index.group.actual], lty=lty.types[index.group.actual], lwd=lwd.types[index.group.actual])	
				}else	
				{ 
					lines(x.actual.sequence, list.groups[[i]]$prog, type="l", col=col.types[index.group.actual], lty=lty.types[index.group.actual], lwd=lwd.types[index.group.actual])
				}    
			}	
			
			# vertical line at actual position
			if(vline.actual) abline(v=(var.selected.act))
			
			# draw horizontal line at specified positions
			switch(type.actual,
					link 	 = abline(h=pos.hline.link),
					response = abline(h=pos.hline.response),
					marginal = abline(h=pos.hline.marginal))
			
			}# end if legend.only
		
		# in conjunction with dev.hold() and buffered=TRUE animation is more fluent
		dev.flush()
			
		# When autosave is activated directly save the plot
		if(autosave.plot)
			{
			# Create unused filename for plot
			for(i in 1:1000)
				{
				graphics.filename.candidate <- paste(graphics.filename,"-", formatC(i, width=3, flag="0"), sep="")
				if(file.exists(paste(graphics.filename.candidate, ".", graphics.extension, sep=""))) next else break
				}
			savePlot(filename = graphics.filename.candidate, type = graphics.extension)	
			}
		
		panel
	} # end body action()
	
	#############################################	
	#    Action when snapshot-button is used  	#
	#############################################
	internal.snapshot <- function(panel)
	{# function must be defined within the namespace where panel is established
		# save plot if snapshot.plot is TRUE
		if(snapshot.plot)      
		{               
			# Create unused filename for plot
			for(i in 1:1000)
				{
				graphics.filename.candidate <- paste(graphics.filename,"-", formatC(i, width=3, flag="0"), sep="")
				if(file.exists(paste(graphics.filename.candidate, ".", graphics.extension, sep=""))) next else break
				}
			savePlot(filename = graphics.filename.candidate, type = graphics.extension)
		}	
		
		# read values from panel and build design matrix for predict-method
		var.selected.act <- panel$var.selected.act
		if(single.covariate)
		{
			X.pred.metr <- data.frame(var.selected.act)
			colnames(X.pred.metr)[1] <- var.selection
			vek.x.continous.actual <- var.selected.act
			names(vek.x.continous.actual) <- var.selection
		}else
		{	
			for(i in 1:num.continous.other)
			{
				val.X.continous.other[i] <- panel[[nam.X.continous.other[i]]]
			}
			
			# build designmatrix from continous covariates
			X.pred.metr <- cbind(var.selected.act, t(val.X.continous.other))
			colnames(X.pred.metr)[1] <- var.selection
			vek.x.continous.actual <- as.vector(X.pred.metr)
			names(vek.x.continous.actual) <- colnames(X.pred.metr)
		}
		
		if(factors.present)
		{
			X.pred <- cbind(X.pred.metr, factor.comb$comb)
		}else
		{
			X.pred <- as.data.frame(X.pred.metr)
		}
		
		pred.link.x.actual      <- predict(model, newdata=X.pred, type="link")
		pred.response.x.actual  <- predict(model, newdata=X.pred, type="response")
		
		output.link.resp            <- cbind(pred.link.x.actual, pred.response.x.actual)
		colnames(output.link.resp)  <- c("link", "response")
		
		if(factors.present)
		{
			rownames(output.link.resp)  <- factor.comb$names
		}else
		{
			rownames(output.link.resp)  <- ""   
		}
		
		# calculate ECDF-value for each continous covariate and  marginal effects in each group
		F.x.continous 	<- NULL
		output.me 		<- NULL
		for(i in 1:num.continous)
		{
			nam.x.marginal.actual       <- colnames(X.continous)[i]
			
			x.marginal.actual           <- X.continous[,nam.x.marginal.actual, drop=TRUE]
			x.marginal.actual.sequence  <- seq(min(x.marginal.actual),max(x.marginal.actual), length=n.effects)
			X.marginal.pred             <- data.frame(x=x.marginal.actual.sequence)
			colnames(X.marginal.pred)   <- nam.x.marginal.actual
			
			if(!single.covariate)
			{	
				nam.X.marginal.other        <- colnames(X.continous)[-i]
				X.marginal.pred             <- cbind(X.marginal.pred, X.pred.metr[,nam.X.marginal.other, drop=FALSE])
				colnames(X.marginal.pred)   <- c(nam.x.marginal.actual, nam.X.marginal.other)   
			}
			
			x.marginal.actual.selected  <- X.pred.metr[,nam.x.marginal.actual, drop=TRUE]
			
			# ECDF-value	
			F.x.continous.actual        <- sum(x.marginal.actual <= x.marginal.actual.selected)/length(x.marginal.actual)
			F.x.continous               <- c(F.x.continous, F.x.continous.actual)
			
			# calculate marginals for actual continous covariate in each group
			marginal.actual.groups <- NULL
			if(factors.present)
			{
				for(i in 1:num.groups)
				{
					# read actual factor combination
					factor.group.actual <- factor.comb$comb[i,,drop=FALSE]
					rownames(factor.group.actual) <- ""
					
					# combine with continous covariates to new designmatrix
					X.marginal.pred.actual <- cbind(X.marginal.pred, factor.group.actual)
					
					marginal.actual.groups.actual   <- splinefun(x=x.marginal.actual.sequence, y=predict(model, newdata=X.marginal.pred.actual, type="response"))(x.marginal.actual.selected, deriv=1)
					marginal.actual.groups          <- c(marginal.actual.groups, marginal.actual.groups.actual)
				}   
			}else
			{								
				marginal.actual.groups  <- splinefun(x=x.marginal.actual.sequence, y=predict(model, newdata=X.marginal.pred, type="response"))(x.marginal.actual.selected, deriv=1)
			}
			
			output.me <- cbind(output.me, marginal.actual.groups)
		}#end for-loop continous covariates
		
		names(F.x.continous) <- colnames(X.continous)
		
		colnames(output.me) <- paste("marg.eff.", colnames(X.continous), sep="")
		
		if(factors.present)
		{
			rownames(output.me) <- factor.comb$names
		}else
		{
			rownames(output.me) <- ""   
		}
		
		if(!latex2console)
			{
			print(summary(model))
			cat(fill=TRUE)
			cat("Actual values of metric covariates", fill=TRUE)
			print(vek.x.continous.actual, fill=TRUE)
			
			cat(fill=TRUE)
			cat("ECDF of actual values of metric covariates", fill=TRUE)
			print(F.x.continous)
			
			cat(fill=TRUE)
			print(output.link.resp)
			
			cat(fill=TRUE)
			print(output.me)
			}else
			{
			cat(fill=TRUE)
			cat("% \\usepackage(booktabs) required in preamble", fill=TRUE)
			cat(fill=TRUE)
			
			# tables for coefficients, resembles table from print(summary(model))
			# for other classes than "lm": is there a slot $coef... 
			if(!is.null(summary(model)$coef))
				{# ... and does it have 4 columns?
				if(dim(summary(model)$coef)[2]==4)
					{
					tab.coef        <- summary(model)$coef
					rownames.plot   <- rownames(tab.coef)
					rownames.plot   <- gsub("^", "\\verb+^+", rownames.plot, fixed=TRUE) # make "^" stable for LaTeX 
					
					coef.est.plot <- format(tab.coef[,1], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
					coef.std.plot <- format(tab.coef[,2], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
					z.value.plot  <- format(tab.coef[,3], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
					p.value.plot  <- format(tab.coef[,4], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
					
					cat("\\begin{table}[ht]", fill=TRUE)
					cat("\\centering" , fill=TRUE)
					cat("\\begin{tabular}{lrrrr} \\toprule", fill=TRUE)
					cat(" &  \\multicolumn{1}{c}{Estimate} & \\multicolumn{1}{c}{Std. Error} & \\multicolumn{1}{c}{z value} & \\multicolumn{1}{c}{Pr(>$|\\mbox{z}|$)} \\\\ \\midrule", fill=TRUE)
					for(i in 1:dim(tab.coef)[1])
					{
						cat(rownames.plot[i], " & ", coef.est.plot[i] , " & ", coef.std.plot[i] , " & ", z.value.plot[i] , " & ", p.value.plot[i] , " \\\\" , sep="", fill=TRUE)
					}
					cat(" \\bottomrule ", sep="", fill=TRUE)
					cat("\\end{tabular}", fill=TRUE)
					cat("\\caption{Model coefficients}", fill=TRUE)
					cat("\\end{table}", fill=TRUE)
					}
				}
			
			# table of current x- and ECDF-values
			vek.x.continous.actual.latex    <- format(vek.x.continous.actual    , trim = TRUE, drop0trailing = TRUE,  big.mark = big.mark, decimal.mark = decimal.mark)
			F.x.continous.latex             <- format(F.x.continous             , trim = TRUE, drop0trailing = TRUE,  big.mark = big.mark, decimal.mark = decimal.mark)
			output.link.resp.latex          <- format(output.link.resp          , trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
			output.link.resp.latex[,2]      <- format(output.link.resp[,2,drop=FALSE], trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
			output.me.latex                 <- format(output.me                 , trim = TRUE, drop0trailing = FALSE, big.mark = big.mark, decimal.mark = decimal.mark)
			
			cat(fill=TRUE)
			cat("\\begin{table}[ht]", fill=TRUE)
			cat("\\centering" , fill=TRUE)
			
			cat("\\begin{tabular}{l")
			for(j in 1:length(vek.x.continous.actual))
			{
				cat("c")
			}
			cat("} \\toprule",fill=TRUE)
			
			cat("\\multicolumn{1}{c}{variable} ")
			for(j in 1:length(vek.x.continous.actual))
			{
				cat(" &  \\multicolumn{1}{c}{",names(vek.x.continous.actual)[j],"} ", sep="")
			}
			cat("\\\\ \\midrule ", sep="", fill=TRUE)
			
			cat("\\multicolumn{1}{c}{value} ")
			
			for(j in 1:length(vek.x.continous.actual))
			{
				cat(" & ",vek.x.continous.actual.latex[j], sep="")
			}
			cat(" \\\\ ", sep="", fill=TRUE)
			
			cat("\\multicolumn{1}{c}{ECDF(value)} ")
			for(j in 1:length(F.x.continous))
			{
				cat(" & ",F.x.continous.latex[j], sep="")
			}
			cat(" \\\\ \\bottomrule ", sep="", fill=TRUE)
			cat("\\end{tabular}", fill=TRUE)
			cat("\\caption{Values of metric covariates where results are calculated}", fill=TRUE)
			cat("\\end{table}", fill=TRUE)
			
			# If factors are used as covariates build corresponding output.
			if(factors.present)
				{
				# table of link and response
				cat(fill=TRUE)
				cat("\\begin{table}[ht]", fill=TRUE)
				cat("\\centering" , fill=TRUE)
				cat("\\begin{tabular}{lcc} \\toprule", fill=TRUE)
				cat("\\multicolumn{1}{c}{groups} &  \\multicolumn{1}{c}{link} & \\multicolumn{1}{c}{response} \\\\ \\midrule", fill=TRUE)
				
				for(i in 1:dim(output.link.resp.latex)[1])
				{
					cat(rownames(output.link.resp.latex)[i], " & ", output.link.resp.latex[i,1]," & ", output.link.resp.latex[i,2], " \\\\" , sep="", fill=TRUE)
				}
				cat(" \\bottomrule ", sep="", fill=TRUE)
				cat("\\end{tabular}", fill=TRUE)
				cat("\\caption{Linear predictor and response in different groups for given values of metric covariates}", fill=TRUE)
				cat("\\end{table}", fill=TRUE)
				
				# table of marginal effects
				cat(fill=TRUE)
				cat("\\begin{table}[ht]", fill=TRUE)
				cat("\\centering" , fill=TRUE)
				
				cat("\\begin{tabular}{l")
				for(j in 1:(length(vek.x.continous.actual)-1))
				{
					cat("r")
				}
				cat("r} \\toprule",fill=TRUE)
				
				for(j in 1:length(vek.x.continous.actual))
				{
					cat(" &  \\multicolumn{1}{c}{",names(vek.x.continous.actual)[j], "}", sep="")
				}
				cat(" \\\\ \\midrule ", sep="", fill=TRUE)
				
				cat("\\multicolumn{1}{c}{value} ")
				for(j in 1:length(vek.x.continous.actual))
				{
					cat(" & ",vek.x.continous.actual.latex[j], sep="")
				}
				cat(" \\\\ ", sep="", fill=TRUE)
				
				cat("\\multicolumn{1}{c}{ECDF(value)} ")
				for(j in 1:length(F.x.continous))
				{
					cat(" & ",F.x.continous.latex[j], sep="")
				}
				cat(" \\\\ \\midrule ", sep="", fill=TRUE)
				
				
				for(i in 1:dim(output.me.latex)[1])
				{
					cat(rownames(output.me.latex)[i], "", sep="")
					for(j in 1:dim(output.me.latex)[2])
					{
						cat(" & ",output.me.latex[i,j], sep="")
					}
					cat(" \\\\ ", sep="", fill=TRUE)
				}
				cat(" \\bottomrule ", sep="", fill=TRUE)
				cat("\\end{tabular}", fill=TRUE)
				cat("\\caption{Marginal effects in different groups for given values of metric covariates}", fill=TRUE)
				cat("\\end{table}", fill=TRUE)				
			}else
			{# latex output if no factors are present
				# table of link and response
				cat(fill=TRUE)
				cat("\\begin{table}[ht]", fill=TRUE)
				cat("\\centering" , fill=TRUE)
				cat("\\begin{tabular}{cc} \\toprule", fill=TRUE)
				cat(" \\multicolumn{1}{c}{link} & \\multicolumn{1}{c}{response} \\\\ \\midrule", fill=TRUE)
				for(i in 1:dim(output.link.resp.latex)[1])
				{
					cat(output.link.resp.latex[i,1]," & ", output.link.resp.latex[i,2], " \\\\" , sep="", fill=TRUE)
				}
				cat(" \\bottomrule ", sep="", fill=TRUE)
				cat("\\end{tabular}", fill=TRUE)
				cat("\\caption{Linear predictor and response for given values of metric covariates}", fill=TRUE)
				cat("\\end{table}", fill=TRUE)
				
				# table of marginal effects
				cat(fill=TRUE)  
				cat("\\begin{table}[ht]", fill=TRUE) 
				cat("\\centering" , fill=TRUE)  
				
				cat("\\begin{tabular}{l")   
				for(j in 1:length(vek.x.continous.actual))  
				{   
					cat("r")    
				}   
				cat("} \\toprule",fill=TRUE)   
				
				for(j in 1:length(vek.x.continous.actual))  
				{   
					cat(" & \\multicolumn{1}{c}{",names(vek.x.continous.actual)[j], "}", sep="")    
				}   
				cat(" \\\\ \\midrule ", sep="", fill=TRUE)  
				
				cat("\\multicolumn{1}{c}{value} ")  
				for(j in 1:length(vek.x.continous.actual))  
				{   
					cat(" & ",vek.x.continous.actual.latex[j], sep="")  
				}   
				cat(" \\\\ ", sep="", fill=TRUE)    
				
				cat("\\multicolumn{1}{c}{ECDF(value)} ")    
				for(j in 1:length(F.x.continous))   
				{   
					cat(" & ",F.x.continous.latex[j], sep="")   
				}   
				cat(" \\\\ \\midrule ", sep="", fill=TRUE)
				
				for(i in 1:dim(output.me.latex)[1])
				{
					cat("\\multicolumn{1}{c}{marginal effect} ")
					for(j in 1:dim(output.me.latex)[2])
					{
						cat(" & ",output.me.latex[i,j], sep="")
					}
					cat(" \\\\ ", sep="", fill=TRUE)
				}
				cat(" \\bottomrule ", sep="", fill=TRUE)
				cat("\\end{tabular}", fill=TRUE)
				cat("\\caption{Marginal effects for given values of metric covariates}", fill=TRUE)
				cat("\\end{table}", fill=TRUE)
			}# end if factors.present
		}# end if latex2console
		panel
	} # end body internal.snapshot
	
	#############################################	
	#  Determine panel dimensions and layout 	#
	#############################################	
	
	# Calculate slider positions and sizes
	y.pos.slider.other <- 2.5*dist.obj.height + slider.height
	
	if(factors.present)
	{
		# Determine maximum length of group name
		length.factorgroup.name <- NULL 
		for(i in 1:num.groups)
		{
			length.factorgroup.name <- c(length.factorgroup.name, length(strsplit(factor.comb$names[i], split=NULL)[[1]]))
		}
		max.length.factorgroup.name <- max(length.factorgroup.name)
	}else
	{#if no groups are present set to fix value
		max.length.factorgroup.name <- 20   
	}
	
	# Calculate button position and size
	button.width        <- max(box.group.character.width*max.length.factorgroup.name, box.group.character.width*18)
	button.pos.x        <- 2*dist.obj.width + slider.width
	button.pos.y        <- dist.obj.height
	
	# Calculate box positions and sizes
	boxes.width         <- button.width
	box.pos.x           <- button.pos.x
	box.type.pos.y      <- button.pos.y + button.height + dist.obj.height
	box.groups.pos.y    <- box.type.pos.y  + box.type.height + dist.obj.height
	
	if(factors.present)
	{
		box.group.height <- box.group.line.height*length(factor.comb$names) + 10
	}else
	{
		box.group.height <- 0   
	}
	
	# Calculate overall panel size
	overall.width   <- box.pos.x + dist.obj.width + boxes.width
	overall.height  <- max((4*dist.obj.height + box.type.height + box.group.height + button.height), ((num.continous-1)*slider.height + y.pos.slider.other + dist.obj.height))
	
	#########################################################	
	#  Define main panel and populate it with GUI controls 	#
	#########################################################
	mainpanel <- rp.control(title=panel.title, size = c(overall.width,overall.height))
		
	# Slider for displayed continous variable
	initial.act <- initial.values[[var.selection]]
	if(is.null(initial.act)) initial.act <- mean(x.actual) 
	
	eval(wrap.rp.slider(panel="mainpanel", variable="var.selected.act", title=paste(label.slider.act, var.selection, sep=""), 
					from = min(x.actual), to=max(x.actual), initval=initial.act,
					pos= c(dist.obj.width, dist.obj.height, slider.width, slider.height) , showvalue =TRUE, action = "func.panel.action"))
	
	# Sliders for other continous variables (if present)
	if(!single.covariate)
	{
		for(i in 1:num.continous.other)
		{
			y.pos.slider <- y.pos.slider.other + (i-1)*slider.height
			
			initial.act <- initial.values[[nam.X.continous.other[i]]]
			if(is.null(initial.act)) initial.act <- mean(X.continous.other[,i])
			
			slidercall <- wrap.rp.slider(panel="mainpanel", variable=nam.X.continous.other[i],title=nam.X.continous.other[i], 
					from = min(X.continous.other[,i]), to=max(X.continous.other[,i]), initval=initial.act,
					pos= c(dist.obj.width, y.pos.slider, slider.width, slider.height) , showvalue =TRUE, action = "func.panel.action") 
			eval(slidercall)
		}
	}
	
	# Button for snapshot
	rp.button(panel=mainpanel, action = internal.snapshot, title = label.button , pos = c(button.pos.x, button.pos.y, button.width, button.height))
	
	# Radiogroup for type
	# if type is preselected check for allowed value
	if(any(preselect.type==c("link","response","marginal")))
		{# if allowed use it
		type.initval <- preselect.type[1]
		}else
		{# if not allowed set as link
		type.initval <- "link"	
		}

	# Cheat R CMD check	
	radiogroup.call <- "rp.radiogroup(panel=mainpanel, variable=type, vals=c('link', 'response', 'marginal'), initval=type.initval, labels=label.types,
                        title = label.box.type, action = func.panel.action, pos = c(box.pos.x, box.type.pos.y, boxes.width, box.type.height))"
	eval(parse(text=radiogroup.call))
		
	# Checkbox for groups, if factors are employed in the model
	if(factors.present)
	    {
		if(is.null(preselect.groups))
			{
			init.logical.index.groups  <- rep(TRUE, times=num.groups)
			}else
			{
			init.logical.index.groups  <- rep(FALSE, times=num.groups)
			init.logical.index.groups[preselect.groups]  <- TRUE
			}	
			
		# Cheat R CMD check	
		checkbox.call <- "rp.checkbox(panel=mainpanel, variable=groups, action = func.panel.action, initval=init.logical.index.groups,
                          labels = factor.comb$names, title = label.box.groups, pos = c(box.pos.x, box.groups.pos.y, boxes.width, box.group.height))"
		eval(parse(text=checkbox.call)) 
	    }		
		
	# Initalize panel
	rp.do(panel=mainpanel, func.panel.action)
	
	# when autosave is activated close panel after initialization
	if(autosave.plot) rp.control.dispose(panel=mainpanel)
} 


wrap.rp.slider <- function(panel, variable, from, to, action, title=NA, 
		log = FALSE, showvalue = FALSE, resolution = 0, initval = NULL, 
		pos = NULL, horizontal = TRUE)
	{
	if(is.na(title))
	{
		title.call <- paste("'",variable,"'", sep="")
	}else
	{
		title.call <- paste("'",title,"'", sep="")
	}
	
	slider.call <- paste("rp.slider(panel=", panel,", variable=",variable, ", from=",from,", to=",to,", action=", action,", title=",title.call,", pos=",deparse(pos),
			", log =",deparse(log),", showvalue = ",deparse(showvalue),", resolution = ",resolution,", initval = ",deparse(initval),
			", horizontal = ",deparse(horizontal),")", sep="")
	
	return(parse(text=slider.call)) 
	}


factor.combinations <- function(X, factor.sep="-", level.sep=".", count=TRUE)
	{
	logicalindex.factors <- sapply(X, is.factor)
	if(!any(logicalindex.factors)) stop("no factors in data.frame")
	
	X.factor <- X[,logicalindex.factors, drop=FALSE]
	if(dim(X.factor)[2]==1)
		{# just a single factor in the covariates, groups are the factor levels 
		factorgroup <- data.frame(levels(X.factor[,1]))
		colnames(factorgroup) <- colnames(X.factor)
		
		groups.names <- paste(colnames(factorgroup)[1], factorgroup[,1], sep=level.sep)
		
		# count occurences of levels
		groups.counts <- as.vector(table(X.factor))
		
		output <- list(comb=factorgroup, names=groups.names, counts=groups.counts)
		return(output)
		}
	
	factor.levels 	  <- lapply(X.factor, levels)
	num.factor.levels <- sapply(factor.levels, length)
	num.factors       <- dim(X.factor)[2]
	
	# first factor
	fac.act <- rep(factor.levels[[1]], each=prod(tail(num.factor.levels,-1)))
	factorgroups <- data.frame(fac.act)
	
	for(j in c(2:num.factors))
		{
		if(j==num.factors)
			{# for the last factor
			num.rep <- 1	
			}else
			{	
			num.rep      <- prod(tail(num.factor.levels,-j))
			}
		
		fac.act      <- rep(factor.levels[[j]], each=num.rep)
		factorgroups <- cbind(factorgroups, fac.act)
		}
	
	colnames(factorgroups) <- colnames(X.factor)
	
	# check for orderd factors and if so assign order to corresponding column
	for(j in 1:dim(X.factor)[2])
		{
		if(is.ordered(X.factor[,j]))
			{
			factorgroups[,j] <- factor(factorgroups[,j], levels=levels(X.factor[,j]), ordered = TRUE)
			}
		}
	
	# build group names from factor combinations
	groups.names <- as.vector(NULL)
	for(i in 1:dim(factorgroups)[1])
		{
		groups.name <- as.vector(NULL)
		for(j in 1:dim(factorgroups)[2])
			{
			groups.name.actual <- paste(colnames(factorgroups)[j],factorgroups[i,j],sep=level.sep)
			groups.name <- paste(groups.name, groups.name.actual, sep=factor.sep)
			}
		groups.name <- substring(groups.name,2)
		groups.names <- c(groups.names,groups.name)
		}
	
	# count occurences of groups
	if(count)
		{	
		num.groups    <- prod(num.factor.levels)
		groups.counts <- as.vector(NULL)
		for(i in 1:num.groups)
			{
			vec.occur <- rep(1, times=dim(X.factor)[1])
			for(j in 1:num.factors)
				{
				dummyvec.act <- (X.factor[,j]==factorgroups[i,j])*1 
				vec.occur    <- vec.occur*dummyvec.act
				}
			groups.counts.act <- sum(vec.occur)
			groups.counts     <- c(groups.counts, groups.counts.act)				
			}
		}else
		{
		groups.counts <- NULL
		}
	
	output <- list(comb=factorgroups, names=groups.names, counts=groups.counts)
	return(output)
	}