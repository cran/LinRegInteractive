### R code from vignette source 'LinRegInteractive.Rnw'

###################################################
### code chunk number 1: options
###################################################
options(prompt = " ", continue = "     ", digits = 4, show.signif.stars = FALSE)


###################################################
### code chunk number 2: LinRegInteractive.Rnw:95-98 (eval = FALSE)
###################################################
## data("creditdata")
## model.2.fac <- glm(credit ~ amount + I(amount^2)  + age + duration*teleph  
## + housing, family = binomial(link="probit"), data = creditdata)


###################################################
### code chunk number 3: LinRegInteractive.Rnw:105-106 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac) 


###################################################
### code chunk number 4: LinRegInteractive.Rnw:142-143 (eval = FALSE)
###################################################
## demo(VignetteFigures, package = "LinRegInteractive", ask = FALSE)


###################################################
### code chunk number 5: LinRegInteractive.Rnw:162-166 (eval = FALSE)
###################################################
## require("splines")
## model.2.fac.npamount <- glm(credit ~ bs(amount) + age + duration*teleph  
## + housing, family = binomial(link="probit"), data = creditdata)
## glm.interactive(model.2.fac.npamount) 


###################################################
### code chunk number 6: LinRegInteractive.Rnw:199-204 (eval = FALSE)
###################################################
## require("AER")
## data("MurderRates")
## model <- glm(I(executions > 0) ~ time + income + noncauc + lfp + southern, 
## data = MurderRates, family = binomial)
## glm.interactive(model)


###################################################
### code chunk number 7: LinRegInteractive.Rnw:299-301 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac,
## initial.values = list(amount=5000, duration=24, age=30))


###################################################
### code chunk number 8: LinRegInteractive.Rnw:310-311 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, preselect.var = "duration")


###################################################
### code chunk number 9: LinRegInteractive.Rnw:318-319 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, preselect.type = "response")


###################################################
### code chunk number 10: LinRegInteractive.Rnw:327-328 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, preselect.groups = c(1:3))


###################################################
### code chunk number 11: LinRegInteractive.Rnw:338-339 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, legend.width.factor = 1.1)


###################################################
### code chunk number 12: LinRegInteractive.Rnw:346-350 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, 
## preselect.var     = "duration",
## snapshot.plot     = TRUE,
## graphics.filename = "D:/Temp/fig-credprobit-duration")


###################################################
### code chunk number 13: LinRegInteractive.Rnw:357-365 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac,
## initial.values      = list(amount=5000, duration=24, age=30), 
## preselect.var       = "duration",
## preselect.type      = "marginal",
## preselect.groups    = c(2,3,5,6),
## autosave.plot       = TRUE,
## graphics.filename   = "fig-credprobit-duration-marg",
## legend.width.factor = 1.05)


###################################################
### code chunk number 14: LinRegInteractive.Rnw:374-377 (eval = FALSE)
###################################################
## data("creditdata")
## model.2.fac <- glm(credit ~ amount + I(amount^2)  + age + duration*teleph
## + housing, family = binomial(link="probit"), data = creditdata)


###################################################
### code chunk number 15: LinRegInteractive.Rnw:382-385 (eval = FALSE)
###################################################
## data("creditdata")
## model.3.fac <- glm(credit ~ amount + I(amount^2)  + age + duration*teleph 
## + housing + job, family = binomial(link="probit"), data = creditdata)


###################################################
### code chunk number 16: LinRegInteractive.Rnw:394-397 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, 
## factor.sep = "|",
## level.sep  = ">")


###################################################
### code chunk number 17: LinRegInteractive.Rnw:404-408 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac,
## latex2console = TRUE,
## decimal.mark  = ",",
## big.mark      = ".")


###################################################
### code chunk number 18: LinRegInteractive.Rnw:435-440 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, 
## dev.height       = 11,
## dev.width        = 11,
## dev.width.legend = 5,
## dev.pointsize    = 8) 


###################################################
### code chunk number 19: LinRegInteractive.Rnw:449-452 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, 
## lwd = rep(c(1,2),each=3),
## col = c(1,"blue",2))


###################################################
### code chunk number 20: LinRegInteractive.Rnw:458-463 (eval = FALSE)
###################################################
## glm.interactive(model.3.fac,  
## lwd = rep(c(1,2), each=12),
## col = rep(c(1,"blue",2), each=4),  
## lty = c(1,2,3,4),
## dev.width.legend = 8)


###################################################
### code chunk number 21: LinRegInteractive.Rnw:471-477 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, 
## preselect.var  = "duration",
## preselect.type = "response",
## main           = "Interaction between 'duration' and factor 'teleph'",
## xlab           = "duration (months)",
## ylab           = "probability of credit default")


###################################################
### code chunk number 22: LinRegInteractive.Rnw:484-485 (eval = FALSE)
###################################################
## glm.interactive(model.3.fac, dev.width.legend = 8)


###################################################
### code chunk number 23: LinRegInteractive.Rnw:490-491 (eval = FALSE)
###################################################
## glm.interactive(model.3.fac, legend.cex = 0.7)


###################################################
### code chunk number 24: LinRegInteractive.Rnw:496-497 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, legend.pos = "top")


###################################################
### code chunk number 25: LinRegInteractive.Rnw:504-505 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, legend.add = FALSE)


###################################################
### code chunk number 26: LinRegInteractive.Rnw:510-513 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac,
## legend.add   = FALSE,
## legend.space = TRUE)


###################################################
### code chunk number 27: LinRegInteractive.Rnw:519-520 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, legend.only = TRUE)


###################################################
### code chunk number 28: LinRegInteractive.Rnw:528-529 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, rug.ticksize = NA)


###################################################
### code chunk number 29: LinRegInteractive.Rnw:534-535 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, rug.col = "gray50")


###################################################
### code chunk number 30: LinRegInteractive.Rnw:544-545 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, vline.actual = FALSE)


###################################################
### code chunk number 31: LinRegInteractive.Rnw:551-555 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac,        
## pos.hline.link     = NA,   
## pos.hline.response = NA,   
## pos.hline.marginal = NA)   


###################################################
### code chunk number 32: LinRegInteractive.Rnw:560-561 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, pos.hline.response = 0.56)


###################################################
### code chunk number 33: LinRegInteractive.Rnw:573-585 (eval = FALSE)
###################################################
## windows(10,7, pointsize = 10)
## layoutmatrix <- matrix(c(1,2,2), 1, 3)
## layout(layoutmatrix)
## palette(c("darkred","red","salmon","darkblue","blue","lightblue"))
## par(cex = 1, mar = c(5,5,2,2)+0.1)
## 
## glm.interactive(model.2.fac,
## preselect.var       = "amount",
## preselect.type      = "response",
## dev.defined         = TRUE,
## legend.width.factor = 1.1,
## snapshot.plot       = TRUE)


###################################################
### code chunk number 34: LinRegInteractive.Rnw:593-598 (eval = FALSE)
###################################################
## glm.interactive(model.3.fac,
## box.type.height           = 90, 
## box.group.character.width = 6, 
## box.group.line.height     = 25, 
## dist.obj.height           = 2)


###################################################
### code chunk number 35: LinRegInteractive.Rnw:606-614 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac,
## panel.title      = "Probit Modell",
## label.button     = "Schnappschuss",
## label.slider.act = "Dargestellte Variable: ",
## label.box.type   = "Typ",
## label.types      = c("Linearer Praediktor", "Wahrscheinlichkeit",
##                      "Marginaler Effekt"),
## label.box.groups = "Gruppen")


###################################################
### code chunk number 36: LinRegInteractive.Rnw:634-639 (eval = FALSE)
###################################################
## data("creditdata")
## require("gam")
## model.gam <- gam(credit ~ s(amount) + lo(age) + duration*teleph + housing, 
##     family = binomial, data = creditdata)
## glm.interactive(model.gam) 


###################################################
### code chunk number 37: LinRegInteractive.Rnw:645-650 (eval = FALSE)
###################################################
## data("creditdata")
## require("mgcv") 
## model.mgcv <- gam(credit ~ s(amount) + s(age) + duration*teleph + housing,  
## family = binomial, data = creditdata)
## glm.interactive(model.mgcv)


###################################################
### code chunk number 38: LinRegInteractive.Rnw:658-664 (eval = FALSE)
###################################################
## data("munichrent03")
## require("splines")
## model.rent <- lm(rent ~ bs(yearc) + area*location + upkitchen,
## data=munichrent03)
## model.rent$data <- munichrent03
## lm.interactive(model.rent)


###################################################
### code chunk number 39: LinRegInteractive.Rnw:831-842 (eval = FALSE)
###################################################
## model.cd.manygroups <- glm(credit ~ amount + I(amount^2) + age 
## + duration*teleph + housing + intuse, family=binomial, data=creditdata)
## 
## factor.combs       <- factor.combinations(creditdata[,c("teleph",
## "housing","intuse")])
## logic.index.groups <- factor.combs$counts > 10
## index.groups       <- seq(along=factor.combs$counts)[logic.index.groups]
## 
## glm.interactive(model.cd.manygroups,
## preselect.var    = "amount",
## preselect.groups = index.groups)


###################################################
### code chunk number 40: LinRegInteractive.Rnw:847-854 (eval = FALSE)
###################################################
## glm.interactive(model.2.fac, 
## preselect.var  = "amount",
## preselect.type = "response",
## rug.ticksize   = 0)
## segments(creditdata$amount, par("usr")[3], creditdata$amount, 
## par("fig")[3], col = rgb(0,0,0,0.2))
## savePlot("credidefault-customrug", "pdf") 


