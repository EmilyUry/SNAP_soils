


#' ---
#' title: "Baysian Model Averaging"
#' author: "Emily Ury"
#' date: "May 13, 2020"
#' output: github_document
#' ---
#'

#' **Notes:** this is the recommended approach for model selection
#' to identify which variables to include in the final regression
#' model. This approach fits and summarizes all possible multiple
#' regression models and reports the importance of each variable. 
#' 
#' Use the `bas.lm` function in the BAS package. 
#' 
#' Prior inclusion of 1 is set to all design variables (Site, Treatment)
#' so they will be included in all models (**include.always**).
#' 
#' Summary output provides top five models. USe this to decided what
#' to include in the final model (high marginal posterior inclusion
#' probabilities). The `image()` function provides a graphical sumamry. 
#' 
#' *See Ref for more info: https://cran.r-project.org/web/packages/BAS/vignettes/BAS-vignette.html* 
#' 


library(BAS)

setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")
x <- read.csv("2019_SNAP_master.csv", header = TRUE)
names(x) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")
x <- x[,-c(23, 24, 29)] ## remove ICNO3, ICPO4 and Br
x$PO4[is.na(x$PO4)] <- 1   ### set the below detection
x$Mg[is.na(x$Mg)] <- 0.001  ## set the below detection
x$Roots[x$Roots == 0] <- 0.01 
lX<-log(x[,c(11,14:19, 22, 28, 29)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)
x0<-x[x$Depth=="(0-5)",]

phys<-c("SM","LOI","BD","logRoots", "pH")
chem<-c("logCl","logSO4","logNa","logK","logMg","Ca", "logNH4", "logNO3", "logPO4")


x.bas <- x[, c("Cmin_c", "Site", "Treatment", "Depth", phys, chem)]
x.bas$Site<-factor(x.bas$Site)
x.bas$Treatment<-factor(x.bas$Treatment)
x.bas$Depth<-factor(x.bas$Depth)

bas.out<-bas.lm(Cmin_c ~ . ,data = x.bas,
                modelprior = uniform(),
                prior="JZS", pivot=TRUE,
                include.always = (Cmin_c ~ Site + Treatment + Depth))
summary(bas.out)

par(mfrow = c(1,1))
image(bas.out,rotate=FALSE,las=1)


par(mfrow=c(2,2),las=1)
plot(bas.out,ask=FALSE)


#' ## Measured Treatment Effect, depth 0-5 only

x0.bas <- x0[, c("Cmin_c", "Site", "Treatment", phys, chem)]
x0.bas$Site<-factor(x0.bas$Site)
x0.bas$Treatment<-factor(x0.bas$Treatment)

bas.out<-bas.lm(Cmin_c ~ . ,data = x0.bas,
                modelprior = uniform(),
                prior="JZS", pivot=TRUE,
                include.always = (Cmin_c ~ Site + Treatment))
summary(bas.out)
par(mfrow = c(1,1))
image(bas.out,rotate=FALSE,las=1)
par(mfrow=c(2,2),las=1)
plot(bas.out,ask=FALSE)


#' ## Measured Treatment Effect, depth 0-5 only, 
#' ### Site 1 only

x0.bas <- x0[, c("Cmin_c", "Treatment", phys, chem)]
x0.bas$Treatment<-factor(x0.bas$Treatment)

bas.out<-bas.lm(Cmin_c ~ . ,data = x0.bas,
                modelprior = uniform(),
                prior="JZS", pivot=TRUE,
                subset=(x0$Site==1),
                include.always = (Cmin_c ~ Treatment))
summary(bas.out)
par(mfrow = c(1,1))
image(bas.out,rotate=FALSE,las=1)
par(mfrow=c(2,2),las=1)
plot(bas.out,ask=FALSE)

#' ## Measured Treatment Effect, depth 0-5 only, 
#' ### Site 3 only

x0.bas <- x0[, c("Cmin_c", "Treatment", phys, chem)]
x0.bas$Treatment<-factor(x0.bas$Treatment)

bas.out<-bas.lm(Cmin_c ~ . ,data = x0.bas,
                modelprior = uniform(),
                prior="JZS", pivot=TRUE,
                subset=(x0$Site==3),
                include.always = (Cmin_c ~ Treatment))
summary(bas.out)
par(mfrow = c(1,1))
image(bas.out,rotate=FALSE,las=1)
par(mfrow=c(2,2),las=1)
plot(bas.out,ask=FALSE)


#' ## Measured Treatment Effect, depth 0-5 only, 
#' ### Site 5 only

x0.bas <- x0[, c("Cmin_c", "Treatment", phys, chem)]
x0.bas$Treatment<-factor(x0.bas$Treatment)

bas.out<-bas.lm(Cmin_c ~ . ,data = x0.bas,
                modelprior = uniform(),
                prior="JZS", pivot=TRUE,
                subset=(x0$Site==5),
                include.always = (Cmin_c ~ Treatment))
summary(bas.out)
par(mfrow = c(1,1))
image(bas.out,rotate=FALSE,las=1)
par(mfrow=c(2,2),las=1)
plot(bas.out,ask=FALSE)


#' ###Proxy Effect

x.basPE<-x.bas[,!(colnames(x.bas) %in% c("logNa","logCl"))]
bas.PE<-bas.lm(Cmin_c ~ . ,data = x.basPE,
               modelprior = uniform(),
               prior="JZS", pivot=TRUE,
               include.always = (Cmin_c ~ Treatment + Site))
summary(bas.PE)
plot(confint(coef(bas.PE),parm=4:6))

bas.PE2<-bas.lm(Cmin_c ~ . + Site:Treatment + Site:SM,data = x.basPE,
                modelprior = uniform(),
                prior="JZS", pivot=TRUE,
                include.always = (~ Site*Treatment))
summary(bas.PE2)


par(mfrow=c(2,2),las=1)
plot(bas.PE2,ask=FALSE)


plot(confint(coef(bas.PE2),parm=c(4,6,5,15,19,17,16,20,18))) ### ?? not sure about this

p.PE2<-predict(bas.PE2,estimator="MPM")
variable.names(p.PE2)
# # something in this neighborhood goes fatally wrong
# keep<-(!is.na(x.basPE$BD))
# r.PE2<-(x.basPE$Cmin_c[keep] -p.PE2$fit)
# par(mfrow=c(2,2))
# plot(x.basPE$Moisture[keep],r.PE2,las=1, ylim = c(-100,100), xlim = c(0,100))
# plot(x.basPE$Carbon[keep],r.PE2,las=1)
# plot(x.basPE$Ca[keep],r.PE2,las=1)
# boxplot(r.PE2~x.basPE$Site[keep],las=1)


### there is a bit more in the tutorial but I don't understad it...