

#' ---
#' title: "Generalized Least Squares Regression"
#' author: "Emily Ury"
#' date: "May 13, 2020"
#' output: github_document
#' ---
#'
#'
#' **Notes:** Mutliple linear regression (MLR) and generalizations of it to adjust
#' for confounding effects
#' 
#' See http://homepage.ntu.edu.tw/∼ckuan/pdf/et01/et_Ch4.pdf.
#' 
#' Two approaches:
#'  1. Site/Treatment model
#'  2. Chloride model
#' 
#' ### Model Assumptions
#'  MLR models assume error terms are independent and have constant variance (homoskedasticity)
#'  Violations: model error terms are correlated within plots because cores are
#'  collected close together.
#'  Evidence of variation in the model's residuals increasing with increasing moisture. 
#'  
#' ### Generalization of the Multiple Regression Model
#'  GLS is an extension of the basic linear model which is designed to allow
#'  for heteroskedastic and correlated within-group erros.
#'  The `gls` function from the `nlme` package in R can be used. 
#'  Errors are allowed to be correlated and/or have unequal variance. 
#'  
#'  Here is an example of a nested progression of GLS models, the first assuming
#'  homoskedastic errors, the second assuming an error variance that depends on the
#'  variable **soil moisture** (`SM`) and the third adding correlated within-plot
#'  errors to the seconf model. 
#'  An `ANOVA` test is used to assess the added explanatory value of each model over the 
#'  previous one. Based on this analysis we find strong evidence for including an
#'  error variance that depends on Moisture and evidence for the addition of correlated 
#'  within group errors. 
#'  
#'  We thank Edwin Iversen for help with the above notes and the following code. 
#'  
#'  
#' ## IID Error Model
#'  

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
x0$Plot2<-rep("C",nrow(x0))
x0$Plot2[x0$Treatment=="Nutrients"]<-"N"
x0$Plot2[x0$Treatment=="Salt"]<-"S"
x0$Plot2[x0$Treatment=="SN"]<-"B" ## B="Both"
x0$ID<-paste(x0$Site,x0$Plot2,x0$Core,sep="")
rownames(x0)<-x0$ID   ### create unique rownames for each core like, "1S1" etc.
table(x0$Treatment,x0$Plot2)

phys<-c("SM","LOI","BD","logRoots", "pH")
chem<-c("logCl","logSO4","logNa","logK","logMg","Ca", "logNH4", "logNO3", "logPO4")


x0 <- x0[, c("Cmin_c", "Site", "Plot2", "Depth", "Core", phys, chem)]
x1 <- x0
x1["5S2",1] <- 5.11
x1$Site2<-factor(paste("S",x1$Site,sep=""))
table(x1$Site2)

x1$SbyP<-factor(paste(x1$Site2,x1$Plot2,sep=""))
table(x1$SbyP)


#' ## Nested progression of Variance Models
#' ### First: assumes homoskedastic errors
#' 
gls.out0<-gls(Cmin_c~Site2+Plot2+SM+BD+logCl+logK,
              data=x1)
summary(gls.out0)
plot(gls.out0)


#' ## Variance Model 2
#' ### Assumes soil moisture `SM` affects variance

gls.out1<-gls(Cmin_c~Site2+Plot2+SM+BD+logCl+logK,
              weights=varConstPower(form=~SM),
              data=x1)
summary(gls.out1)
plot(gls.out1)


#' ## Variance/Covariance Model
#' ### Adds correlated within plot errors to model 2

gls.out2<-gls(Cmin_c~Site2+Plot2+SM+BD+logCl+logK,
              weights=varConstPower(form=~SM),
              correlation=corAR1(0.25,form=~Core|SbyP),
              data=x1)
summary(gls.out2)
plot(gls.out2)

#' ## ANOVA

anova(gls.out0,gls.out1,gls.out2)




