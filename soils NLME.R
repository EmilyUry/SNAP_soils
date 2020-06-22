#' ---
#' title: "soils Linear Mixed Effects model with NLME"
#' author: "Emily Ury"
#' date: "May 12, 2020"
#' output: github_document
#' ---
#'
#' See the tutorial"
#' https://crumplab.github.io/psyc7709/book/docs/a-tutorial-for-using-the-lme-function-from-the-nlme-package-.html
#' 
#' 

library(nlme)



setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")
x <- read.csv("2019_SNAP_master.csv", header = TRUE)
names(x) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")
x <- x[,-c(23, 24, 29)] ## remove ICNO3, ICPO4 and Br

lX<-log(x[,c(14:19, 22, 28, 29)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

x$ID<-paste(x$Site,x$Treatment,x$Core,sep="")

## for now, lets ignore the nutrient and sn treatments

x <- x[which(x$Treatment == "Salt" | x$Treatment == "Control"), ]
x$Treatment <- as.factor(x$Treatment)

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium

par(mfrow = c(1,1), mar = c(4,4,3,3))
boxplot(DOC ~ Treatment*Site, data = x)
plot(x$logSO4, x$DOC, pch = c(21,22)[x$Treatment], col = col3[x$Site], bg = ifelse(x$Depth == "(5-10)", "white", col3[x$Site]))


## and also ignore the shallow depth
x <- x[which(x$Depth == "(0-5)"),]


##nlme package can't deal with missing values (this is one way it differs from lme4),
## so we must run an na.omit

#x <- na.omit(x)
x <- x[-22,]


x$Response <- x$DOC
x$Response <- x$LOI
x$Response <- x$Phenol
x$Response <- x$Cmin_s
x$Response <- x$SIR_s




null1 <- nlme::lme(fixed = Response ~ 1,   #defines the fixed effect, in this case none only the intercept
    data = x,
    random = ~ 1 | Site) 

model1 <- nlme::lme(fixed = Response ~ Treatment,   #defines the fixed effect, in this case none only the intercept
              data = x,
              random = ~ 1 | Site) 
summary(null1)

anova(null1, model1)

nupp1 <- lme4::lmer(Response ~  1 + (1|Site), data = x, REML = F)

null1
nupp1
