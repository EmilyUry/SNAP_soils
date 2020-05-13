

#' ---
#' title: "Exploratory Data Analysis"
#' author: "Emily Ury"
#' date: "May 12, 2020"
#' output: github_document
#' ---
#'


#' Exploratory data analysis (EDA) as suggested by the Stats Consulting Group at
#' Duke. The goal of these stats is to determine what controls soil respiration
#' including both site variation across the plots and treatment effects
#' of salt and nutrient additions. 
#'
#' The goal of EDA is to assess the data set without making any assumptions.
#' 
#' 1. PCA
#' 2. Heirarchical clustering
#' 

#' General notes:
#' 
#' Log transformations:
#' The variables Cl, SO4 and Na are non-negative, highly right-skewed and span
#' several orders of magnitude, so they may be log transformed (particularly if 
#' their relationship with the response appears non-linear and convex).
#' If the scatter plots with these variables and response looks conical, consider 
#' log transforming the response as well.
#' 
#' Use the top PCs in place of the individual variables. 
#'  
#' Heirarchical clustering can be used to identify patters of covariation by
#' creating a dendrogram. If correlation structure varies by site or treatment, 
#' perform clustering within different levels to look for different patterns.
#' Bi-clustered heat map depiction indicates what the key categorical variables
#' are (or unmeasured variables) --> the design factor! Reveals patterns of 
#' missingness, repeated samples, outliers, data quality issues. 
#' 
#' 


library(BAS)
library(nlme)

setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")
x <- read.csv("2019_SNAP_master.csv", header = TRUE)
names(x) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond", "BD", 
               "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
               "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
               "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")
x <- x[,-c(23, 24, 29)] ## remove ICNO3, ICPO4 and Br
x0<-x[x$Depth=="(0-5)",]
summary(x0[,6:29])
par(mfrow=c(4,3), mar = c(2,2,2,1))
for (i in 7:29) hist(x0[,i],main=colnames(x)[i],las=1)


par(mfrow=c(3,2))
for (i in 14:19){
hist(log(x0[,i]),
     main=paste("Log of ",colnames(x)[i],sep=""),
     las=1)}

lX<-log(x[,c(11,14:19, 22, 28, 29)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

x0<-x[x$Depth=="(0-5)",]



phys<-c("SM","LOI","BD","logRoots", "pH")
chem<-c("logCl","logSO4","logNa","logK","logMg","Ca", "logNH4", "logNO3", "logPO4")
## set color list
p.cols<-rep("#d6604d",nrow(x0)) ## Dry site = red
p.cols[x0$Site==3]<-"#9970AB" ## Intermediate site
p.cols[x0$Site==5]<-"#4393C3" ## Wet site
pairs(x0[,phys],pch=16,col=p.cols)
pairs(x0[,chem],pch=16,col=p.cols)

table(x0$Treatment)
x0$Plot2<-rep("C",nrow(x0))
x0$Plot2[x0$Treatment=="Nutrients"]<-"N"
x0$Plot2[x0$Treatment=="Salt"]<-"S"
x0$Plot2[x0$Treatment=="SN"]<-"B" ## B="Both"
x0$ID<-paste(x0$Site,x0$Plot2,x0$Core,sep="")
rownames(x0)<-x0$ID
table(x0$Treatment,x0$Plot2)


col4 <- c("#FDE725FF", "#55C667FF", "#33638DFF", "#481567FF") ## viridis
x0$Plot2 <- as.factor(x0$Plot2)
#pairs(x0[,chem],pch=16,col=col4[x0$Plot2])



## X, Y pair plots
chembio<-c(chem,phys)
par(mfrow=c(3,2), mar = c(4,4,2,2))
for (i in 1:length(chembio)){
plot(x0[,chembio[i]], x0[,24], xlab=chembio[i],
     ylab="Respiration",las=1,col=p.cols,pch=16)
  plot(x0[,chembio[i]], log(x0[,24]), xlab=chembio[i],
       ylab="Log Respiration",las=1,col=p.cols,pch=16)
}









col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium
col4 <- c("#FDE725FF", "#55C667FF", "#33638DFF", "#481567FF") ## viridis
col4a <- c("#FDE72599", "#55C66799", "#33638D99", "#48156799") ## viridis transparent

