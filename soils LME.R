#' ---
#' title: "soils Linear Mixed Effects model"
#' author: "Emily Ury"
#' date: "May 12, 2020"
#' output: github_document
#' ---
#'
#' See the tutorial"
#' https://www.dropbox.com/work/My%20data/Stats%20resources?preview=bw_lme_tutorial_part2.pdf
#' 
#' 

library(lme4)

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

x$Site <- as.factor(x$Site)


col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium

par(mfrow = c(1,1), mar = c(4,4,3,3))
boxplot(DOC ~ Treatment*Site, data = x)
plot(x$logSO4, x$DOC, pch = c(21,22)[x$Treatment], col = col3[x$Site], bg = ifelse(x$Depth == "(5-10)", "white", col3[x$Site]))

x <- x[which(x$Depth == "(0-5)"),]


x$Response <- x$DOC
x$Response <- x$LOI
x$Response <- x$Phenol
x$Response <- x$Cmin_s
x$Response <- x$SIR_s


boxplot(Response ~ Treatment*Site, data = x)
plot(x$Cl, x$Response, pch = c(22), col = col3[x$Site], bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))


## Using treatment as a categorical predictor
model1 <- lmer(Response ~ Treatment + (1 + Treatment|Site) , data = x, REML = F)
null <- lmer(Response ~  1 + (1 + Treatment|Site), data = x, REML = F)
anova(null, model1)

## Using treatment as a categorical predictor
model1 <- lmer(Response ~ Treatment + (1|Site) , data = x, REML = F)
null <- lmer(Response ~  1 + (1|Site), data = x, REML = F)
anova(null, model1)
summary(model1)

## When using the untransformed variables
model2 <- lmer((Response) ~ (Cl) + (SO4) + (pH) + (1|Site), data = x, REML = F)
summary(model2)
null2 <- lmer((Response) ~ 1 + (1|Site), data = x, REML = F)
anova(null2, model2)

## when using the log transformed variables
model3 <- lmer((Response) ~ logCl + logSO4 + pH + (1|Site), data = x, REML = F)
summary(model3)
null3 <- lmer((Response) ~ 1 + (1|Site), data = x, REML = F)

anova(null3, model3)

## when using the log transformed variables and scaling them
model4 <- lmer(scale(Response) ~ scale(logCl) + scale(logSO4) + scale(pH) + (1|Site), data = x, REML = F)
summary(model4)
null4 <- lmer(scale(Response) ~ 1 + (1|Site), data = x, REML = F)
anova(null4, model4)


## try dropping out the variables
model4a <- lmer(scale(Response) ~ scale(logCl) + scale(logSO4) + (1|Site), data = x, REML = F)
model4b <- lmer(scale(Response) ~ scale(logCl) + scale(pH) + (1|Site), data = x, REML = F)
model4c <- lmer(scale(Response) ~ scale(logSO4) + scale(pH) + (1|Site), data = x, REML = F)
model4d <- lmer(scale(Response) ~ scale(logCl) + (1|Site), data = x, REML = F)
model4e <- lmer(scale(Response) ~ scale(pH) + (1|Site), data = x, REML = F)
model4f <- lmer(scale(Response) ~ scale(logSO4) + (1|Site), data = x, REML = F)
summary(model4e)
anova(null4, model4)  # full

anova(null4, model4a)  # drop pH
anova(null4, model4b)  # drop SO4
anova(null4, model4c)  # drop Cl
anova(null4, model4d)  # Cl alone
anova(null4, model4e)  # ph alone
anova(null4, model4f)  # SO4 alone

anova(model4, model4d)
summary(model4e)

model4d <- lmer(Response ~ logCl + (1+ logCl|Site), data = x, REML = F)
null4d <- lmer(Response ~ 1 + (1+ logCl|Site), data = x, REML = F)
anova(null4d, model4d)  # Cl alone




## testing for an interaction
model5 <- lmer((Response) ~ logCl * logSO4 * pH + (1|Site), data = x, REML = F)
null5 <- lmer((Response) ~ logCl + logSO4 + pH + (1|Site), data = x, REML = F)
summary(model5)
anova(null5, model5)








## when adding a random slope
model5 <- lmer((Response) ~ logCl + logSO4 + pH + (1+ logCl|Site) + (1+ logSO4|Site), data = x, REML = F)
summary(model5)
null5 <- lmer((Response) ~ 1 + (1 + logCl|Site) + (1+ logSO4|Site), data = x, REML = F)
anova(null5, model5)




## Using LOI as a predictor
## Using treatment as a categorical predictor
model1 <- lmer(Response ~ Treatment + LOI + (1 + Treatment|Site) , data = x, REML = F)
null <- lmer(Response ~  LOI + (1 + Treatment|Site), data = x, REML = F)
anova(null, model1)

## When using the untransformed variables
model2 <- lmer((Response) ~ (Cl) + (SO4) + (pH) + LOI + (1|Site), data = x, REML = F)
null2 <- lmer((Response) ~ LOI + (1|Site), data = x, REML = F)
anova(null2, model2)

## when using the log transformed variables
model3 <- lmer((Response) ~ logCl + logSO4 + pH + LOI + (1|Site), data = x, REML = F)
null3 <- lmer((Response) ~ LOI + (1|Site), data = x, REML = F)
anova(null3, model3)


## when using the log transformed variables
model3 <- lmer((Response) ~ logCl + LOI + (1|Site), data = x, REML = F)
null3 <- lmer((Response) ~ LOI + (1|Site), data = x, REML = F)
anova(null3, model3)

