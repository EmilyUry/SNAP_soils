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
boxplot(pH ~ Treatment*Site, data = x)




## Using treatment as a categorical predictor... no significance
model1 <- lmer(DOC ~ Treatment + (1 + Treatment|Site) , data = x, REML = F)
null <- lmer(DOC ~  1 + (1 + Treatment|Site), data = x, REML = F)
anova(null, model1)

## When using the untransformed variables - the model is significantly different from the null
model2 <- lmer((DOC) ~ (Cl) + (SO4) + (pH) + (1|Site), data = x, REML = F)
summary(model2)
null2 <- lmer((DOC) ~ 1 + (1|Site), data = x, REML = F)
anova(null2, model2)

## when using the log transformed variables, the significance goes away :(
model2 <- lmer((DOC) ~ logCl + logSO4 + pH + (1|Site), data = x, REML = F)
summary(model2)
null2 <- lmer((DOC) ~ 1 + (1|Site), data = x, REML = F)

anova(null2, model2)

## when using the log transformed variables and scaling them ... no difference
model2 <- lmer(scale(DOC) ~ scale(logCl) + scale(logSO4) + scale(pH) + (1|Site), data = x, REML = F)
summary(model2)
null2 <- lmer(scale(DOC) ~ 1 + (1|Site), data = x, REML = F)
anova(null2, model2)


## try dropping out the variables
model2 <- lmer(scale(DOC) ~ scale(logCl) + scale(logSO4) + scale(pH) + (1|Site), data = x, REML = F)

model2a <- lmer(scale(DOC) ~ scale(logCl) + scale(logSO4) + (1|Site), data = x, REML = F)
model2b <- lmer(scale(DOC) ~ scale(logCl) + scale(pH) + (1|Site), data = x, REML = F)
model2c <- lmer(scale(DOC) ~ scale(logSO4) + scale(pH) + (1|Site), data = x, REML = F)
model2d <- lmer(scale(DOC) ~ scale(logCl) + (1|Site), data = x, REML = F)
model2e <- lmer(scale(DOC) ~ scale(pH) + (1|Site), data = x, REML = F)
model2f <- lmer(scale(DOC) ~ scale(logSO4) + (1|Site), data = x, REML = F)


null2 <- lmer(scale(DOC) ~ 1 + (1|Site), data = x, REML = F)
anova(null2, model2)
anova(null2, model2a)
anova(null2, model2b)
anova(null2, model2c)
anova(null2, model2d)
anova(null2, model2e)
anova(null2, model2f)

### NOTHING IS SIGNIFICANT




# try adding a random slope to the full model
model2 <- lmer(scale(DOC) ~ scale(logCl) + (1 + scale(logCl)|Site), data = x, REML = F)
null2 <- lmer(scale(DOC) ~ 1 + (1 + scale(logCl)|Site), data = x, REML = F)

anova(null2, model2)


