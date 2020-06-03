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


### model [DOC] by treatment and depth with Site and ID as a Random Effect

model1 <- lmer(DOC ~ Treatment + (1|Site) + (1|ID), data = x, REML = F)
null <- lmer(DOC ~  1 + (1|Site) + (1|ID), data = x, REML = F)
anova(null, model1)

### we did not find depth to have explanatory power in this model, so we removed it
### treatment also does not explain DOC

model1 <- lmer(DOC ~ logCl + (1|Site) + (1|ID), data = x, REML = F)
null <- lmer(DOC ~ Depth + (1|Site) + (1|ID), data = x, REML = F)
reduced <- lmer(DOC ~ 1 + (1|Site) + (1|ID), data = x, REML = F)
anova(null, model1)



### Same models, now with RANDOM SLOPES

model1 <- lmer(DOC ~ Treatment + Depth + (1+Treatment|Site), data = x, REML = F)
null<- lmer(DOC ~  Depth + (1+Treatment|Site), data = x, REML = F)
anova(null, model1)
anova(null, null.d)

### we did not find depth to have explanatory power in this model, so we removed it
### treatment also does not explain DOC

model1 <- lmer(DOC ~ logCl + (1|Site) + (1|ID), data = x, REML = F)
null <- lmer(DOC ~ Depth + (1|Site) + (1|ID), data = x, REML = F)
reduced <- lmer(DOC ~ 1 + (1|Site) + (1|ID), data = x, REML = F)
anova(null, model1)



anova(reduced, model1)
## chloride does not explain DOC





### model [DOC] by treatment and depth with Site and ID as a Random Effect

model1 <- lmer(DOC ~  pH + (1|Site) + (1|Treatment), data = x, REML = F)
null <- lmer(DOC ~  pH + (1|Site), data = x, REML = F)
anova(null, model1)

### we did not find depth to have explanatory power in this model, so we removed it
### treatment also does not explain DOC

model1 <- lmer(DOC ~ logCl + (1|Site) + (1|ID), data = x, REML = F)
null <- lmer(DOC ~ Depth + (1|Site) + (1|ID), data = x, REML = F)
reduced <- lmer(DOC ~ 1 + (1|Site) + (1|ID), data = x, REML = F)
anova(null, model1)



### FUll
model1 <- lmer(DOC ~ Treatment + pH + Depth + (1+Treatment|Site) + (1|ID), data = x, REML = F)
null <- lmer(DOC ~  1 + pH + Depth + (1|Site) + (1|ID), data = x, REML = F)
anova(null, model1)

## No depth
model1 <- lmer(DOC ~ Treatment + pH  + (1+Treatment|Site) , data = x, REML = F)
null <- lmer(DOC ~  1 + pH  + (1|Site), data = x, REML = F)
anova(null, model1)
summary(null)



### FUll
model1 <- lmer(DOC ~ Treatment + SM + Depth + (1+Treatment|Site) + (1|ID), data = x, REML = F)
null <- lmer(DOC ~  Depth + SM + (1+Treatment|Site) + (1|ID), data = x, REML = F)
anova(null, model1)

## No depth
model1 <- lmer(DOC ~ Treatment + SM  pH + LOI + (1+Treatment|Site) , data = x, REML = F)
null <- lmer(DOC ~  SM  + (1 + Treatment|Site), data = x, REML = F)
anova(null, model1)
summary(null)



